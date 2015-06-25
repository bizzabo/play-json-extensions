package org.cvogt.play.json

import scala.reflect.macros.blackbox
import play.api.libs.json._
import collection.immutable.ListMap

sealed trait AdtEncoder
object AdtEncoder{
  trait TypeTagAdtEncoder extends AdtEncoder{
    import scala.reflect.runtime.universe.TypeTag
    def extractClassJson[T: TypeTag](json: JsObject): Option[JsObject]
    def encodeObject[T: TypeTag]: JsValue
    def encodeClassType[T: TypeTag](json: JsObject): JsObject
  }

  trait ClassTagAdtEncoder extends AdtEncoder{
    import scala.reflect.ClassTag
    def extractClassJson[T: ClassTag](json: JsObject): Option[JsObject]
    def encodeObject[T: ClassTag]: JsValue
    def encodeClassType[T: ClassTag](json: JsObject): JsObject
  }

  object TypeAsField extends ClassTagAdtEncoder{
    import scala.reflect._
    def extractClassJson[T: ClassTag](json: JsObject) = {
      Some(json).filter(
        _ \ "type" == JsString(classTag[T].runtimeClass.getSimpleName)
      )
    }

    def encodeObject[T: ClassTag] = {
      JsString(classTag[T].runtimeClass.getSimpleName.dropRight(1))
    }
      
    def encodeClassType[T: ClassTag](json: JsObject) = {
      json ++ JsObject(Seq("type" -> JsString(classTag[T].runtimeClass.getSimpleName)))
    }
  }
}

private[json] class Macros(val c: blackbox.Context){
  import c.universe._
  val pkg = q"_root_.org.cvogt.play.json"
  val pjson = q"_root_.play.api.libs.json"

  /**
  Generates a list of all known classes and traits in an inheritance tree.
  Includes the given class itself.
  Does not include subclasses of non-sealed classes and traits.
  TODO: move this to scala-extensions
  */
  private def knownTransitiveSubclasses(sym: ClassSymbol): Seq[ClassSymbol] = {
    sym +: (
      if(sym.isModuleClass){
        Seq()
      } else {      
        sym.knownDirectSubclasses.flatMap(s => knownTransitiveSubclasses(s.asClass))
      }
    ).toSeq
  }

  private def caseClassFieldsTypes(tpe: Type): ListMap[String, Type] = {
    val params = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }

  def formatCaseClass[T: c.WeakTypeTag](ev: Tree): Tree = {
    val T = c.weakTypeOf[T]
    if(!isCaseClass(T))
      c.error(c.enclosingPosition, s"not a case class: $T")
    val (results,mkResults) = caseClassFieldsTypes(T).map{
      case (k,t) =>
        val name = TermName(c.freshName)
        (name, q"""val $name: JsResult[$t] = {
            val path = (JsPath() \ $k)
            val resolved = path.asSingleJsResult(json)
            val result = (json \ $k).validate[$t].repath(path)
            (resolved,result) match {
              case (_,result:JsSuccess[_]) => result
              case _ => resolved.flatMap(_ => result)
            }
          }
          """)
    }.unzip
    val jsonFields = caseClassFieldsTypes(T).map{
      case (k,_) => q"""${Constant(k)} -> Json.toJson(obj.${TermName(k)})"""
    }

    q"""
      {
        import $pjson._
        new Format[$T]{ 
          def reads(json: JsValue) = {
            ..$mkResults
            val errors = Seq[JsResult[_]](..$results).collect{
              case JsError(values) => values
            }.flatten
            if(errors.isEmpty){
              JsSuccess(new $T(..${results.map(r => q"$r.get")}))
            } else JsError(errors)
          }
          def writes(obj: $T) = JsObject(Seq[(String,JsValue)](..$jsonFields).filterNot(_._2 == JsNull))
        }
      }
      """
  }

  def formatAdt[T: c.WeakTypeTag](encoder: Tree): Tree = {
    val T = c.weakTypeOf[T].typeSymbol.asClass

    val allSubs = knownTransitiveSubclasses(T)
    
    allSubs.foreach{ sym =>
      lazy val modifiers = sym.toString.split(" ").dropRight(1).mkString
      if(!sym.isFinal && !sym.isSealed && !sym.isModuleClass ){
        c.error(c.enclosingPosition, s"required sealed or final, found $modifiers: ${sym.fullName}")
      }
      if(!sym.isCaseClass && !sym.isAbstract){
        c.error(c.enclosingPosition, s"required abstract, trait or case class, found $modifiers: ${sym.fullName}")
      }
    }

    val concreteSubs = allSubs.filterNot(_.isAbstract)
    
    // hack to detect breakage of knownDirectSubclasses as suggested in 
    // https://gitter.im/scala/scala/archives/2015/05/05 and
    // https://gist.github.com/retronym/639080041e3fecf58ba9
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    def checkSubsPostTyper = if (allSubs != knownTransitiveSubclasses(T))
      c.error(c.macroApplication.pos,
s"""macro call formatAdt[$T] happend in a place, where typechecking of $T hasn't been completed yet.
Completion is required in order to find all subclasses (using .knownDirectSubclasses transitively).
Try moving the call into a separate file, a sibbling package, a separate sbt sub project or else.
"""
      )

    val checkSubsPostTyperTypTree =
      new global.TypeTreeWithDeferredRefCheck()(() => { checkSubsPostTyper ; global.TypeTree(global.NoType) }).asInstanceOf[TypTree]

    val isModuleJson = (sym: ClassSymbol) =>
      q"""
        (json: JsValue) => {
          Some(json).filter{
            _ == $encoder.encodeObject[$sym]
          }
        }
      """

    val writes = concreteSubs.map{
      sym => if(sym.isModuleClass){
        cq"`${TermName(sym.name.decodedName.toString)}` => $encoder.encodeObject[$sym]"//
      } else {
        assert(sym.isCaseClass)
        cq"""obj: $sym => {
          $encoder.encodeClassType[$sym](
            Json.toJson[$sym](obj).as[JsObject]
          )
        }
        """
      }
    }

    val (extractors, reads) = concreteSubs.map{
      sym =>
        val name = TermName(c.freshName)
        (
          {
            val extractor = if(sym.isModuleClass){
              q"""
                (json: JsValue) =>
                  ${isModuleJson(sym)}(json).map(_ => JsSuccess(${sym.asClass.module}))
              """
            } else {
              assert(sym.isCaseClass)
              q"""
                (json: JsValue) =>
                  $encoder.extractClassJson[${sym}](json.as[JsObject])
                          .map(Json.fromJson[$sym](_))
              """
            }
            q"object $name extends $pkg.Extractor($extractor)"
          },
          cq"""$name(json) => json"""
        )
    }.unzip
    
    val t = q"""
      {
        import $pjson._
        ..$extractors
        new Format[$T]{
          type T = $checkSubsPostTyperTypTree;
          def reads(json: JsValue) = {
            json match {
              case ..$reads
              case _ => throw new Exception("formatAdt failed to read as type "+${Literal(Constant(T.toString))}+s": $${json.getClass}$$json")
            }
          }
          def writes(obj: $T) = {
            obj match {
              case ..$writes
              case _ => throw new Exception("formatAdt failed to write as type "+${Literal(Constant(T.toString))}+s": $${obj.getClass}$$obj")
            }
          }
        }
      }
      """
    //println(t)
    t
  }

  protected def isCaseClass(tpe: Type)
    = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass
}
trait ImplicitCaseClassFormatDefault{
  implicit def formatCaseClass[T]
    (implicit ev: CaseClass[T])
    : Format[T] = macro Macros.formatCaseClass[T]
}
object implicits extends ImplicitCaseClassFormatDefault

class Extractor[T,R](f: T => Option[R]){
  def unapply(arg: T): Option[R] = f(arg)
}

/**
Type class for case classes
*/
final class CaseClass[T]
object CaseClass{
  def checkCaseClassMacro[T:c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._
    val T = c.weakTypeOf[T]
    assert(
      T.typeSymbol.isClass && T.typeSymbol.asClass.isCaseClass
    )
    q"new CaseClass[$T]"
  }
  /**
  fails compilation if T is not a case class
  meaning this can be used as an implicit to check that a type is a case class
  */
  implicit def checkCaseClass[T]: CaseClass[T] = macro checkCaseClassMacro[T]
}

object Jsonx{

  /**
  Generates a PlayJson Format[T] for a case class T with any number of fields (>22 included)
  */
  def formatCaseClass[T]
    (implicit ev: CaseClass[T])
    : Format[T]
    = macro Macros.formatCaseClass[T]

  /**
  Generates a PlayJson Format[T] for a sealed trait that only has case object children
  */
  def formatAdt[T](encoder: AdtEncoder): Format[T]
    = macro Macros.formatAdt[T]
}
