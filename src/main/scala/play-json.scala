package org.cvogt.play.json
import scala.reflect.macros.blackbox
import play.api.libs.json._
import collection.immutable.ListMap
import scala.annotation.implicitNotFound

package object internals{
  /*
  // this would allow implicitlyOption for primitives. move to scala-extensions
  final case class FetchedFormat[T](format: Option[Format[T]])
  object FetchedFormat{
    implicit def fetch[T](implicit format: Format[T] = null): FetchedFormat[T] = FetchedFormat(Option(format))
  }
  def implicitlyOption[T](implicit ev: FetchedFormat[T]) = ev.format
  */
  /** does not work for primitive types */
  def implicitlyOption[T](implicit ev: T = null): Option[T] = Option(ev)

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
      q"new _root_.org.cvogt.play.json.internals.CaseClass[$T]"
    }
    /**
    fails compilation if T is not a case class
    meaning this can be used as an implicit to check
    */
    implicit def checkCaseClass[T]: CaseClass[T] = macro checkCaseClassMacro[T]
  }

  final class SingletonObject[T]
  object SingletonObject{
    def checkSingletonObjectMacro[T:c.WeakTypeTag](c: blackbox.Context) = {
      import c.universe._
      val T = c.weakTypeOf[T]
      assert(
        T.typeSymbol.isClass && T.typeSymbol.asClass.isModuleClass
      )
      q"new _root_.org.cvogt.play.json.internals.SingletonObject[$T]"
    }
    /**
    fails compilation if T is not a singleton object class
    meaning this can be used as an implicit to check
    */
    implicit def checkSingletonObject[T]: SingletonObject[T] = macro checkSingletonObjectMacro[T]
  }

  import scala.collection._
  import scala.collection.generic.CanBuildFrom
  private [json] implicit class TraversableLikeExtensions[A, Repr](val coll: TraversableLike[A, Repr]) extends AnyVal{
    /** Eliminates duplicates based on the given equivalence function.
    There is no guarantee which elements stay in case element two elements are considered equivalent.
    this has runtime O(n^2)
    @param symmetric comparison function which tests whether the two arguments are considered equivalent. */
    def distinctWith[That](equivalent: (A,A) => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      var l = List[A]()
      val b = bf(coll.repr)
      for (elem <- coll) {
        l.find{
          case first => equivalent(elem,first)
        }.getOrElse{
          l = elem +: l
          b += elem
        }
      }
      b.result
    }
  }
}

import internals._

@implicitNotFound("""could not find implicit value for parameter helper: play.api.libs.json.Reads[${T}]
TRIGGERED BY: could not find implicit value for parameter helper: org.cvogt.play.json.OptionValidationDispatcher[${T}]
TO SOLVE THIS
1. Make sure there is a Reads[${T}] or Format[${T}] in the implicit scope
2. In case of Reads[Option[...]] you need to either
   import org.cvogt.play.json.implicits.optionWithNull // suggested
   or
   import org.cvogt.play.json.implicits.optionNoError // buggy play-json 2.3 behavior
3. In case of Reads[... .type]
   import org.cvogt.play.json.SingletonEncoder.simpleName
   import org.cvogt.play.json.implicits.formatSingleton
""")
final class OptionValidationDispatcher[T] private[json] (val validate: JsLookupResult => JsResult[T]) extends AnyVal

object `package`{
  implicit def nonOptionValidationDispatcher[T:Reads] = {
    new OptionValidationDispatcher(_.validate[T])
  }
  implicit def optionValidationDispatcher[T](implicit reads: Reads[Option[T]]) = {
    new OptionValidationDispatcher(_.validateOpt[T])
  }
  implicit class JsLookupResultExtensions(res: JsLookupResult){
    /**
    properly validate Option and non-Option fields alike
    */
    def validateAuto[T](implicit helper: OptionValidationDispatcher[T]): JsResult[T] = helper.validate(res)
    /**
    Backport of >2.4.1 validateOpt as an extension method
    */
    def validateOpt[T](implicit reads: Reads[Option[T]]): JsResult[Option[T]] = res match {
      case JsUndefined() => JsSuccess(None.asInstanceOf[Option[T]])
      case JsDefined(a) => reads.reads(a)
    }
  }
  implicit class JsValueExtensions(res: JsValue){
    /**
    properly validate Option and non-Option fields alike
    */
    def validateAuto[T:OptionValidationDispatcher]: JsResult[T] = JsDefined(res).validateAuto[T]
    /**
    Backport of >2.4.1 validateOpt as an extension method
    */
    def validateOpt[T](implicit reads: Reads[Option[T]]): JsResult[Option[T]] = JsDefined(res).validateOpt[T]
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

  private def primaryConstructor(tpe: Type): MethodSymbol = {
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor =>
        if(!m.isPublic)
          c.error(c.enclosingPosition, s"Only classes with public primary constructor are supported. Found: $tpe")
        m
    }.get
  }
  private def caseClassFieldsTypes(tpe: Type): ListMap[String, Type] = {
    val paramLists = primaryConstructor(tpe).paramLists
    val params = paramLists.head

    if(paramLists.size > 1)
      c.error(c.enclosingPosition, s"Only one parameter list classes are supported. Found: $tpe")

    params.foreach{
      p => if(!p.isPublic)
        c.error(c.enclosingPosition, s"Only classes with all public constructor arguments are supported. Found: $tpe")
    }

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }

  def formatAuto[T: c.WeakTypeTag]/*(format: Tree)*/: Tree = {
    val T = c.weakTypeOf[T]
    import internals.TraversableLikeExtensions
    def defaultFormatter =
      if( isModuleClass(T) ){
        q"""
          implicit def simpleName = SingletonEncoder.simpleName
          implicits.formatSingleton
        """    
      }else if( isCaseClass(T) && caseClassFieldsTypes(T).size == 1 ){
        val ArgType = caseClassFieldsTypes(T).head._2
        val name = TermName(c.freshName)
        q"""
        implicit def $name = Jsonx.formatAuto[$ArgType]
        Jsonx.formatInline[$T]
        """
      }else if( isCaseClass(T) ){
        val fieldFormatters = caseClassFieldsTypes(T).map{
          case (_,t) => t
        }.toVector.distinctWith(_ =:= _).map{ t =>
            val name = TermName(c.freshName)
            q"implicit def $name = Jsonx.formatAuto[$t]"
        }
        val t = q"""
        ..$fieldFormatters
        Jsonx.formatCaseClass[$T]
        """
        t
      } else if( T.typeSymbol.isClass && T.typeSymbol.asClass.isSealed && T.typeSymbol.asClass.isAbstract ) {
        val fieldFormatters = T.typeSymbol.asClass.knownDirectSubclasses.map{ t =>
          val name = TermName(c.freshName)
          q"implicit def $name = Jsonx.formatAuto[$t]"
        }
        q"""
        ..$fieldFormatters
        Jsonx.formatSealed[$T]
        """
      } else {
        q"implicitly[Format[$T]]" // produces error message if no formatter defined
      }

    val t = q"""
      {
        import $pjson._
        import $pkg._
        internals.implicitlyOption[Format[$T]].getOrElse{
          $defaultFormatter
        }
      }
    """
    //println(t)
    t
  }

  def formatInline[T: c.WeakTypeTag]: Tree = {
    val T = c.weakTypeOf[T]
    val fields = caseClassFieldsTypes(T)
    if(fields.size != 1)
      c.error(c.enclosingPosition, s"class with exactly one argument required, but found: $T")
    val (field,tpe) = fields.head
    q"""
      {
        import $pjson._
        import $pkg._
        new Format[$T]{
          def reads(json: JsValue) = json.validate[$tpe].map(new $T(_))
          def writes(obj: $T) = Json.toJson(obj.${TermName(field)})
        }
      }
    """    
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
            val result = (json \ $k).validateAuto[$t].repath(path)
            (resolved,result) match {
              case (_,result:JsSuccess[_]) => result
              case _ => resolved.flatMap(_ => result)
            }
          }
          """)
    }.unzip
    val jsonFields = caseClassFieldsTypes(T).map{
      case (k,t) => q"""${Constant(k)} -> Json.toJson[$t](obj.${TermName(k)})(implicitly[Writes[$t]])"""
    }

    q"""
      {
        import $pjson._
        import $pkg._
        new Format[$T]{
          def reads(json: JsValue) = {
            ..$mkResults
            val errors = Seq[JsResult[_]](..$results).collect{
              case JsError(values) => values
            }.flatten
            if(errors.isEmpty){
              try{
                JsSuccess(new $T(..${results.map(r => q"$r.get")}))
              } catch {
                case e: _root_.java.lang.IllegalArgumentException =>
                  val sw = new _root_.java.io.StringWriter()
                  val pw = new _root_.java.io.PrintWriter(sw)
                  e.printStackTrace(pw)
                  JsError(play.api.data.validation.ValidationError(sw.toString,e))
              }
            } else JsError(errors)
          }
          def writes(obj: $T) = JsObject(Seq[(String,JsValue)](..$jsonFields).filterNot(_._2 == JsNull))
        }
      }
      """
  }

  private def verifyKnownDirectSubclassesPostTyper( _T: Type, macroCall: String ) = {
    val T = _T.typeSymbol.asClass

    val subs = T.knownDirectSubclasses
    
    // hack to detect breakage of knownDirectSubclasses as suggested in 
    // https://gitter.im/scala/scala/archives/2015/05/05 and
    // https://gist.github.com/retronym/639080041e3fecf58ba9
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    def checkSubsPostTyper = if (subs != T.knownDirectSubclasses)
      c.error(c.macroApplication.pos,
s"""macro call $macroCall happend in a place, where typechecking of $T hasn't been completed yet.
Completion is required in order to find all direct subclasses.
Try moving the call into a separate file, a sibbling package, a separate sbt sub project or else.
"""
      )

    val checkSubsPostTyperTypTree =
      new global.TypeTreeWithDeferredRefCheck()(() => { checkSubsPostTyper ; global.TypeTree(global.NoType) }).asInstanceOf[TypTree]
  }

  private def assertSealedAbstract[T: c.WeakTypeTag]{
    val T = c.weakTypeOf[T].typeSymbol.asClass

    if(!T.isSealed || !T.isAbstract ){
      lazy val modifiers = T.toString.split(" ").dropRight(1).mkString
      c.error(c.enclosingPosition, s"required sealed trait or sealed abstract class, found $modifiers: ${T.fullName}")
    }    
  }

  def formatSingletonImplicit[T: c.WeakTypeTag](encodeSingleton: Tree, ev: Tree): Tree = formatSingleton[T](encodeSingleton)

  def formatSingleton[T: c.WeakTypeTag](encodeSingleton: Tree): Tree = {
    SingletonObject.checkSingletonObjectMacro[T](c)
    val T = c.weakTypeOf[T].typeSymbol.asClass
    val t = q"""
      {
        import $pjson._
        import $pkg._
        val encoded = $encodeSingleton.apply(classOf[$T])
        new Format[$T]{
          def reads(json: JsValue) = {
            if(json == encoded)
              JsSuccess(${T.module})
            else JsError(s"not " + ${T.fullName})            
          }
          def writes(obj: $T) = encoded
        }
      }
      """
    //println(t)
    t
  }

  def formatSealed[T: c.WeakTypeTag]: Tree = {
    assertSealedAbstract[T]

    val T = c.weakTypeOf[T]
    val subs = T.typeSymbol.asClass.knownDirectSubclasses.toVector // toVector for ordering
    
    val writes = subs.map{
      sym => cq"""obj: $sym => Json.toJson[$sym](obj)(implicitly[Format[$sym]])"""
    }

    val reads = subs.map{
      sym => q"""json.validateAuto[$sym]"""
    }.reduce( (l,r) => q"$l orElse $r" )
    
    val t = q"""
      {
        import $pjson._
        import $pkg._
        new Format[$T]{
          type VerifyKnownDirectSubclassesPostTyper = ${verifyKnownDirectSubclassesPostTyper(T: Type, s"formatSealed[$T]")}
          def reads(json: JsValue) = $reads
          def writes(obj: $T) = {
            obj match {
              case ..$writes
              case _ => throw new Exception("formatSealed found unexpected object of type "+${Literal(Constant(T.toString))}+s": $${obj.getClass}$$obj")
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

  protected def isModuleClass(tpe: Type)
    = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isModuleClass
}

object implicits{
  /** very simple optional field Reads that maps "null" to None */
  implicit def optionWithNull[T](implicit rds: Reads[T]): Reads[Option[T]] = Reads.optionWithNull[T]

  /** Stupidly reads a field as an Option mapping any error (format or missing field) to None */
  implicit def optionNoError[A](implicit reads: Reads[A]): Reads[Option[A]] = Reads.optionNoError[A]

  /** Stupidly reads a field as an Option mapping any error (format or missing field) to None */
  implicit def formatSingleton[T](
    implicit encodeSingleton: SingletonEncoder, ev: SingletonObject[T]
  ): Format[T]
    = macro Macros.formatSingletonImplicit[T]
}

import scala.reflect.ClassTag
final case class SingletonEncoder(apply: java.lang.Class[_] => JsValue)
object SingletonEncoder{
  import java.lang.Class
  import scala.reflect.NameTransformer
  def camel2underscore(str: String) = (
    str.take(1)
    ++
    "[0-9A-Z]".r.replaceAllIn(
      str.drop(1),
      "_" + _.group(0).toLowerCase
    )
  )
  def decodeName(name: String) = NameTransformer.decode(name.dropRight(1))
  implicit def simpleName = SingletonEncoder(cls => JsString(decodeName(cls.getSimpleName)))
  implicit def simpleNameLowerCase = SingletonEncoder(cls => JsString(camel2underscore(decodeName(cls.getSimpleName))))
  implicit def simpleNameUpperCase = SingletonEncoder(cls => JsString(camel2underscore(decodeName(cls.getSimpleName)).toUpperCase))
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
  Serialize one member classes such as value classes as their single contained value instead of a wrapping js object.
  */
  def formatInline[T]: Format[T]
    = macro Macros.formatInline[T]

  /**
  Generates a PlayJson Format[T] for a sealed trait that dispatches to Writes of it's concrete subclasses.
  CAREFUL: It uses orElse for Reads in an unspecified order, which can produce wrong results
  in case of ambiguities.
  */
  def formatSealed[T]: Format[T]
    = macro Macros.formatSealed[T]

  /** serializes a singleton object of given type with the given encoder */
  def formatSingleton[T](
    implicit encodeSingleton: SingletonEncoder
  ): Format[T]
    = macro Macros.formatSingleton[T]

  /**
  Fully automatic, recursive formatter generator.
  Recognizes overridden formatters from companion objects or implicit scope
  Does currently only for for case classes, sealed traits, objects and manually defined formatters.
  Automatically, recursively delegates to formatCaseClass, formatSealed, formatInline, formatSingleton, implicitly[Format[...]]
  Note: defaults to inline single-value case classes. Override if required.
  Currently not supported: classes with type arguments including tuples
  */
  def formatAuto[T]: Format[T]
    = macro Macros.formatAuto[T]
}
