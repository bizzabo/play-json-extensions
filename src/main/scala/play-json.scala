package ai.x.play.json
import scala.reflect.macros.blackbox
import _root_.play.api.libs.json._
import collection.immutable.ListMap
import scala.annotation.implicitNotFound
import scala.reflect.NameTransformer

package object internals {
  /*
  // this would allow implicitlyOption for primitives. move to scala-extensions
  final case class FetchedFormat[T](format: Option[Format[T]])
  object FetchedFormat{
    implicit def fetch[T](implicit format: Format[T] = null): FetchedFormat[T] = FetchedFormat(Option(format))
  }
  def implicitlyOption[T](implicit ev: FetchedFormat[T]) = ev.format
  */
  /** does not work for primitive types */
  def implicitlyOption[T]( implicit ev: T = null ): Option[T] = Option( ev )

  /** Type class for case classes
   */
  final class CaseClass[T]
  object CaseClass {
    def checkCaseClassMacro[T: c.WeakTypeTag]( c: blackbox.Context ) = {
      import c.universe._
      val T = c.weakTypeOf[T]
      if ( !T.typeSymbol.isClass || !T.typeSymbol.asClass.isCaseClass ) c.error( c.enclosingPosition, s"$T does not have case modifier" )
      q"new _root_.ai.x.play.json.internals.CaseClass[$T]"
    }
    /** fails compilation if T is not a case class
     *  meaning this can be used as an implicit to check
     */
    implicit def checkCaseClass[T]: CaseClass[T] = macro checkCaseClassMacro[T]
  }

  final class SingletonObject[T]
  object SingletonObject {
    def checkSingletonObjectMacro[T: c.WeakTypeTag]( c: blackbox.Context ) = {
      import c.universe._
      val T = c.weakTypeOf[T]
      if ( !T.typeSymbol.isClass || !T.typeSymbol.asClass.isModuleClass ) c.error( c.enclosingPosition, s"$T is not an object" )
      q"new _root_.ai.x.play.json.internals.SingletonObject[$T]"
    }
    /** fails compilation if T is not a singleton object class
     *  meaning this can be used as an implicit to check
     */
    implicit def checkSingletonObject[T]: SingletonObject[T] = macro checkSingletonObjectMacro[T]
  }

  import scala.collection._
  private[json] implicit class IterableExtensions[A, Repr]( val coll: Iterable[A] ) extends AnyVal {
    /** Eliminates duplicates based on the given equivalence function.
     *  There is no guarantee which elements stay in case element two elements are considered equivalent.
     *  this has runtime O(n^2)
     *
     *  @param equivalent comparison function which tests whether the two arguments are considered equivalent.
     */
    def distinctWith[That]( equivalent: ( A, A ) => Boolean ): Iterable[A] = {
      var l = List[A]()
      val b = Iterable.newBuilder[A]
      b.sizeHint( coll )
      for ( elem <- coll ) {
        l.find {
          case first => equivalent( elem, first )
        }.getOrElse {
          l = elem +: l
          b += elem
        }
      }
      b.result
    }
  }
}

import internals._

@implicitNotFound( """could not find implicit value for parameter helper: play.api.libs.json.Reads[${T}]
TRIGGERED BY: could not find implicit value for parameter helper: ai.x.play.json.OptionValidationDispatcher[${T}]
TO SOLVE THIS
1. Make sure there is a Reads[${T}] or Format[${T}] in the implicit scope
2. In case of Reads[Option[...]] you need to either
   import ai.x.play.json.implicits.optionWithNull // suggested
   or
   import ai.x.play.json.implicits.optionNoError // buggy play-json 2.3 behavior
3. In case of Reads[... .type]
   import ai.x.play.json.SingletonEncoder.simpleName
   import ai.x.play.json.implicits.formatSingleton
""" )
final class OptionValidationDispatcher[T] private[json] ( val validate: JsLookupResult => JsResult[T] ) extends AnyVal

object OptionValidationDispatcher {
  // these methods allow to dispatch via overloading
  // this is required to dispatch when not usign implicit search such as in the implementation of formatAuto
  def dispatch[T]( reads: Reads[T] )( disambiguate: AnyRef = null ): OptionValidationDispatcher[T] = {
    new OptionValidationDispatcher[T]( _.validate[T]( reads ) )
  }
  def dispatch[T]( reads: Reads[T] )(): OptionValidationDispatcher[Option[T]] = {
    new OptionValidationDispatcher[Option[T]]( _.validateOpt[T]( reads ) )
  }

  // these methods allow dispatch via implicit search
  implicit def dispatchNonOption[T: Reads]: OptionValidationDispatcher[T] = {
    new OptionValidationDispatcher[T]( _.validate[T] )
  }
  implicit def dispatchOption[T: Reads]: OptionValidationDispatcher[Option[T]] = {
    new OptionValidationDispatcher[Option[T]]( _.validateOpt[T] )
  }
}

object debugMacro {
  def apply[T]( tree: T ): T = macro Macros.debugMacro
}

object `package` {
  implicit class JsLookupResultExtensions( res: JsLookupResult ) {
    /** properly validate Option and non-Option fields alike */
    def validateAuto[T]( implicit helper: OptionValidationDispatcher[T] ): JsResult[T] = helper.validate( res )
  }
  implicit class JsValueExtensions( res: JsValue ) {
    /** properly validate Option and non-Option fields alike */
    def validateAuto[T]( implicit helper: OptionValidationDispatcher[T] ): JsResult[T] = JsDefined( res ).validateAuto[T]
  }
}

private[json] class Macros( val c: blackbox.Context ) {
  import c.universe._
  val pkg = q"_root_.ai.x.play.json"
  val pjson = q"_root_.play.api.libs.json"

  /** like identity but prints desugared code and tree */
  def debugMacro( tree: Tree ): Tree = {
    println( "code:\n  " + tree )
    println( "Tree:\n  " + showRaw( tree ) )
    tree
  }

  private def primaryConstructor( tpe: Type ): MethodSymbol = {
    tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor =>
        if ( !m.isPublic )
          c.error( c.enclosingPosition, s"Only classes with public primary constructor are supported. Found: $tpe" )
        m
    }.get
  }
  private def caseClassFieldsTypes( tpe: Type ): ListMap[String, Type] = {
    val paramLists = primaryConstructor( tpe ).paramLists
    val params = paramLists.head

    if ( paramLists.size > 1 )
      c.error( c.enclosingPosition, s"Only one parameter list classes are supported. Found: $tpe" )

    params.foreach {
      p =>
        if ( !p.isPublic )
          c.error( c.enclosingPosition, s"Only classes with all public constructor arguments are supported. Found: $tpe" )
    }

    ListMap( params.map { field =>
      (
        field.name.toTermName.decodedName.toString,
        field.infoIn( tpe ) )
    }: _* )
  }
  private def caseClassFieldsDefaults( tpe: Type ): ListMap[String, Option[Tree]] = {
    if ( tpe.companion == NoType ) {
      ListMap()
    } else {
      ListMap( tpe.companion.member( TermName( "apply" ) ).asTerm.alternatives.find( _.isSynthetic ).get.asMethod.paramLists.flatten.zipWithIndex.map {
        case ( field, i ) =>
          (
            field.name.toTermName.decodedName.toString,
            {
              val method = TermName( s"apply$$default$$${i + 1}" )
              tpe.companion.member( method ) match {
                case NoSymbol => None
                case _        => Some( q"${tpe.typeSymbol.companion}.$method" )
              }
            }
          )
      }: _* )
    }
  }

  def formatAuto[T: c.WeakTypeTag]( encoder: Tree ): Tree = formatAutoInternal( c.weakTypeOf[T], encoder )
  def formatAutoInternal( T: Type, encoder: Tree ): Tree = {
    import internals.IterableExtensions
    def defaultFormatter =
      if ( T <:< typeOf[Option[_]] ) {
        val s = T.typeArgs.head
        q"""
          Format.optionWithNull(${formatAutoInternal( s, encoder )})
        """
      } else if ( isModuleClass( T ) ) {
        q"""
          implicit def simpleName = SingletonEncoder.simpleName
          implicits.formatSingleton
        """
      } else if ( isCaseClass( T ) && caseClassFieldsTypes( T ).size == 1 ) {
        val ArgType = caseClassFieldsTypes( T ).head._2
        val name = TermName( c.freshName )
        q"""
        implicit def $name = $pkg.Jsonx.formatAuto[$ArgType]
        $pkg.Jsonx.formatInline[$T]
        """
      } else if ( isCaseClass( T ) ) {
        val fieldFormatters = caseClassFieldsTypes( T ).map {
          case ( _, t ) => t
        }.toVector.distinctWith( _ =:= _ ).map { t =>
          val name = TermName( c.freshName )
          q"implicit def $name = $pkg.Jsonx.formatAuto[$t]"
        }
        val t = q"""
        ..$fieldFormatters
        $pkg.Jsonx.formatCaseClass[$T]
        """
        t
      } else if ( T.typeSymbol.isClass && T.typeSymbol.asClass.isSealed && T.typeSymbol.asClass.isAbstract ) {
        val fieldFormatters = T.typeSymbol.asClass.knownDirectSubclasses.map { t =>
          val name = TermName( c.freshName )
          q"implicit def $name = Jsonx.formatAuto[$t]"
        }
        q"""
        ..$fieldFormatters
        $pkg.Jsonx.formatSealed[$T]
        """
      } else {
        q"implicitly[Format[$T]]" // produces error message if no formatter defined
      }

    q"""
      {
        import $pjson.{ Format }
        implicit val encoder: NameEncoder = $encoder
        $pkg.internals.implicitlyOption[Format[$T]].getOrElse{
          $defaultFormatter
        }
      }
    """
  }

  private def illegalArgExceptionToJsError( inner: Tree ): Tree = {
    q"""
      {
        import $pjson.{ JsSuccess, JsError, JsonValidationError }
        try { JsSuccess($inner) }
        catch {
          case e: _root_.java.lang.IllegalArgumentException =>
            val sw = new _root_.java.io.StringWriter()
            val pw = new _root_.java.io.PrintWriter(sw)
            e.printStackTrace(pw)
            JsError(JsonValidationError(sw.toString,e))
        }
      }
     """
  }

  def formatInline[T: c.WeakTypeTag]: Tree = {
    val T = c.weakTypeOf[T]
    val fields = caseClassFieldsTypes( T )
    if ( fields.size != 1 )
      c.error( c.enclosingPosition, s"class with exactly one argument required, but found: $T" )
    val ( field, tpe ) = fields.head
    q"""
      {
        import $pjson.{ Json, Format, JsValue }
        new Format[$T]{
          def reads(json: JsValue) = json.validate[$tpe].flatMap(field => ${illegalArgExceptionToJsError( q"new $T(field)" )})
          def writes(obj: $T) = Json.toJson(obj.${TermName( field )})
        }
      }
    """
  }

  def formatCaseClassUseDefaults[T: c.WeakTypeTag]( ev: Tree, encoder: Tree ): Tree = formatCaseClassInternal[T]( ev, encoder, useDefaults = true )

  def formatCaseClass[T: c.WeakTypeTag]( ev: Tree, encoder: Tree ): Tree = formatCaseClassInternal[T]( ev, encoder, useDefaults = false )

  private def formatCaseClassInternal[T: c.WeakTypeTag]( ev: Tree, encoder: Tree, useDefaults: Boolean ): Tree = {
    val T = c.weakTypeOf[T]
    if ( !isCaseClass( T ) )
      c.error( c.enclosingPosition, s"not a case class: $T" )
    val defaults = caseClassFieldsDefaults( T )
    def orDefault( t: Tree, name: String ) = {
      val default = defaults.get( name ).flatten
      default.filter( _ => useDefaults ).map( d => q"$t orElse $pjson.JsSuccess($d)" ).getOrElse( t )
    }
    val ( results, mkResults ) = caseClassFieldsTypes( T ).map {
      case ( k, t ) =>
        val name = TermName( c.freshName )
        val path = q"""(json \ (encoder.encode($k)))"""
        val result = q"""{
          import $pkg._
          bpath.validateAuto[$t].repath(path)
        }"""
        // FIXME: the below needs cleanup
        ( name, q"""val $name: JsResult[$t] = {
            val bpath = $path
            val path = ($pjson.JsPath() \ $k)
            val resolved = path.asSingleJsResult(json)
            val result = if(bpath.isInstanceOf[$pjson.JsDefined]) $result else ${orDefault( result, k )}
            (resolved,result) match {
              case (_,result:$pjson.JsSuccess[_]) => result
              case _ => resolved.flatMap(_ => result)
            }
          }
          """ )
    }.unzip
    val jsonFields = caseClassFieldsTypes( T ).map {
      case ( k, t ) => q"""${Constant( NameTransformer.decode( k ) )} -> $pjson.Json.toJson[$t](obj.${TermName( NameTransformer.encode( k ) )})(implicitly[$pjson.Writes[$t]])"""
    }

    q"""
      {
        import $pjson.{ OFormat, JsValue, JsResult, JsError, JsObject, JsNull }
        new OFormat[$T]{
          def reads(json: JsValue) = {
            ..$mkResults
            val errors = Seq[JsResult[_]](..$results).collect{
              case JsError(values) => values
            }.flatten
            if(errors.isEmpty){
              ${illegalArgExceptionToJsError( q"""new $T(..${results.map( r => q"$r.get" )})""" )}
            } else JsError(errors)
          }
          def writes(obj: $T) = JsObject(Seq[(String,JsValue)](..$jsonFields)
            .filterNot(_._2 == JsNull)
            .map({case (name, value) => ((encoder.encode(name)), value)}))
        }
      }
      """
  }

  private def verifyKnownDirectSubclassesPostTyper( _T: Type, macroCall: String ): Tree = {
    val T = _T.typeSymbol.asClass

    val subs = T.knownDirectSubclasses

    // hack to detect breakage of knownDirectSubclasses as suggested in
    // https://gitter.im/scala/scala/archives/2015/05/05 and
    // https://gist.github.com/retronym/639080041e3fecf58ba9
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    def checkSubsPostTyper = if ( subs != T.knownDirectSubclasses )
      c.error(
        c.macroApplication.pos,
        s"""macro call $macroCall happened in a place, where typechecking of $T hasn't been completed yet.
Completion is required in order to find all direct subclasses.
Try moving the call lower in the file, into a separate file, a sibbling package, a separate sbt sub project or else.
This is caused by https://issues.scala-lang.org/browse/SI-7046 and can only be avoided by manually moving the call.
"""
      )

    val checkSubsPostTyperTypTree =
      new global.TypeTreeWithDeferredRefCheck()( () => { checkSubsPostTyper; global.TypeTree( global.NoType ) } ).asInstanceOf[TypTree]
    q"type VerifyKnownDirectSubclassesPostTyper = $checkSubsPostTyperTypTree"
  }

  private def assertClass[T: c.WeakTypeTag]( msg: String = s"required class or trait" ) = {
    val T = c.weakTypeOf[T].typeSymbol
    if ( !T.isClass ) {
      c.error( c.enclosingPosition, msg + ", found " + T )
    }
  }

  private def assertSealedAbstract[T: c.WeakTypeTag] = {
    assertClass[T]()
    val T = c.weakTypeOf[T].typeSymbol.asClass
    if ( !T.isSealed || !T.isAbstract ) {
      lazy val modifiers = T.toString.split( " " ).dropRight( 1 ).mkString
      c.error( c.enclosingPosition, s"required sealed trait or sealed abstract class, found $modifiers ${T.fullName}" )
    }
  }

  def formatSingletonImplicit[T: c.WeakTypeTag]( encodeSingleton: Tree, ev: Tree ): Tree = formatSingleton[T]( encodeSingleton )

  def formatSingleton[T: c.WeakTypeTag]( encodeSingleton: Tree ): Tree = {
    SingletonObject.checkSingletonObjectMacro[T]( c )
    val T = c.weakTypeOf[T].typeSymbol.asClass
    val t = q"""
      {
        import $pjson.{ Format, JsError, JsSuccess, JsValue }
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

  def formatSealed[T: c.WeakTypeTag, FormatT <: Format[T]: c.WeakTypeTag]: Tree = formatSealedInternal[T, FormatT]( None )
  def formatSealedWithFallback[T: c.WeakTypeTag, Fallback <: T: c.WeakTypeTag, FormatT <: Format[T]: c.WeakTypeTag]: Tree = formatSealedInternal[T, FormatT]( Some( c.weakTypeOf[Fallback].typeSymbol.asType ) )
  def formatSealedInternal[T: c.WeakTypeTag, FormatT <: Format[T]: c.WeakTypeTag]( fallback: Option[TypeSymbol] ): Tree = {
    assertSealedAbstract[T]

    val formatClass = c.weakTypeOf[FormatT].typeSymbol

    val T = c.weakTypeOf[T]
    val subs =
      T.typeSymbol
        .asClass
        .knownDirectSubclasses
        .toVector

    if ( subs.isEmpty )
      c.error( c.enclosingPosition, s"""
No child classes found for $T. If there clearly are child classes,
try moving the call into a separate file, a sibbling package, a separate sbt sub project or else.
This can be caused by https://issues.scala-lang.org/browse/SI-7046 which can only be avoided by manually moving the call.
      """ )

    val writes = subs.map {
      sym => cq"""obj: $sym => implicitly[$pjson.$formatClass[$sym]].writes(obj)"""
    }

    val reads = subs
      // don't include fallback
      .filterNot( t => fallback.map( _.toType =:= t.asType.toType ).getOrElse( false ) )
      .map { sym =>
        q"""{
        import $pkg._
        json.validateAuto[$sym]
      }"""
      }
      .reduce( ( l, r ) => q"$l orElse $r" )

    // add fallback last
    val readsWithFallback = fallback.map( f => q"""{
      import $pkg._
      $reads orElse json.validateAuto[$f]
    }""" ) getOrElse reads

    val rootName = Literal( Constant( T.toString ) )
    val subNames = Literal( Constant( subs.map( _.fullName ).mkString( ", " ) ) )

    q"""
      {
        new $pjson.$formatClass[$T]{
          ${verifyKnownDirectSubclassesPostTyper( T: Type, s"formatSealed[$T, $pjson.$formatClass[$T]" )}
          def reads(json: $pjson.JsValue) = $readsWithFallback orElse $pjson.JsError("Could not deserialize to any of the subtypes of "+ $rootName +". Tried: "+ $subNames)
          def writes(obj: $T) = {
            obj match {
              case ..$writes
              case _ => throw new Exception("formatSealed found unexpected object of type "+${Literal( Constant( T.toString ) )}+s": $${obj.getClass}$$obj")
            }
          }
        }
      }
      """
  }

  protected def isCaseClass( tpe: Type ) = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass

  protected def isModuleClass( tpe: Type ) = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isModuleClass
}

object implicits {
  /** very simple optional field Reads that maps "null" to None */
  implicit def optionWithNull[T]( implicit rds: Reads[T] ): Reads[Option[T]] = Reads.optionWithNull[T]

  /** Stupidly reads a field as an Option mapping any error (format or missing field) to None */
  implicit def optionNoError[A]( implicit reads: Reads[A] ): Reads[Option[A]] = Reads.optionNoError[A]

  /** Stupidly reads a field as an Option mapping any error (format or missing field) to None */
  implicit def formatSingleton[T](
    implicit
    encodeSingleton: SingletonEncoder, ev: SingletonObject[T]
  ): Format[T] = macro Macros.formatSingletonImplicit[T]
}

final case class SingletonEncoder( apply: java.lang.Class[_] => JsValue )
object SingletonEncoder {
  import scala.reflect.NameTransformer
  def camel2underscore( str: String ): String = JsonNaming.SnakeCase( str )
  def decodeName( name: String ) = NameTransformer.decode( name.dropRight( 1 ) )
  implicit def simpleName = SingletonEncoder( cls => JsString( decodeName( cls.getSimpleName ) ) )
  implicit def simpleNameLowerCase = SingletonEncoder( cls => JsString( camel2underscore( decodeName( cls.getSimpleName ) ) ) )
  implicit def simpleNameUpperCase = SingletonEncoder( cls => JsString( camel2underscore( decodeName( cls.getSimpleName ) ).toUpperCase ) )
}

sealed trait NameEncoder {
  def encode( str: String ): String
}

case class BaseNameEncoder() extends NameEncoder {
  override def encode( str: String ): String = str
}

case class CamelToSnakeNameEncoder() extends NameEncoder {
  override def encode( str: String ): String = JsonNaming.SnakeCase( str )
}

object Encoders {
  implicit val encoder: NameEncoder = BaseNameEncoder()
}

object Jsonx {
  /** Generates a PlayJson Format[T] for a case class T with any number of fields (>22 included)
   */
  def formatCaseClass[T]( implicit ev: CaseClass[T], encoder: NameEncoder ): OFormat[T] = macro Macros.formatCaseClass[T]

  /** Generates a PlayJson Format[T] for a case class T with any number of fields (>22 included)
   *  Uses default values when fields are not found
   */
  def formatCaseClassUseDefaults[T]( implicit ev: CaseClass[T], encoder: NameEncoder ): OFormat[T] = macro Macros.formatCaseClassUseDefaults[T]

  /** Serialize one member classes such as value classes as their single contained value instead of a wrapping js object.
   */
  def formatInline[T]: Format[T] = macro Macros.formatInline[T]

  /** Generates a PlayJson Format[T] for a sealed trait that dispatches to Writes of it's concrete subclasses.
   *  CAREFUL: It uses orElse for Reads in an unspecified order, which can produce wrong results
   *  in case of ambiguities.
   */
  def formatSealed[T]: Format[T] = macro Macros.formatSealed[T, Format[T]]

  /** Generates a PlayJson Format[T] for a sealed trait that dispatches to Writes of it's concrete subclasses.
   *  Uses provided type Fallback as the last resort. Fallback needs to be a subtype of T
   *  and ideally: case class Fallback(json: JsValue) extend T
   *  and using formatInline[Fallback] as the serializer
   *  CAREFUL: It uses orElse for Reads in an unspecified order, which can produce wrong results
   *  in case of ambiguities.
   */
  def formatSealedWithFallback[T, Fallback <: T]: Format[T] = macro Macros.formatSealedWithFallback[T, Fallback, Format[T]]

  /** Generates a PlayJson OFormat[T] for a sealed trait that dispatches to Writes of it's concrete subclasses.
   *  CAREFUL: It uses orElse for Reads in an unspecified order, which can produce wrong results
   *  in case of ambiguities.
   */
  def oFormatSealed[T]: OFormat[T] = macro Macros.formatSealed[T, OFormat[T]]

  /** Generates a PlayJson OFormat[T] for a sealed trait that dispatches to Writes of it's concrete subclasses.
   *  Uses provided type Fallback as the last resort. Fallback needs to be a subtype of T
   *  and ideally: case class Fallback(json: JsValue) extend T
   *  and using formatInline[Fallback] as the serializer
   *  CAREFUL: It uses orElse for Reads in an unspecified order, which can produce wrong results
   *  in case of ambiguities.
   */
  def oFormatSealedWithFallback[T, Fallback <: T]: OFormat[T] = macro Macros.formatSealedWithFallback[T, Fallback, OFormat[T]]

  /** serializes a singleton object of given type with the given encoder */
  def formatSingleton[T](
    implicit
    encodeSingleton: SingletonEncoder
  ): Format[T] = macro Macros.formatSingleton[T]

  /** Fully automatic, recursive formatter generator.
   *  Recognizes overridden formatters from companion objects or implicit scope
   *  Does currently only for for case classes, sealed traits, objects and manually defined formatters.
   *  Automatically, recursively delegates to formatCaseClass, formatSealed, formatInline, formatSingleton, implicitly[ Format[...] ]
   *  Note: defaults to inline single-value case classes. Override if required.
   *  Currently not supported: classes with type arguments including tuples
   */
  def formatAuto[T]( implicit encoder: NameEncoder ): Format[T] = macro Macros.formatAuto[T]
}
