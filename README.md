Play-Json extensions
==========================

### Sbt

    libraryDependencies += "org.cvogt" %% "play-json-extensions" % "0.1"

### Serialize case classes of arbitrary size (23+ fields allowed)

    case class Foo(
      _1:Int,_2:Int,_3:Int,_4:Int,_5:Int,
      _21:Int,_22:Int,_23:Int,_24:Int,_25:Int,
      _31:Int,_32:Int,_33:Int,_34:Int,_35:Int,
      _41:Int,_42:Int,_43:Int,_44:Int,_45:Int,
      _51:Int,_52:Int,_53:Int,_54:Int,_55:Int
    )

    val foo = Foo(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    

#### Create explicit formatter
    import org.cvogt.play.json.Jsonx
    implicit def jsonFormat = Jsonx.formatCaseClass[Foo]

#### or import implicit default formatter
    import org.cvogt.play.json.implicits.formatCaseClass

#### or use implicit default as fallback
    object formatters extends org.cvogt.play.json.ImplicitCaseClassFormatDefault{
      // <- more specific formatters here, that take priority over default
    }
    import formatters._

#### Then use ordinary play-json
    val json = Json.toJson( foo )
    assert(foo == json.as[Foo])
