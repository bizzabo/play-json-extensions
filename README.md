Play-Json extensions
==========================

### Sbt play-json >= 2.4

    libraryDependencies += "org.cvogt" %% "play-json-extensions" % "0.3.0"

    // last version for play-json 2.3 was 0.2

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

#### Then use ordinary play-json
    val json = Json.toJson( foo )
    assert(foo == json.as[Foo])

#### Serialize tuples
    import org.cvogt.play.json.tuples._
    val json = Json.parse("""[1,1.0,"Test"]""")
    val res = Json.fromJson[(Int,Double,String)](json)
    assert(JsSuccess((1,1.0,"Test")) === res)

### Option for play-json 2.4

#### implicit Reads
    import org.cvogt.test.play.json.implicits.optionNoError // play 2.3 behavior
    // or
    import org.cvogt.test.play.json.implicits.optionWithNull // play 2.4 suggested behavior

#### automatic option validation: `validateAuto`
    val json = (Json.parse("""{}""") \ "s")
    json.validateAuto[Option[String]] == JsResult(None) // works as expected correctly

    // play-json built-ins
    json.validate[Option[String]] // JsError: "'s' is undefined on object: {}"
    json.validateOpt[String] == JsResult(None) // manual alternative (provided here, built-into play-json >= 2.4.2)
    
### experimental features (will change)

#### automatic formatting of sealed traits of ADTs
    sealed trait SomeAdt
    case object A extends SomeAdt
    final case class X(i: Int, s: String) extends SomeAdt
    object X{
      implicit def jsonFormat: InvariantFormat = Jsonx.formatCaseClass[X]
    }
    object SomeAdt{
      implicit def jsonFormat: InvariantFormat = Jsonx.formatAdt[SomeAdt](AdtEncoder.TypeAsField)
    }

    Json.parse("""A""").as[SomeAdt] == A
    Json.parse("""{"i": 5, "s":"foo", "type": "X"}""").as[SomeAdt] == X(5,"foo")

#### import implicit default formatter for case classes
    import org.cvogt.play.json.implicits.formatCaseClass

#### implicit default as fallback
    object formatters extends org.cvogt.play.json.ImplicitCaseClassFormatDefault{
      // <- more specific formatters here, that take priority over default
    }
    import formatters._

