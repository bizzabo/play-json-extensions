Play-Json extensions
==========================

### latest versions

**For Scala 2.11.x, 2.12.x, and 2.13.x**

play-json 2.7.x: `libraryDependencies += "ai.x" %% "play-json-extensions" % "0.40.2"`

**For 2.11.x only**

play-json 2.5.x: `libraryDependencies += "ai.x" %% "play-json-extensions" % "0.9.0"`

play-json 2.4.x: `libraryDependencies += "org.cvogt" %% "play-json-extensions" % "0.6.1"`

play-json 2.3.x:
`libraryDependencies += "org.cvogt" %% "play-json-extensions" % "0.2"`

(if you were using formatAdt or InvariantFormat you may want to upgrade to 0.4.0 first and then to 0.6.0)

### all versions and scaladoc

See https://www.javadoc.io/doc/ai.x/play-json-extensions_2.11/

(for older versions https://www.javadoc.io/doc/org.cvogt/play-json-extensions_2.11/)

### De-/Serialize case classes of arbitrary size (23+ fields allowed)

```scala
    case class Foo(
      _1:Int,_2:Int,_3:Int,_4:Int,_5:Int,
      _21:Int,_22:Int,_23:Int,_24:Int,_25:Int,
      _31:Int,_32:Int,_33:Int,_34:Int,_35:Int,
      _41:Int,_42:Int,_43:Int,_44:Int,_45:Int,
      _51:Int,_52:Int,_53:Int,_54:Int,_55:Int
    )

    val foo = Foo(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
```

#### Create explicit formatter

```scala
    import ai.x.play.json.Jsonx
    implicit lazy val jsonFormat = Jsonx.formatCaseClass[Foo]

    // if your case class uses Option make sure you import
    // one of the below implicit Option Reads to avoid
    // "could not find implicit value for parameter helper: ai.x.play.json.OptionValidationDispatcher"

    // note: formatCaseClass catches IllegalArgumentException and turns them into JsError enclosing the stack trace as the message
    // this allows using require(...) in class constructors and still get JsErrors out of serialization
```

#### Then use ordinary play-json

```scala
    val json = Json.toJson( foo )
    assert(foo == json.as[Foo])
```

#### deserialization uses default values

```scala
    case class Bar(s: String, i: Int = 6)
    implicit lazy val format = Jsonx.formatCaseClassUseDefaults[Bar]
    assert(Bar("asd",6) == Json.parse("""{"s":"asd"}""").validate[Bar].get)
```
  
#### De-/Serialize tuples

```scala
    import ai.x.play.json.tuples._
    val json = Json.parse("""[1,1.0,"Test"]""")
    val res = Json.fromJson[(Int,Double,String)](json)
    assert(JsSuccess((1,1.0,"Test")) === res)
```

#### De-/Serialize single value classes

```scala
    case class Foo(i: Int)
    val json = Json.parse("1")
    val res = Json.fromJson[Foo](json)
    assert(JsSuccess(Foo(1)) === res)
```

### Option for play-json 2.4

#### implicit Option Reads

```scala
    import ai.x.play.json.implicits.optionWithNull // play 2.4 suggested behavior
    // or
    import ai.x.play.json.implicits.optionNoError // play 2.3 behavior
```

#### automatic option validation: `validateAuto`

```scala
    val json = (Json.parse("""{}""") \ "s")
    json.validateAuto[Option[String]] == JsResult(None) // works as expected correctly

    // play-json built-ins
    json.validate[Option[String]] // JsError: "'s' is undefined on object: {}"
    json.validateOpt[String] == JsResult(None) // manual alternative (provided here, built-into play-json >= 2.4.2)
```
    
#### automatic formatting of sealed traits, delegating to formatters of the subclasses
#### formatSealed uses orElse of subclass Reads in random order, careful in case of ambiguities of field-class correspondances

```scala
    sealed trait SomeAdt
    case object A extends SomeAdt
    final case class X(i: Int, s: String) extends SomeAdt
    object X{
      implicit lazy val jsonFormat: Format[X] = Jsonx.formatCaseClass[X]
    }
    object SomeAdt{
      import ai.x.play.json.SingletonEncoder.simpleName  // required for formatSingleton
      import ai.x.play.json.implicits.formatSingleton    // required if trait has object children
      implicit lazy val jsonFormat: Format[SomeAdt] = Jsonx.formatSealed[SomeAdt]
    }

    Json.parse("""A""").as[SomeAdt] == A
    Json.parse("""{"i": 5, "s":"foo", "type": "X"}""").as[SomeAdt] == X(5,"foo")
```

#### formatSealedWithFallback[A,B <: A] is like formatSealed but provides a fallback for unknown cases.
#### It makes sure the class B is tried last, which allows it to be permissive enough for a fallback.
#### This allows graceful schema evolution without breaking old readers. Example:

```scala
    sealed trait SomeAdt
    final case class X(i: Int, s: String) extends SomeAdt
    final case class Unknown(json: JsValue) extends SomeAdt
    object SomeAdt{
      implicit lazy val jsonFormat: Format[SomeAdt] = {
        implicit lazy val XFormat: Format[X] = Jsonx.formatCaseClass[X]
        implicit lazy val UnknownFormat: Format[Unknown] = Jsonx.formatInline[Unknown]
        Jsonx.formatSealedWithFallback[SomeAdt]
      }
    }

    Json.parse("""{"i": 5, "s":"foo", "type": "X"}""").as[SomeAdt] == X(5,"foo")
    val unknownJson = Json.parse("""{"x":"y"}""")
    unknownJson.as[Unknown] == Unknown(unknownJson)
```

#### formatSealedWithFallback[A,B <: A] is like formatSealed but provides a fallback for unknown cases.
#### It makes sure the class B is tried last, which allows it to be permissive enough for a fallback.
#### This allows graceful schema evolution without breaking old readers. Example:

```scala
    sealed trait SomeAdt
    final case class X(i: Int, s: String) extends SomeAdt
    final case class Unknown(json: JsValue) extends SomeAdt
    object SomeAdt{
      implicit lazy val jsonFormat: Format[SomeAdt] = {
        implicit lazy val XFormat: Format[X] = Jsonx.formatCaseClass[X]
        implicit lazy val UnknownFormat: Format[Unknown] = Jsonx.formatInline[Unknown]
        Jsonx.formatSealedWithFallback[SomeAdt]
      }
    }

    Json.parse("""{"i": 5, "s":"foo", "type": "X"}""").as[SomeAdt] == X(5,"foo")
    val unknownJson = Json.parse("""{"x":"y"}""")
    unknownJson.as[Unknown] == Unknown(unknownJson)
```

### experimental features (will change)
#### Serialization nirvana - formatAuto FULLY automatic de-serializer (note: needs more optimized internal implementation)

```scala
    sealed trait SomeAdt
    case object A extends SomeAdt
    final case class X(i: Int, s: String) extends SomeAdt
    object Baz
    case class Bar(a: Int, b:Float, foo: Baz.type, o: Option[Int])
    case class Foo(_1:Bar,_11:SomeAdt, _2:String,_3:Int,_4:Int,_5:Int,_21:Int,_22:Int,_23:Int,_24:Int,_25:Int,_31:Int,_32:Int,_33:Int,_34:Int,_35:Int,_41:Int,_42:Int,_43:Int,_44:Int,_45:Int,_51:Int,_52:Int,_53:Int,_54:Int,_55:Int)
    val foo = Foo(Bar(5,1.0f, Baz, Some(4): Option[Int]),A,"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    val foo2 = Foo(Bar(5,1.0f, Baz, None: Option[Int]),X(5,"x"),"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    
    import ai.x.play.json.implicits.optionWithNull
    val fmt2: Format[Foo] = Jsonx.formatAuto[Foo] // not implicit to avoid infinite recursion

    {
      implicit lazy val fmt3: Format[Foo] = fmt2    
      val json = Json.toJson( foo )
      assert(foo === json.as[Foo])
      val json2 = Json.toJson( foo2 )
      assert(foo2 === json2.as[Foo])
    }
```
