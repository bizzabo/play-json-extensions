package org.cvogt.test.play.json

import org.scalatest.FunSuite

import play.api.libs.json._
import org.joda.time._

import org.cvogt.play.json._
import org.cvogt.play.json.tuples._

final case class RecursiveClass(o: Option[RecursiveClass], s:String)
object RecursiveClass{
  import implicits.optionWithNull
  implicit def jsonFormat: Format[RecursiveClass] = Jsonx.formatCaseClass[RecursiveClass]   
}
sealed trait RecursiveAdt
final case class RecursiveChild(o: Option[RecursiveAdt], s:String) extends RecursiveAdt
object RecursiveFormat{
  import implicits.optionWithNull
  implicit def jsonFormat: Format[RecursiveAdt] = Jsonx.formatSealed[RecursiveAdt]
  implicit def jsonFormat2: Format[RecursiveChild] = Jsonx.formatCaseClass[RecursiveChild]   
}
object Adt{
  sealed trait SomeAdt
  case object ChoiceA extends SomeAdt
  case object ChoiceB extends SomeAdt
  case object `Choice.C` extends SomeAdt
  final case class X(i: Int, s: String) extends SomeAdt
  object X{
    implicit def jsonFormat = Jsonx.formatCaseClass[X]
  }
  final case class Y(i: Int, s: String) extends SomeAdt
  object Y{
    implicit def jsonFormat = Jsonx.formatCaseClass[Y]
  }
}
object AdtWithEmptyLeafs{
  sealed trait SomeAdt
  final case class A() extends SomeAdt
  object A{
    implicit def jsonFormat = Jsonx.formatCaseClass[A]
  }
  final case class B() extends SomeAdt
  object B{
    implicit def jsonFormat = Jsonx.formatCaseClass[B]
  }
}

class PlayJsonExtensionsTest extends FunSuite{
  import implicits.optionWithNull
  test("de/serialize case class > 22"){
    case class Bar(a: Int, b:Float)
    case class Foo(_1:Bar,_2:String,_3:Int,_4:Int,_5:Int,_21:Int,_22:Int,_23:Int,_24:Int,_25:Int,_31:Int,_32:Int,_33:Int,_34:Int,_35:Int,_41:Int,_42:Int,_43:Int,_44:Int,_45:Int,_51:Int,_52:Int,_53:Int,_54:Int,_55:Int)
    val foo = Foo(Bar(5,1.0f),"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    implicit def fmt2 = Jsonx.formatCaseClass[Foo]
    val json = Json.toJson( foo )
    assert(foo === json.as[Foo])
  }
  test("de/serialize empty case class"){
    case class Bar()
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    val bar = Bar()
    val json = Json.toJson( bar )
    assert(bar === json.as[Bar])
  }
  test("formatCaseClass with explicit return type"){
    case class Bar()
    implicit def fmt1: Format[Bar] = Jsonx.formatCaseClass[Bar]
    val bar = Bar()
    val json = Json.toJson( bar )
    assert(bar === json.as[Bar])
  }
  test("serializing None skips fields"){
    // note, using null for a Scala String doesn't work with play Json
    case class Bar(a: Option[String], b: String, d: Option[String])
    val bar = Bar(None,"foo",Some("foo"))
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    val json = Json.parse(Json.stringify( // <- otherwise c = JsString(null), not JsNull
      Json.toJson(bar)
    ))
    assert(JsSuccess(bar) === json.validate[Bar])
    assert(
      Set("b"->JsString("foo"), "d"->JsString("foo"))
      === json.as[JsObject].fields.toSet
    )
  }
  test("require to JsError"){
    // note, using null for a Scala String doesn't work with play Json
    case class Bar(a: Int){
      require(a > 5, "a needs to be larger than 5")
    }
    case class Baz(bar: Bar)
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    implicit def fmt2 = Jsonx.formatCaseClass[Baz]
    assert(Baz(Bar(6)) === Json.parse("""{"bar":{"a":6}}""").validate[Baz].get)
    val capturedFailedRequire = Json.parse("""{"bar":{"a":5}}""").validate[Baz]
    assert(
      capturedFailedRequire.asInstanceOf[JsError].errors.head._2.head.message contains "requirement failed: a needs to be larger than 5"
    )
    assert(
      capturedFailedRequire.asInstanceOf[JsError].errors.head._1.toString === "/bar"
    )
  }
  test("serialize Adt"){
    import Adt._
    implicit def simpleName = SingletonEncoder.simpleName
    import implicits.formatSingleton
    implicit val jsonFormat = Jsonx.formatSealed[SomeAdt]
    val a: SomeAdt = ChoiceA
    val b: SomeAdt = ChoiceB
    val c: SomeAdt = `Choice.C`
    val x = X(99,"Chris")
    val y = Y(99,"Chris")
    assert("ChoiceA" === Json.toJson(ChoiceA).as[JsString].value)
    assert("ChoiceB" === Json.toJson(ChoiceB).as[JsString].value)
    assert("Choice.C" === Json.toJson(`Choice.C`).as[JsString].value)
    assert("ChoiceA" === Json.toJson(a).as[JsString].value)
    assert("ChoiceB" === Json.toJson(b).as[JsString].value)
    assert("Choice.C" === Json.toJson(c).as[JsString].value)

    assert(x !== y)
    assert(JsSuccess(ChoiceA) === Json.fromJson[SomeAdt](Json.toJson(ChoiceA)))
    assert(JsSuccess(ChoiceB) === Json.fromJson[SomeAdt](Json.toJson(ChoiceB)))
    assert(JsSuccess(`Choice.C`) === Json.fromJson[SomeAdt](Json.toJson(`Choice.C`)))

    /* disabling tests for ambiguity, not supported at the moment
    assert(JsSuccess(x) === Json.fromJson[SomeAdt](Json.toJson[SomeAdt](x)))
    assert(JsSuccess(y) === Json.fromJson[SomeAdt](Json.toJson[SomeAdt](y)))
    assert(JsSuccess(x) === Json.fromJson[SomeAdt](Json.toJson(x)))
    assert(JsSuccess(y) === Json.fromJson[SomeAdt](Json.toJson(y)))
    */
  }
  test("serialize Adt with empty leafs"){
    import AdtWithEmptyLeafs._
    implicit val jsonFormat = Jsonx.formatSealed[SomeAdt]
    val x = A()
    val y = B()
    /* disabling tests for ambiguity, not supported at the moment
    assert(JsSuccess(x) === Json.fromJson[SomeAdt](Json.toJson[SomeAdt](x)))
    assert(JsSuccess(y) === Json.fromJson[SomeAdt](Json.toJson[SomeAdt](y)))
    assert(JsSuccess(x) === Json.fromJson[SomeAdt](Json.toJson(x)))
    assert(JsSuccess(y) === Json.fromJson[SomeAdt](Json.toJson(y)))
    */
  }
  test("serialize recursive class"){
    val x = RecursiveClass(Some(RecursiveClass(Some(RecursiveClass(None,"c")),"b")),"a")
    val json = Json.toJson[RecursiveClass](x)(implicitly[Format[RecursiveClass]])
    val res = Json.fromJson[RecursiveClass](json)(implicitly[Format[RecursiveClass]])
    assert(JsSuccess(x) === res)
  }
  test("serialize recursive child"){
    import RecursiveFormat._
    val x = RecursiveChild(Some(RecursiveChild(Some(RecursiveChild(None,"c")),"b")),"a")
    val json = Json.toJson[RecursiveChild](x)(implicitly[Format[RecursiveChild]])
    val res = Json.fromJson[RecursiveChild](json)(implicitly[Format[RecursiveChild]])
    assert(JsSuccess(x) === res)
  }
  test("serialize recursive Adt"){
    import RecursiveFormat._
    val x = RecursiveChild(Some(RecursiveChild(Some(RecursiveChild(None,"c")),"b")),"a")
    val json = Json.toJson[RecursiveAdt](x)(implicitly[Format[RecursiveAdt]])
    val res = Json.fromJson[RecursiveAdt](json)(implicitly[Format[RecursiveAdt]])
    assert(JsSuccess(x) === res)
  }
  test("deserialize case class error messages"){
    val json = Json.parse("""{"i":"test"}""")
    val res = Json.fromJson[Adt.X](json)
    res match {
      case JsError(_errors) =>
        val errors = _errors.map{case (k,v) => (k.toString,v)}.toMap
        assert(
          2 === _errors.size
        )
        assert(
          "error.expected.jsnumber" === errors("/i").head.message
        )
        assert(
          "error.path.missing" === errors("/s").head.message
        )
      case _ => assert(false)
    }
  }
  test("deserialize tuple"){
    val json = Json.parse("""[1,1.0,"Test"]""")
    val res = Json.fromJson[(Int,Double,String)](json)
    assert(JsSuccess((1,1.0,"Test")) === res)
    assert(JsSuccess((1,1.0,"Test")) === Json.toJson(res.get).validate[(Int,Double,String)])
  }
  test("deserialize tuple wrong size"){
    case class Foo(bar: (Int,Double,String))
    implicit def jsonFoo = Jsonx.formatCaseClass[Foo]
    val json = Json.parse("""{"bar": [1,1.1]}""")
    val res = Json.fromJson[Foo](json)
    res match {
      case JsError(_errors) =>
        val errors = _errors.map{case (k,v) => (k.toString,v)}.toMap
        assert(
          "Expected array of size 3, found: [1,1.1]" === errors("/bar").head.message
        )
      case _ => assert(false)
    }
  }
}

abstract class JsonTestClasses{
  implicit def option[A](implicit reads: Reads[A]): Reads[Option[A]]
  case class A(s: String)
  object A{ implicit def jsonFormat = Jsonx.formatCaseClass[A] }
  case class B(s: Option[String])
  object B{ implicit def jsonFormat = Jsonx.formatCaseClass[B] }
  case class C(i: Int, b: Option[B])
  object C{ implicit def jsonFormat = Jsonx.formatCaseClass[C] }
  case class A2(s: String)
  object A2{ implicit def jsonFormat = Json.format[A2] }
  case class B2(s: Option[String])
  object B2{ implicit def jsonFormat = Json.format[B2] }
  case class C2(i: Int, b: Option[B2])
  object C2{ implicit def jsonFormat = Json.format[C2] }

  case class Mandatory(s: List[String])
  object Mandatory{ implicit def jsonFormat = Jsonx.formatCaseClass[Mandatory] }
  case class Optional(o: Option[Mandatory])
  object Optional{ implicit def jsonFormat = Jsonx.formatCaseClass[Optional] }

  case class Mandatory2(s: List[String])
  object Mandatory2{ implicit def jsonFormat = Jsonx.formatCaseClass[Mandatory2] }
  case class Optional2(o: Option[Mandatory2])
  object Optional2{ implicit def jsonFormat = Jsonx.formatCaseClass[Optional2] }

  case class ListInner(string: String)
  object ListInner{ implicit def jsonFormat = Jsonx.formatCaseClass[ListInner] }
  case class ListOuter(inner: List[ListInner])
  object ListOuter{ implicit def jsonFormat = Jsonx.formatCaseClass[ListOuter] }
  case class ClassOuter(outer: List[ListOuter])
  object ClassOuter{ implicit def jsonFormat = Jsonx.formatCaseClass[ClassOuter] }

  case class ListInner2(string: String)
  object ListInner2{ implicit def jsonFormat = Jsonx.formatCaseClass[ListInner2] }
  case class ListOuter2(inner: List[ListInner2])
  object ListOuter2{ implicit def jsonFormat = Jsonx.formatCaseClass[ListOuter2] }
  case class ClassOuter2(outer: List[ListOuter2])
  object ClassOuter2{ implicit def jsonFormat = Jsonx.formatCaseClass[ClassOuter2] }
}
class JsonTests extends FunSuite{
  test("json optionWithNull"){
    object JsonTestClasses extends JsonTestClasses{
      implicit def option[A](implicit reads: Reads[A]): Reads[Option[A]] = implicits.optionWithNull[A]
    }
    import JsonTestClasses._

    assert((Json.parse("""{}""") \ "s").validate[Option[String]].isInstanceOf[JsError])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validate[Option[String]])
    assert(JsSuccess(None) === (Json.parse("""{}""") \ "s").validateOpt[String])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validateOpt[String])
    assert(JsSuccess(None) === (Json.parse("""{}""") \ "s").validateAuto[Option[String]])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validateAuto[Option[String]])

    assert(Json.fromJson[Option[String]](Json.parse("""5""")).isInstanceOf[JsError])
    assert(Json.fromJson[Option[String]](Json.parse("""{}""")).isInstanceOf[JsError])

    assert(Json.fromJson[B](Json.parse("""{"s": {}}""")).isInstanceOf[JsError])
    assert(JsSuccess(A("foo")) === Json.fromJson[A](Json.parse("""{"s": "foo"}""")))
    assert(JsSuccess(B(Some("foo"))) === Json.fromJson[B](Json.parse("""{"s": "foo"}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""{"s": null}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""{}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""5""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""null""")))

    assert(Json.fromJson[B](Json.parse("""{"s": {}}""")).isInstanceOf[JsError])
    assert(A2("foo") === Json.fromJson[A2](Json.parse("""{"s": "foo"}""")).get)
    assert(B2(Some("foo")) === Json.fromJson[B2](Json.parse("""{"s": "foo"}""")).get)
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""{"s": null}""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""{}""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""5""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""null""")))

    assert(JsSuccess(Optional(None)) === Json.fromJson[Optional](Json.parse("""{}""")))
    assert(JsSuccess(Optional(Some(Mandatory(List("test"))))) === Json.fromJson[Optional](Json.parse("""{"o":{"s":["test"]}}""")))
    assert(Json.parse("""{"o":{}}""").validate[Optional].isInstanceOf[JsError])

    assert(JsSuccess(Optional2(None)) === Json.fromJson[Optional2](Json.parse("""{}""")))
    assert(JsSuccess(Optional2(Some(Mandatory2(List("test"))))) === Json.fromJson[Optional2](Json.parse("""{"o":{"s":["test"]}}""")))
    assert(Json.parse("""{"o":{}}""").validate[Optional2].isInstanceOf[JsError])

    assert(JsSuccess(ClassOuter(Nil)) === Json.fromJson[ClassOuter](Json.parse("""{"outer": []}""")))
    assert(JsSuccess(ClassOuter2(Nil)) === Json.fromJson[ClassOuter2](Json.parse("""{"outer": []}""")))
  }

  test("json optionNoError"){
    object JsonTestClasses extends JsonTestClasses{
      implicit def option[A](implicit reads: Reads[A]): Reads[Option[A]] = implicits.optionNoError[A]
    }
    import JsonTestClasses._

    assert((Json.parse("""{}""") \ "s").validate[Option[String]].isInstanceOf[JsError])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validate[Option[String]])
    assert(JsSuccess(None) === (Json.parse("""{}""") \ "s").validateOpt[String])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validateOpt[String])
    assert(JsSuccess(None) === (Json.parse("""{}""") \ "s").validateAuto[Option[String]])
    assert(JsSuccess(Some("foo")) === (Json.parse("""{"s": "foo"}""") \ "s").validateAuto[Option[String]])

    assert(JsSuccess(None) === Json.fromJson[Option[String]](Json.parse("""5""")))
    assert(JsSuccess(None) === Json.fromJson[Option[String]](Json.parse("""{}""")))

    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""{"s": {}}""")))
    assert(JsSuccess(A("foo")) === Json.fromJson[A](Json.parse("""{"s": "foo"}""")))
    assert(JsSuccess(B(Some("foo"))) === Json.fromJson[B](Json.parse("""{"s": "foo"}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""{"s": null}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""{}""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""5""")))
    assert(JsSuccess(B(None)) === Json.fromJson[B](Json.parse("""null""")))

    assert(Json.fromJson[B2](Json.parse("""{"s": {}}""")).isInstanceOf[JsError])
    assert(A2("foo") === Json.fromJson[A2](Json.parse("""{"s": "foo"}""")).get)
    assert(B2(Some("foo")) === Json.fromJson[B2](Json.parse("""{"s": "foo"}""")).get)
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""{"s": null}""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""{}""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""5""")))
    assert(JsSuccess(B2(None)) === Json.fromJson[B2](Json.parse("""null""")))

    assert(JsSuccess(Optional(None)) === Json.fromJson[Optional](Json.parse("""{}""")))
    assert(JsSuccess(Optional(Some(Mandatory(List("test"))))) === Json.fromJson[Optional](Json.parse("""{"o":{"s":["test"]}}""")))
    assert(JsSuccess(Optional(None)) === Json.fromJson[Optional](Json.parse("""{"o":{}}""")))
    
    assert(JsSuccess(Optional2(None)) === Json.fromJson[Optional2](Json.parse("""{}""")))
    assert(JsSuccess(Optional2(Some(Mandatory2(List("test"))))) === Json.fromJson[Optional2](Json.parse("""{"o":{"s":["test"]}}""")))
    assert(JsSuccess(Optional2(None)) === Json.fromJson[Optional2](Json.parse("""{"o":{}}""")))

    assert(JsSuccess(ClassOuter(Nil)) === Json.fromJson[ClassOuter](Json.parse("""{"outer": []}""")))
    assert(JsSuccess(ClassOuter2(Nil)) === Json.fromJson[ClassOuter2](Json.parse("""{"outer": []}""")))
  }

  test("test formatInline"){
    case class Foo(i: Int)
    implicit def fmt = Jsonx.formatInline[Foo]
    val f = Foo(1)
    assert(JsSuccess(f) === Json.parse("1").validate[Foo])
    assert(JsSuccess(f) === Json.toJson(f).validate[Foo])

    implicit def fmt2 = Jsonx.formatInline[Bar]
    val b = new Bar(1)
    assert(JsSuccess(b) === Json.parse("1").validate[Bar])
    assert(JsSuccess(b) === Json.toJson(b).validate[Bar])
  }
  case class DontInline(a: Int)
  object DontInline{
    implicit def format = Jsonx.formatCaseClass[DontInline]
  }
  case class Inline(a: Int)
  test("formatAuto"){
    sealed trait SomeAdt
    case object A extends SomeAdt
    final case class X(i: Int, s: String/*, recursion: SomeAdt*/) extends SomeAdt
    object Baz
    case class Bar(a: Int, b:Float, foo: Baz.type, o: Option[Int])
    case class Foo(_1:Bar,_11:SomeAdt, _2:String,_3:Int,_4:Int,_5:Int,_21:Int,_22:Int,_23:Int,_24:Int,_25:Int,_31:Int,_32:Int,_33:Int,_34:Int,_35:Int,_41:Int,_42:Int,_43:Int,_44:Int,_45:Int,_51:Int,_52:Int,_53:Int,_54:Int,_55:Int)
    val foo = Foo(Bar(5,1.0f, Baz, Some(4): Option[Int]),A,"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    val foo2 = Foo(Bar(5,1.0f, Baz, None: Option[Int]),X(5,"x"/*,X(4,"z",A)*/),"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)

    import org.cvogt.play.json.implicits.optionWithNull
    ;{
      val fmt: Format[SomeAdt] = Jsonx.formatAuto[SomeAdt]
    };{
      val fmt: Format[Option[SomeAdt]] = Jsonx.formatAuto[Option[SomeAdt]]
    };{
      val fmt: Format[A.type] = Jsonx.formatAuto[A.type]
    };{
      val fmt: Format[Option[A.type]] = Jsonx.formatAuto[Option[A.type]]
    };{
      val fmt: Format[X] = Jsonx.formatAuto[X]
    };{
      val fmt: Format[Option[X]] = Jsonx.formatAuto[Option[X]]
    };{
      val fmt: Format[Baz.type] = Jsonx.formatAuto[Baz.type]
    };{
      val fmt: Format[Option[Baz.type]] = Jsonx.formatAuto[Option[Baz.type]]
    };{
      val fmt: Format[Bar] = Jsonx.formatAuto[Bar]
    };{
      val fmt: Format[Option[Bar]] = Jsonx.formatAuto[Option[Bar]]
    };{
      val fmt: Format[Int] = Jsonx.formatAuto[Int]
    };{
      val fmt: Format[Option[Int]] = Jsonx.formatAuto[Option[Int]]
    };{
      val fmt: Format[Foo] = Jsonx.formatAuto[Foo]
    };{
      val fmt: Format[Option[Foo]] = Jsonx.formatAuto[Option[Foo]]
    }

    val fmt2: Format[Foo] = Jsonx.formatAuto[Foo] // not implicit to avoid infinite recursion

    {
      implicit def fmt3: Format[Foo] = fmt2    
      val json = Json.toJson( foo )
      assert(foo === json.as[Foo])
      assert(JsSuccess(Some(foo)) === json.validateAuto[Option[Foo]])
      val json2 = Json.toJson( foo2 )
      assert(foo2 === json2.as[Foo])
    }

    def fmt3: Format[DontInline] = Jsonx.formatAuto[DontInline]
    def fmt4: Format[Inline] = Jsonx.formatAuto[Inline]
    assert("5" ===  Json.toJson( Inline(5) )(fmt4).toString)
    assert("""{"a":5}""" ===  Json.toJson( DontInline(5) )(fmt3).toString)

  }
}
class Bar(val i: Int) extends AnyVal
