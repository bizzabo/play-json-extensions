package org.cvogt.test.play.json

import org.scalatest.FunSuite

import play.api.libs.json._
import org.joda.time._

import org.cvogt.play.json._

object Adt{
  sealed trait SomeAdt
  case object ChoiceA extends SomeAdt
  case object ChoiceB extends SomeAdt
  final case class X(i: Int, s: String) extends SomeAdt
  object X{
    implicit def jsonFormat = Jsonx.formatCaseClass[X]
  }
  final case class Y(i: Int, s: String) extends SomeAdt
  object Y{
    implicit def jsonFormat = Jsonx.formatCaseClass[Y]
  }
}

class CaseClassTest extends FunSuite{
  test("de/serialize case class > 22"){
    case class Bar(a: Int, b:Float)
    case class Foo(_1:Bar,_2:String,_3:Int,_4:Int,_5:Int,_21:Int,_22:Int,_23:Int,_24:Int,_25:Int,_31:Int,_32:Int,_33:Int,_34:Int,_35:Int,_41:Int,_42:Int,_43:Int,_44:Int,_45:Int,_51:Int,_52:Int,_53:Int,_54:Int,_55:Int)
    val foo = Foo(Bar(5,1.0f),"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    implicit def fmt2 = Jsonx.formatCaseClass[Foo]
    val json = Json.toJson( foo )
    assert(foo === json.as[Foo])
  }
  test("magically de/serialize case class > 22"){
    import org.cvogt.play.json.implicits.formatCaseClass
    case class Bar(a: Int, b:Float)
    case class Foo(_1:Bar,_2:String,_3:Int,_4:Int,_5:Int,_21:Int,_22:Int,_23:Int,_24:Int,_25:Int,_31:Int,_32:Int,_33:Int,_34:Int,_35:Int,_41:Int,_42:Int,_43:Int,_44:Int,_45:Int,_51:Int,_52:Int,_53:Int,_54:Int,_55:Int)
    val foo = Foo(Bar(5,1.0f),"sdf",3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)
    val json = Json.toJson( foo )
    assert(foo === json.as[Foo])
  }
  case class Baz(a: Int)
  case class Bar(a: Int)
  test("magical implicit formatter default with overrides"){
    object formatters extends org.cvogt.play.json.ImplicitCaseClassFormatDefault{
      implicit def fmt = new Reads[Bar]{
        def reads(json: JsValue) = JsSuccess(Bar(1))
      }      
    }
    import formatters._
    val json = Json.parse("""{"a": 2}""")
    assert(Baz(2) === json.as[Baz])
    assert(Bar(1) === json.as[Bar])
  }
  test("serializing None skips fields"){
    // note, using null for a Scala String doesn't work with play Json
    case class Bar(a: Option[String], b: String, d: Option[String])
    val bar = Bar(None,"foo",Some("foo"))
    implicit def fmt1 = Jsonx.formatCaseClass[Bar]
    val json = Json.parse(Json.stringify( // <- otherwise c = JsString(null), not JsNull
      Json.toJson(bar)
    ))
    assert(bar === json.as[Bar])
    assert(
      Set("b"->JsString("foo"), "d"->JsString("foo"))
      === json.as[JsObject].fields.toSet
    )
  }
  test("serialize Adt"){
    import Adt._
    implicit val jsonFormat = Jsonx.formatAdt[SomeAdt](AdtEncoder.TypeAsField)
    val a: SomeAdt = ChoiceA
    val b: SomeAdt = ChoiceB
    val x = X(99,"Chris")
    val y = Y(99,"Chris")
    assert("ChoiceA" === Json.toJson(ChoiceA).as[JsString].value)
    assert("ChoiceB" === Json.toJson(ChoiceB).as[JsString].value)
    assert("ChoiceA" === Json.toJson(a).as[JsString].value)
    assert("ChoiceB" === Json.toJson(b).as[JsString].value)

    assert(x !== y)
    assert(JsSuccess(ChoiceA) === Json.fromJson[SomeAdt](Json.toJson(ChoiceA)))
    assert(JsSuccess(ChoiceB) === Json.fromJson[SomeAdt](Json.toJson(ChoiceB)))
    assert(JsSuccess(x) === Json.fromJson[SomeAdt](Json.toJson(x)))
    assert(JsSuccess(y) === Json.fromJson[SomeAdt](Json.toJson(y)))
  }
  test("deserialize error messages"){
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
}
