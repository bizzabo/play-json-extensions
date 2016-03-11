package org.cvogt.test.play.json

import org.scalatest.FunSuite

import play.api.libs.json.Json
import org.cvogt.play.json.{Jsonx, Tags}

sealed trait ApiRequest
case class Withdraw(amount: BigDecimal) extends ApiRequest
case class Deposit(amount: BigDecimal) extends ApiRequest
case class Batch(requests: List[ApiRequest])


class TypeTagsTest extends FunSuite {

  object ApiFormats {
    private implicit val tags = Tags.CaseInsensitivePreservingShortTags("_type")
    implicit val depositFmt = Jsonx.formatTagged(Json.format[Deposit])
    implicit val withdrawFmt = Jsonx.formatTagged(Json.format[Withdraw])
  }
  test("json formatTagged") {
    import ApiFormats._

    val deposit = Deposit(99.9)
    assert((Json.toJson(deposit) \ "_type").as[String] === "Deposit")
    assert((Json.toJson(deposit) \ "amount").as[BigDecimal] === 99.9)
    assert(Json.parse(s"""{"_type": "DePoSiT", "amount": 99.9}""").as[Deposit] === deposit)

    val withdraw = Withdraw(10L)
    assert((Json.toJson(withdraw) \ "_type").as[String] === "Withdraw")
    assert((Json.toJson(withdraw) \ "amount").as[BigDecimal] === 10)
    assert(Json.parse(s"""{"_type": "withdraw", "amount": 10.0}""").as[Withdraw] === withdraw)
  }

  test("formatTagged should fail if enclosing format produces non JsObjects") {
    case class Counter(count: Int)

    implicit val tags = Tags.CaseInsensitivePreservingShortTags("_type")
    implicit val taggedFmt = Jsonx.formatTagged(Jsonx.formatInline[Counter])

    val errorMessage = intercept[Exception] { Json.toJson(Counter(50)) }.getMessage
    assert(errorMessage.startsWith("Cannot put type-tag"))
  }


  object RootApiFormats {
    private implicit val tags = Tags.CaseInsensitivePreservingShortTags("_type")

    private implicit val depositFmt = Json.format[Deposit]
    private implicit val withdrawFmt = Json.format[Withdraw]

    implicit val apifmt = Jsonx.formatSealedTagged[ApiRequest]
    implicit val batchfmt = Json.format[Batch]
  }

  test("json formatSealedTagged") {
    import RootApiFormats._
    val deposit = Deposit(20)
    val withdraw = Withdraw(10)
    val batch = Batch(List(deposit, withdraw))

    assert(((Json.toJson(batch) \ "requests")(0) \ "_type").as[String] === "Deposit")
    assert(((Json.toJson(batch) \ "requests")(1) \ "_type").as[String] === "Withdraw")

    assert(Json.parse(s"""{"_type": "withdraw", "amount": 10.0}""").as[ApiRequest] === withdraw)
    assert(Json.parse(s"""{"_type": "deposit", "amount": 20.0}""").as[ApiRequest] === deposit)
  }

}
