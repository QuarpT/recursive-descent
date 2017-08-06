package org.peterc.rdescent.examples.json

import org.specs2.mutable.Specification
import org.peterc.rdescent.examples.json.JsonRules._

class JsonSpec extends Specification {
  "Json parsing" should {
    "parse json string" in {
      Json[JsString](
        """
          |"Hello"
        """.stripMargin).get.value === "Hello"
    }
    "parse json number" in {
      Json[JsNumber]("123").get.value === BigDecimal(123)
    }
    "parse empty array" in {
      Json[JsArray]("[]").get.elements === Seq.empty
    }
    "parse empty object" in {
      Json[JsObject]("{}").get.elements === Map.empty
    }
    "parse non empty array" in {
      Json[JsArray]("[1, 2, 3]").get === JsArray(Seq(JsNumber(1), JsNumber(2), JsNumber(3)))
    }
    "parse non empty object" in {
      val expected = JsObject(Map(
        "first" -> JsString("hello"),
        "second" -> JsString("there")
      ))

      Json[JsObject](
        """
          | { "first" : "hello", "second" : "there" }
        """.stripMargin).get === expected
    }
    "parse complex array" in {
      val expected = JsArray(Seq(
        JsObject(Map("james" -> JsString("bond"))),
        JsNumber(2),
        JsNumber(3)
      ))

      Json[JsArray](
        """
          | [{ "james" : "bond" }, 2, 3]
        """.stripMargin).get === expected
    }

    "parse complex object" in {
      val expected = JsObject(Map(
        "first" -> JsNumber(1),
        "second" -> JsArray(Seq(
          JsNumber(2),
          JsNumber(3)
        ))
      ))

      Json[JsObject](
        """
          | { "first": 1, "second": [2,3] }
        """.stripMargin).get === expected
    }
  }
}
