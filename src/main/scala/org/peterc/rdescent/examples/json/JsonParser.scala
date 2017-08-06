package org.peterc.rdescent.examples.json

import org.peterc.rdescent.tokenizer._
import org.peterc.rdescent.tokenizer.implicits._
import org.peterc.rdescent._
import shapeless.Typeable
import shapeless.syntax.typeable._

object JsonRules {

  //TOKENIZER

  val tokenizers: Set[Tokenizer] = Set(
    JsString.tokenizer("\"([^\"]*)\"".r, 1),
    JsNumber.tokenizer("[0-9]+(\\.[0-9]+)?".r),
    OpenArrayBracket.tokenizer("\\[".r),
    CloseArrayBracket.tokenizer("\\]".r),
    OpenCurlyBracket.tokenizer("\\{".r),
    CloseCurlyBracket.tokenizer("\\}".r),
    Colon.tokenizer(":".r),
    Comma.tokenizer(",".r)
  )

  // AST

  sealed trait JsValue {
    def asOpt[T](implicit castU: Typeable[T]): Option[T] = this.cast[T]
  }

  case class JsNumber(value: BigDecimal) extends Token[BigDecimal] with JsValue
  case class JsString(value: String) extends Token[String] with JsValue
  case object OpenArrayBracket extends TokenUnit
  case object CloseArrayBracket extends TokenUnit
  case object OpenCurlyBracket extends TokenUnit
  case object CloseCurlyBracket extends TokenUnit
  case object Colon extends TokenUnit
  case object Comma extends TokenUnit

  case class JsArray(elements: Seq[JsValue]) extends JsValue
  object JsArray {
    def empty(openArrayBracket: OpenArrayBracket.type, closeArrayBracket: CloseArrayBracket.type) = JsArray(Vector.empty)
    def body(openArrayBracket: OpenArrayBracket.type, jsArray: JsArray, closeArrayBracket: CloseArrayBracket.type) = jsArray
    def content(jsValue: JsValue, comma: Comma.type, arrayBody: JsArray) = JsArray(jsValue +: arrayBody.elements)
    def oneItem(jsValue: JsValue) = JsArray(Vector(jsValue))
  }

  case class JsObject(elements: Map[String, JsValue]) extends JsValue
  object JsObject {
    def keyValue(jsString: JsString, colon: Colon.type, jsValue: JsValue) = JsObject(Map(jsString.value -> jsValue))
    def objectContent(head: JsObject, comma: Comma.type, tail: JsObject) = JsObject(head.elements ++ tail.elements)
    def emptyObject(openCurlyBracket: OpenCurlyBracket.type, closeCurlyBracket: CloseCurlyBracket.type) = JsObject(Map.empty)
    def jsObject(openCurlyBracket: OpenCurlyBracket.type, body: JsObject, closeCurlyBracket: CloseCurlyBracket.type) = body
  }

  // AXIOMS

  val numberAx = Axiom[JsNumber]
  val stringAx = Axiom[JsString]
  val openArrayBracketAx = Axiom[OpenArrayBracket.type]
  val closeArrayBracketAx = Axiom[CloseArrayBracket.type]
  val openCurlyBracketAx = Axiom[OpenCurlyBracket.type]
  val closeCurlyBracketAx = Axiom[CloseCurlyBracket.type]
  val colonAx = Axiom[Colon.type]
  val commaAx = Axiom[Comma.type]

  // RULES

  lazy val jsObjectKeyValue: Rule[JsObject] = stringAx * colonAx * jsValueRule > JsObject.keyValue

  lazy val jsObjectBody: Rule[JsObject] =
    jsObjectKeyValue |
    jsObjectKeyValue * commaAx * jsObjectBody > JsObject.objectContent

  lazy val jsObjectRule: Rule[JsObject] =
    openCurlyBracketAx * closeCurlyBracketAx > JsObject.emptyObject |
    openCurlyBracketAx * jsObjectBody * closeCurlyBracketAx > JsObject.jsObject

  lazy val jsArrayBodyRule: Rule[JsArray] =
    jsValueRule > JsArray.oneItem |
    jsValueRule * commaAx * jsArrayBodyRule > JsArray.content

  lazy val jsArrayRule: Rule[JsArray] =
    openArrayBracketAx * jsArrayBodyRule * closeArrayBracketAx > JsArray.body |
    openArrayBracketAx * closeArrayBracketAx > JsArray.empty

  lazy val jsValueRule: Rule[JsValue] =
    numberAx |
    stringAx |
    jsArrayRule |
    jsObjectRule
  
}

object Json {
  import JsonRules._

  def apply[T <: JsValue](s: String)(implicit castU: Typeable[T]): Option[T] = {
    for {
      jsValue <- jsValueRule.fullyParse(s, tokenizers)
      typedResult <- jsValue.asOpt[T]
    } yield typedResult
  }

}
