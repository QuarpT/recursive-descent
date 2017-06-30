package org.peterc.srdp.examples.json

import org.peterc.srdp.Tokenizer._
import org.peterc.srdp.Tokenizer.ImplicitExtractors._
import org.peterc.srdp._
import shapeless.Typeable
import shapeless.syntax.typeable._

object JsonRules {

  //TOKENIZER

  val tokenizers: Set[TokenCreation] = Set(
    JsString.tokenizer("\"([^\"]*)\"".r),
    Number.tokenizer("([0-9]+(\\.[0-9]+)?)".r),
    OpenArrayBracket.tokenizer("(\\[)".r),
    CloseArrayBracket.tokenizer("(\\])".r),
    OpenCurlyBracket.tokenizer("(\\{)".r),
    CloseCurlyBracket.tokenizer("(\\})".r),
    Colon.tokenizer("(:)".r),
    Comma.tokenizer("(,)".r)
  )

  // AST

  sealed trait ArrayBody
  sealed trait JsObjectBody
  sealed trait JsValue extends ArrayBody

  case class Number(value: BigDecimal) extends Token[BigDecimal] with JsValue
  case class JsString(value: String) extends Token[String] with JsValue

  case object OpenArrayBracket extends TokenUnit
  case object CloseArrayBracket extends TokenUnit
  case object OpenCurlyBracket extends TokenUnit
  case object CloseCurlyBracket extends TokenUnit
  case object Colon extends TokenUnit
  case object Comma extends TokenUnit

  case class JsArrayContent(jsValue: JsValue, comma: Comma.type, arrayBody: ArrayBody) extends ArrayBody
  case class JsArray(openArrayBracket: OpenArrayBracket.type, body: ArrayBody, closeArrayBracket: CloseArrayBracket.type) extends JsValue
  case class JsKeyValue(jsString: JsString, colon: Colon.type, jsValue: JsValue) extends JsObjectBody
  case class JsObjectContent(jsKeyValue: JsKeyValue, comma: Comma.type, jsObjectBody: JsObjectBody) extends JsObjectBody
  case class JsEmptyObject(openCurlyBracket: OpenCurlyBracket.type, closeCurlyBracket: CloseCurlyBracket.type) extends JsValue
  case class JsObject(openCurlyBracket: OpenCurlyBracket.type, body: JsObjectBody, closeCurlyBracket: CloseCurlyBracket.type) extends JsValue

  // AXIOMS

  val numberAx = Axiom[Number]
  val stringAx = Axiom[JsString]
  val openArrayBracketAx = Axiom[OpenArrayBracket.type]
  val closeArrayBracketAx = Axiom[CloseArrayBracket.type]
  val openCurlyBracketAx = Axiom[OpenCurlyBracket.type]
  val closeCurlyBracketAx = Axiom[CloseCurlyBracket.type]
  val colonAx = Axiom[Colon.type]
  val commaAx = Axiom[Comma.type]

  // RULES

  lazy val jsObjectKeyValue: Rule[JsKeyValue] = stringAx * colonAx * jsValueRule > JsKeyValue

  lazy val jsObjectBody: Rule[JsObjectBody] =
    jsObjectKeyValue |
    jsObjectKeyValue * commaAx * jsObjectBody > JsObjectContent

  lazy val jsObjectRule: Rule[JsValue] =
    openCurlyBracketAx * closeCurlyBracketAx > JsEmptyObject |
    openCurlyBracketAx * jsObjectBody * closeCurlyBracketAx > JsObject

  lazy val jsArrayBodyRule: Rule[ArrayBody] =
    jsValueRule |
    jsValueRule * commaAx * jsArrayBodyRule > JsArrayContent

  lazy val jsArrayRule: Rule[JsValue] = openArrayBracketAx * jsArrayBodyRule * closeArrayBracketAx > JsArray

  lazy val jsValueRule: Rule[JsValue] =
    numberAx |
    stringAx |
    jsArrayRule |
    jsObjectRule
  
}

object Json {
  import JsonRules._

  def apply[A](s: String)(implicit castU: Typeable[A]): Option[A] = {
    for {
      parsed <- parse(s).map(toPrimitives)
      result <- parsed.cast[A]
    } yield result
  }

  implicit class StringJson(val sc: StringContext) extends AnyVal {
    def json[A](args: Any*)(implicit castU: Typeable[A]): Option[A] = {
      apply(sc.raw(args:_*).toString)
    }
  }

  def parse(s: String): Option[JsValue] = {
    jsValueRule.fullyParse(s, tokenizers)
  }

  def toPrimitives(jsValue: JsValue): Any = jsValue match {
    case Number(v) => v
    case JsString(s) => s
    case JsArray(_, body, _) => arrayBodyToPrimitives(body)
    case JsObject(_, body, _) => objectBodyToPrimitives(body)
    case JsEmptyObject(_, _) => Map.empty
  }

  def arrayBodyToPrimitives(arrayBody: ArrayBody): Seq[Any] = arrayBody match {
    case JsArrayContent(value, x, tail) => toPrimitives(value) +: arrayBodyToPrimitives(tail)
    case value: JsValue => Seq(toPrimitives(value))
  }

  def objectBodyToPrimitives(objectBody: JsObjectBody): Map[String, Any] = objectBody match {
    case JsKeyValue(key, _, value) => Map(key.value -> toPrimitives(value))
    case JsObjectContent(keyValue, _, content) => objectBodyToPrimitives(keyValue) ++ objectBodyToPrimitives(content)
  }
}
