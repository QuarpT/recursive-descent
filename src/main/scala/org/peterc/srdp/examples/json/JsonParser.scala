package org.peterc.srdp.examples.json

import org.peterc.srdp.Tokenizer._
import org.peterc.srdp.{Tokenizer, _}

object JsonTokens {
  import Tokenizer._

  case class NumberToken(value: BigDecimal) extends Token[BigDecimal]
  case class StringToken(value: String) extends Token[String]
  case object OpenArrayBracketToken extends TokenUnit
  case object CloseArrayBracketToken extends TokenUnit
  case object OpenCurlyBracketToken extends TokenUnit
  case object CloseCurlyBracketToken extends TokenUnit
  case object ColonToken extends TokenUnit

  val tokenizers: Set[Tokenizer] = Set(
    NumberToken.tokenizer("[0-9]+(\\.[0-9]+)?".r, BigDecimal.apply),
    StringToken.tokenizer("\"[^\"]*\"".r, s => s.substring(1, s.length - 1)),
    OpenArrayBracketToken.tokenizer("\\[".r),
    CloseArrayBracketToken.tokenizer("\\]".r),
    OpenCurlyBracketToken.tokenizer("\\{".r),
    CloseCurlyBracketToken.tokenizer("\\}".r),
    ColonToken.tokenizer(":".r)
  )
}

object JsonRules {
  import JsonTokens._

  // AST

  case class JsNumber(n: BigDecimal)
  case class JsString(s: String)
  case object OpenArrayBracket
  case object CloseArrayBracket
  case object OpenCurlyBracket
  case object CloseCurlyBracket
  case object Colon

  // AXIOMS
//
//  val numberAxiom = Axiom { case NumberToken(n) => JsNumber(n) }
//  val stringAxiom = Axiom { case StringToken(s) => JsString(s) }
//  val openArrayBracketAxiom = Axiom { case OpenArrayBracketToken => OpenArrayBracket }
//  val closeArrayBracketAxiom = Axiom { case CloseArrayBracketToken => CloseArrayBracket }
//  val openCurlyBracketAxiom = Axiom { case OpenCurlyBracketToken => OpenCurlyBracket }
//  val closeCurlyBracketAxiom = Axiom { case CloseCurlyBracketToken => CloseCurlyBracket }
//  val colonAiom = Axiom { case ColonToken => Colon }
//
//  // RULES
//
//  lazy val bracketRule: Rule[Expression] = openBracketAxiom * expressionRule * closeBracketAxiom > BracketExpression
//
//  lazy val expressionRule: Rule[Expression] =
//    bracketRule |
//      bracketRule * operatorAxiom * expressionRule > BinaryOperation |
//      numberAxiom * operatorAxiom * expressionRule > BinaryOperation |
//      numberAxiom
}

