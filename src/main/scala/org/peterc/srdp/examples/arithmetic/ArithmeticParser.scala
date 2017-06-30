package org.peterc.srdp.examples.arithmetic

import org.peterc.srdp.Tokenizer.ImplicitExtractors._
import org.peterc.srdp.Tokenizer._
import org.peterc.srdp._

object ArithmeticRules {

  // TOKENIZERS

  val tokenizers: Set[TokenCreation] = Set(
    OpenBracket.tokenizer("(\\()".r),
    CloseBracket.tokenizer("(\\))".r),
    Number.tokenizer("([0-9]+)".r),
    Operator.tokenizer("(\\+|\\-|/|\\*)".r)
  )

  // AST

  sealed trait Expression

  case class Operator(value: Char) extends Token[Char]
  case object OpenBracket extends TokenUnit
  case object CloseBracket extends TokenUnit
  case class Number(value: Int) extends Token[Int] with Expression

  case class BracketExpression(openBracket: OpenBracket.type, expression: Expression, closeBracket: CloseBracket.type) extends Expression
  case class BinaryOperation(ex1: Expression, operator: Operator, ex2: Expression) extends Expression

 // RULES

  lazy val bracketRule: Rule[Expression] = Axiom[OpenBracket.type] * expressionRule * Axiom[CloseBracket.type] > BracketExpression

  lazy val expressionRule: Rule[Expression] =
    bracketRule |
    bracketRule * Axiom[Operator] * expressionRule > BinaryOperation |
    Axiom[Number] * Axiom[Operator] * expressionRule > BinaryOperation |
    Axiom[Number]
}

object Arithmetic {
  import ArithmeticRules._

  implicit class StringArithmetic(val sc: StringContext) extends AnyVal {
    def eval(args: Any*): Option[Int] = {
     parse(sc.raw(args:_*).toString).map(evaluate)
    }
  }

  def parse(s: String): Option[Expression] = {
    expressionRule.fullyParse(s, tokenizers)
  }

  def evaluate(expression: Expression): Int = expression match {
    case Number(n) => n
    case BracketExpression(_, e, _) => evaluate(e)
    case BinaryOperation(ex1, Operator('+'), ex2) => evaluate(ex1) + evaluate(ex2)
    case BinaryOperation(ex1, Operator('-'), ex2) => evaluate(ex1) - evaluate(ex2)
    case BinaryOperation(ex1, Operator('*'), ex2) => evaluate(ex1) * evaluate(ex2)
    case BinaryOperation(ex1, Operator(_), ex2) => evaluate(ex1) / evaluate(ex2)
  }
}
