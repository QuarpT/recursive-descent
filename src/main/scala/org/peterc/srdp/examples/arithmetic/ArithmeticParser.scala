package org.peterc.srdp.examples.arithmetic

import org.peterc.srdp.Tokenizer._
import org.peterc.srdp._

object ArithmeticRules {

  // TOKENIZERS

  val tokenizers: Set[Tokenizer] = Set(
    OpenBracket.tokenizer("\\(".r),
    CloseBracket.tokenizer("\\)".r),
    Number.tokenizer("[0-9]+".r, _.toInt),
    Operator.tokenizer("\\+|\\-|/|\\*".r, _.head)
  )

  // AST

  sealed trait Expression

  case class Operator(value: Char) extends Token[Char]
  sealed trait OpenBracket
  case object OpenBracket extends TokenUnit with OpenBracket
  sealed trait CloseBracket
  case object CloseBracket extends TokenUnit with CloseBracket

  case class Number(value: Int) extends Token[Int] with Expression
  case class BracketExpression(openBracket: OpenBracket, expression: Expression, closeBracket: CloseBracket) extends Expression
  case class BinaryOperation(ex1: Expression, operator: Operator, ex2: Expression) extends Expression

  // AXIOMS

  val numberAxiom = Axiom[Number]
  val operatorAxiom = Axiom[Operator]
  val openBracketAxiom = Axiom[OpenBracket]
  val closeBracketAxiom = Axiom[CloseBracket]

 // RULES

  lazy val bracketRule: Rule[Expression] = openBracketAxiom * expressionRule * closeBracketAxiom > BracketExpression

  lazy val expressionRule: Rule[Expression] =
    bracketRule |
    bracketRule * operatorAxiom * expressionRule > BinaryOperation |
    numberAxiom * operatorAxiom * expressionRule > BinaryOperation |
    numberAxiom
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
