package org.peterc.srdp.examples.arithmetic

import org.peterc.srdp._
import org.peterc.srdp.examples.arithmetic.ArithmeticTokens.NumberToken

import scala.util.matching.Regex

object ArithmeticTokens {
  import Tokenizer._

  case class NumberToken(value: Int) extends Token[Int]
  case class OperatorToken(value: Char) extends Token[Char]

  case object OpenBracketToken extends TokenUnit
  case object CloseBracketToken extends TokenUnit

  val tokenizers: Set[Tokenizer] = Set(
    OpenBracketToken.tokenizer("\\(".r),
    CloseBracketToken.tokenizer("\\)".r),
    NumberToken.tokenizer("[0-9]+".r, _.toInt),
    OperatorToken.tokenizer("\\+|\\-|/|\\*".r, _.head)
  )
}

object ArithmeticRules {
  import ArithmeticTokens._

  trait Expression {
    def evaluate: Int
  }

  case class Number(n: Int) extends Expression {
    override def evaluate: Int = n
  }

  case class Operator(o: Char)
  trait OpenBracket
  case object OpenBracket extends OpenBracket
  trait CloseBracket
  case object CloseBracket extends CloseBracket

  case class BracketExpression(openBracket: OpenBracket, expression: Expression, closeBracket: CloseBracket) extends Expression {
    override def evaluate: Int = expression.evaluate
  }

  case class BinaryOperation(ex1: Expression, operator: Operator, ex2: Expression) extends Expression {
    override def evaluate: Int = operator.o match {
      case '+' => ex1.evaluate + ex2.evaluate
      case '-' => ex1.evaluate - ex2.evaluate
      case '*' => ex1.evaluate * ex2.evaluate
      case _ => ex1.evaluate / ex2.evaluate
    }
  }

  // AXIOMS

  val numberAxiom = Axiom {
    case NumberToken(n) => Number(n)
  }

  val operatorAxiom = Axiom {
    case OperatorToken(o) => Operator(o)
  }

  val openBracketAxiom = Axiom {
    case OpenBracketToken => OpenBracket
  }

  val closeBracketAxiom = Axiom {
    case CloseBracketToken => CloseBracket
  }

 // RULES

  lazy val bracketRule: Rule[Expression] = openBracketAxiom * expressionRule * closeBracketAxiom > BracketExpression

  lazy val expressionRule: Rule[Expression] =
    bracketRule |
    bracketRule * operatorAxiom * expressionRule > BinaryOperation |
    numberAxiom * operatorAxiom * expressionRule > BinaryOperation |
    numberAxiom
}

