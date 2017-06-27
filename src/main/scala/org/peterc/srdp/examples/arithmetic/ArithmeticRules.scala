package org.peterc.srdp.examples.arithmetic

import org.peterc.srdp._

object ArithmeticTokens {
  case class NumberToken(value: Int) extends Token[Int]
  case class OperatorToken(value: Char) extends Token[Char]

  case object OpenBracketToken extends TokenUnit
  case object CloseBracketToken extends TokenUnit
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

  case class OperatorExpression(number: Number, operator: Operator, expression: Expression) extends Expression {
    override def evaluate: Int = operator.o match {
      case '+' => number.evaluate + expression.evaluate
      case '-' => number.evaluate - expression.evaluate
      case '*' => number.evaluate * expression.evaluate
      case _ => number.evaluate / expression.evaluate
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

  lazy val expressionRule: Rule[Expression] =
    openBracketAxiom * expressionRule * closeBracketAxiom > BracketExpression |
    numberAxiom * operatorAxiom * expressionRule > OperatorExpression |
    numberAxiom
}

