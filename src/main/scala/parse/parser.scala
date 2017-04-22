package parse
import parse.Rule.ImplicitCat

sealed trait Token

sealed trait RuleOutput {
  def merge(ruleOutput: RuleOutput): RuleOutput
}
case object Unmatched extends RuleOutput {
  override def merge(ruleOutput: RuleOutput): RuleOutput = ruleOutput
}
case class Matched(parsed: Seq[Tree], remaining: Seq[Token])
case class Matches(matches: Seq[Matched]) extends RuleOutput {
  def ruleOutput: RuleOutput = matches.headOption.fold[RuleOutput](Unmatched)(_ => this)
  override def merge(ruleOutput: RuleOutput): RuleOutput = ruleOutput match {
    case ms: Matches => Matches(ms.matches ++ matches)
    case Unmatched => this
  }
}
object RuleOutput {
  def merge(output: Seq[RuleOutput]): RuleOutput = Matches(output.foldLeft(Seq.empty[Matched]) {
    case (accum, ms: Matches) => ms.matches ++ ms.matches
    case (accum, _) => accum
  }).ruleOutput
}


sealed trait Tree

object Rule {
  implicit class ImplicitCat(rule: Rule) {
    def *(concatMe: => Rule): ImplicitCat = new RuleConcat(rule, concatMe)
    def >(fun: Seq[Tree] => Seq[Tree]): Rule = CompletingRule(rule, fun)
  }

  implicit class ImplicitOr(rule: Rule) {
    def |(orMe: Rule): Rule = RuleOr(rule, orMe)
  }
}

case class CompletingRule(rule: Rule, fun: Seq[Tree] => Seq[Tree]) extends Rule {
  override def parse(remaining: Matched): RuleOutput = {
    rule.parse(remaining) match {
      case Matches(ms) => Matches(ms.map(m => Matched(fun(m.parsed), m.remaining)))
      case Unmatched => Unmatched
    }
  }
}

case class RuleOr(ruleLeft: Rule, ruleRight: Rule) extends Rule {
  override def parse(remaining: Matched): RuleOutput = ruleLeft.parse(remaining) merge ruleRight.parse(remaining)
}

class RuleConcat(ruleLeft: Rule, ruleRight: => Rule) extends Rule {
  override def parse(remaining: Matched): RuleOutput = {
    ruleLeft.parse(remaining) match {
      case Matches(matches) => applyRightRuleToMatches(matches)
      case Unmatched => Unmatched
    }
  }

  private def applyRightRuleToMatches(matches: Seq[Matched]): RuleOutput = {
    val ruleOutput = Matches(matches.foldLeft(Seq.empty[Matched]) { (accum, matched) =>
      ruleRight.parse(matched) match {
        case Matches(a) => a ++ accum
        case Unmatched => accum
      }
    })
    ruleOutput.ruleOutput
  }
}

sealed trait Rule {
  def parse(remaining: Matched): RuleOutput
  def parse(tokens: Seq[Token]): RuleOutput = {
    parse(Matched(Seq.empty, tokens))
  }
}

object Arithmetic {
  case class NumToken(x: Int) extends Token
  case object OperatorToken extends Token

  sealed trait Expression extends Tree
  case class Operation(op: Operator, x: Expression, y: Expression) extends Expression
  case class Operator(op: Char) extends Tree
  case class Num(num: Int) extends Expression

  object NumRule extends Rule {
    override def parse(matched: Matched): RuleOutput = matched.remaining match {
      case NumToken(x) :: tail => Matches(Matched(matched.parsed :+ Num(x), tail) :: Nil)
      case _ => Unmatched
    }
  }

  object OperatorRule extends Rule {
    override def parse(matched: Matched): RuleOutput = matched.remaining match {
      case OperatorToken :: tail => Matches(Matched(matched.parsed :+ Operator('+'), tail) :: Nil)
      case _ => Unmatched
    }
  }

  lazy val expressionRule: Rule = NumRule * OperatorRule * expressionRule > identity | NumRule

}

object Main {
  import Arithmetic._
  def main(args: Array[String]) = {
    println("start")
//    println(expressionRule)
    val tokens = Seq(NumToken(1), OperatorToken, NumToken(2))
    val parsed = expressionRule.parse(tokens)
    println(parsed)
  }
}
