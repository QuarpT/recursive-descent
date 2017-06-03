package parse

sealed trait Token
case class NumToken(x: Int) extends Token
case object OperatorToken extends Token


sealed trait RuleOutput
case object Unmatched extends RuleOutput
case class Matched(parsed: Seq[Tree], remaining: Seq[Token])
case class Matches(matches: Seq[Matched]) extends RuleOutput {
  def ruleOutput: RuleOutput = matches.headOption.fold[RuleOutput](Unmatched)(_ => this)
}
object RuleOutput {
  def merge(output: Seq[RuleOutput]): RuleOutput = Matches(output.foldLeft(Seq.empty[Matched]) {
    case (accum, ms: Matches) => ms.matches ++ ms.matches
    case (accum, _) => accum
  }).ruleOutput
}


sealed trait Tree
sealed trait Expression extends Tree
case class Operation(op: Operator, x: Expression, y: Expression) extends Expression
case class Operator(op: Char) extends Tree
case class Num(num: Int) extends Expression

sealed trait Rule {
  def parse(remaining: Matched): RuleOutput

  def parse(tokens: Seq[Token]): RuleOutput = {
    parse(Matched(Seq.empty, tokens))
  }

  def concatRules(rules: Seq[Rule])(trim: Seq[Tree] => Seq[Tree]): Rule = new Rule {
    override def parse(matched: Matched): RuleOutput = {

      def applyRuleToMatches(rule: Rule, matches: Matches): RuleOutput = {
        val resultMatches = matches.matches.foldLeft(Matches(Seq.empty)) { (nextLevel, matchMe) =>
          val result = rule.parse(matchMe)
          println(result)
          result match {
            case r: Matches => Matches(r.matches ++ nextLevel.matches)
            case _ => nextLevel
          }
        }
        resultMatches.ruleOutput
      }

      def applyRules(rules: Seq[Rule], ruleOutput: RuleOutput): RuleOutput = (rules, ruleOutput) match {
        case (Nil, _) => ruleOutput
        case (_, Unmatched) => Unmatched
        case (rule :: tail, matches: Matches) => applyRules(tail, applyRuleToMatches(rule, matches))
      }

      val ruleOutput = applyRules(rules, Matches(Seq(matched)))

      ruleOutput match {
        case Matches(ms) => ms.map(m => m.)
        case Unmatched => Unmatched
      }

    }
  }

  def orRules(rules: Seq[Rule]): Rule = new Rule {
    override def parse(remaining: Matched): RuleOutput = {
      val processed: Seq[RuleOutput] = rules.map { rule =>
        rule.parse(remaining)
      }
      RuleOutput.merge(processed)
    }
  }

}


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

object ExpressionRule extends Rule {
  override def parse(matched: Matched): RuleOutput = {
    orRules(Seq(NumRule, concatRules(Seq(NumRule, OperatorRule, ExpressionRule)) {
      case Matches(ms) => ms.map(m => m.)
      case Unmatched => Unmatched
    })).parse(matched)
  }
}

object Main {
  def main(args: Array[String]) = {
    println("start")
    val tokens = Seq(NumToken(1), OperatorToken)
    val parsed = ExpressionRule.parse(tokens)
    println(parsed)
  }
}
