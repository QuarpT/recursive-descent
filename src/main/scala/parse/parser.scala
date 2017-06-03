package parse
import parse.Rule.ImplicitCat
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil}

sealed trait Token

case class Matched[A <: HList](parsed: A, remaining: Seq[Token])
case class RuleOutput[A <: HList](matches: Seq[Matched[A]]) {
  def merge(ruleOutput: RuleOutput[A]): RuleOutput[A] = RuleOutput(matches ++ ruleOutput.matches)
}

sealed trait Tree

object Rule {
  implicit class ImplicitCat[A](rule: Rule[A]) {
    def *[B](concatMe: => Rule[B]): ImplicitCat[Prepend[A, B]#Out] = new RuleConcat(rule, concatMe)
    def >[B](fun: A => B): Rule = CompletingRule(rule, fun)
  }

  implicit class ImplicitOr(rule: Rule) {
    def |(orMe: Rule): Rule = RuleOr(rule, orMe)
  }
}

case class CompletingRule[A <: HList, B <: HList](rule: Rule[A], fun: A => B) extends Rule {
  override def parse(remaining: Matched): RuleOutput = {
    val matches = rule.parse(remaining)

    RuleOutput(matches.matches.map(m => Matched(fun(m.parsed), m.remaining)))
    match {
      case Matches(ms) =>
      case Unmatched => Unmatched
    }
  }
}

case class RuleOr(ruleLeft: Rule, ruleRight: Rule) extends Rule {
  override def parse(remaining: Matched): RuleOutput = ruleLeft.parse(remaining) merge ruleRight.parse(remaining)
}

class RuleConcat[A <: HList, B <: HList](ruleLeft: Rule[A], ruleRight: => Rule[B])(implicit prepend : Prepend[A, B]) extends Rule[Prepend[A, B]#Out] {
  override def parse[C <: HList](remaining: Matched[C])(implicit prepend: Prepend[C,  Prepend[A, B]#Out]): RuleOutput[Prepend[C, Prepend[A, B]#Out]#Out] = {
    val parsedleft: RuleOutput[Prepend[C, A]#Out] = ruleLeft.parse(remaining)
    val parsedRight: RuleOutput[Prepend[HNil, B]#Out] = ruleRight.parse(parsedleft.matches.head.remaining)
    val lol = parsedleft.matches.head.parsed ++ parsedRight.matches.head.parsed
    parsedleft.matches.foldLeft(Seq.empty)
  }
}

sealed trait Rule[A <: HList] {
  def parse[B <: HList](remaining: Matched[B])(implicit prepend : Prepend[B, A]): RuleOutput[Prepend[B, A]#Out]
  def parse(tokens: Seq[Token]): RuleOutput[Prepend[HNil, A]#Out] = parse(Matched[HNil](HNil, tokens))
}

object Arithmetic {
  case class NumToken(x: Int) extends Token
  case object OperatorToken extends Token

  sealed trait Expression extends Tree
  case class Operation(op: Operator, x: Expression, y: Expression) extends Expression
  case class Operator(op: Char) extends Tree
  case class Num(num: Int) extends Expression

  object NumRule extends Rule[::[Num, HNil]] {
     def parse[B <: HList](parseMe: Matched[B])(implicit prepend : Prepend[B, ::[Num , HNil]]): RuleOutput[Prepend[B, ::[Num , HNil]]#Out] = parseMe.remaining match {
      case NumToken(x) +: tail =>
        val z: Prepend[B, ::[Num , HNil]]#Out = parseMe.parsed ++ (Num(x) :: HNil)
        RuleOutput(Seq(Matched(z, tail)))
      case _ => RuleOutput(Seq.empty)
    }
  }

//  object OperatorRule extends Rule {
//    override def parse(matched: Matched): RuleOutput = matched.remaining match {
//      case OperatorToken :: tail => Matches(Matched(matched.parsed :+ Operator('+'), tail) :: Nil)
//      case _ => Unmatched
//    }
//  }

  lazy val expressionRule: Rule = NumRule * expressionRule > identity | NumRule

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
