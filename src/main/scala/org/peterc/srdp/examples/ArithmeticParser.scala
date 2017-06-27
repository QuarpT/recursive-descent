package org.peterc.srdp.examples

import org.peterc.srdp.{Parsed, Rule, Token}

object ArithmeticParser {
  import Rule._
  trait Expression

  case class NumToken(x: Int) extends Token

  case class Num(x: Int) extends Expression
  case class Nums(exp1: Num, exp: Expression) extends Expression
  case class ThreeNums(x: Num, y: Num, z: Num) extends Expression
  case class FiveNums(a: Num, b: Num, c: Num, d: Num, e: Num) extends Expression
  case class TwoNums(x: Num, y: Num) extends Expression

  object NumAxiom extends Rule[Num] {
    override def parse[B >: Num](remaining: Seq[Token]): Seq[Parsed[Num]] = remaining.headOption match {
      case Some(NumToken(n)) => Seq(Parsed[Num](Num(n), remaining.tail))
      case _ => Seq.empty
    }
  }

  lazy val numsRule5: Rule[Expression] = NumAxiom * NumAxiom * NumAxiom * NumAxiom * NumAxiom > FiveNums
}
