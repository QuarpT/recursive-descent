package parse

object ArithmeticParser {
  import Rule._
  trait Expression

  case class NumToken(x: Int) extends Token

  case class Num(x: Int) extends Expression
  case class Nums(exp1: Num, exp: Expression) extends Expression
  case class ThreeNums(x: Num, y: Num, z: Num) extends Expression
  case class TwoNums(x: Num, y: Num) extends Expression

  object NumAxiom extends Rule[Num] {
    override def parse[B >: Num](remaining: Seq[Token]): Seq[Parsed[Num]] = remaining.headOption match {
      case Some(NumToken(n)) => Seq(Parsed[Num](Num(n), remaining.tail))
      case _ => Seq.empty
    }
  }

  lazy val numsRule: Rule[Expression] = NumAxiom * NumAxiom > TwoNums
  lazy val numsRule2: Rule[Expression] = NumAxiom * NumAxiom * NumAxiom > ThreeNums
}
