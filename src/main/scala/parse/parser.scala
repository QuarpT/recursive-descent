package parse

trait Token

case class Parsed[+A](parsed: A, remaining: Seq[Token])

class RuleConcat1[A, B, C](left: Rule[A], right: => Rule[B], converter: (A, B) => C) extends Rule[C] {
  override def parse[Z >: C](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    left.parse(remaining).foldLeft(Seq.empty[Parsed[C]]) { (accum, pl) =>
      right.parse(pl.remaining).map { pr =>
        Parsed(converter(pl.parsed, pr.parsed), pr.remaining)
      }
    }
  }
}

class Concat1[A, B](left: Rule[A], right: => Rule[B]) {
  def >[C](converter: (A, B) => C) = {
    new RuleConcat1(left, right, converter)
  }
}

case class RuleOr[A](left: Rule[A], right: Rule[A]) extends Rule[A] {
  override def parse[Z >: A](remaining: Seq[Token]): Seq[Parsed[A]] = left.parse(remaining) ++ right.parse(remaining)
}

trait Rule[+A] {
  def parse[B >: A](remaining: Seq[Token]): Seq[Parsed[B]]
}

object Rule {
  implicit class ImplicitCat[A](r0: Rule[A]) {
    def *[B](r1: => Rule[B]) = new Concat1(r0, r1)
  }

  implicit class ImplicitOr[+A](r0: Rule[A]) {
    def |[Z >: A](r1: Rule[Z]): Rule[Z] = RuleOr(r0, r1)
  }
}

object Arithmetic {
  import Rule._
  trait Expression

  case class NumToken(x: Int) extends Token

  case class Num(x: Int) extends Expression
  case class Nums(exp1: Num, exp: Expression) extends Expression

  object NumAxiom extends Rule[Num] {
    override def parse[B >: Num](remaining: Seq[Token]): Seq[Parsed[Num]] = remaining.headOption match {
      case Some(NumToken(n)) => Seq(Parsed[Num](Num(n), remaining.tail))
      case _ => Seq.empty
    }
  }

  lazy val numsRule: Rule[Expression] = NumAxiom * numsRule > Nums | NumAxiom

}


