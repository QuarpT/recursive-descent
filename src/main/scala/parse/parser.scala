package parse

trait Token

case class Parsed[+A](parsed: A, remaining: Seq[Token])

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

