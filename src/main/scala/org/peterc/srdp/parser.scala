package org.peterc.srdp

trait Token[+A] {
  def value: A
}

trait TokenUnit extends Token[Unit] {
  override def value = ()
}

case class Parsed[+A](parsed: A, remaining: Seq[Token[_]])

case class RuleOr[A](left: Rule[A], right: Rule[A]) extends Rule[A] {
  override def parse[Z >: A](remaining: Seq[Token[_]]): Seq[Parsed[A]] = left.parse(remaining) ++ right.parse(remaining)
}

trait Rule[+A] {
  def parse[B >: A](remaining: Seq[Token[_]]): Seq[Parsed[B]]
  def fullyParsed(remaining: Seq[Token[_]]): Option[A] = {
    parse(remaining).find(_.remaining.isEmpty).map(_.parsed)
  }
}

object Rule {
  implicit class ImplicitCat[A](r0: Rule[A]) {
    def *[B](r1: => Rule[B]) = new Concat1(r0, r1)
  }

  implicit class ImplicitOr[+A](r0: Rule[A]) {
    def |[Z >: A](r1: Rule[Z]): Rule[Z] = RuleOr(r0, r1)
  }
}

case class Axiom[A,B](partial: PartialFunction[Token[_], B]) extends Rule[B] {

  def optionPartial(token: Token[_]): Option[B] = {
    partial.andThen(Some.apply).applyOrElse(token, {_: Any => None})
  }

  override def parse[C >: B](remaining: Seq[Token[_]]): Seq[Parsed[C]] = {
    val parsed = for {
      head <- remaining.headOption
      parsed <- optionPartial(head)
    } yield {
      Parsed(parsed, remaining.tail)
    }
    parsed.toSeq
  }
}


