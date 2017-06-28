package org.peterc.srdp

import org.peterc.srdp.Tokenizer.Tokenizer

import scala.reflect.ClassTag


case class Parsed[+A](parsed: A, remaining: Seq[Token[_]])

case class RuleOr[A](left: Rule[A], right: Rule[A]) extends Rule[A] {
  override def parse[Z >: A](remaining: Seq[Token[_]]): Seq[Parsed[A]] = left.parse(remaining) ++ right.parse(remaining)
}

trait Rule[+A] {
  def parse[B >: A](remaining: Seq[Token[_]]): Seq[Parsed[B]]
  def fullyParse(remaining: Seq[Token[_]]): Option[A] = {
    parse(remaining).find(_.remaining.isEmpty).map(_.parsed)
  }
  def fullyParseAll(remaining: Seq[Token[_]]): Seq[A] = {
    parse(remaining).filter(_.remaining.isEmpty).map(_.parsed)
  }
  def fullyParse(s: String, tokenizers: Set[Tokenizer]): Option[A] = {
    val tokens = Tokenizer.tokenizeNoWhitespace(s, tokenizers)
    fullyParse(tokens)
  }
}

object Rule {
  implicit class ImplicitCat[A](r0: Rule[A]) {
    def *[B](r1: => Rule[B]): Concat1[A, B] = new Concat1(r0, r1)
  }

  implicit class ImplicitOr[+A](r0: Rule[A]) {
    def |[Z >: A](r1: Rule[Z]): Rule[Z] = RuleOr(r0, r1)
  }
}

class Axiom[A : ClassTag] extends Rule[A] {
  // I don't like this but it enables the API to have nice syntax for generating Axioms
  val clazz = implicitly[ClassTag[A]].runtimeClass

  override def parse[B >: A](remaining: Seq[Token[_]]): Seq[Parsed[B]] = {
    remaining.headOption.flatMap {
      case h if clazz.isInstance(h) => Some(Parsed(h.asInstanceOf[B], remaining.tail))
      case _ => None
    }.toSeq
  }
}

object Axiom {
  def apply[A: ClassTag]: Axiom[A] = new Axiom[A]
}


