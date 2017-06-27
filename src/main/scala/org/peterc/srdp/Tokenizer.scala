package org.peterc.srdp

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Token[+A] {
  def value: A
}

trait TokenUnit extends Token[Unit] {
  override def value = ()
}

object Tokenizer {

  case object WhitespaceToken extends TokenUnit

  val whiteSpaceTokenizer = WhitespaceToken.tokenizer("\\s".r)

  implicit class ImplicitTokenizer[A](a: Token[A]) {
    def tokenizer(regex: Regex): Tokenizer = (regex, _ => a)
  }

  implicit class ImplicitTokenizerF[A](a: A => Token[A]) {
    def tokenizer(regex: Regex, toTokenValue: String => A): Tokenizer = (regex, {s => a(toTokenValue(s))})
  }

  type Tokenizer = (Regex, String => Token[_])

  case class Tokenized(remaining: String, tokens: Seq[Token[_]])

  def tokenizeWithWhitespace(string: String, tokenizers: Set[Tokenizer]): Seq[Token[_]] = {
    tokenize(string, tokenizers + whiteSpaceTokenizer).filter {
      case WhitespaceToken => false
      case _ => true
    }
  }

  def tokenize(string: String, tokenizers: Set[Tokenizer]): Seq[Token[_]] = {
    tokenize(Tokenized(string, Seq.empty), tokenizers).toSeq.flatMap(_.tokens)
  }

  private def tokenize(tokenized: Tokenized, tokenizers: Set[Tokenizer]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    if (remaining.isEmpty)
      Some(tokenized)
    else for {
      tokenizedOnce <- tokenizeOnce(tokenized, tokenizers)
      result <- tokenize(tokenizedOnce, tokenizers)
    } yield result
  }

  private def tokenizeOnce(tokenized: Tokenized, tokenizers: Set[Tokenizer]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    val previousTokens = tokenized.tokens
    val matches: Set[Tokenized] = for {
      tokenizer <- tokenizers
      prefix <- tokenizer._1.findPrefixOf(remaining)
    } yield {
      Tokenized(remaining.substring(prefix.length), previousTokens :+ tokenizer._2(prefix))
    }
    matches.reduceOption((a,b) => if (a.remaining.length < b.remaining.length) a else b)
  }
}
