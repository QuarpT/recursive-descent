package org.peterc.srdp

import scala.annotation.tailrec
import scala.util.matching.Regex

case class Tokenizer()

object Tokenizer {

  implicit class ImplicitTokenizer[A](a: Token[A]) {
    def tokenizer(regex: Regex): Tokenizer = (regex, _ => a)
  }

  implicit class ImplicitTokenizerF[A](a: A => Token[A]) {
    def tokenizer(regex: Regex, toTokenValue: String => A): Tokenizer = (regex, {s => a(toTokenValue(s))})
  }

  type Tokenizer = (Regex, String => Token[_])

  case class Tokenized(remaining: String, tokens: Seq[Token[_]])

  def tokenize(string: String, tokenizers: Set[Tokenizer]): Seq[Token[_]] = {
    tokenize(Tokenized(string, Seq.empty), tokenizers).toSeq.flatMap(_.tokens)
  }

  def tokenize(tokenized: Tokenized, tokenizers: Set[Tokenizer]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    if (remaining.isEmpty)
      Some(tokenized)
    else for {
      tokenized <- tokenizeOnce(tokenized, tokenizers)
      result <- tokenize(tokenized, tokenizers)
    } yield result
  }

  def tokenizeOnce(tokenized: Tokenized, tokenizers: Set[Tokenizer]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    val previousTokens = tokenized.tokens
    val matches = tokenizers.flatMap { tokenizer =>
//      println(tokenizer._1.findPrefixOf(remaining))
      tokenizer._1.findPrefixOf(remaining).map { prefix =>
        Tokenized(remaining.substring(prefix.length), previousTokens :+ tokenizer._2(prefix))
      }
    }
    matches.headOption.map(_ => matches.reduce((a,b) => if (a.remaining.length < b.remaining.length) a else b))
  }
}
