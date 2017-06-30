package org.peterc.srdp

import scala.util.matching.Regex

trait Token[+A] {
  def value: A
}

trait TokenUnit extends Token[Unit] {
  override def value = ()
}

trait TokenValueExtractor[T] {
  def extract(s: String): T
}

object Tokenizer {

  case object WhitespaceToken extends TokenUnit

  val whiteSpaceTokenizer = WhitespaceToken.tokenizer("(\\s)".r)

  implicit class ImplicitTokenizer[A](a: Token[A]) {
    def tokenizer(regex: Regex): TokenCreation = TokenCreation(regex, _ => a)
  }

  implicit class ImplicitTokenizerF[A](a: A => Token[A]) {
    def tokenizer(regex: Regex, toTokenValue: String => A): TokenCreation = TokenCreation(regex, { s => a(toTokenValue(s))})
    def tokenizer(regex: Regex)(implicit extractor: TokenValueExtractor[A]): TokenCreation = TokenCreation(regex, { s: String => a(extractor.extract(s))})
  }

  object ImplicitExtractors {
    implicit object StringTokenExtractor extends TokenValueExtractor[String] {
      override def extract(s: String): String = s
    }
    implicit object BigDecimalTokenExtractor extends TokenValueExtractor[BigDecimal] {
      override def extract(s: String): BigDecimal = BigDecimal(s)
    }
    implicit object IntTokenExtractor extends TokenValueExtractor[Int] {
      override def extract(s: String): Int = s.toInt
    }
    implicit object DoubleTokenExtractor extends TokenValueExtractor[Double] {
      override def extract(s: String): Double = s.toDouble
    }
    implicit object BigIntTokenExtractor extends TokenValueExtractor[BigInt] {
      override def extract(s: String): BigInt = BigInt(s)
    }
    implicit object CharTokenExtractor extends TokenValueExtractor[Char] {
      override def extract(s: String): Char = s.head
    }
  }

  case class TokenCreation(regex: Regex, create: String => Token[_])

  case class Tokenized(remaining: String, tokens: Seq[Token[_]])

  def tokenizeNoWhitespace(string: String, tokenizers: Set[TokenCreation]): Seq[Token[_]] = {
    tokenize(string, tokenizers + whiteSpaceTokenizer).filter {
      case WhitespaceToken => false
      case _ => true
    }
  }

  def tokenize(string: String, tokenizers: Set[TokenCreation]): Seq[Token[_]] = {
    tokenize(Tokenized(string, Seq.empty), tokenizers).toSeq.flatMap(_.tokens)
  }

  private def tokenize(tokenized: Tokenized, tokenizers: Set[TokenCreation]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    if (remaining.isEmpty)
      Some(tokenized)
    else for {
      tokenizedOnce <- tokenizeOnce(tokenized, tokenizers)
      result <- tokenize(tokenizedOnce, tokenizers)
    } yield result
  }

  private def tokenizeOnce(tokenized: Tokenized, tokenizers: Set[TokenCreation]): Option[Tokenized] = {
    val remaining = tokenized.remaining
    val previousTokens = tokenized.tokens
    val matches: Set[Tokenized] = for {
      tokenCreation <- tokenizers
      prefix <- tokenCreation.regex.findPrefixMatchOf(remaining)
    } yield {
      Tokenized(remaining.substring(prefix.matched.length), previousTokens :+ tokenCreation.create(prefix.group(1)))
    }
    matches.reduceOption((a,b) => if (a.remaining.length < b.remaining.length) a else b)
  }
}
