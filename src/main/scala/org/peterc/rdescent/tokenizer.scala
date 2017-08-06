package org.peterc.rdescent

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

object tokenizer {
  import implicits._
  case object WhitespaceToken extends TokenUnit

  val whiteSpaceTokenizer = WhitespaceToken.tokenizer("(\\s|\\n)".r)

  object implicits {

    implicit class ImplicitTokenizer[A](a: Token[A]) {
      def tokenizer(regex: Regex, group: Int = 0): Tokenizer = Tokenizer(regex, _ => a, group)
    }

    implicit class ImplicitTokenizerFun[A](a: A => Token[A]) {
      def tokenizer(regex: Regex, toTokenValue: String => A): Tokenizer = Tokenizer(regex, s => a(toTokenValue(s)), 0)
      def tokenizer(regex: Regex, toTokenValue: String => A, group: Int): Tokenizer = Tokenizer(regex, s => a(toTokenValue(s)), group)
      def tokenizer(regex: Regex)(implicit extractor: TokenValueExtractor[A]): Tokenizer = Tokenizer(regex, s => a(extractor.extract(s)), 0)
      def tokenizer(regex: Regex, group: Int)(implicit extractor: TokenValueExtractor[A]): Tokenizer = Tokenizer(regex, s => a(extractor.extract(s)), group)
    }

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

  case class Tokenizer(regex: Regex, create: String => Token[_], group: Int)

  case class Tokenized(remaining: String, tokens: Seq[Token[_]])

  def tokenizeNoWhitespace(string: String, tokenizers: Set[Tokenizer]): Seq[Token[_]] = {
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
      tokenCreation <- tokenizers
      prefix <- tokenCreation.regex.findPrefixMatchOf(remaining)
    } yield {
      Tokenized(remaining.substring(prefix.matched.length), previousTokens :+ tokenCreation.create(prefix.group(tokenCreation.group)))
    }
    matches.reduceOption((a,b) => if (a.remaining.length < b.remaining.length) a else b)
  }
}
