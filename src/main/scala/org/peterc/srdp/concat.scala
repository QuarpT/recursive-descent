package org.peterc.srdp

class RuleConcat5[A, B, C, D, E, F, Y](concat4: RuleConcat4[A, B, C, D, E, (A, B, C, D, E)], r2: => Rule[F], converter: (A, B, C, D, E, F) => Y) extends Rule[Y] {
  override def parse[Z >: Y](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    def binaryConverter(a: (A, B, C, D, E), f: F): Y = converter(a._1, a._2, a._3, a._4, a._5, f)
    val prevRuleResults = concat4.parse(remaining)
    RuleConcat.applyNextRule(prevRuleResults, r2, binaryConverter)
  }
}

class Concat5[A, B, C, D, E, F](concat4: RuleConcat4[A, B, C, D, E, (A, B, C, D, E)], r2: => Rule[F]) {
  def >[Z](converter: (A, B, C, D, E, F) => Z) = new RuleConcat5(concat4, r2, converter)
}

class RuleConcat4[A, B, C, D, E, Y](concat3: RuleConcat3[A, B, C, D, (A, B, C, D)], r2: => Rule[E], converter: (A, B, C, D, E) => Y) extends Rule[Y] {
  override def parse[Z >: Y](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    def binaryConverter(a: (A, B, C, D), e: E): Y = converter(a._1, a._2, a._3, a._4, e)
    val prevRuleResults = concat3.parse(remaining)
    RuleConcat.applyNextRule(prevRuleResults, r2, binaryConverter)
  }
}

class Concat4[A, B, C, D, E](concat3: RuleConcat3[A, B, C, D, (A, B, C, D)], r2: => Rule[E]) {
  def >[Z](converter: (A, B, C, D, E) => Z) = new RuleConcat4(concat3, r2, converter)
  def *[F](r3: => Rule[F]) = new Concat5[A,B,C,D,E,F](new RuleConcat4(concat3, r2, (a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e)), r3)
}

class RuleConcat3[A, B, C, D, Y](concat2: RuleConcat2[A, B, C, (A, B, C)], r2: => Rule[D], converter: (A, B, C, D) => Y) extends Rule[Y] {
  override def parse[Z >: Y](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    def binaryConverter(a: (A, B, C), d: D): Y = converter(a._1, a._2, a._3, d)
    val prevRuleResults = concat2.parse(remaining)
    RuleConcat.applyNextRule(prevRuleResults, r2, binaryConverter)
  }
}

class Concat3[A, B, C, D](concat2: RuleConcat2[A, B, C, (A, B, C)], r2: => Rule[D]) {
  def >[Z](converter: (A, B, C, D) => Z) = new RuleConcat3(concat2, r2, converter)
  def *[E](r3: => Rule[E]) = new Concat4[A,B,C,D,E](new RuleConcat3(concat2, r2, (a: A, b: B, c: C, d: D) => (a, b, c, d)), r3)
}

class RuleConcat2[A, B, C, Y](concat1: RuleConcat1[A, B, (A, B)], r2: => Rule[C], converter: (A, B, C) => Y) extends Rule[Y] {
  override def parse[Z >: Y](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    def binaryConverter(a: (A, B), c: C): Y = converter(a._1, a._2, c)
    val prevRuleResults = concat1.parse(remaining)
    RuleConcat.applyNextRule(prevRuleResults, r2, binaryConverter)
  }
}

class Concat2[A, B, C](concat1: RuleConcat1[A, B, (A, B)], r2: => Rule[C]) {
  def >[Z](converter: (A, B, C) => Z) = new RuleConcat2(concat1, r2, converter)
  def *[D](r3: => Rule[D]) = new Concat3[A,B,C,D](new RuleConcat2(concat1, r2, (a: A, b: B, c: C) => (a, b, c)), r3)
}

class RuleConcat1[A, B, Y](r0: Rule[A], r1: => Rule[B], converter: (A, B) => Y) extends Rule[Y] {
  override def parse[Z >: Y](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    val prevRuleResults = r0.parse(remaining)
    RuleConcat.applyNextRule(prevRuleResults, r1, converter)
  }
}

object RuleConcat {
  def applyNextRule[A, B, Y](prevRuleResults: Seq[Parsed[A]], nextRule: Rule[B], converter: (A, B) => Y): Seq[Parsed[Y]] = {
    for {
      prevRuleResult <- prevRuleResults
      nextRuleResults = nextRule.parse(prevRuleResult.remaining)
      nextRuleResult <- nextRuleResults
    } yield {
      Parsed(converter(prevRuleResult.parsed, nextRuleResult.parsed), nextRuleResult.remaining)
    }
  }
}

class Concat1[A, B](r0: Rule[A], r1: => Rule[B]) {
  def >[Z](converter: (A, B) => Z) = new RuleConcat1(r0, r1, converter)
  def *[C](r2: => Rule[C]) = new Concat2[A,B,C](new RuleConcat1(r0, r1, (a: A, b: B) => (a, b)), r2)
}

