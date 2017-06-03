package parse

class RuleConcat2[A, B, C, D](r0: Rule[A], r1: => Rule[B], r2: => Rule[C], converter: (A, B, C) => D) extends Rule[D] {
  override def parse[Z >: D](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    r0.parse(remaining).foldLeft(Seq.empty[Parsed[D]]) { (accum, p0) =>
      accum ++ r1.parse(p0.remaining).foldLeft(Seq.empty[Parsed[D]]) { (accum, p1) =>
        accum ++ r2.parse(p1.remaining).map { p2 =>
          Parsed(converter(p0.parsed, p1.parsed, p2.parsed), p2.remaining)
        }
      }
    }
  }
}

class Concat2[A, B, C](r0: Rule[A], r1: => Rule[B], r2: => Rule[C]) {
  def >[D](converter: (A, B, C) => D) = {
    new RuleConcat2(r0, r1, r2, converter)
  }
}

class RuleConcat1[A, B, C](left: Rule[A], right: => Rule[B], converter: (A, B) => C) extends Rule[C] {
  override def parse[Z >: C](remaining: Seq[Token]): Seq[Parsed[Z]] = {
    left.parse(remaining).foldLeft(Seq.empty[Parsed[C]]) { (accum, pl) =>
      accum ++ right.parse(pl.remaining).map { pr =>
        Parsed(converter(pl.parsed, pr.parsed), pr.remaining)
      }
    }
  }
}

class Concat1[A, B](r0: Rule[A], r1: => Rule[B]) {
  def >[C](converter: (A, B) => C) = {
    new RuleConcat1(r0, r1, converter)
  }
  def *[C](r2: => Rule[C]) = new Concat2[A,B,C](r0, r1, r2)
}


