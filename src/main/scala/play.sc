
import org.peterc.srdp.examples.ArithmeticParser
import org.peterc.srdp.examples._
import ArithmeticParser._
import org.peterc.srdp.Parsed

val tokens = Seq(NumToken(1), NumToken(2), NumToken(3), NumToken(4), NumToken(5))

val r: Seq[Parsed[Expression]] = numsRule5.parse(tokens)

