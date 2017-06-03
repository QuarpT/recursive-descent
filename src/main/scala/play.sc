
import parse.ArithmeticParser._

val tokens = Seq(NumToken(1), NumToken(2), NumToken(3))

numsRule.parse(tokens)
numsRule2.parse(tokens)
