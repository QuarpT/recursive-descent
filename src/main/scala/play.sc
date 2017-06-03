
import parse.Arithmetic._

val tokens = Seq(NumToken(1), NumToken(2), NumToken(3))

numsRule.parse(tokens)
