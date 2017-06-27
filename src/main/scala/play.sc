
import org.peterc.srdp.examples._
import org.peterc.srdp.Parsed
import org.peterc.srdp.examples.arithmetic.ArithmeticRules
import org.peterc.srdp.examples.arithmetic.ArithmeticRules._
import org.peterc.srdp.examples.arithmetic.ArithmeticTokens._

val tokens = Seq(OpenBracketToken, NumberToken(5), OperatorToken('+'), NumberToken(14), CloseBracketToken)

val r = expressionRule.fullyParsed(tokens).map(_.evaluate)

