
import org.peterc.srdp.examples._
import org.peterc.srdp.{Parsed, Tokenizer}
import org.peterc.srdp.examples.arithmetic.{ArithmeticRules, ArithmeticTokens}
import org.peterc.srdp.examples.arithmetic.ArithmeticRules._
import org.peterc.srdp.examples.arithmetic.ArithmeticTokens._

val tokens = Seq(OpenBracketToken, NumberToken(5), OperatorToken('+'), NumberToken(14), CloseBracketToken)

val r = expressionRule.parse(tokens)

//val tokens = Tokenizer.tokenize("4", ArithmeticTokens.tokenizers)
