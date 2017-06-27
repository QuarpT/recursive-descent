
import org.peterc.srdp.examples._
import org.peterc.srdp.{Parsed, Tokenizer}
import org.peterc.srdp.examples.arithmetic.{ArithmeticRules, ArithmeticTokens}
import org.peterc.srdp.examples.arithmetic.ArithmeticRules._
import org.peterc.srdp.examples.arithmetic.ArithmeticTokens._


val tokens = Tokenizer.tokenize("(100*20)/(4/2)", ArithmeticTokens.tokenizers)
val r = expressionRule.fullyParsed(tokens).map(_.evaluate)
