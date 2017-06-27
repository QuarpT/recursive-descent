
import org.peterc.srdp.examples._
import org.peterc.srdp.{Parsed, Tokenizer}
import org.peterc.srdp.examples.arithmetic.{ArithmeticRules, ArithmeticTokens}
import org.peterc.srdp.examples.arithmetic.ArithmeticRules._
import org.peterc.srdp.examples.arithmetic.ArithmeticTokens._
val s = "3 * 2"
eval"$s"
