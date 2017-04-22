import parse.{ExpressionRule, NumToken, OperatorToken}

val tokens = Seq(NumToken(1), OperatorToken)
ExpressionRule.parse(tokens)