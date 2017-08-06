# Recursive Descent

Recursive Descent is a Scala typed recursive descent parser DSL.
By defining Axioms and rules it is possible to quickly parse strings in abstract syntax trees.

### Examples

Examples in org.peterc.rdescent.examples.
Examples include an Arithmetic and Json parser.

This README uses extracts from the Arithmetic parser to provide an overview.

### Tokenizing

Tokenizing is preliminary step the used by the parser.
You will need to create a set of Tokenizers using the DSL.
A Token should be a case object or a case class that wraps one of the primitive types below.
Tokens you define also represent your grammar's axioms.
You can quickly create Tokenizers using the API for types:

* String
* BigDecimal
* BigInt
* Int
* Double
* Char

Define your Tokens:

```scala
sealed trait Expression

case class Operator(value: Char) extends Token[Char]
case object OpenBracket extends TokenUnit
case object CloseBracket extends TokenUnit
case class Number(value: Int) extends Token[Int] with Expression
```

Define your Tokenizers:

```scala
val tokenizers: Set[Tokenizer] = Set(
    OpenBracket.tokenizer("\\(".r),
    CloseBracket.tokenizer("\\)".r),
    Number.tokenizer("[0-9]+".r),
    Operator.tokenizer("\\+|\\-|/|\\*".r)
)
```

### Parsing to ASTs

Parse to ASTs by defining a grammar consisting of axioms and rules.
Rules can be recursive.
Tokens can be converted to axioms:

```scala
Axiom[Number]
```

Create case classes to consume the parsed output of your rules.
These case classes (rules/branches) and the Tokens (axioms/leaves) will represent your AST:

```scala
case class BracketExpression(openBracket: OpenBracket.type, expression: Expression, closeBracket: CloseBracket.type) extends Expression
case class BinaryOperation(ex1: Expression, operator: Operator, ex2: Expression) extends Expression
```

Define your rules:

```scala

lazy val bracketRule: Rule[Expression] = Axiom[OpenBracket.type] * expressionRule * Axiom[CloseBracket.type] > BracketExpression

lazy val expressionRule: Rule[Expression] =
    bracketRule |
    bracketRule * Axiom[Operator] * expressionRule > BinaryOperation |
    Axiom[Number] * Axiom[Operator] * expressionRule > BinaryOperation |
    Axiom[Number]
```

Retrieve the parsed expression:
```scala
val expression: Expression = expressionRule.fullyParse(s, tokenizers)
```

### Evaluation

Write an evaluator for the parsed expression:

```scala
def evaluate(expression: Expression): Int = expression match {
    case Number(n) => n
    case BracketExpression(_, e, _) => evaluate(e)
    case BinaryOperation(ex1, Operator('+'), ex2) => evaluate(ex1) + evaluate(ex2)
    case BinaryOperation(ex1, Operator('-'), ex2) => evaluate(ex1) - evaluate(ex2)
    case BinaryOperation(ex1, Operator('*'), ex2) => evaluate(ex1) * evaluate(ex2)
    case BinaryOperation(ex1, Operator(_), ex2) => evaluate(ex1) / evaluate(ex2)
}
```