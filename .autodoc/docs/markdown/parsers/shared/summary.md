[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/parsers/shared)

The code in the `.autodoc/docs/json/parsers/shared` folder is essential for parsing ErgoScript, a scripting language used in the Ergo blockchain platform. The main entry point for parsing is the `SigmaParser.scala` file, which provides methods to parse ErgoScript code into ErgoTree expressions and SType (Sigma Type) objects. The `SigmaParser` object extends several traits, including `Exprs`, `Types`, and `Core`, which define methods for parsing ErgoScript expressions, types, and constructing ErgoTree nodes, respectively.

For example, to parse a simple ErgoScript expression, you can use the following code:

```scala
val code = "1 + 2"
val parsed = SigmaParser(code)
val tree = parsed.get.value
```

The `Types.scala` file provides parsers for type terms that can produce values of `SType`. The parsed type expressions are used to type-check ErgoScript code and to generate ErgoTree IR. Example usage:

```scala
val input = "Int => Boolean"
val result = parse(input, Type(_))
result.get // returns SFunc(Array(SInt), SBoolean)
```

The `lang` subfolder contains several files for parsing ErgoScript:

- `Basic.scala`: Provides parsers for basic lexical elements, such as numbers, operators, and keywords.
- `Core.scala`: Defines a trait called "Core" that contains common aliases and keywords used in almost every parser in the file. It also provides methods for constructing ErgoTree unary and binary operations.
- `Exprs.scala`: Defines a set of parsers for ErgoScript expressions, organized in a trait called `Exprs`. Example usage:

  ```scala
  val input = "if (x > 0) x * 2 else x / 2"
  val parsedExpr = fastparse.parse(input, Exprs.Expr(_))
  ```

- `Identifiers.scala`: Provides functionality for parsing and identifying identifiers and keywords in the Sigma programming language.
- `Literals.scala`: A collection of parsers for literal expressions used in the Sigma programming language.

Overall, the code in this folder is crucial for parsing ErgoScript code and generating an AST, which can then be used for further processing, such as type checking, optimization, or code generation. The parsers are implemented using the FastParse library and are designed to work together to handle the various constructs and expressions found in ErgoScript.
