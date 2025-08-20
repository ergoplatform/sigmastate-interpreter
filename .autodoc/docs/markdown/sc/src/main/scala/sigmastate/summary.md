[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sc/src/main/scala/sigmastate)

The `sigmastate` folder in the `.autodoc/docs/json/sc/src/main/scala` directory contains essential components for the Sigma protocol implementation in the Ergo blockchain. The code in this folder focuses on building and manipulating SigmaState's intermediate representation (IR) of smart contracts, as well as compiling, type inference, and binding of global names in the Sigma programming language.

The `CompilerReflection.scala` file is a placeholder for future code related to compiler reflection in the larger project. It imports the `sigmastate` and `wrappers.scala.WOptions` packages, which are related to the Sigma protocol and Scala programming options, respectively.

The `eval` subfolder contains the `IRContext.scala` and `TreeBuilding.scala` files, which provide methods and values for building and manipulating the IR of smart contracts in the Ergo blockchain. The `IRContext` trait is extended by two classes, `RuntimeIRContext` and `CompiletimeIRContext`, which are used in different contexts. The `RuntimeIRContext` is used by blockchain nodes to validate transactions, while the `CompiletimeIRContext` is used by script development tools to compile ErgoScript into ErgoTree bytecode.

Example usage:

```scala
val context = new RuntimeIRContext()
val lambda = context.builder.lambda { (ctx: Context) =>
  val input = ctx.INPUTS(0)
  val output = ctx.OUTPUTS(0)
  SigmaProp.isProven(input.propositionBytes) && !SigmaProp.isProven(output.propositionBytes)
}
context.verifyIsProven(lambda) // returns Success(())
```

The `lang` subfolder contains the `SigmaBinder.scala`, `SigmaCompiler.scala`, and `SigmaTyper.scala` files, which are essential components for the Sigma programming language implementation. The `SigmaBinder` class resolves references to global names and infers their types, while the `SigmaCompiler` class compiles ErgoScript source code into executable code. The `SigmaTyper` class handles type inference and analysis of Sigma expressions.

Example usage:

```scala
val settings = CompilerSettings(NetworkPrefix.Testnet)
val compiler = SigmaCompiler(settings)
val env = ScriptEnv.Empty
val code = "HEIGHT > 1000"
val result = compiler.compile(env, code)
val compiledCode = result.code
val compiledGraph = result.compiledGraph
val buildTree = result.buildTree
```

In summary, the code in the `sigmastate` folder provides essential components for the Sigma protocol implementation in the Ergo blockchain, focusing on building and manipulating the IR of smart contracts and the Sigma programming language. These components are used extensively throughout the project to ensure that programs are well-typed and free of errors.
