[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sc/src/main/scala/sigmastate/lang)

The `sigmastate.lang` package in the `.autodoc/docs/json/sc/src/main/scala` folder contains essential components for the Sigma programming language implementation, focusing on compiling, type inference, and binding of global names.

`SigmaBinder.scala` provides the `SigmaBinder` class, which resolves references to global names and infers their types. It takes a `ScriptEnv`, `SigmaBuilder`, `NetworkPrefix`, and `PredefinedFuncRegistry` as input parameters. The `bind` method is the main public method that takes an `SValue` as input and returns a new `SValue` with resolved references and inferred types. This class is crucial for ensuring that Sigma programs are well-typed and error-free.

`SigmaCompiler.scala` is responsible for compiling ErgoScript source code into executable code. It takes a `CompilerSettings` object as input and provides methods for parsing, typechecking, and compiling ErgoScript code. The `compile` method returns a `CompilerResult` object containing the compiled code, graph, and expression tree. This class is used by other parts of the project that need to execute ErgoScript code, such as smart contracts validating transactions.

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

`SigmaTyper.scala` handles type inference and analysis of Sigma expressions. It takes a `SigmaBuilder`, `PredefinedFuncRegistry`, and a `lowerMethodCalls` flag as input parameters. The main function, `assignType`, recursively traverses the input expression tree, assigns types to each node, and checks for type consistency and correctness. This class is essential for ensuring that Sigma programs are well-typed and error-free.

In summary, the `sigmastate.lang` package provides essential components for the Sigma programming language implementation, focusing on compiling, type inference, and binding of global names. These components are used extensively throughout the Sigma compiler and runtime to ensure that programs are well-typed and free of errors.
