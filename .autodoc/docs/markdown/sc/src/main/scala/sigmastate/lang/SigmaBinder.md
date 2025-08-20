[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/sc/src/main/scala/sigmastate/lang/SigmaBinder.scala)

The code in this file is part of the Sigma programming language implementation and provides a class called `SigmaBinder` that is responsible for resolving references to global names and inferring their types. The class takes an instance of `ScriptEnv`, `SigmaBuilder`, `NetworkPrefix`, and `PredefinedFuncRegistry` as input parameters. The `ScriptEnv` is a map of global names to their values, while `SigmaBuilder` is used to construct the AST of the program. `NetworkPrefix` is used to decode an Ergo address from a string, and `PredefinedFuncRegistry` is a registry of predefined functions.

The `SigmaBinder` class has a single public method called `bind` that takes an `SValue` as input and returns a new `SValue` with all references to global names resolved and their types inferred. The `bind` method calls the private `eval` method to perform the actual rewriting of the AST. The `eval` method uses the Kiama library to traverse the AST and apply a set of rules to rewrite it. The rules include resolving references to global names, inferring their types, and rewriting lambda expressions, collections, and other constructs.

The `SigmaBinder` class also includes an object called `SrcCtxCallbackRewriter` that extends the `CallbackRewriter` trait. This object is used to rewrite the source context of an `SValue` when it is replaced by a new `SValue`. This is done to preserve the source context of the original `SValue` in the new `SValue`.

Overall, the `SigmaBinder` class is an important component of the Sigma programming language implementation that is used to resolve references to global names and infer their types. It is used extensively throughout the Sigma compiler and runtime to ensure that programs are well-typed and free of errors.
## Questions: 
 1. What is the purpose of the `SigmaBinder` class?
- The `SigmaBinder` class is used to rewrite the AST with respect to the environment to resolve all references to global names and infer their types.

2. What is the purpose of the `SrcCtxCallbackRewriter` object?
- The `SrcCtxCallbackRewriter` object is used to rewrite the source context of a new term to match the source context of the old term.

3. What are some of the predefined global names that can be referenced in the `eval` method of the `SigmaBinder` class?
- Some of the predefined global names that can be referenced in the `eval` method of the `SigmaBinder` class include `HEIGHT`, `MinerPubkey`, `INPUTS`, `OUTPUTS`, `LastBlockUtxoRootHash`, `EmptyByteArray`, `SELF`, `CONTEXT`, `Global`, and `None`.