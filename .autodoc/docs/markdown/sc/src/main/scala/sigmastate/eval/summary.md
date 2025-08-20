[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sc/src/main/scala/sigmastate/eval)

The `.autodoc/docs/json/sc/src/main/scala/sigmastate/eval` folder contains essential components for building and manipulating SigmaState's intermediate representation (IR) of smart contracts in the Ergo blockchain. The folder includes two files: `IRContext.scala` and `TreeBuilding.scala`.

`IRContext.scala` defines the `IRContext` trait, which provides methods and values for building and manipulating the IR of smart contracts. It extends two other traits, `TreeBuilding` and `GraphBuilding`, which provide methods for building and manipulating the IR's abstract syntax tree and control flow graph, respectively. The trait also imports the `SigmaProp` object, which contains methods for creating and manipulating SigmaState's signature propositions.

The `IRContext` trait is extended by two classes, `RuntimeIRContext` and `CompiletimeIRContext`, which are used in different contexts. The `RuntimeIRContext` is used by blockchain nodes to validate transactions, while the `CompiletimeIRContext` is used by script development tools to compile ErgoScript into ErgoTree bytecode.

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

`TreeBuilding.scala` contains the `TreeBuilding` trait, which is responsible for transforming a graph-based IR of a function into an ErgoTree expression. ErgoTree is a language used in Ergo Platform to represent complex spending conditions for a transaction output. The main function of this trait is `buildTree`, which takes a function `f` from graph-based IR and an optional `constantsProcessing` parameter. If `constantsProcessing` is specified, each constant is segregated, and a placeholder is inserted in the resulting expression. The `buildTree` function returns an ErgoTree expression that corresponds to the input function `f`.

The trait contains several helper objects and methods to process different types of operations, such as arithmetic, relational, logical, and context property operations. These helper objects and methods are used in the recursive process of building the ErgoTree expression.

In summary, the code in this folder provides a set of tools and methods for building and manipulating SigmaState's IR, which is used to represent smart contracts in the Ergo blockchain. The `IRContext` trait and its subclasses, along with the `TreeBuilding` trait, are essential components of the SigmaState project and are used extensively throughout the codebase.
