
# ErgoScript Compiler

Sigma frontend implements the following pipeline:

`SourceCode` --> `parse` --> `bind` -> `typecheck` -> `buildGraph` -> `buildTree` -> `ErgoTree`

Here:
- `SourceCode` - a string of unicode characters
- `parse` - method `SigmaCompiler.parse`
- `bind` - method `SigmaBinder.bind`
- `typecheck` - method `SigmaTyper.typecheck`
- `buildGraph` - method `IRContext.buildGraph`
- `buildTree` - method `IRContext.buildTree`
- `ErgoTree` - an intermediate representation which can be processed by Sigma [Interpreter](https://github.com/ScorexFoundation/sigmastate-interpreter/blob/master/interpreter/shared/src/main/scala/sigmastate/interpreter/Interpreter.scala#L46)
 
## Parser
`parse` takes a string and produces abstract syntax tree, AST, of a Sigma expression represented by `Value` type in Scala.

In case of any errors it throws `ParserException`

## Binder
`SigmaBinder` takes an AST of successfully parsed Sigma expression and resolves 
global variables and predefined functions that are looked up in the provided environment.
Binder transforms environment values of predefined Scala types (such as Int, Boolean, Box, etc.)
into constant nodes (IntConstant, BoxConstant, etc) of the corresponding type. (See also `Constant` class)

In case of any error it throws `BinderException`

## Typer
`SigmaTyper` takes an AST from the output of `SigmaBinder` and assigns types
to all tree nodes. Since AST is immutable data structure the typer produces a new tree. 

Type assignment is performed by `assignType` tree transformation which assign correct types for all 
tree nodes.

In case of any error it throws `TyperException`

## Graph Building

`IRContext.buildGraph` takes an AST from the output of `SigmaTyper` and builds a graph where nodes are operations and edges are dependencies between operations.
During graph building the following optimizations are performed:
- constant propagation
- common subexpression elimination
- dead code elimination

## Tree Building

`IRContext.buildTree` takes a graph from the output of `IRContext.buildGraph` and builds the resulting ErgoTree.
 
## IR contexts

- `IRContext` - the main interface of graph IR which mixes in both GraphBuilding and TreeBuilding traits.
  Since v5.0 it is not used by Interpreter and thus not part of consensus.

- `CompiletimeIRContext` - the main implementation of IRContext
