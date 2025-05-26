[![CI](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml/badge.svg)](https://github.com/ScorexFoundation/sigmastate-interpreter/actions/workflows/ci.yml)

# ErgoScript compiler and ErgoTree interpreter

This repository contains implementations of ErgoScript compiler and ErgoTree
Interpreter for a family of Sigma-protocol based authentication languages (or simply
Sigma language).

This JS package is cross-compiled from [Scala
implementation](https://github.com/ScorexFoundation/sigmastate-interpreter) using Scala.js
compiler.

The modules published here can be used directly from JavaScript.

# Getting Started

Run following command to add Sigma.JS as a project dependency:

```bash
npm install sigmastate-js
```

## Package organization

All classes of this package are separated into several modules (which can also be thought
as layers). Each module contains a subset of all the class exported to JavaScript. You can
decide which modules to import in your application depending on which classes from this
package you want to use. 
Each subsequent module contains all the classes from the previous modules and some new
classes thus forming a layering of modules.

See TypeScript [definitions](sigmastate-js.d.ts) for the list of all exported classes,
methods and documentation.

NOTE, you only need to import only one of the modules, the one which contains all the
classes you need. This will allow optimizing the size of the final bundle.

The modules are compiled from Scala classes, which are exported to JavaScript (here is an
[example](../core/js/src/main/scala/sigma/js/Type.scala)).
The Scala declarations correspond to the TypeScript definitions. 
Each exported type have two parts: the first part is a Scala class, the second part is a
companion object. In Scala the companion object is used to declare static methods and has
the same name as the corresponding class. In TypeScript the companion object is exported
with `$` suffix, thus if X is the JS class, then X$ is the JS object, which corresponds to X.

## The list of modules and their exported classes
- [sigma-core module](../core/js) - contains core classes of Sigma.js library
  - [Type](../core/js/src/main/scala/sigma/js/Type.scala)
  - [GroupElement](../core/js/src/main/scala/sigma/js/GroupElement.scala)
  - [SigmaProp](../core/js/src/main/scala/sigma/js/SigmaProp.scala)
  - [AvlTree](../core/js/src/main/scala/sigma/js/AvlTree.scala)
  
- [sigma-data module](../data/js) - contains classes for working with ErgoTree, addresses and all related serializers 
  - all classes from sigma-core module
  - [Value](../data/js/src/main/scala/sigma/js/Value.scala)
  - [ErgoTree](../data/js/src/main/scala/sigma/ast/js/ErgoTree.scala)
  - [Box](../data/js/src/main/scala/sigma/js/Box.scala)
  - [Address](../data/js/src/main/scala/org/ergoplatform/js/Address.scala)
  - [Expr](../data/js/src/main/scala/sigma/ast/js/Expr.scala)
  
- [sigma-interpreter module](../interpreter/js) - contains classes for proving sigma proposition and their verification 
  - all classes from sigma-data module
  - [ProverHints](../interpreter/js/src/main/scala/sigma/interpreter/js/ProverHints.scala)
  - [ProverSecret](../interpreter/js/src/main/scala/sigma/interpreter/js/ProverSecret.scala)
  - [SigmaPropProver](../interpreter/js/src/main/scala/sigma/interpreter/js/SigmaPropProver.scala)
  - [SigmaPropVerifier](../interpreter/js/src/main/scala/sigma/interpreter/js/SigmaPropVerifier.scala)

- [sigma-sdk module](../sdk/js) - contains classes for reducing and signing transactions
  - all classes from sigma-interpreter module
  - [BlockchainParameters](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/BlockchainParameters.scala)
  - [BlockchainStateContext](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/BlockchainStateContext.scala)
  - [ContractTemplate](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/ContractTemplate.scala)
  - [Header](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/Header.scala)
  - [PreHeader](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/PreHeader.scala)
  - [ProverBuilder](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/ProverBuilder.scala)
  - [ReducedTransaction](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/ReducedTransaction.scala)
  - [SigmaProver](../sdk/js/src/main/scala/org/ergoplatform/sdk/js/SigmaProver.scala)

- [sigma-compiler module](../sc/js) - contains classes for working with ErgoScript compiler
  - [SigmaCompiler](../sc/js/src/main/scala/sigmastate/lang/js/SigmaCompiler.scala)
  
## Examples

### How to create Sigma type descriptors

Import `Type$` module, then use its fields to access pre-defined descriptors of simple
types (e.g. `Type$.Int`).

Use factory methods like `Type$.pairType` to create more complex type descriptors. For
example,`Type$.pairType(Type$.Int, Type$.Long)` will create a descriptor of a pair of Int
and Long types `(Int, Long)`.

See also examples in tests [Type.spec.js](tests/js/Type.spec.js)

### How to create Sigma values

Import `Value$` module, then use its factory methods.
See examples in tests [Value.spec.js](tests/js/Value.spec.js)

### How to work with ErgoTree

Import `ErgoTree$` module, and `ErgoTree` class then use its methods.
See examples in tests [ErgoTree.spec.js](tests/js/ErgoTree.spec.js)

### Compile ErgoScript to ErgoTree

Import `SigmaCompiler$` module and `SigmaCompiler` class, then use its methods.
See compiler tests in [SigmaCompiler.spec.js](tests/js/SigmaCompiler.spec.js)

### Other examples
See tests in [tests/js](tests/js) folder.
