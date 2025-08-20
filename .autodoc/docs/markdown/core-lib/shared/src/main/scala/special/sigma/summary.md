[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/core-lib/shared/src/main/scala/special/sigma)

The `special.sigma` package in the `core-lib` project contains the core implementation of the Sigma language, which is a domain-specific language (DSL) for writing smart contracts on the Ergo platform. This package is located at `.autodoc/docs/json/core-lib/shared/src/main/scala/special/sigma`. The code in this folder is responsible for defining the data structures, operations, and functions that are used to create, manipulate, and validate Sigma expressions.

Here is a brief overview of the main files in this folder:

1. `SigmaDsl.scala`: This file defines the main entry point for the Sigma DSL. It provides a high-level API for creating and manipulating Sigma expressions, as well as for evaluating and validating them. The `SigmaDsl` object extends the `SigmaDslBuilder` trait, which defines the core functionality of the DSL.

   Example usage:
   ```scala
   import special.sigma.SigmaDsl

   val sigmaExpr = SigmaDsl.sigmaProp(SigmaDsl.anyOf(Seq(SigmaDsl.Height > 100, SigmaDsl.Self.R4[Long].get > 1000)))
   ```

2. `SigmaDslBuilder.scala`: This file defines the `SigmaDslBuilder` trait, which is the core interface for building Sigma expressions. It provides methods for creating constants, variables, and operations, as well as for constructing complex expressions using combinators.

   Example usage:
   ```scala
   import special.sigma.SigmaDslBuilder

   class MySigmaDslBuilder extends SigmaDslBuilder {
     // Implement the required methods here
   }

   val myBuilder = new MySigmaDslBuilder()
   val sigmaExpr = myBuilder.sigmaProp(myBuilder.anyOf(Seq(myBuilder.Height > 100, myBuilder.Self.R4[Long].get > 1000)))
   ```

3. `SigmaDslTypes.scala`: This file defines the data types used in the Sigma DSL, such as `SigmaProp`, `Box`, `AvlTree`, and `Context`. These types are used to represent the various elements of a Sigma expression, and they provide methods for manipulating and validating the data they contain.

   Example usage:
   ```scala
   import special.sigma.SigmaDslTypes._

   val box: Box = ...
   val value: Long = box.value
   val scriptBytes: Coll[Byte] = box.propositionBytes
   ```

4. `SigmaDslFuncs.scala`: This file defines the functions that can be used in Sigma expressions, such as `anyOf`, `allOf`, `atLeast`, and `blake2b256`. These functions are used to create complex expressions and to perform operations on the data types defined in `SigmaDslTypes.scala`.

   Example usage:
   ```scala
   import special.sigma.SigmaDslFuncs._

   val sigmaProps: Seq[SigmaProp] = ...
   val combinedSigmaProp = anyOf(sigmaProps)
   ```

In summary, the `special.sigma` package provides the core implementation of the Sigma language, which is used for writing smart contracts on the Ergo platform. The code in this folder defines the data structures, operations, and functions that are used to create, manipulate, and validate Sigma expressions. Developers working with the Ergo platform can use the Sigma DSL to create complex smart contracts and to interact with the Ergo blockchain.
