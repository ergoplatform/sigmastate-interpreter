[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/core-lib/shared/src/main/scala/special)

The `special` package in the `core-lib` project contains essential components for working with collections, reflection, and the Sigma language. It provides utility functions, data structures, and methods for creating, manipulating, and validating various data types and expressions.

In the `CoreLibReflection.scala` file, classes and their methods are registered for reflection, allowing methods to be called dynamically at runtime. This is useful in larger projects where the code structure may not be known at compile time. For example, the `SigmaProp` class registers methods like `$bar$bar`, `isValid`, and `propBytes`.

The `SpecialPredef.scala` file provides a set of utility functions and objects for common operations such as looping, casting, and handling Options. For instance, the `loopUntil` function can be used to perform a loop until a certain condition is met:

```scala
val result = loopUntil(0, (x: Int) => x >= 10, (x: Int) => x + 1) // result: 10
```

The `Types.scala` file defines types and functions related to tuples, allowing the representation and manipulation of tuples of arbitrary types in a type-safe manner. For example, you can create a tuple of `Int`, `String`, and `Double`:

```scala
import special.Types._

val t = Coll(1, "two", 3.0)
val rt = tupleRType(Array(RType.IntType, RType.StringType, RType.DoubleType))
```

The `collection` subfolder provides classes and methods for working with collections in a Scala project, such as `Coll`, `PairColl`, and `CollBuilder`. For example, you can create a collection of integers and find its length:

```scala
val coll = CollBuilder.fromItems(1, 2, 3, 4, 5)
val length = coll.length // 5
```

The `sigma` subfolder contains the core implementation of the Sigma language, a DSL for writing smart contracts on the Ergo platform. Developers can use the Sigma DSL to create complex smart contracts and interact with the Ergo blockchain. For example, you can create a Sigma expression:

```scala
import special.sigma.SigmaDsl

val sigmaExpr = SigmaDsl.sigmaProp(SigmaDsl.anyOf(Seq(SigmaDsl.Height > 100, SigmaDsl.Self.R4[Long].get > 1000)))
```

The `wrappers` subfolder provides wrapper classes for the `Option` and `RType` data types, simplifying their manipulation. For instance, the `OptionWrapSpec` methods can be used instead of the built-in `Option` methods:

```scala
val opt: Option[Int] = Some(5)
val valueOrDefault: Int = OptionWrapSpec.getOrElse(opt, 0) // returns 5
```

Overall, the `special` package offers essential components for working with collections, reflection, and the Sigma language, streamlining the development process and enhancing code readability and maintainability.
