[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala)

The code in the `.autodoc/docs/json/common/shared/src/main/scala` folder and its subfolders play a crucial role in providing compatibility, utility functions, and language processing capabilities for the larger project. The folder contains three subfolders: `java7`, `scalan`, and `sigmastate`.

The `java7` subfolder contains the `Math.scala` file, which provides arithmetic operations ensuring compatibility with Java 1.7. This is essential for non-JVM contexts like RoboVM. The `Math` object contains methods like `addExact`, `subtractExact`, and `multiplyExact`, which perform arithmetic operations and throw an `ArithmeticException` if the result overflows the range of the corresponding type.

```scala
val result = Math.addExact(2, 3) // result is 5
```

The `scalan` subfolder contains utility classes and traits for optimizing performance, providing runtime type information, and extending the functionality of built-in Scala types. For example, the `Nullable` class can be used to avoid unnecessary allocations and memory accesses when working with optional values:

```scala
val nullableValue = new Nullable(42)
val result = nullableValue.getOrElse(0) // 42
```

The `sigmastate` subfolder manages the versioning of the Ergo protocol and ErgoTree, and provides utility functions for working with arrays. The `VersionContext.scala` file defines the `VersionContext` object, which represents the currently activated protocol version and the currently executed ErgoTree version.

```scala
val versionContext = VersionContext(activatedVersion, ergoTreeVersion)
if (versionContext.isJitActivated) {
  // Use JIT costing interpreter
} else {
  // Use another interpreter
}
```

The `util.scala` file provides utility functions for working with arrays, such as `safeNewArray` and `safeConcatArrays_v5`, which ensure that the length of the arrays is within the allowed limit.

```scala
val arr1: Array[Int] = Array(1, 2, 3)
val arr2: Array[Int] = Array(4, 5, 6)
val result: Array[Int] = util.safeConcatArrays_v5(arr1, arr2)
```

In summary, the code in this folder and its subfolders is essential for ensuring compatibility, providing utility functions, and managing versioning in the larger project. These components can be used for arithmetic operations, working with optional values, managing protocol and ErgoTree versions, and working with arrays safely.
