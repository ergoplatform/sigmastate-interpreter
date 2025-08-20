[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/sigmastate)

The code in the `sigmastate` folder is responsible for managing the versioning of the Ergo protocol and ErgoTree, as well as providing utility functions for working with arrays in the context of the larger project. The `VersionContext.scala` file defines the `VersionContext` object, which represents the currently activated protocol version and the currently executed ErgoTree version. This object is used to version the code across the whole repository and ensure that the correct versions are being used throughout the codebase.

For example, to check if the JIT costing interpreter should be used, you can call the `isJitActivated` method:

```scala
val versionContext = VersionContext(activatedVersion, ergoTreeVersion)
if (versionContext.isJitActivated) {
  // Use JIT costing interpreter
} else {
  // Use another interpreter
}
```

The `util.scala` file provides utility functions for working with arrays, such as `safeNewArray` and `safeConcatArrays_v5`. These functions ensure that the length of the arrays is within the allowed limit, preventing potential issues with memory allocation or array manipulation.

```scala
val arr1: Array[Int] = Array(1, 2, 3)
val arr2: Array[Int] = Array(4, 5, 6)
val result: Array[Int] = util.safeConcatArrays_v5(arr1, arr2)
```

The `kiama` subfolder contains code from the Kiama Scala library for language processing, which can be used in the larger project for various language processing tasks, such as tree decoration, tree transformation, dynamic semantics, and pretty-printing. The `rewriting` subfolder contains code for term rewriting strategies, which can be used to transform and manipulate code in the larger project. The `util` subfolder provides utility methods for comparing values, collections, and sequences, which can be useful when working with Abstract Syntax Tree (AST) nodes and other data structures in the project.

In summary, the code in the `sigmastate` folder plays a crucial role in managing the versioning of the Ergo protocol and ErgoTree, providing utility functions for working with arrays, and offering various components and utilities for language processing tasks in the larger project. These components can be used for tree decoration, tree transformation, dynamic semantics, and pretty-printing, as well as for comparing values, collections, and sequences.
