[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/scalan/util)

The `.autodoc/docs/json/common/shared/src/main/scala/scalan/util` folder contains utility functions and classes for working with collections, strings, graphs, and memoization. These utilities can be used throughout the larger project to perform common operations and extend the functionality of built-in Scala types.

`CollectionUtil.scala` provides utility functions for working with collections in Scala, such as concatenating arrays, computing hash codes, creating multimaps, and performing relational joins on sequences. The `AnyOps` and `TraversableOps` implicit classes provide additional methods for traversing trees and manipulating collections. Example usage:

```scala
import scalan.util.CollectionUtil._

val arr1 = Array(1, 2, 3)
val arr2 = Array(4, 5, 6)
val concatArr = concatArrays_v5(arr1, arr2) // Array(1, 2, 3, 4, 5, 6)
```

`Extensions.scala` contains implicit classes and methods that extend the functionality of built-in Scala types, such as converting between numeric types, performing arithmetic operations with overflow checking, and converting boolean values to bytes. The `Ensuring` implicit class provides a way to add runtime assertions to any value. Example usage:

```scala
import scalan.util.Extensions._

val x: Int = 100
val y: Short = 200
val z: Byte = 1

val xByte: Byte = x.toByteExact
val yInt: Int = y.toIntExact
val zShort: Short = z.toShortExact
```

`GraphUtil.scala` implements depth-first search algorithms to traverse a graph and return a set of reachable nodes or a topologically ordered sequence of reachable nodes. These methods can be used to analyze graphs and extract information about their structure. Example usage:

```scala
import scalan.util.GraphUtil

val graph = Map(1 -> List(2, 3), 2 -> List(4), 3 -> List(4), 4 -> List())
val starts = debox.Buffer.of(1)
val neighbours = (n: Int) => debox.Buffer.fromIterable(graph.getOrElse(n, List()))

val reachableNodes = GraphUtil.depthFirstSetFrom(starts)(neighbours)
val orderedNodes = GraphUtil.depthFirstOrderFrom(starts, neighbours)
```

`MemoizedFunc.scala` provides a class for transforming a given function into a memoized equivalent function, caching the results of a function for repeated invocations with the same argument. Example usage:

```scala
val memoizedFunc = new MemoizedFunc((x: Int) => x * x)

val result1 = memoizedFunc(5)
val result2 = memoizedFunc(5)
memoizedFunc.reset()
val result3 = memoizedFunc(5)
```

`StringUtil.scala` contains utility functions for manipulating strings, such as quoting, appending complex data structures to a `StringBuilder`, cleaning file names, and constructing file paths. The `StringUtilExtensions` class provides extension methods for strings, such as checking if a string is null or empty and providing a default value for empty strings. Example usage:

```scala
import scalan.util.StringUtil._

val input = "example.txt"
val cleanName = cleanFileName(input) // "example-txt"
```
