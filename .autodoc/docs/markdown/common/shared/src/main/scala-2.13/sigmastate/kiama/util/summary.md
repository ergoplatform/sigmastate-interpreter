[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala-2.13/sigmastate/kiama/util)

The `Collections.scala` file in the `.autodoc/docs/json/common/shared/src/main/scala-2.13/sigmastate/kiama/util` folder provides utility functions for working with collections in Scala and Java. These utility functions are particularly useful when working with mixed Scala and Java codebases, as they simplify the process of converting between collection types and building collections of the desired type.

The `Collections` object contains the following methods:

1. `javaCollectionToVector`: This method takes a Java `Collection` and converts it to a Scala `Vector`. This can be useful when working with Java libraries that return collections that need to be used in Scala code. For example:

```scala
import java.util.ArrayList
import sigmastate.kiama.util.Collections._

val javaList = new ArrayList[String]()
javaList.add("Hello")
javaList.add("World")

val scalaVector = javaCollectionToVector(javaList)
```

2. `mapToJavaMap`: This method takes a Scala `Map` and converts it to a Java `Map`. This can be useful when working with Java libraries that require Java maps as input. For example:

```scala
import java.util.Map
import sigmastate.kiama.util.Collections._

val scalaMap = Map("one" -> 1, "two" -> 2)
val javaMap: Map[String, Integer] = mapToJavaMap(scalaMap)
```

3. `seqToJavaList`: This method takes a Scala `Seq` and converts it to a Java `List`. This can be useful when working with Java libraries that require Java lists as input. For example:

```scala
import java.util.List
import sigmastate.kiama.util.Collections._

val scalaSeq = Seq("one", "two", "three")
val javaList: List[String] = seqToJavaList(scalaSeq)
```

4. `newBuilder`: This method creates a new `Builder` instance for a given collection type. There are two overloads for this method:

   - The first overload takes a `Factory` instance, which is used to create a new collection of the desired type. For example:

     ```scala
     import scala.collection.mutable.ArrayBuffer
     import sigmastate.kiama.util.Collections._

     val factory = ArrayBuffer
     val builder = newBuilder(factory)
     ```

   - The second overload takes a `CanBuildFrom` instance, which is used to create a new collection of the desired type from an existing collection. For example:

     ```scala
     import scala.collection.immutable.Vector
     import sigmastate.kiama.util.Collections._

     val canBuildFrom = implicitly[CanBuildFrom[Vector[Int], Int, Vector[Int]]]
     val builder = newBuilder(canBuildFrom)
     ```

In summary, the utility functions provided in the `Collections.scala` file can be used to simplify working with collections in mixed Scala and Java codebases. They provide a convenient way to convert between collection types and build collections of the desired type.
