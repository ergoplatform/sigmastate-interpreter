[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/scalan)

The `.autodoc/docs/json/common/shared/src/main/scala/scalan` folder contains various utility classes and traits that can be used throughout the larger project to optimize performance, provide runtime type information, and extend the functionality of built-in Scala types.

For example, the `Nullable` class in `AnyVals.scala` can be used to avoid unnecessary allocations and memory accesses when working with optional values. This can be particularly useful in performance-critical code:

```scala
val nullableValue = new Nullable(42)
val result = nullableValue.getOrElse(0) // 42
```

The `DFunc` class in `DFunc.scala` allows defining specialized functions that can be called with unboxed values, improving performance:

```scala
class IntAddition extends DFunc[Int, Int] {
  def apply(x: Int): Int = x + 1
}

val addOne = new IntAddition
val result = addOne(5) // 6
```

The `ExactIntegral` type-class in `ExactIntegral.scala` provides arithmetic operations with overflow checks for integral types:

```scala
import scalan.ExactIntegral._

val x: Int = 2147483647
val y: Int = 1
val z: Int = x + y // throws ArithmeticException: integer overflow
```

The `Lazy` class in `Lazy.scala` can be used to create lazy values that are only evaluated once, saving time and resources:

```scala
val lazyValue = Lazy {
  println("Computing expensive value...")
  Thread.sleep(1000)
  42
}

println("Lazy value created.")
println(lazyValue.value)
println(lazyValue.value)
```

The `OverloadHack` object in `OverloadHack.scala` provides a workaround for method argument type erasure in Scala, allowing developers to define overloaded methods with distinct signatures:

```scala
def m1(l: List[Int])(implicit o: Overloaded1)
def m2(l: List[String])(implicit o: Overloaded2)
```

The `RType` hierarchy in `TypeDesc.scala` enables runtime type checking and ensures the correctness of scripts in the Sigma programming language:

```scala
val clazz = classOf[Boolean]
val srClass = CommonReflection.classes.get(clazz)
if (srClass.isDefined) {
  // do something with srClass
} else {
  // handle case where clazz is not registered
}
```

The utility functions and classes in the `util` subfolder provide additional functionality for working with collections, strings, graphs, and memoization. These utilities can be used throughout the larger project to perform common operations and extend the functionality of built-in Scala types.
