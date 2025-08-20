[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/core-lib/shared/src/main/scala/special/wrappers)

The `WrappersSpec.scala` file, located in the `special.wrappers` package, provides two wrapper classes, `OptionWrapSpec` and `RTypeWrapSpec`, for the `Option` and `RType` data types in Scala, respectively. These wrappers simplify the manipulation of `Option` and `RType` objects, making the code more concise and readable.

The `OptionWrapSpec` class offers several methods for handling `Option` objects:

- `get`: Returns the value of the `Option` object if it is not empty, otherwise throws a `NoSuchElementException`.
- `getOrElse`: Returns the value of the `Option` object if it is not empty, otherwise returns the default value provided as an argument.
- `map`: Applies a function to the value of the `Option` object and returns a new `Option` object with the result.
- `filter`: Applies a predicate function to the value of the `Option` object and returns a new `Option` object with the value if the predicate is true, otherwise returns an empty `Option` object.
- `isDefined`: Returns true if the `Option` object is not empty, otherwise returns false.

Example usage of `OptionWrapSpec`:

```scala
val opt: Option[Int] = Some(5)
val default: Int = 0

val value: Int = OptionWrapSpec.get(opt) // returns 5
val valueOrDefault: Int = OptionWrapSpec.getOrElse(opt, default) // returns 5
val doubled: Option[Int] = OptionWrapSpec.map(opt, x => x * 2) // returns Some(10)
val filtered: Option[Int] = OptionWrapSpec.filter(opt, x => x > 10) // returns None
val isDefined: Boolean = OptionWrapSpec.isDefined(opt) // returns true
```

The `RTypeWrapSpec` class is a wrapper for the `RType` data type in the `scalan` library. It defines a single method `name` that returns the name of the type represented by the `RType` object. This method can be used to retrieve the name of a type represented by an `RType` object without having to access the `name` field directly.

Example usage of `RTypeWrapSpec`:

```scala
import scalan.RType

val rType: RType[Int] = RType[Int]
val typeName: String = RTypeWrapSpec.name(rType) // returns "Int"
```

These wrapper classes can be utilized in the larger project to streamline the manipulation of `Option` and `RType` objects. For instance, the `OptionWrapSpec` methods can be used instead of the built-in `Option` methods, making the code more concise and readable. Similarly, the `RTypeWrapSpec` methods can be employed to retrieve the name of a type represented by an `RType` object without directly accessing the `name` field.
