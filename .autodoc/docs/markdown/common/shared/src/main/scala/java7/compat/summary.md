[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/java7/compat)

The `Math.scala` file in the `.autodoc/docs/json/common/shared/src/main/scala/java7/compat` folder provides a set of arithmetic operations that are compatible with Java 1.7, even though they are available in Java 1.8. This compatibility is essential for non-JVM contexts like RoboVM. The methods in this file are copied from the JDK 1.8 sources to ensure accurate functionality.

The `Math` object contains six methods for performing arithmetic operations: `addExact`, `subtractExact`, and `multiplyExact`, each with two overloads for `Int` and `Long` types. These methods throw an `ArithmeticException` if the result of the operation overflows the range of the corresponding type.

For example, the `addExact` method returns the sum of its arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the signs of the arguments and the result. If both arguments have the opposite sign of the result, an overflow has occurred.

```scala
val result = Math.addExact(2, 3) // result is 5
```

Similarly, the `subtractExact` method returns the difference of the arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the signs of the arguments and the result. If the arguments have different signs and the sign of the result is different than the sign of the first argument, an overflow has occurred.

The `multiplyExact` method returns the product of the arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the result with the maximum value of the corresponding type. If the result is greater than the maximum value, an overflow has occurred.

These methods are useful for performing arithmetic operations in a way that ensures the result is within the range of the corresponding type. For example, if you need to add two `int` values and ensure that the result is within the range of an `int`, you can use the `addExact` method:

```scala
val result = Math.addExact(2, 3) // result is 5
```

If the result of the operation overflows the range of an `int`, an `ArithmeticException` is thrown. This functionality is essential for ensuring that arithmetic operations are performed safely and accurately within the context of the larger project, especially when working with non-JVM contexts like RoboVM.
