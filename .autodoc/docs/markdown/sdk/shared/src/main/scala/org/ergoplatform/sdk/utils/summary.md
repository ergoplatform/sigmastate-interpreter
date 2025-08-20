[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk/shared/src/main/scala/org/ergoplatform/sdk/utils)

The `ArithUtils` object in the `org.ergoplatform.sdk.utils` package provides utility methods for performing arithmetic operations on `Long` values with overflow checking. This is particularly important in cryptographic applications where overflow can lead to security vulnerabilities.

The first method, `addExact`, takes two `Long` values as input and returns their sum. If the addition operation results in an overflow, the method returns `Long.MaxValue`. The method checks for overflow using the bitwise XOR operator. If the XOR of the two input values and the sum of the two input values is negative, then an overflow has occurred.

Example usage:

```scala
val a = 9223372036854775807L // Long.MaxValue
val b = 1L
val sum = ArithUtils.addExact(a, b) // returns Long.MaxValue
```

The second method, `addExact`, is an overloaded version of the first method that allows for an arbitrary number of `Long` values to be added together. The method uses the `foldLeft` method to iterate over the input values and accumulate the sum using the `addExact` method.

Example usage:

```scala
val a = 9223372036854775807L // Long.MaxValue
val b = 1L
val c = 2L
val d = 3L
val sum = ArithUtils.addExact(a, b, c, d) // returns Long.MaxValue
```

The third method, `multiplyExact`, multiplies two `Long` values and returns the result. If the multiplication operation results in an overflow, the method returns `Long.MaxValue`. The method is implemented using the `java7.compat.Math.multiplyExact` method, which throws an exception if an overflow occurs. The method catches the exception and returns `Long.MaxValue` instead.

Example usage:

```scala
val a = 9223372036854775807L // Long.MaxValue
val b = 2L
val product = ArithUtils.multiplyExact(a, b) // returns Long.MaxValue
```

These methods can be used in the larger project to perform arithmetic operations on `Long` values with the added safety of checking for overflow. For example, the `addExact` method could be used to add together the outputs of multiple cryptographic operations, while the `multiplyExact` method could be used to calculate the size of a data structure based on the number of elements and the size of each element.
