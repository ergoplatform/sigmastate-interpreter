[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/java7/compat/Math.scala)

The code in this file provides a set of methods for performing arithmetic operations that are not available in Java 1.7, but are present in Java 1.8. These methods are implemented in a way that supports compatibility with Java 1.7 in non-JVM contexts like RoboVM. The methods are copies from the JDK 1.8 sources.

The `Math` object contains six methods for performing arithmetic operations: `addExact`, `subtractExact`, and `multiplyExact`, each with two overloads for `Int` and `Long` types. These methods throw an `ArithmeticException` if the result of the operation overflows the range of the corresponding type.

The `addExact` method returns the sum of its arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the signs of the arguments and the result. If both arguments have the opposite sign of the result, an overflow has occurred.

The `subtractExact` method returns the difference of the arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the signs of the arguments and the result. If the arguments have different signs and the sign of the result is different than the sign of the first argument, an overflow has occurred.

The `multiplyExact` method returns the product of the arguments, throwing an exception if the result overflows an `int` or a `long`. The method checks for overflow by comparing the result with the maximum value of the corresponding type. If the result is greater than the maximum value, an overflow has occurred.

These methods are useful for performing arithmetic operations in a way that ensures the result is within the range of the corresponding type. For example, if you need to add two `int` values and ensure that the result is within the range of an `int`, you can use the `addExact` method:

```
val result = Math.addExact(2, 3) // result is 5
```

If the result of the operation overflows the range of an `int`, an `ArithmeticException` is thrown.
## Questions: 
 1. What is the purpose of this code?
- This code provides methods introduced since Java 1.8 which are not available in Java 1.7, supporting compatibility with Java 1.7 in non-JVM contexts like RoboVM.

2. What are the input and output of each method?
- The input of each method is either two integers or two longs, and the output is either an integer or a long. Each method throws an ArithmeticException if the result overflows an int or a long.

3. Why are there comments referencing HD 2-12?
- The comments referencing HD 2-12 are used to explain the overflow condition for each method. They refer to Hacker's Delight, a book that provides algorithms for performing arithmetic operations on binary numbers.