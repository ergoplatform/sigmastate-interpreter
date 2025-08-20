[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/scalan/ExactNumeric.scala)

The code defines a trait called `ExactNumeric` which provides numeric operations with overflow checks. The trait overrides three methods: `plus`, `minus`, and `times`. All other methods are implemented by delegating to the corresponding Numeric instance from the standard Scala library. The trait is used in core IR to avoid implicitly using standard Scala implementations.

The `ExactNumeric` trait takes a type parameter `T` which must have an implicit `Numeric` instance. The `Numeric` typeclass provides basic arithmetic operations for numeric types. The `ExactNumeric` trait provides additional overflow checks to ensure that the result of an operation does not exceed the maximum or minimum value of the type `T`. If an overflow is detected, an exception is raised.

The `ExactNumeric` trait provides methods to convert between `T` and `Int` or `Long`. It also provides values for `zero` and `one` of type `T`.

The `ExactNumeric` object provides two implicit conversions from `Int` and `Long` to `ExactNumeric`. These conversions use the `ExactIntegral` instances for `Int` and `Long` respectively. The `ExactIntegral` trait provides exact arithmetic operations for integer types.

This code is used in the larger project to provide numeric operations with overflow checks. It is used in core IR to avoid implicitly using standard Scala implementations. For example, it may be used in a compiler to ensure that arithmetic operations on integer types do not overflow. Here is an example of how the `ExactNumeric` trait may be used:

```scala
import scalan.ExactNumeric._

def add[T: ExactNumeric](x: T, y: T): T = {
  val n = implicitly[ExactNumeric[T]]
  n.plus(x, y)
}

val result = add(1000000000, 2000000000) // throws an exception due to overflow
```
## Questions: 
 1. What is the purpose of the `ExactNumeric` trait?
   
   The `ExactNumeric` trait defines numeric operations with overflow checks and raises an exception when overflow is detected. It is used in core IR to avoid implicitly using standard Scala implementations.

2. What methods does the `ExactNumeric` trait override?
   
   The `ExactNumeric` trait overrides three methods: `plus`, `minus`, and `times`.

3. How are `ExactNumeric` instances defined for `Int` and `Long`?
   
   `ExactNumeric` instances for `Int` and `Long` are defined as implicit values in the `ExactNumeric` object and are the same as the corresponding `ExactIntegral` instances (`IntIsExactIntegral` and `LongIsExactIntegral`). These instances are used wherever `ExactNumeric` is needed.