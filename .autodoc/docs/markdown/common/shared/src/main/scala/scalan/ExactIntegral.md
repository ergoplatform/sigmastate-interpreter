[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/scalan/ExactIntegral.scala)

The code defines a type-class called ExactIntegral, which provides operations on integral types (Byte, Short, Int, Long, BigInt) with overflow checks. The purpose of this type-class is to avoid implicitly using standard Scala implementations in the core IR (Intermediate Representation) of a larger project. 

The type-class has two methods: `quot` and `divisionRemainder`. The `quot` method performs integer division operation `x / y`, while the `divisionRemainder` method returns the remainder from dividing x by y. The exact rules for the `divisionRemainder` method are defined in the concrete instance of the type T. By default, all the methods are implemented by delegating to the corresponding Integral instance from the standard Scala library. 

Each concrete instance of the type-class overrides three methods: `plus`, `minus`, and `times`. An exception is raised when an overflow is detected. The purpose of overriding these methods is to provide overflow checks for the operations. 

The code also defines four implicit objects: `ByteIsExactIntegral`, `ShortIsExactIntegral`, `IntIsExactIntegral`, and `LongIsExactIntegral`. These objects are ExactIntegral instances for Byte, Short, Int, and Long types respectively. Each object overrides the three methods `plus`, `minus`, and `times` to provide overflow checks for the operations. 

For example, the `ByteIsExactIntegral` object overrides the three methods as follows: `plus` method calls `addExact` method on the first argument `x` with the second argument `y`. Similarly, `minus` method calls `subtractExact` method on the first argument `x` with the second argument `y`, and `times` method calls `multiplyExact` method on the first argument `x` with the second argument `y`. 

Overall, the purpose of this code is to provide a type-class that defines operations on integral types with overflow checks. The code can be used in the core IR of a larger project to avoid implicitly using standard Scala implementations. The code also provides four implicit objects that can be used to perform operations on Byte, Short, Int, and Long types with overflow checks. 

Example usage:
```scala
import scalan.ExactIntegral._

val x: Int = 2147483647
val y: Int = 1
val z: Int = x + y // throws ArithmeticException: integer overflow
```
## Questions: 
 1. What is the purpose of the `ExactIntegral` trait?
    
    The `ExactIntegral` trait defines operations on integral types with overflow checks and is used in core IR to avoid implicitly using standard Scala implementations.

2. How are the `plus`, `minus`, and `times` methods implemented for each concrete instance of `ExactIntegral`?
    
    Each concrete instance of `ExactIntegral` overrides the `plus`, `minus`, and `times` methods by delegating to the corresponding integral instance from the standard Scala library, with the exception of `Int` and `Long` which use the `java7.compat.Math` methods.

3. What is the purpose of the `ExactIntegral` object?
    
    The `ExactIntegral` object provides implicit instances of `ExactIntegral` for all integral types, including `Byte`, `Short`, `Int`, and `Long`.