[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/interpreter/shared/src/main/scala/sigmastate/eval/BigIntegerOps.scala)

The code provided defines two objects, `OrderingOps` and `NumericOps`, which contain traits and implicit objects for handling ordering and arithmetic operations on BigIntegers. 

The `OrderingOps` object defines two traits, `BigIntegerOrdering` and `BigIntOrdering`, which extend the `Ordering` trait and implement the `compare` method for comparing `BigInteger` and `BigInt` values, respectively. The object also defines implicit objects for each trait, which can be used to provide ordering for `BigInteger` and `BigInt` values in other parts of the code.

The `NumericOps` object defines two traits, `BigIntIsIntegral` and `BigIntIsExactIntegral`, which extend the `Integral` and `ExactIntegral` traits, respectively, and provide implementations for arithmetic operations on `BigInt` values. The `BigIntIsIntegral` trait defines methods for addition, subtraction, multiplication, negation, and conversion to various numeric types, as well as a `rem` method for computing the remainder of a division operation. The `BigIntIsExactIntegral` trait extends `ExactIntegral` and provides implementations for the same arithmetic operations, as well as a `divisionRemainder` method for computing the remainder of a division operation. 

The `BigIntIsExactOrdering` object extends `ExactOrderingImpl` and provides an implementation of the `compare` method for comparing `BigInt` values using the `BigIntIsIntegral` trait. 

Overall, these objects provide a set of tools for handling ordering and arithmetic operations on `BigInteger` and `BigInt` values, which can be used in other parts of the project to perform computations and comparisons involving these types. For example, the `BigIntIsExactIntegral` trait could be used to perform arithmetic operations on `BigInt` values in a way that ensures exact results, while the `BigIntegerOrdering` and `BigIntOrdering` traits could be used to sort collections of `BigInteger` and `BigInt` values, respectively.
## Questions: 
 1. What is the purpose of the `OrderingOps` object?
- The `OrderingOps` object provides implicit ordering instances for `BigInteger` and `BigInt` types.

2. What is the difference between `BigIntIsIntegral` and `BigIntIsExactIntegral`?
- `BigIntIsIntegral` provides a base implementation of integral methods for `BigInt`, while `BigIntIsExactIntegral` is an instance of the `ExactIntegral` typeclass for `BigInt` that provides exact arithmetic operations.

3. What is the purpose of the `divisionRemainder` method in `BigIntIsExactIntegral`?
- The `divisionRemainder` method is used to implement the `%` operation of ErgoTree for all numeric types, including `BigInt`. It corresponds to the `mod` method of `java.math.BigInteger`.