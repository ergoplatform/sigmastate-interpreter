[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/scalan/ExactOrdering.scala)

The code defines a trait and a class for ordering operations to be used with other Exact traits. The purpose of this code is to provide a way to compare values of different types in a consistent and exact manner. The trait, ExactOrdering[T], extends the Ordering[T] trait from the standard Scala library and overrides the compare method to delegate to the corresponding Ordering instance from the standard library. This trait is used in core IR to avoid implicitly using standard Scala implementations.

The class, ExactOrderingImpl[T], takes an Ordering[T] instance as a parameter and extends the ExactOrdering[T] trait. It provides an implementation for the n parameter of the trait.

The object, ExactOrdering, provides ExactOrdering instances for all types, including Byte, Short, Int, and Long. It defines implicit objects for each type that extend the ExactOrderingImpl[T] class and pass in the corresponding Ordering instance from the standard library.

This code can be used in the larger project to ensure that values of different types are compared in a consistent and exact manner. For example, if the project involves sorting data of different types, the ExactOrdering instances can be used to ensure that the sorting is done in an exact and consistent way. 

Code example:

```
import scalan.ExactOrdering

val list = List(1.toByte, 2.toByte, 3.toByte)
val sortedList = list.sorted(ExactOrdering.ByteIsExactOrdering)
```

In this example, a list of Byte values is sorted using the ByteIsExactOrdering instance of the ExactOrdering trait. This ensures that the sorting is done in an exact and consistent way, regardless of the type of the values in the list.
## Questions: 
 1. What is the purpose of the ExactOrdering trait and how is it implemented?
   
   The ExactOrdering trait is used in core IR to avoid implicitly using standard scala implementations. It delegates to the corresponding Ordering instance from the standard Scala library.

2. What is the purpose of the ExactOrderingImpl class and how is it used?
   
   The ExactOrderingImpl class is used to create instances of ExactOrdering for specific types. It takes an Ordering instance as a parameter and delegates to it.

3. What types are supported by the ExactOrdering object and how are they implemented?
   
   The ExactOrdering object supports Byte, Short, Int, and Long types. They are implemented using implicit objects that extend ExactOrderingImpl and take the corresponding Ordering instance from the Numeric object in the Scala library.