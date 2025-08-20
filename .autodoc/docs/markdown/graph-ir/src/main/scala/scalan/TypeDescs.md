[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/graph-ir/src/main/scala/scalan/TypeDescs.scala)

The `TypeDescs` module provides a set of classes and methods for working with type descriptors in the Scalan framework. Type descriptors are used to represent the types of staged values and functions in the Scalan IR. The main classes in this module are `Elem`, `Cont`, and their subclasses.

`Elem[A]` is an abstract class representing a type descriptor for a staged type `A`. It provides methods for working with type arguments, lifting and unlifting types, and invoking methods on source types. There are several concrete subclasses of `Elem`, such as `BaseElem`, `PairElem`, `SumElem`, and `FuncElem`, which represent different kinds of staged types.

`Cont[F[_]]` is an abstract class representing a type constructor of kind `* -> *`. It provides methods for lifting and unlifting type descriptors, as well as recognizing type descriptors constructed by the type constructor. The `Functor[F[_]]` trait extends `Cont[F[_]]` and adds a `map` method for mapping over the elements of a container type.

The module also provides several utility methods and implicit conversions for working with type descriptors. For example, `pairElement`, `sumElement`, and `funcElement` methods create type descriptors for pairs, sums, and functions, respectively. The `toLazyElem` method converts an `Elem[A]` to a lazy `LElem[A]`. The `invokeUnlifted` method is used to invoke a source type method corresponding to a given `MethodCall` node in the Scalan IR.

In summary, the `TypeDescs` module is an essential part of the Scalan framework, providing the necessary abstractions and utilities for working with type descriptors in the staged computation setting.
## Questions: 
 1. **What is the purpose of the `TypeDescs` abstract class?**

   The `TypeDescs` abstract class is used to define various type descriptors and related utility methods for working with types in the Scalan project. It provides type descriptors for primitive types, pair, sum, and function types, as well as type constructors and functors.

2. **How does the `Elem` abstract class work?**

   The `Elem` abstract class represents a type descriptor for staged types, which correspond to source (unstaged) RTypes defined outside of the IR cake. It provides methods for working with type arguments, lifting and invoking methods on the source type, and checking type compatibility.

3. **What is the purpose of the `Cont` abstract class?**

   The `Cont` abstract class represents a descriptor of a type constructor of `* -> *` kind. It provides methods for lifting and unlifting type descriptors, recognizing type descriptors constructed by the type constructor, and checking if the type constructor is an instance of the Functor type class.