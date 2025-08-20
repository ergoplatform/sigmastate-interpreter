[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/graph-ir/src/main/scala/wrappers/scalan)

The `.autodoc/docs/json/graph-ir/src/main/scala/wrappers/scalan` folder contains code related to the Scalan framework, specifically for defining and working with wrapped types. Wrapped types are types that are represented as objects at runtime and are used to provide additional functionality or constraints on the type.

The main file in this folder is `WRTypes.scala`, which defines a trait called `WRTypes`. This trait extends another trait called `Base` and requires a dependency on a module called `WrappersModule`. The `WRTypes` trait contains two nested traits: `WRType` and `WRTypeCompanion`. The `WRType` trait is a type class that defines a wrapped type `A` and requires an implicit `Elem[A]`, which is a type descriptor for `A`. The `WRTypeCompanion` trait is an empty trait that serves as a marker for companion objects of wrapped types.

Here's an example of how this code could be used to define a wrapped type for non-negative integers:

```scala
trait NonNegativeInt
object NonNegativeInt extends WRTypeCompanion {
  implicit val nonNegativeIntElem: Elem[NonNegativeInt] = new WRType[NonNegativeInt] {
    implicit def eA: Elem[NonNegativeInt] = this
    def name: Ref[String] = "NonNegativeInt"
  }
}
```

The `impl` subfolder contains the `WRTypesImpl.scala` file, which provides functionality for working with wrapped types in the context of a graph. The main component of this file is the `WRTypeCls` class, which provides the core functionality for creating and manipulating wrapped types. It defines several methods and classes for working with wrapped types, such as `WRTypeConst`, `WRTypeConstMethods`, `LiftableRType`, `WRTypeAdapter`, and `WRTypeElem`.

Here's an example of how this code might be used:

```scala
val wrappedIntType = WRTypeCls[Int]
val wrappedIntConst = wrappedIntType.const(42)
val liftedWrappedInt = LiftableRType.lift(42)
val adaptedWrappedInt = WRTypeAdapter(wrappedIntConst)
```

In this example, we create a wrapped type for integers, create a wrapped integer constant with the value 42, lift the integer value 42 into a wrapped type, and adapt the wrapped integer constant to a node in a graph.

In summary, the code in this folder is an essential part of the Scalan framework, providing functionality for defining and working with wrapped types. It defines several classes and methods for creating, manipulating, and adapting wrapped types, making it easier to work with types and their associated operations in the context of a graph.
