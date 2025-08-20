[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/graph-ir/src/main/scala/scalan/util)

The `Variance.scala` file in the `.autodoc/docs/json/graph-ir/src/main/scala/scalan/util` folder defines a sealed trait called `Variance` and three case objects that extend it: `Invariant`, `Covariant`, and `Contravariant`. These objects are used to represent the variance of type parameters in generic classes and functions within the larger project.

Variance is a concept in programming that refers to how subtyping between types relates to subtyping between their generic types. It determines how the subtyping of a generic type is affected by the subtyping of its type parameters.

The `Invariant` case object represents a type parameter that is not affected by subtyping. This means that a value of type A cannot be substituted for a value of type B, even if A is a subtype of B.

```scala
class InvariantContainer[T] // Invariant container
```

The `Covariant` case object represents a type parameter that is affected by subtyping in a positive way. This means that if A is a subtype of B, then a value of type F[A] is also a subtype of F[B], where F is a generic type.

```scala
class CovariantContainer[+T] // Covariant container
```

The `Contravariant` case object represents a type parameter that is affected by subtyping in a negative way. This means that if A is a subtype of B, then a value of type F[B] is a subtype of F[A].

```scala
class ContravariantContainer[-T] // Contravariant container
```

These case objects can be used in the larger project to define the variance of type parameters in generic classes and functions, ensuring that they behave correctly with respect to subtyping. For example, if we have a generic class that represents a container of some type T, we can specify its variance using one of the three case objects:

```scala
class Container[+T] // Covariant container
class Function[-T, +R] // Contravariant input, covariant output
class Pair[T, U <: T] // Invariant T, subtype U
```

By using these case objects, developers can ensure that their generic classes and functions are properly designed and implemented with respect to subtyping. This can help prevent potential issues and bugs related to incorrect subtyping behavior in the larger project.
