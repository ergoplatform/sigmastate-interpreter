[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/sigmastate/kiama/util/Comparison.scala)

# Kiama Comparison Utility Module

The `Comparison` module is a utility module for comparison routines in the Kiama project. The module provides several methods for comparing values, collections, and sequences. The module is located in the `sigmastate.kiama.util` package.

## `same(v1: Any, v2: Any): Boolean`

This method compares two arbitrary values. If both values are references and not tuples, it uses reference equality. If they are tuples, it uses `same` to compare the components. Otherwise, it uses value equality. The method returns a boolean value indicating whether the two values are the same.

## `TOrdering[T]`

This is a class that implements an ordering that says two values are equal if `same` says they are, otherwise earlier elements are greater than later ones.

## `sameCollection(v1: Any, v2: Any): Boolean`

This method compares two `Iterable` collections or options and tuples containing that kind of collection. It uses `same` to compare the individual elements in the same order. The method returns a boolean value indicating whether the two collections are the same.

## `sameElements[T](t1: Seq[_], t2: Seq[_]): Boolean`

This method compares two `Seq` collections or options and tuples containing that kind of collection. It uses `same` to compare the individual elements in any order. The method returns a boolean value indicating whether the two collections are the same.

## `optsame(v1: Any, v2: Any): Boolean`

This method is similar to `same`, except that if the two values are `Some` options containing references, they are unwrapped first, and the contents are compared by reference.

## `contains[T](s: Iterable[T], t: T): Boolean`

This method checks whether the iterable `s` contains `t`. Equality is tested using `same`.

## `distinct[T](s: Seq[T]): Vector[T]`

This method returns a vector with only the distinct elements from the sequence `s`. "Distinct" in this case means compare using `same`.

## `flatDistinct[T](ss: Seq[Seq[T]]): Vector[T]`

This method is similar to `distinct`, but it works over a sequence of sequences.

## `indexOf[T](s: Seq[T], elem: T): Int`

This method returns the first zero-based index at which `elem` occurs in `s` using `same` to perform comparisons, or -1 if `elem` does not occur in `s`.

## `lastIndexOf[T](s: Seq[T], elem: T): Int`

This method returns the last zero-based index at which `elem` occurs in `s` using `same` to perform comparisons, or -1 if `elem` does not occur in `s`.

Overall, the `Comparison` module provides a set of utility methods for comparing values, collections, and sequences in the Kiama project. These methods are useful for comparing AST nodes and other data structures in the project.
## Questions: 
 1. What is the purpose of the `Comparison` object?
- The `Comparison` object is a utility module for comparison routines.

2. What is the difference between the `same` and `optsame` methods?
- The `same` method compares two arbitrary values using value equality, while the `optsame` method compares two values and unwraps them if they are `Some` options containing references, then compares the contents by reference.

3. What does the `distinct` method do?
- The `distinct` method returns a vector with only the distinct elements from the sequence `s`, where "distinct" means compare using `same`.