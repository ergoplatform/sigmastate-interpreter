[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/sigmastate/kiama/util)

The `Comparison` module in the `.autodoc/docs/json/common/shared/src/main/scala/sigmastate/kiama/util` folder is a utility module that provides various methods for comparing values, collections, and sequences in the Kiama project. These methods are particularly useful for comparing Abstract Syntax Tree (AST) nodes and other data structures in the project.

For example, the `same(v1: Any, v2: Any): Boolean` method can be used to compare two arbitrary values. If both values are references and not tuples, it uses reference equality. If they are tuples, it uses `same` to compare the components. Otherwise, it uses value equality. This method returns a boolean value indicating whether the two values are the same.

```scala
val a = (1, 2)
val b = (1, 2)
val c = a
println(same(a, b)) // false
println(same(a, c)) // true
```

The `sameCollection(v1: Any, v2: Any): Boolean` method can be used to compare two `Iterable` collections or options and tuples containing that kind of collection. It uses `same` to compare the individual elements in the same order.

```scala
val list1 = List(1, 2, 3)
val list2 = List(1, 2, 3)
val list3 = List(1, 2, 4)
println(sameCollection(list1, list2)) // true
println(sameCollection(list1, list3)) // false
```

The `distinct[T](s: Seq[T]): Vector[T]` method can be used to return a vector with only the distinct elements from the sequence `s`. "Distinct" in this case means compare using `same`.

```scala
val seq = Seq(1, 2, 2, 3, 3, 3)
val distinctSeq = distinct(seq)
println(distinctSeq) // Vector(1, 2, 3)
```

The `indexOf[T](s: Seq[T], elem: T): Int` method can be used to find the first zero-based index at which `elem` occurs in `s` using `same` to perform comparisons, or -1 if `elem` does not occur in `s`.

```scala
val seq = Seq("apple", "banana", "orange")
println(indexOf(seq, "banana")) // 1
println(indexOf(seq, "grape")) // -1
```

In summary, the `Comparison` module provides a set of utility methods for comparing values, collections, and sequences in the Kiama project. These methods are useful for comparing AST nodes and other data structures in the project, and can be used in various parts of the project where such comparisons are required.
