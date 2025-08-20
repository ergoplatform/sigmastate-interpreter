[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/common/shared/src/main/scala/sigmastate/kiama/rewriting/PlusStrategy.scala)

## Code Explanation: PlusStrategy

The `PlusStrategy` class is a helper class that is used to contain commonality of choice in non-deterministic choice operator and then-else part of a conditional choice. This class is only returned by the non-deterministic choice operator. The `p` and `q` parameters are evaluated at most once.

The `PlusStrategy` class has three properties:

1. `left`: The left alternative of the choice.
2. `right`: The right alternative of the choice.
3. `s`: The strategy itself (lazily computed).

The `left` and `right` properties are lazily evaluated, meaning that they are only computed when they are needed. The `s` property is also lazily computed and is the result of applying the `<+` operator to the `left` and `right` properties.

The `apply` method is the implementation of this strategy. It takes an argument `t` of type `Any` and applies the `s` strategy to it.

This class is useful in the larger project as it provides a way to define non-deterministic choice operators and conditional choices. It allows for the creation of complex strategies that can be used to transform and manipulate code. Here is an example of how this class can be used:

```scala
val p = new PlusStrategy(s1, s2)
val result = p.apply(input)
```

In this example, `s1` and `s2` are two strategies that are combined using the `PlusStrategy` class to create a non-deterministic choice operator. The `apply` method is then called on the resulting `PlusStrategy` object with an input argument `input`. The `apply` method applies the combined strategy to the input and returns the result.
## Questions: 
 1. What is the purpose of the `PlusStrategy` class?
- The `PlusStrategy` class is a helper class that contains commonality of choice in non-deterministic choice operator and then-else part of a conditional choice. It is only returned by the non-deterministic choice operator.

2. What are `p` and `q` in the `PlusStrategy` class?
- `p` and `q` are lazy evaluated strategies that represent the left and right alternatives of the choice respectively.

3. What is the `apply` method in the `PlusStrategy` class?
- The `apply` method is the implementation of the `Strategy` trait that applies the `s` strategy, which is lazily computed as the combination of the left and right alternatives of the choice.