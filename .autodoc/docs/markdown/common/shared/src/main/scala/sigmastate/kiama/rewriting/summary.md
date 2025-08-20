[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/common/shared/src/main/scala/sigmastate/kiama/rewriting)

The code in this folder is part of the Kiama project and is located in the `sigmastate.kiama.rewriting` package. It provides a set of rewriting strategies for terms of any type, allowing for powerful term rewriting with callbacks. This can be used in the larger project to provide a flexible and extensible mechanism for term rewriting.

**CallbackRewriter.scala** extends the `Rewriter` trait and provides a method `rewriting` that is called whenever a rewrite operation has happened. This method takes two arguments, the old term and the new term, and returns a term that should go forward as the new term. The `dispatch`, `rule`, `rulef`, `rulefs`, `strategy`, `strategyf`, and `dup` methods are overridden to use the `dispatch` method to create a new strategy that can be used to rewrite terms.

**PlusStrategy.scala** is a helper class that is used to contain commonality of choice in non-deterministic choice operator and then-else part of a conditional choice. It provides a way to define non-deterministic choice operators and conditional choices, allowing for the creation of complex strategies that can be used to transform and manipulate code. For example:

```scala
val p = new PlusStrategy(s1, s2)
val result = p.apply(input)
```

In this example, `s1` and `s2` are two strategies that are combined using the `PlusStrategy` class to create a non-deterministic choice operator. The `apply` method is then called on the resulting `PlusStrategy` object with an input argument `input`. The `apply` method applies the combined strategy to the input and returns the result.

**Strategy.scala** defines the `Strategy` class, which is abstract and defines a function that takes a term of any type as input and either succeeds producing a new term (`Some`), or fails (`None`). The class also defines several methods for composing strategies, including sequential composition, deterministic choice, and non-deterministic choice. These strategies can be used to transform terms in a larger project. For example:

```scala
val expr: Expr = ... // some expression
val rules: Strategy = ... // set of rewriting strategies
val simplified = rules(expr).getOrElse(expr)
```

This code applies the set of rewriting strategies to the expression `expr`. If any of the strategies succeed, the resulting term is returned. Otherwise, the original expression is returned. This allows us to simplify the expression using a set of rewriting rules, without having to manually apply each rule in turn.

Overall, the code in this folder provides a powerful mechanism for term rewriting with callbacks. It allows clients to register functions that are called whenever a rewrite operation has happened, and provides a set of methods that can be used to create new strategies for rewriting terms.
