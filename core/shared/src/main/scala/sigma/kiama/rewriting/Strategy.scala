/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package sigma.kiama.rewriting

/**
  * Any-rewriting strategies. A strategy is a function that takes a term
  * of any type as input and either succeeds producing a new term (`Some`),
  * or fails (`None`).
  */
abstract class Strategy extends (Any => Option[Any]) {

  /**
    * Alias this strategy as `p` to make it easier to refer to in the
    * combinator definitions.
    */
  p =>

  /**
    * Make one of these strategies with the body `f`.
    */
  def mkStrategy(f : Any => Option[Any]) : Strategy =
    new Strategy {
      def apply(t : Any) =
        f(t)
    }

  /**
    * Sequential composition. Construct a strategy that first applies
    * this strategy. If it succeeds, then apply `q` to the new subject
    * term. Otherwise fail. `q` is evaluated at most once.
    */
  def <*(q : => Strategy) : Strategy =
    mkStrategy(
      t1 =>
        p(t1) match {
          case Some(t2) => q(t2)
          case None     => None
        }
    )

  /**
    * Deterministic choice.  Construct a strategy that first applies
    * this strategy. If it succeeds, succeed with the resulting term.
    * Otherwise, apply `q` to the original subject term. `q` is
    * evaluated at most once.
    */
  def <+(q : => Strategy) : Strategy =
    mkStrategy(
      (t1 : Any) =>
        p(t1) match {
          case Some(t2) => Some(t2)
          case None     => q(t1)
        }
    )

  /**
    * Non-deterministic choice. Normally, construct a strategy that
    * first applies either this strategy or the given strategy. If it
    * succeeds, succeed with the resulting term. Otherwise, apply `q`.
    * Currently implemented as deterministic choice, but this behaviour
    * should not be relied upon.
    * When used as the argument to the `<` conditional choice
    * combinator, `+` just serves to hold the two strategies that are
    * chosen between by the conditional choice.
    * `q` is evaluated at most once.
    */
  def +(q : => Strategy) : PlusStrategy =
    new PlusStrategy(p, q)

  /**
    * Conditional choice: `c < l + r`. Construct a strategy that first
    * applies this strategy (`c`). If `c` succeeds, the strategy applies
    * `l` to the resulting term, otherwise it applies `r` to the original
    * subject term. `lr` is evaluated at most once.
    */
  def <(lr : => PlusStrategy) : Strategy =
    mkStrategy(
      t1 =>
        p(t1) match {
          case Some(t2) => lr.left(t2)
          case None     => lr.right(t1)
        }
    )

}
