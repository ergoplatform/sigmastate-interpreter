/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package sigma.kiama.rewriting

import sigma.kiama.{===>, ==>}

/**
  * Strategy-based term rewriting with callbacks. Clients can register
  * functions that are called whenever a rewrite operation has happened.
  * See the `Rewriter` class documentation for more detail on the methods
  * defined here.
  */
trait CallbackRewriter extends Rewriter {

  /**
    * The method to call when a rewrite operation has happened. It will
    * be called under two circumstances. First, when a `rule` (or similar, such
    * as `rulefs`, or `strategy`) is about to return a new term to replace an old
    * term. (Note that if the rule creates sub-terms in the new term, the
    * results of these operations are not notified, only the root of the
    * new term.) Second, whenever a generic traversal (such as all or one)
    * creates a new node to duplicate an old one. In both cases this method
    * is called with both the old and the new terms. The return value should
    * be a term that should go forward as the new term.
    */
  def rewriting[T](oldTerm : T, newTerm : T) : T

  /**
    * Produce a strategy that first runs the strategy s on the
    * current term. If `s` fails, then fail. Otherwise, pass the original
    * and new terms to the rewriting method and succeed with the term that
    * it returns.
    */
  def dispatch(s : Strategy) : Strategy =
    new Strategy {
      def apply(t : Any) =
        s(t) match {
          case None    => None
          case Some(u) => Some(rewriting(t, u))
        }
    }

  override def rule[T](f : ===>[T]) : Strategy =
    dispatch(super.rule[T](f))

  override def rulef(f : Any => Any) : Strategy =
    dispatch(super.rulef(f))

  override def rulefs[T](f : T ==> Strategy) : Strategy =
    dispatch(super.rulefs[T](f))

  override def strategy[T](f : T ==> Option[T]) : Strategy =
    dispatch(super.strategy(f))

  override def strategyf(f : Any => Option[Any]) : Strategy =
    dispatch(super.strategyf(f))

  /**
    * Product duplication with callback notification.
    */
  override def dup[T <: Product](t : T, children : Array[AnyRef]) : T =
    rewriting(t, super.dup(t, children))

}
