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
  * Helper class to contain commonality of choice in non-deterministic
  * choice operator and then-else part of a conditional choice. Only
  * returned by the non-deterministic choice operator. `p` and `q` are
  * evaluated at most once.
  */
class PlusStrategy(p : => Strategy, q : => Strategy) extends Strategy {

  /**
    * The left alternative of the choice.
    */
  lazy val left = p

  /**
    * The right alternative of the choice.
    */
  lazy val right = q

  /**
    * The strategy itself (lazily computed).
    */
  private lazy val s = left <+ right

  /**
    * Implementation of this strategy. Just apply `s`.
    */
  def apply(t : Any) =
    s(t)

}
