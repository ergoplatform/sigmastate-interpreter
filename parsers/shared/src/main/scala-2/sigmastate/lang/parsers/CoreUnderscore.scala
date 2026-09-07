package sigmastate.lang.parsers

import fastparse.P

/** Retains the Scala 2 underscore-token API and its override behavior. */
trait CoreUnderscore {
  def `_`[Ctx:P]: P[Unit] = Key.W("_")
  def Underscore[Ctx:P]: P[Unit] = `_`[Ctx]
}
