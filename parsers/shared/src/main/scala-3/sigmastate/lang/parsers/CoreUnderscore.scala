package sigmastate.lang.parsers

import fastparse.P

/** Scala 3 does not allow an underscore-only method name. */
trait CoreUnderscore {
  def Underscore[Ctx:P]: P[Unit] = Key.W("_")
}
