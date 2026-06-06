package spire.scalacompat

/** Scala 3 stand-in for `spire.scalacompat.BuilderCompat`, which reaches the test
  * classpath transitively via `debox`/`spire` on Scala 2.x. On Scala 3 `debox` is
  * vendored (see core/shared/src/main/scala-3/debox), so spire is absent and this
  * minimal equivalent is provided for test sources (e.g. `CollGens`).
  *
  * On Scala 2.13 collections (which Scala 3 uses) a `mutable.Builder` requires
  * `addOne`, `clear` and `result`, exactly the members the test builders define. */
abstract class BuilderCompat[A, C] extends scala.collection.mutable.Builder[A, C]
