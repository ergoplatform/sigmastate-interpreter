package sigmastate

import org.scalatest.Tag

/** Tags for tests whose meaningfulness depends on the Scala version they run under.
  * The build excludes these tags per scalaVersion (see scJVM settings in build.sbt),
  * so on non-applicable legs the tests are skipped entirely instead of being reported
  * as canceled by a runtime `assume`.
  */
object SigmaMapTestTags {

  /** Differential comparison against the running scala.collection library: only
    * meaningful under Scala 2.12, the version the SigmaMap ordering is pinned to. */
  final val Scala212OnlyName = "SigmaMap212Only"
  val Scala212Only: Tag = new Tag(Scala212OnlyName)

  /** Relies on small runtime maps preserving insertion order: true for Scala 2.12/2.13,
    * false for 2.11 (no Map1..Map4 optimization there). */
  final val NotScala211Name = "SigmaMapNot211"
  val NotScala211: Tag = new Tag(NotScala211Name)
}
