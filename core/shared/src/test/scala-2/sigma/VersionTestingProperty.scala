package sigma

import org.scalactic.source.Position
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.Tag

/** Decorator trait which allows to redefine `property` so that it is executed repeatedly for each valid
  * [[VersionContext]], which is properly initialized.
  * Thus, the properties can be versioned using `VersionContext.current`.
  *
  * Scala 2 variant: scalatest's `property` is a normal `def` taking an implicit `Position`.
  * The Scala 3 variant (in the scala-3 source dir) overrides the `inline` form instead.
  */
trait VersionTestingProperty extends AnyPropSpec with VersionTesting {

  /** Redefine `property` so that testFun is executed repeatedly for each valid
   * [[VersionContext]] */
  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {
    super.property(testName, testTags:_*) {
      forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
        VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          testFun_Run(testName, testFun)
        }
      }
    }
  }

}
