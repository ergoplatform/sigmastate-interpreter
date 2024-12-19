package sigma

import org.scalactic.source.Position
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.Tag

/** Decorator trait which allows to redefine `property` so that it is executed repeatedly for each valid
  * [[VersionContext]], which is properly initialized.
  * Thus, the properties can be versioned using `VersionContext.current`.
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
