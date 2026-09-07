package sigma

import org.scalatest.{Outcome, Succeeded}
import org.scalatest.propspec.AnyPropSpec

/** Decorator trait which runs each test repeatedly for each valid [[VersionContext]], which is
  * properly initialized.
  *
  * Scala 3 variant: scalatest 3.2.x makes `AnyPropSpec.property` a public, final `inline def`
  * (it captures the source position at the call site), so the classic "override `property` to wrap
  * `testFun`" approach used in the scala-2 variant is impossible here. Instead we wrap test
  * execution via the standard `withFixture(NoArgTest)` hook, which is overridable and is invoked
  * once per test — running the test body once per (activated, ErgoTree) version combination with
  * the corresponding `VersionContext` in scope. Stops at the first non-successful outcome, mirroring
  * the exception propagation of the scala-2 variant.
  */
trait VersionTestingProperty extends AnyPropSpec with VersionTesting {

  override protected def withFixture(test: NoArgTest): Outcome = {
    var outcome: Outcome = Succeeded
    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      if (outcome.isSucceeded) {
        VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          outcome = super.withFixture(test)
        }
      }
    }
    outcome
  }

}
