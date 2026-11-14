package sigmastate

import org.scalactic.source.Position
import org.scalatest.{Outcome, Tag}
import org.scalatest.OutcomeOf.outcomeOf

import scala.collection.mutable

/** Scala 3's inline `property` is final, so registration stays with ScalaTest and
  * the fixture hook wraps execution instead. This also retains each caller's source position.
  */
trait CrossVersionProps extends CrossVersionPropsBase {
  private val unversionedTestNames = mutable.Set.empty[String]

  protected final def markUnversionedProperty(testName: String): Unit =
    unversionedTestNames.synchronized {
      unversionedTestNames += testName
    }

  override def withFixture(test: NoArgTest): Outcome = {
    val unversioned = unversionedTestNames.synchronized {
      unversionedTestNames.contains(test.name)
    }
    if (unversioned) super.withFixture(test)
    else {
      // Keep outer fixtures around the whole property, rather than entering them
      // again for every version. Only the original test body is repeated.
      val versionedTest = new NoArgTest {
        val name = test.name
        val text = test.text
        val configMap = test.configMap
        val scopes = test.scopes
        val tags = test.tags
        val pos = test.pos

        def apply(): Outcome = outcomeOf {
          // ScalaTest converts body exceptions to Outcomes. Re-throw non-successes
          // so testFun_Run prints its version diagnostic and the loops stop immediately.
          runVersionedProperty(test.name, test().toSucceeded)
        }
      }
      super.withFixture(versionedTest)
    }
  }

  /** Bypass profiling and version iteration; inline registration preserves caller positions. */
  protected inline def property2(testName: String, testTags: Tag*)
      (testFun: => Any)
      (implicit pos: Position): Unit = {
    registerTest(testName, testTags*)(testFun)(pos)
    markUnversionedProperty(testName)
  }
}
