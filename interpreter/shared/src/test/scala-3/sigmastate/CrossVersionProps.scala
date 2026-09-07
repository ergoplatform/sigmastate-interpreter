package sigmastate

import debox.cfor
import org.scalactic.source.Position
import org.scalatest.{Outcome, Tag}
import org.scalatest.OutcomeOf.outcomeOf
import org.scalatest.propspec.AnyPropSpecLike
import sigma.VersionContext
import sigmastate.eval.CProfiler

import scala.collection.mutable
import scala.util.DynamicVariable

/** Scala 3's inline `property` is final, so registration stays with ScalaTest and
  * the fixture hook wraps execution instead. This also retains each caller's source position.
  */
trait CrossVersionProps extends AnyPropSpecLike with TestsBase {
  /** Number of times each test property is warmed up before its versioned executions. */
  def perTestWarmUpIters: Int = 0

  private[sigmastate] val _warmupProfiler = new DynamicVariable[Option[CProfiler]](None)

  def warmupProfiler: Option[CProfiler] = _warmupProfiler.value

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
          runProperty(test.name, test().toSucceeded)
        }
      }
      super.withFixture(versionedTest)
    }
  }

  private def runProperty(testName: String, testFun: => Any): Unit = {
    if (perTestWarmUpIters > 0) {
      _warmupProfiler.withValue(Some(new CProfiler)) {
        cfor(0)(_ < perTestWarmUpIters, _ + 1) { _ =>
          testFun_Run(testName, testFun)
        }
      }
      System.gc()
    }
    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        testFun_Run(testName, testFun)
      }
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
