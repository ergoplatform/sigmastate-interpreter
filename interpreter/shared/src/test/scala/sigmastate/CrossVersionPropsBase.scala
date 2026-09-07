package sigmastate

import debox.cfor
import org.scalatest.propspec.AnyPropSpecLike
import sigma.VersionContext
import sigmastate.eval.CProfiler

import scala.util.DynamicVariable

/** Execution shared by the Scala 2 registration and Scala 3 fixture adapters. */
trait CrossVersionPropsBase extends AnyPropSpecLike with TestsBase {
  /** Number of times each test property is warmed up before its versioned executions. */
  def perTestWarmUpIters: Int = 0

  private[sigmastate] val _warmupProfiler = new DynamicVariable[Option[CProfiler]](None)

  def warmupProfiler: Option[CProfiler] = _warmupProfiler.value

  protected final def runVersionedProperty(testName: String, testFun: => Any): Unit = {
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
}
