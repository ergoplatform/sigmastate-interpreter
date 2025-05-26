package sigmastate

import debox.cfor
import org.scalactic.source.Position

import scala.util.DynamicVariable
import org.scalatest.Tag
import sigmastate.eval.CProfiler
import org.scalatest.propspec.AnyPropSpecLike
import sigma.VersionContext

trait CrossVersionProps extends AnyPropSpecLike with TestsBase {
  /** Number of times each test property is warmed up (i.e. executed before final execution). */
  def perTestWarmUpIters: Int = 0

  private[sigmastate] val _warmupProfiler = new DynamicVariable[Option[CProfiler]](None)

  def warmupProfiler: Option[CProfiler] = _warmupProfiler.value

  override protected def property(testName: String, testTags: Tag*)
      (testFun: => Any)
      (implicit pos: Position): Unit = {
    super.property(testName, testTags: _*) {
      // do warmup if necessary
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

  /** This methods is used to bypass profiling and forEachScriptAndErgoTreeVersion */
  protected def property2(testName: String, testTags: Tag*)
      (testFun: => Any)
      (implicit pos: Position): Unit = {
    super.property(testName, testTags: _*)(testFun)
  }
}
