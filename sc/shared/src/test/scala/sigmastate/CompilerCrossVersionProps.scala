package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position


/** Redefines `property` for cross-version testing of ErgoScript compiler. */
trait CompilerCrossVersionProps extends CrossVersionProps with CompilerTestsBase {

  protected val testCompilerWithoutOptimizations: Boolean = false

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {
    super.property(testName, testTags:_*)(testFun)

    if (testCompilerWithoutOptimizations) {
      val testName_opt = s"${testName}_no_opt"
      super.property2(testName_opt, testTags:_*) {
        _enableCompilerOptimization.withValue(false) {
          testFun_Run(testName_opt, testFun)
        }
      }
    }

    val testName_lowering = s"${testName}_MCLowering"
    super.property2(testName_lowering, testTags:_*) {
      if (okRunTestsWithoutMCLowering) {
        _lowerMethodCalls.withValue(false) {
          testFun_Run(testName_lowering, testFun)
        }
      }
    }
  }
}
