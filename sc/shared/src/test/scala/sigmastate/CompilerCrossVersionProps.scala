package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position


/** Redefines `property` for cross-version testing of ErgoScript compiler. */
trait CompilerCrossVersionProps extends CrossVersionProps with CompilerTestsBase {

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {
    super.property(testName, testTags:_*)(testFun)

    if (okRunTestsWithoutMCLowering) {
      val testName2 = s"${testName}_MCLowering"
      _lowerMethodCalls.withValue(false) {
        // run testFun for all versions again, but now with this flag
        super.property(testName2, testTags:_*)(testFun)
      }
    }
  }
}
