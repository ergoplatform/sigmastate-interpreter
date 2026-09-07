package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position

/** Scala 3 keeps ScalaTest's final property method and uses explicit registration
  * in suites that request the additional compiler-mode test names.
  */
trait CompilerCrossVersionProps extends CrossVersionProps with CompilerTestsBase {
  protected inline def compilerProperty(testName: String, testTags: Tag*)
                                      (testFun: => Any)
                                      (implicit pos: Position): Unit = {
    registerTest(testName, testTags*)(testFun)(pos)
    if (okRunTestsWithoutMCLowering) {
      val testName2 = s"${testName}_MCLowering"
      // Retain the Scala 2 registration scope; changing the deferred test body's
      // mode is a separate behavior change from cross-compilation.
      _lowerMethodCalls.withValue(false) {
        registerTest(testName2, testTags*)(testFun)(pos)
      }
    }
  }
}
