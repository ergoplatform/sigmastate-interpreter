package sigmastate

/** Shared compiler-mode registration; adapters select the ScalaTest entry point. */
trait CompilerCrossVersionPropsBase extends CrossVersionProps with CompilerTestsBase {
  protected final def registerCompilerProperties(testName: String)
                                               (register: String => Unit): Unit = {
    register(testName)
    if (okRunTestsWithoutMCLowering) {
      // Preserve Scala 2's registration scope; the deferred body's mode is unchanged.
      _lowerMethodCalls.withValue(false) {
        register(s"${testName}_MCLowering")
      }
    }
  }
}
