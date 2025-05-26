package special.wrappers

import scalan.{BaseLiftableTests, TestLibrary, BaseCtxTests}

/** Base class inhereted by test suite for each wrapper like WOption etc. */
abstract class WrappersTests extends BaseCtxTests with BaseLiftableTests {
  class WrappersCtx extends TestContext with TestLibrary with LiftableTestKit {
  }
}
