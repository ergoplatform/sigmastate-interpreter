package scalan

import sigma.compiler.ir.IRContext
import sigma.{BaseNestedTests, BaseShouldTests, BaseTests, TestUtils}

trait TestContexts extends TestUtils {

  trait TestContextApi { ctx: IRContext =>
    def testName: String
    def emitF(name: String, sfs: (() => Sym)*): Unit
    def emit(name: String, ss: Sym*): Unit = {
      emitF(name, ss.map((s: Ref[_]) => () => s): _*)
    }
    def emit(s1: => Sym): Unit = emitF(testName, () => s1)
    def emit(s1: => Sym, s2: Sym*): Unit = {
      emitF(testName, Seq(() => s1) ++ s2.map((s: Ref[_]) => () => s): _*)
    }
  }
  abstract class TestContext(val testName: String) extends IRContext with TestContextApi {
    def this() = this(currentTestNameAsFileName)

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => Sym)*): Unit =
      Platform.stage(this)(prefix, testName, name, sfs)
  }


}

abstract class BaseCtxTests extends BaseTests with TestContexts

abstract class BaseNestedCtxTests extends BaseNestedTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts