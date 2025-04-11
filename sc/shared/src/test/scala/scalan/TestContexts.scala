package scalan

import sigma.compiler.ir.{GraphIRReflection, IRContext}
import sigma.reflection.RMethod
import sigma.{BaseNestedTests, BaseShouldTests, BaseTests, TestUtils}

trait TestContexts extends TestUtils {

  trait TestContextApi { ctx: IRContext =>
    def invokeAll: Boolean
    def isInvokeEnabled(d: Def[_], m: RMethod): Boolean
    def shouldUnpack(e: Elem[_]): Boolean
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

    override val invokeAll = true
    override def isInvokeEnabled(d: Def[_], m: RMethod) = invokeAll
    override def shouldUnpack(e: Elem[_]) = true

    // workaround for non-existence of by-name repeated parameters
    def emitF(name: String, sfs: (() => Sym)*): Unit =
      Platform.stage(this)(prefix, testName, name, sfs)
  }


}

abstract class BaseCtxTests extends BaseTests with TestContexts {
  val reflection = GraphIRReflection
}

abstract class BaseNestedCtxTests extends BaseNestedTests with TestContexts

abstract class BaseShouldCtxTests extends BaseShouldTests with TestContexts