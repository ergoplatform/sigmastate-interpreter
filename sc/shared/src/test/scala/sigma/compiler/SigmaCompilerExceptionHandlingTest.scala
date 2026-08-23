package sigma.compiler

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SigmaCompilerExceptionHandlingTest
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  // A locally-named class that mimics the Scala.js wrapper name, so the shared JVM test
  // can exercise the string-based JS predicate without depending on scalajs-library.
  class JavaScriptException(msg: String) extends RuntimeException(msg)

  property("JVM StackOverflowError is detected") {
    SigmaCompiler.isStackOverflow(new StackOverflowError, isJVM = true) shouldBe true
  }

  property("JVM InternalError is not misclassified as a stack overflow") {
    SigmaCompiler.isStackOverflow(new InternalError("broken VM"), isJVM = true) shouldBe false
  }

  property("JVM UnknownError is not misclassified as a stack overflow") {
    SigmaCompiler.isStackOverflow(new UnknownError("unknown VM error"), isJVM = true) shouldBe false
  }

  property("JS JavaScriptException wrapping a stack RangeError is detected") {
    SigmaCompiler.isStackOverflow(
      new JavaScriptException("RangeError: Maximum call stack size exceeded"),
      isJVM = false) shouldBe true
  }

  property("JS JavaScriptException wrapping Firefox recursion message is detected") {
    SigmaCompiler.isStackOverflow(
      new JavaScriptException("InternalError: too much recursion"),
      isJVM = false) shouldBe true
  }

  property("JS JavaScriptException wrapping a non-stack RangeError is not misclassified") {
    SigmaCompiler.isStackOverflow(
      new JavaScriptException("RangeError: Invalid array length"),
      isJVM = false) shouldBe false
  }
}
