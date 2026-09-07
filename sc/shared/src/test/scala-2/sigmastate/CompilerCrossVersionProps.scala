package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position


/** Redefines `property` for cross-version testing of ErgoScript compiler. */
trait CompilerCrossVersionProps extends CompilerCrossVersionPropsBase {

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit =
    registerCompilerProperties(testName) { name =>
      super.property(name, testTags: _*)(testFun)(pos)
    }

  /** Explicit registration entry point for suites that need both compiler modes. */
  protected def compilerProperty(testName: String, testTags: Tag*)
                                (testFun: => Any)
                                (implicit pos: Position): Unit =
    property(testName, testTags: _*)(testFun)(pos)
}
