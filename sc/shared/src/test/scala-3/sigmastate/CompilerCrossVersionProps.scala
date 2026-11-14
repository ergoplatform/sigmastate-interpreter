package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position

/** Scala 3 keeps ScalaTest's final property method and uses explicit registration
  * in suites that request the additional compiler-mode test names.
  */
trait CompilerCrossVersionProps extends CompilerCrossVersionPropsBase {
  protected inline def compilerProperty(testName: String, testTags: Tag*)
                                      (testFun: => Any)
                                      (implicit pos: Position): Unit =
    registerCompilerProperties(testName) { name =>
      // registerTest captures its inline expansion position, even with explicit pos.
      registerTest(name, testTags*)(testFun)(pos)
    }
}
