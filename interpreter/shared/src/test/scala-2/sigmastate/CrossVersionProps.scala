package sigmastate

import org.scalactic.source.Position
import org.scalatest.Tag

trait CrossVersionProps extends CrossVersionPropsBase {
  override protected def property(testName: String, testTags: Tag*)
      (testFun: => Any)
      (implicit pos: Position): Unit = {
    super.property(testName, testTags: _*) {
      runVersionedProperty(testName, testFun)
    }
  }

  /** Bypass profiling and version iteration. */
  protected def property2(testName: String, testTags: Tag*)
      (testFun: => Any)
      (implicit pos: Position): Unit = {
    super.property(testName, testTags: _*)(testFun)
  }
}
