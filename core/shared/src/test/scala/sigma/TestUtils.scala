package sigma

import org.scalactic.TripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inside, TestSuite}
import sigma.util.StringUtil
import sigma.util.StringUtil.{StringUtilExtensions, cleanFileName}

/**
  * Created by slesarenko on 11/10/2017.
  */
trait TestUtils extends TestSuite with Matchers with Inside with TripleEquals {
  def testOutDir = "test-out"

  def testSuffixes = Seq("Suite", "Tests", "It", "_")

  lazy val prefix = {
    val suiteName = testSuffixes.foldLeft(getClass.getName)(_.stripSuffix(_))
    val pathComponents = suiteName.split('.')
    StringUtil.fileName(testOutDir, pathComponents: _*)
  }

  /* if runs in continuous integration environment */
  def isCI = sys.env.get("CI").flatMap(toBoolean).getOrElse(false)
  private def toBoolean(s: String): Option[Boolean] =
    scala.util.Try(s.toBoolean).toOption
  def pendingOnCI(): Unit = if (isCI) { pending }

  private val _currentTestName = new ThreadLocal[String]

  override def withFixture(test: NoArgTest) = {
    _currentTestName.set(test.name)
    val outcome = super.withFixture(test)
    _currentTestName.set(null)
    outcome
  }

  protected def currentTestName: String = {
    val testName = _currentTestName.get()
    if (testName.isNullOrEmpty) "_outside_tests_"
    else testName
  }

  protected def currentTestNameAsFileName: String = cleanFileName(currentTestName)
}
