package sigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions
import org.scalatest.verbs.ResultOfStringPassedToVerb
import org.scalatest.funsuite.AnyFunSuite

/**
 * Standard base class for most test suites. See BaseNestedTests and BaseShouldTests for alternatives
 *
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FunSuite</a>.
 */
abstract class BaseTests extends AnyFunSuite with TestUtils

/**
 * Standard base class for test suites with nested tests.
 *
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FunSpec</a>.
 */
abstract class BaseNestedTests extends AnyFunSpec with TestUtils

/**
 * See <a>http://doc.scalatest.org/2.2.4/#org.scalatest.FlatSpec</a>.
 */
abstract class BaseShouldTests extends AnyFlatSpec with TestUtils {
  protected final class InAndIgnoreMethods2(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.rest
    val _inner = new InAndIgnoreMethods(resultOfStringPassedToVerb)
    def beArgFor(testFun: String => Unit) {
      _inner.in(testFun(rest.trim))
    }
  }

  protected implicit def convertToInAndIgnoreMethods2(
      resultOfStringPassedToVerb: ResultOfStringPassedToVerb): InAndIgnoreMethods2 =
    new InAndIgnoreMethods2(resultOfStringPassedToVerb)

}
