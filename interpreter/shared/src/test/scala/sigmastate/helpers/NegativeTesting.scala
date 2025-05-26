package sigmastate.helpers

import debox.cfor
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag
import scala.annotation.tailrec

/** Contains helpers to test expected exceptions and their causes. */
trait NegativeTesting extends Matchers {
  /** Checks that a [[Throwable]] is thrown and satisfies the given predicate.
    *
    * @param fun       block of code to execute
    * @param assertion expected assertion on the thrown exception
    * @param clue      added to the error message
    */
  def assertExceptionThrown(
      fun: => Any,
      assertion: Throwable => Boolean,
      clue: => String = ""): Unit = {
    try {
      fun
      fail("exception is expected but hasn't been thrown")
    }
    catch {
      case e: Throwable =>
        if (!assertion(e))
          fail(
            s"""exception check failed on $e (root cause: ${rootCause(e)})
              |clue: $clue
              |trace:
              |${e.getStackTrace.mkString("\n")}}""".stripMargin)
    }
  }

  /** Returns the root cause of the chain of exceptions. */
  @tailrec
  final def rootCause(t: Throwable): Throwable =
    if (t.getCause == null) t
    else rootCause(t.getCause)

  /** If error then maps it to the root cause, otherwise returns the original value. */
  final def rootCause[A](x: Try[A]): Try[A] = x match {
    case s: Success[_] => s
    case Failure(t) => Failure(rootCause(t))
  }

  /** Creates an assertion which checks the given type and message contents.
    *
    * @tparam E expected type of an exception
    * @param msgParts expected parts of the exception message
    * @return the assertion which can be used in assertExceptionThrown method
    */
  def exceptionLike[E <: Throwable : ClassTag]
      (msgParts: String*): Throwable => Boolean = {
    case t: E => msgParts.forall(part => t.getMessage != null && t.getMessage.contains(part))
    case _ => false
  }

  /** Creates an assertion which checks the root cause exception and message contents.
    *
    * @tparam E expected type of a root cause exception
    * @param msgParts expected parts of the root cause exception message
    * @return the assertion which can be used in assertExceptionThrown method
    */
  def rootCauseLike[E <: Throwable : ClassTag]
      (msgParts: String*): Throwable => Boolean = { t =>
    val root = rootCause(t)
    root match {
      case r: E => msgParts.forall(r.getMessage.contains(_))
      case _ => false
    }
  }

  /** Checks that both computations either succeed with the same value or fail with the same
    * error. If this is not true, exception is thrown.
    *
    * @param f first computation
    * @param g second computation
    * @return result of the second computation `g`
    */
  def sameResultOrError[B](f: => B, g: => B): Try[B] = {
    val b1 = Try(f)
    val b2 = Try(g)
    (b1, b2) match {
      case (Success(b1), res @ Success(b2)) =>
        assert(b1 == b2)
        res
      case (Failure(t1), res @ Failure(t2)) =>
        val c1 = rootCause(t1).getClass
        val c2 = rootCause(t2).getClass
        c1 shouldBe c2
        res
      case _ =>
        val cause = if (b1.isFailure)
          rootCause(b1.asInstanceOf[Failure[_]].exception)
        else
          rootCause(b2.asInstanceOf[Failure[_]].exception)
        sys.error(
          s"""Should succeed with the same value or fail with the same exception, but was:
            |First result: $b1
            |Second result: $b2
            |Root cause: $cause
            |""".stripMargin)
    }
  }

  /** Repeat the given `block` computation `nIters` times.
    *
    * @param nIters number of iterations to repeat the computation
    * @param block  the computation to execute on each iteration
    * @return the result of the last iteration
    */
  def repeatAndReturnLast[A](nIters: Int)(block: => A): A = {
    require(nIters > 0)
    var res = block
    cfor(1)(_ < nIters, _ + 1) { _ =>
      res = block
    }
    res
  }
}
