package sigmastate

import sigma.ast.JitCost
import sigmastate.helpers.TestingCommons

import scala.util.{Failure, Try}

class JitCostSpecification extends TestingCommons {

  type BinOp = (Int, Int) => Int
  type JitBinOp = (JitCost, JitCost) => JitCost

  def testBinary[A](r1: Try[A], r2: Try[A]) = {
    (r1, r2) match {
      case (Failure(exception), Failure(expectedException)) =>
        rootCause(exception).getClass shouldBe expectedException.getClass
      case _ =>
        r1 shouldBe r2
    }
  }

  property("JitCost.$plus") {
    forAll(MinSuccessful(1000)) { (x: Int, y: Int) =>
      {
        val r1 = Try(java7.compat.Math.addExact(x, y))
        val r2 = Try((JitCost(x) + JitCost(y)).value)
        testBinary(r1, r2)
      }

      {
        val r1 = Try(java7.compat.Math.multiplyExact(x, y))
        val r2 = Try((JitCost(x) * y).value)
        testBinary(r1, r2)
      }

      {
        val r1 = Try(x / y)
        val r2 = Try((JitCost(x) / y).value)
        testBinary(r1, r2)
      }

      {
        val r1 = Try(x > y)
        val r2 = Try(JitCost(x) > JitCost(y))
        testBinary(r1, r2)
      }

      {
        val r1 = Try(x >= y)
        val r2 = Try(JitCost(x) >= JitCost(y))
        testBinary(r1, r2)
      }

    }
  }

  property("JitCost scaling") {
    forAll(MinSuccessful(1000)) { (x: Int) =>
      whenever(0 <= x && x <= 100000000) {
        JitCost.fromBlockCost(x).toBlockCost shouldBe x
      }
    }
  }

}
