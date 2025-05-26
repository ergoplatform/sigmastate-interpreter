package sigmastate

import debox.cfor
import org.scalameter.api.Bench
import sigma.BenchmarkGens
import sigma.ast.{ArithOp, EQ, IntConstant, SType}
import sigma.ast.syntax.SValue
import sigma.serialization.OpCodes.PlusCode

object ErgoTreeBenchmarks extends Bench.LocalTime with BenchmarkGens { suite: Bench[Double] =>

  override def maxSize: Int = 10000

  /** Expected approximate results:
    * ::Benchmark allocation of sigma.ast.ArithOp, EQ, IntConstant::
    * name: OpenJDK 64-Bit Server VM
    * osArch: x86_64
    * osName: Mac OS X
    * vendor: AdoptOpenJDK
    * version: 25.232-b09
    * Parameters(size -> 10): 0.007652 ms
    * Parameters(size -> 100): 0.070323 ms
    * Parameters(size -> 1000): 0.696851 ms
    * Parameters(size -> 10000): 5.687967 ms
    */
  performance of "allocation of sigma.ast" in {
    measure method "ArithOp, EQ, IntConstant" in {
      using(sizes) in { size =>
        val arr = new Array[SValue](size)
        cfor(0)(_ < size, _ + 1) { i =>
          val expr =
            ArithOp(
              ArithOp(
                ArithOp(
                  ArithOp(
                    ArithOp(
                      IntConstant(10), 20, PlusCode),
                    30, PlusCode),
                  40, PlusCode),
                50, PlusCode),
              60, PlusCode)
          arr(i) = EQ(expr, 100)
        }
      }
    }
  }

  performance of "SType" in {
    measure method "isValueOfType" in {
      using(sizes) in { size =>
        cfor(0)(_ < size, _ + 1) { i =>
          SType.isValueOfType(i, SType.allPredefTypes(i % 10))
        }
      }
    }
  }
  performance of "Constant" in {
    measure method "isCorrectType" in {
      using(sizes) in { size =>
        cfor(0)(_ < size, _ + 1) { i =>
          sigma.crypto.Platform.isCorrectType(i, SType.allPredefTypes(i % 10))
        }
      }
    }
  }
}
