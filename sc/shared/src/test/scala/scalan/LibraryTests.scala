package scalan

import sigma.compiler.ir.IRContext
import sigma.util.BenchmarkUtil.{measure, measureTime}

class Benchmark[T <: IRContext](createContext: => T) {
  val printDebugInfo: Boolean = false

  def run() = {
    val (ctx, total) = measureTime {
      var ctx = createContext
      measure(10000, okShowIterTime = printDebugInfo, okShowTotalTime = printDebugInfo) { _ =>
        ctx = createContext
      }
      ctx
    }
    if (printDebugInfo) println(s"Def count: ${ctx.defCount}, total: $total msec")
    /*Total time: 9335 ms*/
  }
}

class LibraryTests extends BaseCtxTests {
  test("Benchmark Library creation time") {
    new Benchmark(new TestLibrary {}).run()
  }
}

object MeasureLibraryCreate extends App {
  new Benchmark(new TestLibrary {}).run()
}
