package sigma

import debox.{Set => DSet, Map => DMap}
import scalan.TestLibrary
import org.scalameter.api.Bench
import debox.cfor

trait SymBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>
  val obj = new Object()
  class Ctx extends TestLibrary
  val ctx = new Ctx
  import ctx._
  val syms = arrays.map { case (arr, n) => arr.map(_ => placeholder[Int]) }
  val symIds = syms.map { case arr => arr.map(s => s.node.nodeId) }

  performance of "Set[Sym] vs Set[Int]" in {
    measure method "of add(syms(i))" in {
      using(syms) in { syms =>
        val set = DSet.ofSize[Sym](0)
        cfor(0)(_ < syms.length, _ + 1) { i =>
          set += syms(i)
        }
      }
    }
    measure method "of set += syms(i).rhs.nodeId" in {
      using(syms) in { syms =>
        val set = DSet.ofSize[Int](0)
        cfor(0)(_ < syms.length, _ + 1) { i =>
          set += syms(i).node.nodeId
        }
      }
    }
    measure method "of set += (ids(i)" in {
      using(symIds) in { ids =>
        val set = DSet.ofSize[Int](0)
        cfor(0)(_ < ids.length, _ + 1) { i =>
          set += ids(i)
        }
      }
    }
  }

  performance of "Map[Sym, Object] vs Map[Int, Object]" in {
    measure method "of m.update(syms(i), obj)" in {
      using(syms) in { syms =>
        val m = DMap.ofSize[Sym, Object](0)
        cfor(0)(_ < syms.length, _ + 1) { i =>
          m.update(syms(i), obj)
        }
      }
    }
    measure method "of m.update(syms(i).rhs.nodeId, obj)" in {
      using(syms) in { syms =>
        val m = DMap.ofSize[Int, Object](0)
        cfor(0)(_ < syms.length, _ + 1) { i =>
          m.update(syms(i).node.nodeId, obj)
        }
      }
    }
    measure method "of m.update(ids(i), obj)" in {
      using(symIds) in { ids =>
        val m = DMap.ofSize[Int, Object](0)
        cfor(0)(_ < ids.length, _ + 1) { i =>
          m.update(ids(i), obj)
        }
      }
    }
  }
}

object FastSymBenchmark extends Bench.LocalTime with SymBenchmarkCases {
}

