package sigma

import org.scalameter.api._
import debox.cfor

trait BasicBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>

  performance of "allocation" in {
    measure method "new" in {
      using(sizes) in { case n =>
        val arr = new Array[Integer](n)
        cfor(0)(_ < n, _ + 1) { i =>
          arr(i) = Integer.valueOf(i)
        }
        var s = 0
        cfor(0)(_ < n, _ + 1) { i =>
          s += arr(i).intValue()
        }
      }
    }
  }

  performance of "Seq" in {
    var res: Seq[Int] = null
    measure method "Nil" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Nil }
      }
    }
    measure method "empty" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Seq.empty }
      }
    }
    measure method "apply" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Seq() }
      }
    }
  }

  performance of "Map" in {
    var res: Map[Int,Int] = null
    measure method "empty" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ => res = Map.empty[Int,Int] }
      }
    }
    measure method "apply" in {
      using(sizes) in { case n =>
        cfor(0)(_ < n, _ + 1) { _ =>
          res = Map[Int,Int]()
        }
      }
    }
  }

  performance of "for" in {
    var cell: Int = 0
    measure method "foreach" in {
      using(arrays) in { case (xs, _) =>
        xs.foreach { x => cell = x }
      }
    }
    measure method "cfor" in {
      using(arrays) in { case (xs, _) =>
        cfor(0)(_ < xs.length, _ + 1) { i => cell = xs(i) }
      }
    }
  }

  performance of "Seq vs Array" in {
    var res: Seq[Int] = null
    var head: Int = 0
    var tail: Seq[Int] = null
    measure method "Seq(...)" in {
      using(sizes) in { n =>
        cfor(0)(_ < n, _ + 1) { _ =>
          res = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
          head = res.head
          tail = res.tail
        }
      }
    }
    measure method "Array(...)" in {
      using(sizes) in { n =>
        cfor(0)(_ < n, _ + 1) { _ =>
          res = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
          head = res.head
          tail = res.tail
        }
      }
    }
  }
}

object FastBasicBenchmark extends Bench.LocalTime with BasicBenchmarkCases {
}

