package sigma

import org.scalameter.api.Bench
import sigma.data.{AVHashMap, Nullable}
import debox.cfor

trait MapBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>
  val obj = new Object()
  var xOpt: Option[Object] = None
  var xNullable: Nullable[Object] = Nullable.None
  performance of "put[Object]" in {
    measure method "of debox.Map" in {
      using(arrays) in { case (arr, n) =>
        val m = debox.Map.ofSize[Int, Object](10000)
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          m.update(limit - i, obj)
        }
      }
    }
    measure method "of AVHashMap" in {
      using(arrays) in { case (arr, n) =>
        val m = AVHashMap[Int, Object](10000)
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          m.put(limit - i, obj)
        }
      }
    }
    measure method "of immutable.Map" in {
      var res: Map[Int, Object] = null
      using(arrays) in { case (arr, n) =>
        var m = Map.empty[Int, Object]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          m = m + ((limit - i, obj))
        }
        res = m
      }
    }
  }

  performance of "get[Object]" in {
    measure method "of debox.Map" in {
      val m = debox.Map.ofSize[Int, Object](maxSize)
      var x: Option[Object] = None
      cfor(0)(_ < maxSize, _ + 1) { i =>
        m.update(i, obj)
      }
      using(arrays) in { case (arr, n) =>
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          x = m.get(i * 13 % limit)
        }
      }
    }
    measure method "of AVHashMap" in {
      val m = AVHashMap[Int, Object](maxSize)
      cfor(0)(_ < maxSize, _ + 1) { i =>
        m.put(i, obj)
      }
      using(arrays) in { case (arr, n) =>
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          xNullable = m.get(i * 13 % limit)
        }
      }
    }
    measure method "of immutable.Map" in {
      var m = Map.empty[Int, Object]
      cfor(0)(_ < maxSize, _ + 1) { i =>
        m = m + ((i, obj))
      }
      using(arrays) in { case (arr, n) =>
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          xOpt = m.get(i * 13 % limit)
        }
      }
    }
  }
}

object FastMapBenchmark extends Bench.LocalTime with MapBenchmarkCases {
}

