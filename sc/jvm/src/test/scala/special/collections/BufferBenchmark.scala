package sigma

import debox.Buffer
import debox.cfor
import org.scalameter.api.Bench

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.compat.immutable.ArraySeq

trait BufferBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>
  val obj = new Object()
  performance of "append[Int]" in {
    measure method "of debox.Buffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = Buffer.ofSize[Int](16)
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(arr(i))
        }
        val res = buf.toArray()
      }
    }
    measure method "of ArrayBuilder" in {
      using(arrays) in { case (arr, n) =>
        val buf = mutable.ArrayBuilder.make[Int]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf += (arr(i))
        }
        val res = buf.result()
      }
    }
    measure method "of ArrayBuffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = ArrayBuffer.empty[Int]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(arr(i))
        }
        val res = buf.toArray
      }
    }
    measure method "of ListBuffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = ListBuffer.empty[Int]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(arr(i))
        }
        val res = buf.toList
      }
    }
  }

  performance of "append[Object]" in {
    measure method "of debox.Buffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = Buffer.ofSize[Object](100)
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(obj)
        }
      }
    }
    measure method "of ArrayBuilder" in {
      using(arrays) in { case (arr, n) =>
        val buf = mutable.ArrayBuilder.make[Object]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf += (obj)
        }
        val res = buf.result()
      }
    }
    measure method "of ArrayBuffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = ArrayBuffer.empty[Object]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(obj)
        }
      }
    }
    measure method "of ListBuffer" in {
      using(arrays) in { case (arr, n) =>
        val buf = ListBuffer.empty[Object]
        val limit = arr.length
        cfor(0)(_ < limit, _ + 1) { i =>
          buf.append(obj)
        }
        val res = buf.toList
      }
    }
  }
}

object FastBufferBenchmark extends Bench.LocalTime with BufferBenchmarkCases {
}

