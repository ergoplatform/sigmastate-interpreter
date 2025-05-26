package sigma

import org.scalameter.{execution, Executor}
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import debox.cfor


trait CollBenchmarkCases extends BenchmarkGens { suite: Bench[Double] =>

  performance of "map" in {
    measure method "of Array" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.map(inc)
        }
      }
    }
    measure method "of Coll" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.map(inc)
        }
      }
    }
  }

  performance of "filter" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).filter(p => p._1 == p._2)
        }
      }
    }
    measure method "of PairColl" in {
      @inline def doFilter(c: Coll[Int]) = c.zip(c).filter(p => p._1 == p._2)
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          doFilter(c)
        }
      }
    }
  }

  performance of "map" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).map(p => p._1 + p._2)
        }
      }
    }
    measure method "of PairColl" in {
      def doMap(c: Coll[Int]) = c.zip(c).map(p => (p._1 + p._2).toLong)
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          doMap(c)
        }
      }
    }
  }

  performance of "exists" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).exists(p => p._1 != p._2)
        }
      }
    }
    measure method "of PairColl" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.zip(c).exists(p => p._1 != p._2)
        }
      }
    }
  }
  performance of "foldLeft" in {
    measure method "PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).foldLeft(0)((b, p) => b + p._1 + p._2)
        }
      }
    }
    measure method "of PairColl" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.zip(c).foldLeft[Int](0, p => p._1 + p._2._1 + p._2._2)
        }
      }
    }
  }
  performance of "reverse" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).reverse
        }
      }
    }
    measure method "of PairColl" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.zip(c).reverse
        }
      }
    }
  }
  performance of "mapFirst" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.zip(arr).map(p => (inc(p._1), p._2))
        }
      }
    }
  }
  performance of "unionSet" in {
    measure method "of PairArray" in {
      using(arrays) in { case (arr, n) =>
        val reversed = arr.reverse
        cfor(0)(_ < n, _ + 1) { _ =>
          arr.union(reversed).distinct
        }
      }
    }
    measure method "of PairColl" in {
      using(colls) in { case (c, n) =>
        val reversed = c.reverse
        cfor(0)(_ < n, _ + 1) { _ =>
          c.unionSet(c)
        }
      }
    }
  }
}

object FastCollBenchmark extends Bench.LocalTime with CollBenchmarkCases {
}

object CollBenchmark extends Bench.OfflineRegressionReport with CollBenchmarkCases {
  override def executor: Executor[Double] = new execution.LocalExecutor(
    warmer,
    aggregator,
    measurer
  )
//  lazy val executor = LocalExecutor(
//    new Executor.Warmer.Default,
//    Aggregator.min[Double],
//    measurer)
//  lazy val measurer = new Measurer.Default
//  def reporter: Reporter[Double] = Reporter.Composite(
//    new LoggingReporter[Double],
////    new RegressionReporter(
////      RegressionReporter.Tester.OverlapIntervals(),
////      RegressionReporter.Historian.ExponentialBackoff() ),
//    HtmlReporter(true)
//  )

//  reports.resultDir -> "tmp1"
//  lazy val reporter = HtmlReporter(true)
//  lazy val reporter = new LoggingReporter[Double]
  
//  lazy val persistor = Persistor.None
}

