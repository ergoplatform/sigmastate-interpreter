package sigma

import org.scalameter.api._

trait BenchmarkGens extends CollGens { suite: Bench[Double] =>
  def maxSize = 100000

  val sizes = Gen.exponential("size")(10, maxSize, 10)

  val ranges = for { size <- sizes } yield (0 until size, maxSize / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }

  val colls = arrays.map { case (arr, i) => (builder.fromArray(arr), i) }
}
