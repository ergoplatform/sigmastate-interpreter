package org.ergoplatform.sdk

import sigma.data.ExactIntegral

import scala.collection.mutable
import scala.reflect.ClassTag

object Utils {

  /** Performs outer join operation between left and right collections.
    * Note, the ordering is not deterministic.
    *
    * @param l     projection function executed for each element of `left`
    * @param r     projection function executed for each element of `right`
    * @param inner projection function which is executed for matching items (K, L) and (K, R) with the same K
    * @return map of (K, O) pairs, where each key comes form either left or right collection and values are produced by projections
    */
  def outerJoin[K, L, R, O]
      (left: Map[K, L], right: Map[K, R])
          (l: (K, L) => O, r: (K, R) => O, inner: (K, L, R) => O): Map[K, O] = {
    val res = mutable.HashMap.empty[K, O]
    val lks = left.keySet
    val rks = right.keySet
    val leftOnly = lks diff rks
    val rightOnly = rks diff lks
    val both = lks intersect rks
    for ( lk <- leftOnly )
      res += lk -> l(lk, left(lk))
    for ( rk <- rightOnly )
      res += rk -> r(rk, right(rk))
    for ( k <- both )
      res += k -> inner(k, left(k), right(k))
    res.toMap
  }

  /** Performance optimized deterministic mapReduce primitive.
    * @param arr array to be mapped to (K, V) pairs
    * @param m   mapper function
    * @param r   value reduction function
    * @return pair of arrays (keys, values), where keys appear in order of their first
    *         production by `m` and for each i => values(i) corresponds to keys(i)
    */
  def mapReduce[A, K: ClassTag, V: ClassTag](
      arr: Array[A],
      m: A => (K, V),
      r: ((V, V)) => V): (Array[K], Array[V]) = {
    val keyPositions = new java.util.HashMap[K, Int](32)
    val keys = mutable.ArrayBuilder.make[K]
    val values = new Array[V](arr.length)
    var i = 0
    var nValues = 0
    while (i < arr.length) {
      val (key, value) = m(arr(i))
      val pos = {
        val p: Integer = keyPositions.get(key)
        if (p == null) 0 else p.intValue()
      }
      if (pos == 0) {
        keyPositions.put(key, nValues + 1)
        keys += key
        values(nValues) = value
        nValues += 1
      } else {
        values(pos - 1) = r((values(pos - 1), value))
      }
      i += 1
    }
    val resValues = new Array[V](nValues)
    Array.copy(values, 0, resValues, 0, nValues)
    (keys.result(), resValues)
  }

  def mapToArrays[K: ClassTag, V: ClassTag](m: Map[K, V]): (Array[K], Array[V]) = {
    val keys = mutable.ArrayBuilder.make[K]
    val values = mutable.ArrayBuilder.make[V]
    for ( (k, v) <- m ) {
      keys += k
      values += v
    }
    (keys.result, values.result)
  }

  /** This class can adapt ExactIntegral instance to be used where Integral is required.
    * See methods like sum, subByKey.
    */
  class IntegralFromExactIntegral[A](val ei: ExactIntegral[A]) extends Integral[A] {
    override def quot(x: A, y: A): A = ei.quot(x, y)

    override def rem(x: A, y: A): A = ei.divisionRemainder(x, y)

    override def plus(x: A, y: A): A = ei.plus(x, y)

    override def minus(x: A, y: A): A = ei.minus(x, y)

    override def times(x: A, y: A): A = ei.times(x, y)

    override def negate(x: A): A = ei.negate(x)

    override def fromInt(x: Int): A = ei.fromInt(x)

    override def toInt(x: A): Int = ei.toInt(x)

    override def toLong(x: A): Long = ei.toLong(x)

    override def toFloat(x: A): Float = notSupported

    override def toDouble(x: A): Double = notSupported

    override def compare(x: A, y: A): Int = notSupported

    private def notSupported = throw new NotImplementedError("operation is not supported")

    def parseString(str: String): Option[A] = throw new NotImplementedError("operation is not supported")
  }
}
