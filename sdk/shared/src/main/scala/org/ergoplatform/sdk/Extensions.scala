package org.ergoplatform.sdk

import debox.cfor
import sigma.data.RType
import sigma.rtypeToClassTag // actually used
import sigmastate.eval.CPreHeader
import sigma.{Coll, CollBuilder, PairColl}
import sigma.{Header, PreHeader}

import scala.collection.compat.BuildFrom
import scala.collection.{GenIterable, immutable}
import scala.reflect.ClassTag

object Extensions {

  implicit class CollOps[A](val coll: Coll[A]) extends AnyVal {

    /** Partitions this $coll in two $colls according to a predicate.
      *
      * @param pred the predicate on which to partition.
      * @return a pair of $colls: the first $coll consists of all elements that
      *         satisfy the predicate `p` and the second $coll consists of all elements
      *         that don't. The relative order of the elements in the resulting ${coll}s
      *         will BE preserved (this is different from Scala's version of this method).
      */
    def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
      val (ls, rs) = coll.toArray.partition(pred)
      val b = coll.builder
      implicit val tA: RType[A] = coll.tItem
      (b.fromArray(ls), b.fromArray(rs))
    }

    def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
      var b = immutable.Map.empty[T, U]
      val len = coll.length
      cfor(0)(_ < len, _ + 1) { i =>
        val kv = coll(i)
        if (b.contains(kv._1))
          throw new IllegalArgumentException(s"Cannot transform collection $this to Map: duplicate key in entry $kv")
        b = b + kv
      }
      b
    }

    /** Sums elements of this collection using given Numeric.
      * @return sum of elements or Numeric.zero if coll is empty
      */
    def sum(implicit n: Numeric[A]): A = {
      var sum = n.zero
      val len = coll.length
      cfor(0)(_ < len, _ + 1) { i =>
        sum = n.plus(sum, coll(i))
      }
      sum
    }

    /** Apply m for each element of this collection, group by key and reduce each group using r.
      *
      * @returns one item for each group in a new collection of (K,V) pairs.
      */
    def mapReduce[K: RType, V: RType](
        m: A => (K, V),
        r: ((V, V)) => V): Coll[(K, V)] = {
      val b = coll.builder
      val (keys, values) = Utils.mapReduce(coll.toArray, m, r)
      b.pairCollFromArrays(keys, values)
    }

    /** Partitions this collection into a map of collections according to some discriminator function.
      *
      * @param key the discriminator function.
      * @tparam K the type of keys returned by the discriminator function.
      * @return A map from keys to ${coll}s such that the following invariant holds:
      * {{{
      *  (xs groupBy key)(k) = xs filter (x => key(x) == k)
      * }}}
      * That is, every key `k` is bound to a $coll of those elements `x`
      * for which `key(x)` equa  `k`.
      */
    def groupBy[K: RType](key: A => K): Coll[(K, Coll[A])] = {
      val b = coll.builder
      implicit val tA = coll.tItem
      val res = coll.toArray.groupBy(key).mapValues(b.fromArray(_))
      b.fromMap(res.toMap)
    }

    /** Partitions this collection into a map of collections according to some
      * discriminator function. Additionally projecting each element to a new value.
      *
      *  @param key  the discriminator fu tion.
      *  @param proj projection function to produce new value for each element of this  $coll
      *  @tparam K the type of keys returned by the discriminator function.
      *  @tparam V the type of values returned by the projection function.
      *  @return A map from keys to ${coll}s such that the following invariant holds:
      *  {{{
      *    (xs groupByProjecting (key, proj))(k) = xs filter (x => key(x) == k).map(proj)
      *  }}}
      *  That is, every key `k` is bound to projections of those elements `x`
      *  for which `key(x)` eq ls `k`.
      */
    def groupByProjecting[K: RType, V: RType](key: A => K, proj: A => V): Coll[(K, Coll[V])] = {
      implicit val ctV: ClassTag[V] = RType[V].classTag
      val b = coll.builder
      val res = coll.toArray.groupBy(key).mapValues(arr => b.fromArray(arr.map(proj)))
      b.fromMap(res.toMap)
    }

  }

  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    implicit def tA = source.tItem.tFst
    implicit def tB = source.tItem.tSnd

    /** Maps the first component of each pair in the collection. */
    @inline def mapFirst[A1: RType](f: A => A1): Coll[(A1, B)] = source.asInstanceOf[PairColl[A, B]].mapFirst(f)

    /** Maps the first component of each pair in the collection. */
    @inline def mapSecond[B1: RType](f: B => B1): Coll[(A, B1)] = source.asInstanceOf[PairColl[A, B]].mapSecond(f)

    /** Uses the first component of each pair in the collection as a key for
      * grouping and reducing the corresponding values.
      *
      * @return collection with each found unique key as first component and the reduction
      *         result of the corresponding values.
      */
    def reduceByKey(r: ((B, B)) => B): Coll[(A, B)] = {
      source.mapReduce(identity, r)
    }

    /** Uses the first component of each pair in the collection as a key for
      * grouping and summing the corresponding values using the given Numeric.
      *
      * @return collection with each found unique key as first component and the summation
      *         result of the corresponding values.
      */
    def sumByKey(implicit m: Numeric[B]): Coll[(A, B)] =
      reduceByKey(r => m.plus(r._1, r._2))

    /** Uses the first component of each pair in the collection as a key for
      * grouping the corresponding values into a new collection.
      *
      * @return collection with each found unique key as first component and the
      *         collection of the corresponding values.
      */
    def groupByKey: Coll[(A, Coll[B])] = {
      source.groupByProjecting(_._1, _._2)
    }
  }

  implicit class CollBuilderOps(val builder: CollBuilder) extends AnyVal {
    /** Performs outer join operation between left and right collections.
      *
      * @param l     projection function executed for each element of `left`
      * @param r     projection function executed for each element of `right`
      * @param inner projection function which is executed for matching items (K, L) and (K, R) with the same K
      * @return collection of (K, O) pairs, where each key comes form either left or right
      *         collection and values are produced by projections
      */
    def outerJoin[K: RType, L, R, O: RType]
        (left: Coll[(K, L)], right: Coll[(K, R)])
        (l: ((K, L)) => O,
         r: ((K, R)) => O,
         inner: ((K, (L, R))) => O): Coll[(K, O)] = {
      val res = Utils.outerJoin[K, L, R, O](left.toMap, right.toMap)(
        (k, lv) => l((k, lv)),
        (k, rv) => r((k, rv)),
        (k, lv, rv) => inner((k, (lv, rv))))
      fromMap(res)
    }

    /** Construct a collection of (K,V) pairs using PairColl representation,
      * in which keys and values are stored as separate unboxed arrays. */
    def fromMap[K: RType, V: RType](m: Map[K, V]): Coll[(K, V)] = {
      val (ks, vs) = Utils.mapToArrays(m)
      builder.pairCollFromArrays(ks, vs)
    }
  }

  implicit class HeaderOps(val h: Header) extends AnyVal {
    def toPreHeader: PreHeader = {
      CPreHeader(h.version, h.parentId, h.timestamp, h.nBits, h.height, h.minerPk, h.votes)
    }
  }

}
