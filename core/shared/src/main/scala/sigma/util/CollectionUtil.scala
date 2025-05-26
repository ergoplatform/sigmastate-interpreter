package sigma.util

import scala.collection.{Seq, mutable, GenIterable}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.reflect.ClassTag
import scala.collection.compat._

object CollectionUtil {

  /** @deprecated shouldn't be used other than for backwards compatibility with v3.x, v4.x. */
  def concatArrays_v4[T](xs: Array[T], ys: Array[T]): Array[T] = {
    val len = xs.length + ys.length
    val result = (xs match {
      case _: Array[AnyRef] => new Array[AnyRef](len) // creates an array with invalid type descriptor (i.e. when T == Tuple2)
      case _: Array[Byte] => new Array[Byte](len)
      case _: Array[Short] => new Array[Short](len)
      case _: Array[Int] => new Array[Int](len)
      case _: Array[Long] => new Array[Long](len)
      case _: Array[Char] => new Array[Char](len)
      case _: Array[Float] => new Array[Float](len)
      case _: Array[Double] => new Array[Double](len)
      case _: Array[Boolean] => new Array[Boolean](len)
    }).asInstanceOf[Array[T]]
    Array.copy(xs, 0, result, 0, xs.length)
    Array.copy(ys, 0, result, xs.length, ys.length)
    result
  }

  /** Concatenates two arrays into a new resulting array.
    * All items of both arrays are copied to the result using System.arraycopy.
    * This method takes ClassTag to create proper resulting array.
    * Can be used in v5.0 and above.
    */
  def concatArrays[T:ClassTag](arr1: Array[T], arr2: Array[T]): Array[T] = {
    val l1 = arr1.length
    val l2 = arr2.length
    val length: Int = l1 + l2
    val result: Array[T] = new Array[T](length)
    System.arraycopy(arr1, 0, result, 0, l1)
    System.arraycopy(arr2, 0, result, l1, l2)
    result
  }

  /** Casts the array of `A`s into array of `B`s, where B is a superclass of A. */
  def castArray[A, B >: A : ClassTag](array: Array[A]): Array[B] = {
    val result: Array[B] = new Array[B](array.length)
    System.arraycopy(array, 0, result, 0, array.length)
    result
  }

  /** Computes the deep hash code for the given array.
    *
    * This method calculates the hash code based on the array's elements and type, taking nested arrays
    * into account.
    *
    * @tparam T the type of the elements in the array
    * @param arr the input array for which the deep hash code is to be calculated
    */
  def deepHashCode[T](arr: Array[T]): Int = arr match {
    case arr: Array[AnyRef] => java.util.Arrays.deepHashCode(arr)
    case arr: Array[Byte] => java.util.Arrays.hashCode(arr)
    case arr: Array[Short] => java.util.Arrays.hashCode(arr)
    case arr: Array[Int] => java.util.Arrays.hashCode(arr)
    case arr: Array[Long] => java.util.Arrays.hashCode(arr)
    case arr: Array[Char] => java.util.Arrays.hashCode(arr)
    case arr: Array[Float] => java.util.Arrays.hashCode(arr)
    case arr: Array[Double] => java.util.Arrays.hashCode(arr)
    case arr: Array[Boolean] => java.util.Arrays.hashCode(arr)
  }

  /** Group the given sequence of pairs by first values as keys.
    * @param kvs sequence of values which is traversed once
    * @return a multimap with ArrayBuffer of values for each key.
    */
  def createMultiMap[K,V](kvs: GenIterable[(K,V)]): Map[K, ArrayBuffer[V]] = {
    val res = HashMap.empty[K, ArrayBuffer[V]]
    kvs.foreach { case (k, v) =>
      if (res.contains(k))
        res(k) += v
      else
        res += k -> ArrayBuffer(v)
      ()
    }
    res.toMap
  }

  /** Perform relational inner join of two sequences using the given key projections. */
  def joinSeqs[O, I, K](outer: Iterable[O], inner: Iterable[I])(outKey: O=>K, inKey: I=>K): Iterable[(O,I)] = {
    val kvs = createMultiMap(inner.map(i => (inKey(i), i)))
    val res = outer.flatMap(o => {
      val ko = outKey(o)
      kvs.get(ko) match {
        case Some(inners) =>
          inners.map(i => (o,i))
        case None =>
          Nil
      }
    })
    res
  }

  /** Performs an outer join on two sequences using specified key projection functions and
    * result projection functions.
    *
    * @param outer  the outer sequence for the join operation
    * @param inner  the inner sequence for the join operation
    * @param outKey projects an outer element to its join key
    * @param inKey  projects an inner element to its join key
    * @param projO  projects a non-matching outer element and its key to a result element
    * @param projI  projects a non-matching inner element and its key to a result element
    * @param proj   projects a matching pair of outer and inner elements and their key to a result element
    */
  def outerJoinSeqs[O, I, K, R]
      (outer: Seq[O], inner: Seq[I])
      (outKey: O=>K, inKey: I=>K)
      (projO: (K,O) => R, projI: (K,I) => R, proj:(K,O,I) => R): Seq[(K,R)] = {
    val res = ArrayBuffer.empty[(K,R)]
    val kis = inner.map(i => (inKey(i), i))
    val kvs = createMultiMap(kis)
    val outerKeys = mutable.Set.empty[K]
    for (o <- outer) {
      val ko = outKey(o)
      outerKeys += ko
      if (!kvs.contains(ko))
        res += ((ko, projO(ko, o)))
      else
        for (i <- kvs(ko))
          res += ((ko, proj(ko, o, i)))
    }
    for ((k,i) <- kis if !outerKeys.contains(k))
      res += ((k, projI(k, i)))
    res
  }

  implicit class AnyOps[A](val x: A) extends AnyVal {
    /** Performs a specified action on the source value and returns the result. */
    def perform(action: A => A): A = action(x)

    /** Traverses the tree structure in a depth-first manner using the provided function to generate child nodes.
      *
      * @param f a function that takes a node of type A and returns a list of its children
      * @tparam A the type of the node elements in the tree structure
      * @return a list of traversed nodes in depth-first order
      */
    def traverseDepthFirst(f: A => List[A]): List[A] = {
      var all: List[A] = Nil
      var stack = List(x)
      while (stack.nonEmpty) {
        val h = stack.head
        stack = stack.tail

        var next = f(h).reverse
        while (next.nonEmpty) {
          stack = next.head :: stack
          next = next.tail
        }
        all = h :: all
      }
      all.reverse
    }
  }
  
  implicit class TraversableOps[A, Source[X] <: GenIterable[X]](val xs: Source[A]) extends AnyVal {

    /** Returns a copy of this collection where elements at `items(i)._1` are replaced
      * with `items(i)._2` for each i.
      */
    def updateMany(items: Seq[(Int, A)])(implicit tA: ClassTag[A]): Seq[A] = {
      val res = xs.toArray
      val nItems = items.length
      var i = 0
      while (i < nItems) {
        val item = items(i)
        // this explicit check is necessary for Scala.js to be equivalent with JVM
        if (!res.isDefinedAt(item._1)) {
          throw new IndexOutOfBoundsException("Index out of range: " + item._1)
        }
        res(item._1) = item._2
        i += 1
      }
      res
    }

    /** Traverses the `xs` collection and checks that each item is of type `B`.
      * @return original collection `xs` casted to Source[B]
      * @throws java.lang.AssertionError if at least one item cannot be cast to `B`
      */
    def cast[B:ClassTag]: Source[B] = {
      for (x <- xs) {
        assert(x match { case _: B => true case _ => false}, s"Value $x doesn't conform to type ${reflect.classTag[B]}")
      }
      xs.asInstanceOf[Source[B]]
    }

    /** This methods is for compatibility with Scala 2.11. */
    def distinctBy[K](key: A => K)
        (implicit cbf: BuildFrom[Source[A], A, Source[A]]): Source[A] = {
      val keys = mutable.Set[K]()
      val b = cbf.newBuilder(xs)
      for ( x <- xs ) {
        val k = key(x)
        if (!keys.contains(k)) {
          b += x
          keys += k
        }
      }
      b.result()
    }

    private def flattenIter(i: Iterator[_]): Iterator[_] = i.flatMap(x => x match {
      case nested: GenIterable[_] => nested
      case arr: Array[_] => arr.iterator
      case _ => Iterator.single(x)
    })

    /** Determines if two nested structures have the same elements in the same order.
      * Supports structures containing `GenIterable` and `Array` elements.
      *
      * @param that the other nested structure to compare with
      * @tparam A the type of the elements in the current structure
      * @tparam B the type of the elements in the other structure, a supertype of A
      * @return true if the two nested structures have the same elements in the same order, false otherwise
      */
    def sameElementsNested[B >: A](that: GenIterable[B]): Boolean = {
      val i1: Iterator[Any] = flattenIter(xs.iterator)
      val i2: Iterator[Any] = flattenIter(that.iterator)
      i1.sameElements(i2)
    }
  }
}


