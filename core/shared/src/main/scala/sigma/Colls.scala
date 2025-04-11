package sigma

import sigma.data.RType

/** Indexed (zero-based) collection of elements of type `A`.
  * NOTE: declaring it `abstract class` makes some performance benefits, but doesn't work
  * well with specialization.
  *
  * @define Coll `Coll`
  * @define coll collection
  * @define colls collections
  * @tparam A the collection element type
  */
trait Coll[@specialized A] {
  def builder: CollBuilder
  def toArray: Array[A]

  /** The length of the collection. */
  def length: Int

  /** The size of the collection in elements. */
  def size: Int = this.length

  /** Tests whether the $coll is empty.
    *  @return    `true` if the $coll contains no elements, `false` otherwise.
    */
  def isEmpty: Boolean

  /** Tests whether the $coll is not empty.
    *  @return    `true` if the $coll contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean

  /** Returns true if the index in the valid range.
    * @param  i  index of an element of this collection */
  final def isValidIndex(i: Int): Boolean = 0 <= i && i < this.length

  /** The element at given index.
    *  Indices start at `0`; `xs.apply(0)` is the first element of collection `xs`.
    *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
    *
    *  @param  i       the index
    *  @return         the element at the given index
    *  @throws         ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
    */
  def apply(i: Int): A

  /** Tests whether this $coll contains given index.
    *
    *  The implementations of methods `apply` and `isDefinedAt` turn a `Coll[A]` into
    *  a `PartialFunction[Int, A]`.
    *
    * @param    idx     the index to test
    * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
    */
  def isDefinedAt(idx: Int): Boolean

  /** The element of the collection or default value.
    * If an index is out of bounds (`i < 0 || i >= length`) then `default` value is returned.
    * @param   i   the index
    * @return      the element at the given index or default value if index is out or bounds
    * @since 2.0
    */
  def getOrElse(index: Int, default: A): A

  /** Builds a new collection by applying a function to all elements of this collection.
  *
  *  @param f      the function to apply to each element.
  *  @tparam B     the element type of the returned collection.
  *  @return       a new collection of type `Coll[B]` resulting from applying the given function
  *                `f` to each element of this collection and collecting the results.
  */
  def map[@specialized B: RType](f: A => B): Coll[B]

  /** For this collection (x0, ..., xN) and other collection (y0, ..., yM)
    * produces a collection ((x0, y0), ..., (xK, yK)) where K = min(N, M) */
  def zip[@specialized B](ys: Coll[B]): Coll[(A, B)]

  /** Tests whether a predicate holds for at least one element of this collection.
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` is satisfied by at least one element of this collection, otherwise `false`
    */
  def exists(p: A => Boolean): Boolean

  /** Tests whether a predicate holds for all elements of this collection.
    *  @param   p   the predicate used to test elements.
    *  @return      `true` if this collection is empty or the given predicate `p`
    *               holds for all elements of this collection, otherwise `false`.
    */
  def forall(p: A => Boolean): Boolean

  /** Selects all elements of this collection which satisfy a predicate.
    *  @param p     the predicate used to test elements.
    *  @return      a new collection consisting of all elements of this collection that satisfy the given
    *               predicate `p`. The order of the elements is preserved.
    *  @since 2.0
    */
  def filter(p: A => Boolean): Coll[A]

  /** Applies a binary operator to a start value and all elements of this collection,
    *  going left to right.
    *
    *  @param   zero    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this collection,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x_1, ..., x_n` are the elements of this collection.
    *           Returns `z` if this collection is empty.
    */
  def foldLeft[B](zero: B, op: ((B, A)) => B): B

  /** Produces the range of all indices of this collection as a new collection
    * containing [0 .. length-1] values.
    * @since 2.0
    */
  def indices: Coll[Int]

  /**
    * Builds a new collection by applying a function to all elements of this $coll
    * and using the elements of the resulting collections.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Coll[B]` resulting from applying the given collection-valued function
    *         `f` to each element of this $coll and concatenating the results.
    *  @since 2.0
    */
  def flatMap[B: RType](f: A => Coll[B]): Coll[B]

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from  the index where the search starts.
    *  @return  the length of the longest segment of this $coll starting from index `from`
    *           such that every element of the segment satisfies the predicate `p`.
    *  @since 2.0
    */
  def segmentLength(p: A => Boolean, from: Int): Int

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    *  @since 2.0
    */
  def find(p: A => Boolean): Option[A] = {
    val i = segmentLength(!p(_), 0)
    if (i < length) Some(this(i)) else None
  }

  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
    *
    *  @param   elem   the element value to search for.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    *  @since 2.0
    */
  def indexOf(elem: A, from: Int): Int = this.indexWhere(x => elem == x, from)

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def lastIndexWhere(p: A => Boolean, end: Int): Int

  /** Selects first ''n'' elements.
    *  @param  n    the number of elements to take from this $coll.
    *  @return a $coll consisting only of the first `n` elements of this $coll,
    *          or else the whole $coll, if it has less than `n` elements.
    *          If `n` is negative, returns an empty $coll.
    */
  def take(n: Int): Coll[A]

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    *  @param  from     the index of the first replaced element
    *  @param  patch    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
    *  @since 2.0
    */
  def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A]

  /** A copy of this $coll with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *  @since 2.0
    */
  def updated(index: Int, elem: A): Coll[A]

  /** Returns a copy of this collection where elements at `indexes` are replaced with `values`.
    * @since 2.0
    */
  def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A]

  /** Produces a new collection which contains all distinct elements of this $coll and also all elements of
    *  a given collection that are not in this collection.
    *  This is order preserving operation considering only first occurrences of each distinct elements.
    *  Any collection `xs` can be transformed to a sequence with distinct elements by using xs.unionSet(Col()).
    *
    *  NOTE: Use append if you don't need set semantics.
    *
    *  @param that   the collection to add.
    *  @since 2.0
    */
  def unionSet(that: Coll[A]): Coll[A]

  /** Computes the multiset difference between this $coll and another sequence.
    *
    *  @param that   the sequence of elements to remove
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new collection which contains all elements of this $coll
    *                except some of occurrences of elements that also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *                part of the result, but any following occurrences will.
    *  @since 2.0
    */
  def diff(that: Coll[A]): Coll[A] = {
    val res = toArray.diff(that.toArray)
    builder.fromArray(res)
  }

  /** Computes the multiset intersection between this $coll and another sequence.
    *
    *  @param that   the sequence of elements to intersect with.
    *  @return       a new collection which contains all elements of this $coll
    *                which also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *                in the result, but any following occurrences will be omitted.
    *  @since 2.0
    */
  def intersect(that: Coll[A]): Coll[A] = {
    val res = toArray.intersect(that.toArray)
    builder.fromArray(res)
  }

  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    */
  def slice(from: Int, until: Int): Coll[A]

  /** Puts the elements of other collection after the elements of this collection (concatenation of 2 collections) */
  def append(other: Coll[A]): Coll[A]

  /** Returns new $coll with elements in reversed order.
    *
    *  @return A new $coll with all elements of this $coll in reversed order.
    */
  def reverse: Coll[A]

  private def trim[T](arr: Array[T]) = arr.take(arr.length min 100)
  override def toString = s"Coll(${trim(toArray).mkString(",")})"
  implicit def tItem: RType[A]

  /** Builds a new $coll from this $coll without any duplicate elements.
    *
    *  @return  A new $coll which contains the first occurrence of every element of this $coll.
    */
  def distinct: Coll[A] = {
    unionSet(builder.emptyColl[A])
  }
}

/** Base trait for specialized (Structure-Of-Arrays) representation of collection of pairs
  * (i.e. `Coll[(A, B)]`).
  * Some instances of `Coll[(A, B)]` may be instances of this trait, but it is NOT guaranteed,
  * since some of them may be instances of `CollOverArray[(A, B)]`.
  */
trait PairColl[@specialized L, @specialized R] extends Coll[(L,R)] {
  def ls: Coll[L]
  def rs: Coll[R]
  def mapFirst[T1: RType](f: L => T1): Coll[(T1, R)]
  def mapSecond[T1: RType](f: R => T1): Coll[(L, T1)]
}

/** Interface to access global collection methods.
  * See default implementation in CollOverArrayBuilder.
  */
trait CollBuilder {
  /** Constructs a new collection of pairs out of the pair of collections by zipping them.
    * The resulting collection is semantically equivalent to `as.zip(bs)`.
    * @param as collection of first items
    * @param bs collection of second items
    * @return an instance of [[PairColl]] interface with represents the resulting collection of pairs.
    */
  def pairColl[@specialized A, @specialized B](as: Coll[A], bs: Coll[B]): PairColl[A,B]

  /** Constructs a new collection of pairs out of the pair of arrays by wrapping them in collections
    * and delegating to [[pairColl]] method.
    * The resulting collection is semantically equivalent to as.zip(bs).
    * @param as collection of first items
    * @param bs collection of second items
    * @return an instance of [[PairColl]] interface with represents the resulting collection of pairs.
    */
  def pairCollFromArrays[A: RType, B: RType](as: Array[A], bs: Array[B]): PairColl[A,B] =
    pairColl(fromArray(as), fromArray(bs))

  /** Construct a new collection from the given list of arguments.
    * The arguments should be of the same type for which there should be
    * an implicit type descriptor at the call site. */
  def fromItems[T](items: T*)(implicit cT: RType[T]): Coll[T]

  /** Deconstruct collection of (A,B) pairs into pair of collections.
    * If `xs` is represented as PairColl, then this is O(1) operation (no data is touched). */
  def unzip[@specialized A, @specialized B](xs: Coll[(A,B)]): (Coll[A], Coll[B])

  /** Element-wise xor of two collections. */
  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte]

  /** Wrap array into collection. */
  def fromArray[@specialized T: RType](arr: Array[T]): Coll[T]

  /** Creates a new collection by replicating value `v`.
    * @param  n  how many times to replicate value `v`
    * @param  v  value to replicate
    * @return    collection of the form (v, v, v, ... v) of n elements.*/
  def replicate[@specialized T: RType](n: Int, v: T): Coll[T]

  /** Create an empty collection with items of the given type.
    * Even though there are no items, the type of them is specified. */
  def emptyColl[T](implicit tT: RType[T]): Coll[T]
}

