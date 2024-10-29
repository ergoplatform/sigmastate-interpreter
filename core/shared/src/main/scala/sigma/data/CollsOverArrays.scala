package sigma.data

import debox.{Buffer, cfor}
import sigma.Evaluation.stypeToRType
import sigma.data.CollOverArray.equalsPairCollWithCollOverArray
import sigma.data.RType._
import sigma.util.{CollectionUtil, MaxArrayLength, safeConcatArrays_v5}
import sigma.{Coll, CollBuilder, PairColl, VersionContext, requireSameLength}
import sigma.rtypeToClassTag

class CollOverArray[@specialized A](val toArray: Array[A], val builder: CollBuilder)
                                   (implicit tA: RType[A]) extends Coll[A] {
  require(toArray.length <= MaxArrayLength,
    s"Cannot create collection with size ${toArray.length} greater than $MaxArrayLength")

  override def tItem: RType[A] = tA

  @inline def length: Int = toArray.length

  @inline def apply(i: Int): A = toArray.apply(i)

  override def isEmpty: Boolean = length == 0

  override def nonEmpty: Boolean = length > 0

  override def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < toArray.length) toArray(i) else default

  def map[@specialized B: RType](f: A => B): Coll[B] = {
    implicit val ctB = RType[B].classTag
    builder.fromArray(toArray.map(f))
  }

  def foreach(f: A => Unit): Unit = toArray.foreach(f)

  def exists(p: A => Boolean): Boolean = toArray.exists(p)

  def forall(p: A => Boolean): Boolean = toArray.forall(p)

  def filter(p: A => Boolean): Coll[A] = builder.fromArray(toArray.filter(p))

  def foldLeft[B](zero: B, op: ((B, A)) => B): B = toArray.foldLeft(zero)((b, a) => op((b, a)))

  def slice(from: Int, until: Int): Coll[A] = builder.fromArray(toArray.slice(from, until))

  @inline def zip[@specialized B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)

  @inline def startsWith(ys: Coll[A]): Boolean = toArray.startsWith(ys.toArray)

  @inline def endsWith(ys: Coll[A]): Boolean = toArray.endsWith(ys.toArray)

  def append(other: Coll[A]): Coll[A] = {
    if (toArray.length <= 0) return other
    val result = if (VersionContext.current.isJitActivated) {
      // in v5.0 and above this fixes the ClassCastException problem
      safeConcatArrays_v5(toArray, other.toArray)(tA.classTag)
    } else {
      CollectionUtil.concatArrays_v4(toArray, other.toArray)
    }
    builder.fromArray(result)
  }

  def reverse: Coll[A] = {
    val limit = length
    val res = new Array[A](limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      res(i) = toArray(limit - i - 1)
    }
    builder.fromArray(res)
  }

  def indices: Coll[Int] = builder.fromArray(toArray.indices.toArray)

  override def flatMap[B: RType](f: A => Coll[B]): Coll[B] = {
    implicit val ctB = RType[B].classTag
    builder.fromArray(toArray.flatMap(x => f(x).toArray))
  }

  override def segmentLength(p: A => Boolean, from: Int): Int = toArray.segmentLength(p, from)

  override def indexWhere(p: A => Boolean, from: Int): Int = {
    val start = math.max(from, 0) // This is necessary for Scala 2.13 as indexWhere is implemented differently
    toArray.indexWhere(p, start)
  }

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = toArray.lastIndexWhere(p, end)

  override def take(n: Int): Coll[A] = {
    if (n <= 0) builder.emptyColl
    else if (n >= length) this
    else {
      val res = new Array[A](n)
      Array.copy(toArray, 0, res, 0, n)
      builder.fromArray(res)
    }
  }

  override def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A] = {
    // TODO optimize: avoid using `patch` as it do boxing
    val res = toArray.patch(from, patch.toArray, replaced).toArray
    builder.fromArray(res)
  }

  override def updated(index: Int, elem: A): Coll[A] = {
    // TODO optimize: avoid using `updated` as it do boxing
    val res = toArray.updated(index, elem)
    builder.fromArray(res)
  }

  override def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A] = {
    requireSameLength(indexes, values)
    val resArr = toArray.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= toArray.length) throw new IndexOutOfBoundsException(pos.toString)
      resArr(pos) = values(i)
      i += 1
    }
    builder.fromArray(resArr)
  }

  override def unionSet(that: Coll[A]): Coll[A] = {
    val set = debox.Set.ofSize[A](this.length)
    val res = Buffer.ofSize[A](this.length)

    @inline def addItemToSet(x: A) = {
      if (!set(x)) {
        set.add(x)
        res += x
      }
    }

    def addToSet(arr: Array[A]) = {
      val limit = arr.length
      cfor(0)(_ < limit, _ + 1) { i =>
        val x = arr(i)
        addItemToSet(x)
      }
    }

    addToSet(this.toArray)
    addToSet(that.toArray)

    builder.fromArray(res.toArray())
  }

  override def equals(obj: scala.Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case obj: CollOverArray[_] if obj.tItem == this.tItem =>
      java.util.Objects.deepEquals(obj.toArray, this.toArray)
    case obj: PairColl[Any, Any] if obj.tItem == this.tItem =>
      if (VersionContext.current.isV6SoftForkActivated) {
        equalsPairCollWithCollOverArray(obj, this.asInstanceOf[CollOverArray[Any]])
      } else {
        false
      }
    case _ => false
  })

  override def hashCode(): Int = CollectionUtil.deepHashCode(toArray)
}

object CollOverArray {

  // comparing PairColl and CollOverArray instances
  private[data] def equalsPairCollWithCollOverArray(pc: PairColl[Any, Any], coa: CollOverArray[Any]): Boolean = {
    val ls = pc.ls
    val rs = pc.rs
    val ts = coa.toArray
    if (ts.length == ls.length && ts.isInstanceOf[Array[(Any, Any)]]) {
      val ta = ts.asInstanceOf[Array[(Any, Any)]]
      var eq = true
      cfor(0)(_ < ta.length && eq, _ + 1) { i =>
        eq = java.util.Objects.deepEquals(ta(i)._1, ls(i)) && java.util.Objects.deepEquals(ta(i)._2, rs(i))
      }
      eq
    } else {
      false
    }
  }
  
}

private[sigma] class CollOverArrayBuilder extends CollBuilder {
  builder =>

  @inline override def pairColl[@specialized A, @specialized B](as: Coll[A], bs: Coll[B]): PairColl[A, B] = {
    if (VersionContext.current.isJitActivated) {
      // v5.0 and above
      val asLen = as.length
      val bsLen = bs.length
      // if necessary, use minimal length and slice longer collection
      if (asLen == bsLen)
        new PairOfCols(as, bs)
      else if (asLen < bsLen) {
        new PairOfCols(as, bs.slice(0, asLen))
      } else {
        new PairOfCols(as.slice(0, bsLen), bs)
      }
    } else {
      // The v4.x implementation doesn't check the case when `as` and `bs` have different lengths.
      // In which case appending two `PairOfCols` leads to invalid pairing of elements.
      // To fix the problem, the longer collection have to be truncated (which is consistent
      // with how zip is implemented for Arrays)
      new PairOfCols(as, bs)
    }
  }

  private def fromBoxedPairs[A, B](seq: Seq[(A, B)])(implicit tA: RType[A], tB: RType[B]): PairColl[A, B] = {
    val len = seq.length
    val resA = Array.ofDim[A](len)(tA.classTag)
    val resB = Array.ofDim[B](len)(tB.classTag)
    cfor(0)(_ < len, _ + 1) { i =>
      val item = seq.apply(i).asInstanceOf[(A, B)]
      resA(i) = item._1
      resB(i) = item._2
    }
    pairCollFromArrays(resA, resB)(tA, tB)
  }

  override def fromItems[T](items: T*)(implicit cT: RType[T]): Coll[T] = cT match {
    case pt: PairType[a, b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      fromBoxedPairs(items)(tA, tB)
    case _ =>
      new CollOverArray(items.toArray(cT.classTag), builder)
  }

  override def fromArray[@specialized T: RType](arr: Array[T]): Coll[T] = RType[T] match {
    case pt: PairType[a, b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      fromBoxedPairs[a, b](arr.asInstanceOf[Array[(a, b)]])(tA, tB)
    case _ =>
      new CollOverArray(arr, builder)
  }

  override def replicate[@specialized T: RType](n: Int, v: T): Coll[T] = RType[T] match {
    case pt: PairType[a, b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      val tuple = v.asInstanceOf[(a, b)]
      new PairOfCols(replicate(n, tuple._1)(tA), replicate(n, tuple._2)(tB))
    case _ =>
      fromArray(Array.fill(n)(v))
  }

  override def unzip[@specialized A, @specialized B](xs: Coll[(A, B)]): (Coll[A], Coll[B]) = xs match {
    case pa: PairColl[_, _] => (pa.ls, pa.rs)
    case _ =>
      val limit = xs.length
      implicit val tA = xs.tItem.tFst
      implicit val tB = xs.tItem.tSnd
      val ls = Array.ofDim[A](limit)(tA.classTag)
      val rs = Array.ofDim[B](limit)(tB.classTag)
      cfor(0)(_ < limit, _ + 1) { i =>
        val p = xs(i)
        ls(i) = p._1
        rs(i) = p._2
      }
      (fromArray(ls), fromArray(rs))
  }

  override def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte] =
    left.zip(right).map { case (l, r) => (l ^ r).toByte }

  override def emptyColl[T](implicit cT: RType[T]): Coll[T] = cT match {
    case pt: PairType[a, b] =>
      val ls = emptyColl(pt.tFst)
      val rs = emptyColl(pt.tSnd)
      pairColl(ls, rs).asInstanceOf[Coll[T]]
    case _ =>
      new CollOverArray[T](cT.emptyArray, builder)
  }
}

class PairOfCols[@specialized L, @specialized R](val ls: Coll[L], val rs: Coll[R]) extends PairColl[L, R] {

  override def equals(that: scala.Any): Boolean = (this eq that.asInstanceOf[AnyRef]) || (that match {
    case that: PairColl[_, _] if that.tItem == this.tItem =>
      ls == that.ls && rs == that.rs
    case that: CollOverArray[Any] if that.tItem == this.tItem =>
      if (VersionContext.current.isV6SoftForkActivated) {
        equalsPairCollWithCollOverArray(this.asInstanceOf[PairColl[Any, Any]], that)
      } else {
        false
      }
    case _ => false
  })

  override def hashCode() = ls.hashCode() * 41 + rs.hashCode()

  @inline implicit def tL: RType[L] = ls.tItem

  @inline implicit def tR: RType[R] = rs.tItem

  override lazy val tItem: RType[(L, R)] = {
    RType.pairRType(tL, tR)
  }

  override def builder: CollBuilder = ls.builder

  override def toArray: Array[(L, R)] = ls.toArray.zip(rs.toArray)

  @inline override def length: Int = if (ls.length <= rs.length) ls.length else rs.length

  @inline override def apply(i: Int): (L, R) = (ls(i), rs(i))

  override def isEmpty: Boolean = length == 0

  override def nonEmpty: Boolean = length > 0

  override def isDefinedAt(idx: Int): Boolean = ls.isDefinedAt(idx) && rs.isDefinedAt(idx)

  override def getOrElse(i: Int, default: (L, R)): (L, R) =
    if (i >= 0 && i < this.length)
      this.apply(i)
    else {
      val d = default // force thunk
      (d._1, d._2)
    }

  override def map[@specialized V: RType](f: ((L, R)) => V): Coll[V] = {
    val limit = ls.length
    val res = new Array[V](limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      res(i) = f((ls(i), rs(i)))
    }
    new CollOverArray(res, builder)
  }

  override def exists(p: ((L, R)) => Boolean): Boolean = {
    val len = ls.length
    var i = 0
    while (i < len) {
      val found = p((ls(i), rs(i)))
      if (found) return true
      i += 1
    }
    false
  }

  override def forall(p: ((L, R)) => Boolean): Boolean = {
    val len = ls.length
    var i = 0
    while (i < len) {
      val ok = p((ls(i), rs(i)))
      if (!ok) return false
      i += 1
    }
    true
  }

  override def filter(p: ((L, R)) => Boolean): Coll[(L, R)] = {
    val len = ls.length
    val resL: Buffer[L] = Buffer.empty[L](ls.tItem.classTag)
    val resR: Buffer[R] = Buffer.empty[R](rs.tItem.classTag)
    var i = 0
    while (i < len) {
      val l = ls.apply(i)
      val r = rs.apply(i)
      val ok = p((l, r))
      if (ok) {
        resL += l
        resR += r
      }
      i += 1
    }
    builder.pairCollFromArrays(resL.toArray(), resR.toArray())
  }

  override def foldLeft[B](zero: B, op: ((B, (L, R))) => B): B = {
    val limit = length
    var state = zero
    cfor(0)(_ < limit, _ + 1) { i =>
      val l = ls.apply(i)
      val r = rs.apply(i)
      state = op((state, (l, r)))
    }
    state
  }

  override def slice(from: Int, until: Int): PairColl[L, R] = builder.pairColl(ls.slice(from, until), rs.slice(from, until))

  def append(other: Coll[(L, R)]): Coll[(L, R)] = {
    val arrs = builder.unzip(other)
    builder.pairColl(ls.append(arrs._1), rs.append(arrs._2))
  }

  override def reverse: Coll[(L, R)] = {
    val lLen = ls.length
    val rLen = rs.length
    if (lLen == rLen) {
      builder.pairColl(ls.reverse, rs.reverse)
    } else if (lLen < rLen) {
      builder.pairColl(ls.reverse, rs.slice(0, lLen).reverse)
    } else {
      builder.pairColl(ls.slice(0, rLen).reverse, rs.reverse)
    }
  }

  def zip[@specialized B](ys: Coll[B]): PairColl[(L, R), B] = builder.pairColl(this, ys)

  def startsWith(ys: Coll[(L, R)]): Boolean = ys match {
    case yp: PairOfCols[L, R] => ls.startsWith(yp.ls) && rs.startsWith(yp.rs)
    case _ => toArray.startsWith(ys.toArray)
  }

  def endsWith(ys: Coll[(L, R)]): Boolean = ys match {
    case yp: PairOfCols[L, R] => ls.endsWith(yp.ls) && rs.endsWith(yp.rs)
    case _ => toArray.endsWith(ys.toArray)
  }

  override def indices: Coll[Int] = if (ls.length <= rs.length) ls.indices else rs.indices

  override def flatMap[B: RType](f: ((L, R)) => Coll[B]): Coll[B] =
    builder.fromArray(toArray.flatMap(p => f(p).toArray))

  override def segmentLength(p: ((L, R)) => Boolean, from: Int): Int = {
    toArray.segmentLength(p, from)
  }

  override def indexWhere(p: ((L, R)) => Boolean, from: Int): Int = toArray.indexWhere(p, from)

  override def lastIndexWhere(p: ((L, R)) => Boolean, end: Int): Int = toArray.lastIndexWhere(p, end)

  override def take(n: Int): Coll[(L, R)] = builder.pairColl(ls.take(n), rs.take(n))

  override def patch(from: Int, patch: Coll[(L, R)], replaced: Int): Coll[(L, R)] = {
    val (lsPatch, rsPatch) = builder.unzip(patch)
    val lp = ls.patch(from, lsPatch, replaced)
    val rp = rs.patch(from, rsPatch, replaced)
    builder.pairColl(lp, rp)
  }

  override def updated(index: Int, elem: (L, R)): Coll[(L, R)] = {
    val lu = ls.updated(index, elem._1)
    val ru = rs.updated(index, elem._2)
    builder.pairColl(lu, ru)
  }

  override def updateMany(indexes: Coll[Int], values: Coll[(L, R)]): Coll[(L, R)] = {
    requireSameLength(indexes, values)
    val resL = ls.toArray.clone()
    val resR = rs.toArray.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= length) throw new IndexOutOfBoundsException(pos.toString)
      resL(pos) = values(i)._1
      resR(pos) = values(i)._2
      i += 1
    }
    builder.pairColl(builder.fromArray(resL), builder.fromArray(resR))
  }

  override def unionSet(that: Coll[(L, R)]): Coll[(L, R)] = {
    val set = new java.util.HashSet[(L, R)](32)
    implicit val ctL = ls.tItem.classTag
    implicit val ctR = rs.tItem.classTag
    val resL = Buffer.empty[L]
    val resR = Buffer.empty[R]

    def addToSet(item: (L, R)) = {
      if (!set.contains(item)) {
        set.add(item)
        resL += item._1
        resR += item._2
      }
    }

    var i = 0
    val thisLen = math.min(ls.length, rs.length)
    while (i < thisLen) {
      addToSet((ls(i), rs(i)))
      i += 1
    }
    i = 0
    val thatLen = that.length
    while (i < thatLen) {
      addToSet(that(i))
      i += 1
    }
    builder.pairCollFromArrays(resL.toArray, resR.toArray)
  }

  override def mapFirst[T1: RType](f: L => T1): Coll[(T1, R)] = {
    builder.pairColl(ls.map(f), rs)
  }

  override def mapSecond[T1: RType](f: R => T1): Coll[(L, T1)] = {
    builder.pairColl(ls, rs.map(f))
  }
}
