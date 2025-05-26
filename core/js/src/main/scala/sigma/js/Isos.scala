package sigma.js

import sigma.{Coll, Colls}
import sigma.data.{CBigInt, Iso, RType}

import java.math.BigInteger
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.JSConverters.JSRichOption

/** Definitions of isomorphisms for sigma-core module.
  * @see sigma.data.Iso
  */
object Isos {

  implicit def isoUndefOr[A, B](implicit iso: Iso[A, B]): Iso[js.UndefOr[A], Option[B]] = new Iso[js.UndefOr[A], Option[B]] {
    override def to(x: js.UndefOr[A]): Option[B] = x.toOption.map(iso.to)
    override def from(x: Option[B]): js.UndefOr[A] = x.map(iso.from).orUndefined
  }

  implicit def isoArrayToColl[A, B](iso: Iso[A, B])
      (implicit ctA: ClassTag[A], tB: RType[B]): Iso[js.Array[A], Coll[B]] = new Iso[js.Array[A], Coll[B]] {
    override def to(x: js.Array[A]): Coll[B] = Colls.fromArray(x.map(iso.to).toArray(tB.classTag))
    override def from(x: Coll[B]): js.Array[A] = js.Array(x.toArray.map(iso.from): _*)
  }

  implicit def isoArrayToIndexed[A, B](iso: Iso[A, B])
      (implicit cB: ClassTag[B]): Iso[js.Array[A], IndexedSeq[B]] = new Iso[js.Array[A], IndexedSeq[B]] {
    override def to(x: js.Array[A]): IndexedSeq[B] = x.map(iso.to).toArray(cB).toIndexedSeq
    override def from(x: IndexedSeq[B]): js.Array[A] = js.Array(x.map(iso.from): _*)
  }

  implicit val isoBigInt: Iso[js.BigInt, sigma.BigInt] = new Iso[js.BigInt, sigma.BigInt] {
    override def to(x: js.BigInt): sigma.BigInt = {
      CBigInt(new BigInteger(x.toString(10)))
    }

    override def from(x: sigma.BigInt): js.BigInt = {
      val bi = x.asInstanceOf[CBigInt].wrappedValue
      val s  = bi.toString(10)
      js.BigInt(s)
    }
  }

  implicit val isoBigIntToLong: Iso[js.BigInt, Long] = new Iso[js.BigInt, Long] {
    override def to(x: js.BigInt): Long = java.lang.Long.parseLong(x.toString(10))

    override def from(x: Long): js.BigInt = js.BigInt(x.toString)
  }
}
