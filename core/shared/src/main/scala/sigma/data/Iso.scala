package sigma.data

import scorex.util.encode.Base16
import sigma.Extensions.CollBytesOps
import sigma.{Coll, Colls}

/** Type-class of isomorphisms between types.
  * Isomorphism between two types `A` and `B` essentially say that both types
  * represents the same information (entity) but in a different way.
  *
  * Each isomorphism defined by two functions:
  * - `to: A => B` - conversion from `A` to `B`
  * - `from: B => A` - conversion from `B` to `A`
  *
  * <p>
  * such that the information is not lost during conversion, so that both are true:
  * 1) a == from(to(a))
  * 2) b == to(from(b))
  * <p>
  * It is used to define type-full conversions:
  * - different conversions between Java and Scala data types.
  * - conversion between internal Ergo representations and API representations
  * - conversions between exported JS classes and internal Ergo representations
  */
abstract class Iso[A, B] {
  def to(a: A): B
  def from(b: B): A
  def andThen[C](iso: Iso[B,C]): Iso[A,C] = ComposeIso(iso, this)
  def inverse: Iso[B, A] = InverseIso(this)
}
final case class InverseIso[A,B](iso: Iso[A,B]) extends Iso[B,A] {
  override def to(a: B): A = iso.from(a)
  override def from(b: A): B = iso.to(b)
}
final case class ComposeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]) extends Iso[A, C] {
  def from(c: C): A = iso1.from(iso2.from(c))
  def to(a: A): C = iso2.to(iso1.to(a))
}

object Iso {
  implicit def identityIso[A]: Iso[A, A] = new Iso[A, A] {
    override def to(a: A): A = a
    override def from(b: A): A = b
  }

  implicit def inverseIso[A, B](implicit iso: Iso[A, B]): Iso[B, A] = InverseIso[A, B](iso)

  val isoStringToArray: Iso[String, Array[Byte]] = new Iso[String, Array[Byte]] {
    override def to(x: String): Array[Byte] = Base16.decode(x).get
    override def from(x: Array[Byte]): String = Base16.encode(x)
  }

  val isoStringToColl: Iso[String, Coll[Byte]] = new Iso[String, Coll[Byte]] {
    override def to(x: String): Coll[Byte] = Colls.fromArray(Base16.decode(x).get)
    override def from(x: Coll[Byte]): String = x.toHex
  }

}