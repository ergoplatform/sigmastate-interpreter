package sigma

import supertagged.TaggedType

import scala.annotation.nowarn
import scala.reflect.classTag

/** Contains cores definitions which serves as a basis for [[sigma]] package implementations. */
package object data {
  /** Shadow the implicit from sigma package so it doesn't interfere with the resolution
    * of ClassTags below.
    */
  @nowarn private def rtypeToClassTag = ???

  val StringClassTag = classTag[String]
  val BigIntClassTag = classTag[BigInt]
  val UnsignedBigIntClassTag = classTag[UnsignedBigInt]
  val GroupElementClassTag = classTag[GroupElement]
  val SigmaPropClassTag = classTag[SigmaProp]
  val SigmaBooleanClassTag = classTag[SigmaBoolean]
  val AvlTreeClassTag = classTag[AvlTree]
  val BoxClassTag = classTag[Box]
  val ContextClassTag = classTag[Context]
  val HeaderClassTag = classTag[Header]
  val PreHeaderClassTag = classTag[PreHeader]
  val AnyValueClassTag = classTag[AnyValue]
  val SigmaDslBuilderClassTag = classTag[SigmaDslBuilder]
  val CollBuilderClassTag = classTag[CollBuilder]

  /** Immutable empty array of integers, should be used instead of allocating new empty arrays. */
  val EmptyArrayOfInt = Array.empty[Int]

  /** Immutable empty Seq[Int] backed by empty array.
    * You should prefer using it instead of `Seq[Int]()` or `Seq.empty[Int]`
    */
  val EmptySeqOfInt: Seq[Int] = EmptyArrayOfInt

  /** Create a new empty buffer around pre-allocated empty array.
    * This method is preferred, rather that creating empty debox.Buffer directly
    * because it allows to avoid allocation of the empty array.
    * Note, this method allocates a new Buffer, but the underlying empty array is shared.
    * This is safe because empty arrays are immutable.
    */
  def emptyDBufferOfInt: debox.Buffer[Int] = debox.Buffer.unsafe(EmptyArrayOfInt)

  /** Constructor of tuple value with more than 2 items.
    * Such long tuples are represented as Coll[Any].
    * This representaion of tuples is different from representation of pairs (x, y),
    * where Tuple2 type is used instead of Coll. */
  def TupleColl(items: Any*): Coll[Any] = Colls.fromItems(items: _*)(sigma.AnyType)

  type KeyValueColl = Coll[(Coll[Byte], Coll[Byte])]

  trait BaseDigestColl extends TaggedType[Coll[Byte]]

  object Digest32Coll extends BaseDigestColl

  type Digest32Coll = Digest32Coll.Type

  implicit val Digest32CollRType: RType[data.Digest32Coll] = RType[Coll[Byte]].asInstanceOf[RType[data.Digest32Coll]]
}
