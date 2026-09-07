package sigma

import sigma.compat.TaggedType

import scala.reflect.ClassTag

/** Contains cores definitions which serves as a basis for [[sigma]] package implementations. */
package object data {
  // ClassTags are built directly from `classOf` rather than via `classTag[T]` (which performs an
  // implicit ClassTag search). This prevents the `sigma.rtypeToClassTag` implicit from the
  // enclosing package object from being selected here: on Scala 3 it would be, and during the
  // cyclic initialization of the `sigma` / `sigma.data` package objects it would dereference a
  // not-yet-initialized RType, throwing an NPE at class-load time. (On Scala 2 a private
  // same-named shadow def used to suppress that implicit, but the shadow has no effect in Scala 3.)
  val StringClassTag = ClassTag[String](classOf[String])
  val BigIntClassTag = ClassTag[BigInt](classOf[BigInt])
  val UnsignedBigIntClassTag = ClassTag[UnsignedBigInt](classOf[UnsignedBigInt])
  val GroupElementClassTag = ClassTag[GroupElement](classOf[GroupElement])
  val SigmaPropClassTag = ClassTag[SigmaProp](classOf[SigmaProp])
  val SigmaBooleanClassTag = ClassTag[SigmaBoolean](classOf[SigmaBoolean])
  val AvlTreeClassTag = ClassTag[AvlTree](classOf[AvlTree])
  val BoxClassTag = ClassTag[Box](classOf[Box])
  val ContextClassTag = ClassTag[Context](classOf[Context])
  val HeaderClassTag = ClassTag[Header](classOf[Header])
  val PreHeaderClassTag = ClassTag[PreHeader](classOf[PreHeader])
  val AnyValueClassTag = ClassTag[AnyValue](classOf[AnyValue])
  val SigmaDslBuilderClassTag = ClassTag[SigmaDslBuilder](classOf[SigmaDslBuilder])
  val CollBuilderClassTag = ClassTag[CollBuilder](classOf[CollBuilder])

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
