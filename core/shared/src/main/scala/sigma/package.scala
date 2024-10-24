
import sigma.data._
import sigma.data.RType.SomeType

import scala.reflect.ClassTag

/** The following implicit values are used as type descriptors of all the predefined Sigma types.
  * @see [[RType]] class
  */
package object sigma {

  val AnyType: RType[Any] = GeneralType[Any](ClassTag.Any)

  implicit val BooleanType: RType[Boolean] = PrimitiveType[Boolean](ClassTag.Boolean, Array.emptyBooleanArray)

  implicit val ByteType   : RType[Byte]    = PrimitiveType[Byte](ClassTag.Byte, Array.emptyByteArray)

  implicit val ShortType  : RType[Short]   = PrimitiveType[Short](ClassTag.Short, Array.emptyShortArray)

  implicit val IntType    : RType[Int]     = PrimitiveType[Int](ClassTag.Int, Array.emptyIntArray)

  implicit val LongType   : RType[Long]    = PrimitiveType[Long](ClassTag.Long, Array.emptyLongArray)

  implicit val UnitType   : RType[Unit]    = PrimitiveType[Unit](ClassTag.Unit, Array[Unit]()(ClassTag.Unit))

  implicit val StringType : RType[String]  = GeneralType(StringClassTag)

  implicit val BigIntRType      : RType[BigInt]       = GeneralType(BigIntClassTag)
  implicit val UnsignedBigIntRType      : RType[UnsignedBigInt]       = GeneralType(UnsignedBigIntClassTag)
  implicit val GroupElementRType: RType[GroupElement] = GeneralType(GroupElementClassTag)
  implicit val SigmaPropRType   : RType[SigmaProp]    = GeneralType(SigmaPropClassTag)
  implicit val SigmaBooleanRType: RType[SigmaBoolean] = GeneralType(SigmaBooleanClassTag)


  implicit val AvlTreeRType:   RType[AvlTree]   = GeneralType(AvlTreeClassTag)

  implicit val BoxRType:       RType[Box]       = GeneralType(BoxClassTag)
  implicit val ContextRType:   RType[Context]   = GeneralType(ContextClassTag)

  implicit val HeaderRType: RType[Header] = GeneralType(HeaderClassTag)
  implicit val PreHeaderRType: RType[PreHeader] = GeneralType(PreHeaderClassTag)

  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(AnyValueClassTag)

  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(SigmaDslBuilderClassTag)

  /** Implicit resolution of `Coll[A]` type descriptor, given a descriptor of `A`. */
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)

  /** Allows implicit resolution to find appropriate instance of ClassTag in
    * the scope where RType is implicitly available.
    * NOTE, this implicit is shadowed internally in some places (search for `def rtypeToClassTag`),
    * hence renaming it requires changing of the shadowing methods.
    */
  implicit def rtypeToClassTag[A](implicit t: RType[A]): ClassTag[A] = t.classTag

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(CollBuilderClassTag)

  private def sameLengthErrorMsg[A, B](xs: Coll[A], ys: Coll[B]) =
    s"Collections should have same length but was ${xs.length} and ${ys.length}:\n xs=$xs;\n ys=$ys"

  def requireSameLength[A, B](xs: Coll[A], ys: Coll[B]) = {
    require(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  /** Generic representation of tuples values. */
  type TupleData = Coll[Any]

  /** Returns an RType object representing a tuple type with the given SomeType array types.
    *
    * @param types An array of SomeType representing the types of each item in the tuple.
    * @return An RType object for the tuple type.
    */
  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)

  /** The primary reference to global Coll operations. Can be used to create collections from Array etc.
    *
    * @see CollBuilder
    */
  val Colls: CollBuilder = new CollOverArrayBuilder
}