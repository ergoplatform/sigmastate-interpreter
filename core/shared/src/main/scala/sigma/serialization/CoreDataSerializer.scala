package sigma.serialization

import debox.cfor
import sigma.ast._
import sigma.data._
import sigma.util.Extensions.{BigIntOps, BigIntegerOps, CoreAvlTreeOps, GroupElementOps, SigmaPropOps}
import sigma.validation.ValidationRules.CheckSerializableTypeCode
import sigma.{Evaluation, _}

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** This works in tandem with ConstantSerializer, if you change one make sure to check the other.*/
class CoreDataSerializer {

  /** Use type descriptor `tpe` to deconstruct type structure and recursively serialize subcomponents.
    * Primitive types are leaves of the type tree, and they are served as basis of recursion.
    * The data value `v` is expected to conform to the type described by `tpe`.
    */
  def serialize[T <: SType](v: T#WrappedType, tpe: T, w: CoreByteWriter): Unit = tpe match {
    case SUnit => // don't need to save anything
    case SBoolean => w.putBoolean(v.asInstanceOf[Boolean])
    case SByte => w.put(v.asInstanceOf[Byte])
    case SShort => w.putShort(v.asInstanceOf[Short])
    case SInt => w.putInt(v.asInstanceOf[Int])
    case SLong => w.putLong(v.asInstanceOf[Long])
    case SString =>
      val bytes = v.asInstanceOf[String].getBytes(StandardCharsets.UTF_8)
      w.putUInt(bytes.length)
      w.putBytes(bytes)
    case SBigInt =>
      val bi = v.asInstanceOf[BigInt].toBigInteger
      require(bi.fitsIn256Bits, s"BigInt value $bi doesn't fit into 256 bits")
      val data = bi.toByteArray
      w.putUShort(data.length)
      w.putBytes(data)
    case SGroupElement =>
      GroupElementSerializer.serialize(v.asInstanceOf[GroupElement].toECPoint, w)
    case SSigmaProp =>
      val p = v.asInstanceOf[SigmaProp]
      SigmaBoolean.serializer.serialize(p.toSigmaBoolean, w)
    case SAvlTree =>
      AvlTreeData.serializer.serialize(v.asInstanceOf[AvlTree].toAvlTreeData, w)
    case tColl: SCollectionType[a] =>
      val coll = v.asInstanceOf[tColl.WrappedType]
      w.putUShort(coll.length)
      tColl.elemType match {
        case SBoolean =>
          w.putBits(coll.asInstanceOf[Coll[Boolean]].toArray)
        case SByte =>
          w.putBytes(coll.asInstanceOf[Coll[Byte]].toArray)
        case _ =>
          val arr = coll.toArray
          cfor(0)(_ < arr.length, _ + 1) { i =>
            val x = arr(i)
            serialize(x, tColl.elemType, w)
          }
      }

    case t: STuple =>
      val arr = Evaluation.fromDslTuple(v, t).asInstanceOf[t.WrappedType]
      val len = arr.length
      assert(arr.length == t.items.length, s"Type $t doesn't correspond to value $arr")
      if (len > 0xFFFF)
        sys.error(s"The tuple length (${arr.length}) is too long (maximum limit is ${0xFFFF}). " +
          "Please shorten the tuple to meet the limit. " +
          "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
      var i = 0
      while (i < arr.length) {
        serialize[SType](arr(i), t.items(i), w)
        i += 1
      }

    // TODO v6.0 (3h): support Option[T] (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/659)
    case _ =>
      CheckSerializableTypeCode(tpe.typeCode)
      throw new SerializerException(s"Don't know how to serialize ($v, $tpe)")
  }

  /** Reads a data value from Reader. The data value bytes is expected to confirm
    * to the type descriptor `tpe`.
    * The data structure depth is limited by r.maxTreeDepth which is
    * SigmaSerializer.MaxTreeDepth by default.
    */
  def deserialize[T <: SType](tpe: T, r: CoreByteReader): T#WrappedType = {
    val depth = r.level
    r.level = depth + 1
    val res = (tpe match {
      case SUnit => ()
      case SBoolean => r.getUByte() != 0
      case SByte => r.getByte()
      case SShort => r.getShort()
      case SInt => r.getInt()
      case SLong => r.getLong()
      case SString =>
        val size = r.getUIntExact
        // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
        // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
        // in which case the getBytes will throw NegativeArraySizeException
        val bytes = r.getBytes(size)
        new String(bytes, StandardCharsets.UTF_8)
      case SBigInt =>
        val size: Short = r.getUShort().toShort
        if (size > SBigInt.MaxSizeInBytes) {
          throw new SerializerException(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes: $size")
        }
        val valueBytes = r.getBytes(size)
        CBigInt(new BigInteger(valueBytes))
      case SGroupElement =>
        CGroupElement(GroupElementSerializer.parse(r))
      case SSigmaProp =>
        CSigmaProp(SigmaBoolean.serializer.parse(r))
      case SAvlTree =>
        CAvlTree(AvlTreeData.serializer.parse(r))
      case tColl: SCollectionType[a] =>
        val len = r.getUShort()
        deserializeColl(len, tColl.elemType, r)
      case tuple: STuple =>
        val arr = tuple.items.map { t =>
          deserialize(t, r)
        }.toArray[Any]
        val coll = Colls.fromArray(arr)(sigma.AnyType)
        Evaluation.toDslTuple(coll, tuple)
      case t =>
        CheckSerializableTypeCode(t.typeCode)
        throw new SerializerException(s"Not defined DataSerializer for type $t")
    }).asInstanceOf[T#WrappedType]
    r.level = r.level - 1
    res
  }

  def deserializeColl[T <: SType](len: Int, tpeElem: T, r: CoreByteReader): Coll[T#WrappedType] =
    tpeElem match {
      case SBoolean =>
        Colls.fromArray(r.getBits(len)).asInstanceOf[Coll[T#WrappedType]]
      case SByte =>
        Colls.fromArray(r.getBytes(len)).asInstanceOf[Coll[T#WrappedType]]
      case _ =>
        implicit val tItem = (tpeElem match {
          case tTup: STuple if tTup.items.length == 2 =>
            Evaluation.stypeToRType(tpeElem)
          case _: STuple =>
            collRType(sigma.AnyType)
          case _ =>
            Evaluation.stypeToRType(tpeElem)
        }).asInstanceOf[RType[T#WrappedType]]
        val b = { // this code works both for Scala 2.12 and 2.13
          implicit val ct = tItem.classTag
          mutable.ArrayBuilder.make[T#WrappedType]
        }
        for (_ <- 0 until len) {
          b += deserialize(tpeElem, r)
        }
        Colls.fromArray(b.result())
    }
}

object CoreDataSerializer extends CoreDataSerializer