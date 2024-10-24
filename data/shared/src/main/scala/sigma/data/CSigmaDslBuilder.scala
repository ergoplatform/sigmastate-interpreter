package sigma.data

import debox.cfor
import org.ergoplatform.ErgoBox
import org.ergoplatform.validation.ValidationRules
import scorex.crypto.hash.{Blake2b256, Sha256}
import scorex.utils.{Ints, Longs}
import sigma.ast.{AtLeast, SBigInt, SubstConstants}
import scorex.utils.Longs
import sigma.ast.{AtLeast, SType, SubstConstants}
import sigma.crypto.{CryptoConstants, EcPointType, Ecp}
import sigma.eval.Extensions.EvalCollOps
import sigma.serialization.{DataSerializer, GroupElementSerializer, SigmaSerializer}
import sigma.serialization.{GroupElementSerializer, SerializerException, SigmaSerializer}
import sigma.util.Extensions.BigIntegerOps
import sigma.validation.SigmaValidationSettings
import sigma.{AvlTree, BigInt, Box, Coll, CollBuilder, Evaluation, GroupElement, SigmaDslBuilder, SigmaProp, VersionContext}

import java.math.BigInteger

/** A default implementation of [[SigmaDslBuilder]] interface.
  *
  * @see [[SigmaDslBuilder]] for detailed descriptions
  */
class CSigmaDslBuilder extends SigmaDslBuilder { dsl =>
  implicit val validationSettings: SigmaValidationSettings = ValidationRules.currentSettings

  override val Colls: CollBuilder = sigma.Colls

  override def BigInt(n: BigInteger): BigInt = CBigInt(n)

  override def toBigInteger(n: BigInt): BigInteger = n.asInstanceOf[CBigInt].wrappedValue

  /** Wraps the given elliptic curve point into GroupElement type. */
  def GroupElement(p: Ecp): GroupElement = p match {
    case ept: EcPointType => CGroupElement(ept)
    case m => sys.error(s"Point of type ${m.getClass} is not supported")
  }

  /** Wraps the given sigma proposition into SigmaDsl value of type SigmaProp. */
  def SigmaProp(sigmaTree: SigmaBoolean): SigmaProp = new CSigmaProp(sigmaTree)

  /** Extract `sigma.data.SigmaBoolean` from DSL's `SigmaProp` type. */
  @inline def toSigmaBoolean(p: SigmaProp): SigmaBoolean = p.asInstanceOf[CSigmaProp].sigmaTree

  /** Extract `sigmastate.AvlTreeData` from DSL's `AvlTree` type. */
  def toAvlTreeData(p: AvlTree): AvlTreeData = p.asInstanceOf[CAvlTree].treeData

  /** Extract `sigmastate.crypto.Ecp` from DSL's `GroupElement` type. */
  def toECPoint(ge: GroupElement): Ecp = ge.asInstanceOf[CGroupElement].wrappedValue

  /** Creates a new AvlTree instance with the given parameters.
    *
    * @see AvlTreeData for details
    */
  override def avlTree(
      operationFlags: Byte,
      digest: Coll[Byte],
      keyLength: Int,
      valueLengthOpt: Option[Int]): CAvlTree = {
    val treeData = AvlTreeData(digest, AvlTreeFlags(operationFlags), keyLength, valueLengthOpt)
    CAvlTree(treeData)
  }

  /** Wraps the given tree data into SigmaDsl value of type [[AvlTree]]. */
  def avlTree(treeData: AvlTreeData): AvlTree = {
    CAvlTree(treeData)
  }

  /** Wraps the given [[ErgoBox]] into SigmaDsl value of type [[Box]].
    *
    * @param ebox the value to be wrapped
    * @see [[sigmastate.SBox]], [[sigma.Box]]
    */
  def Box(ebox: ErgoBox): Box = CBox(ebox)

  /** Extracts [[ErgoBox]] from the given [[Box]] instance. This is inverse to the Box method. */
  def toErgoBox(b: Box): ErgoBox = b.asInstanceOf[CBox].ebox

  /** HOTSPOT: don't beautify this code */
  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    val len = props.length
    val res = new Array[SigmaBoolean](len)
    cfor(0)(_ < len, _ + 1) { i =>
      res(i) = toSigmaBoolean(props(i))
    }
    res
  }

  @inline private def toEcPointType(ge: GroupElement): EcPointType =
    toECPoint(ge).asInstanceOf[EcPointType]

  override def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = {
    if (props.length > AtLeast.MaxChildrenCount)
      throw new IllegalArgumentException(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: ${props.length}")
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree       = AtLeast.reduce(bound, sigmaTrees)
    CSigmaProp(tree)
  }

  override def allOf(conditions: Coll[Boolean]): Boolean =
    conditions.forall(c => c)

  override def anyOf(conditions: Coll[Boolean]): Boolean =
    conditions.exists(c => c)

  override def xorOf(conditions: Coll[Boolean]): Boolean = {
    if (VersionContext.current.isJitActivated) {
      val len = conditions.length
      if (len == 0) false
      else if (len == 1) conditions(0)
      else {
        var res = conditions(0)
        cfor(1)(_ < len, _ + 1) { i =>
          res ^= conditions(i)
        }
        res
      }
    } else {
      // This is buggy version used in v4.x interpreter (for ErgoTrees v0, v1)
      conditions.toArray.distinct.length == 2
    }
  }

  override def allZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree       = CAND.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def anyZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree       = COR.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte] =
    Colls.xor(l, r)

  override def sigmaProp(b: Boolean): SigmaProp = {
    CSigmaProp(TrivialProp(b))
  }

  override def blake2b256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Blake2b256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def sha256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Sha256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = {
    val bi = new BigInteger(bytes.toArray).to256BitValueExact
    this.BigInt(bi)
  }

  override def longToByteArray(l: Long): Coll[Byte] =
    Colls.fromArray(Longs.toByteArray(l))

  override def byteArrayToLong(bytes: Coll[Byte]): Long =
    Longs.fromByteArray(bytes.toArray)

  override def proveDlog(ge: GroupElement): SigmaProp =
    CSigmaProp(ProveDlog(toECPoint(ge).asInstanceOf[EcPointType]))

  override def proveDHTuple(
      g: GroupElement,
      h: GroupElement,
      u: GroupElement,
      v: GroupElement): SigmaProp = {
    val dht = ProveDHTuple(toEcPointType(g), toEcPointType(h), toEcPointType(u), toEcPointType(v))
    CSigmaProp(dht)
  }

  private lazy val _generatorElement = this.GroupElement(CryptoConstants.dlogGroup.generator)

  override def groupGenerator: GroupElement = _generatorElement

  /**
    * @return the identity of the Dlog group used in ErgoTree
    */
  def groupIdentity: GroupElement = {
    this.GroupElement(CryptoConstants.dlogGroup.identity)
  }

  override def substConstants[T](
      scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T]): Coll[Byte] = {
    val constants = try newValues.toArrayOfConstants
    catch {
      case e: Throwable =>
        throw new RuntimeException(s"Cannot evaluate substConstants($scriptBytes, $positions, $newValues)", e)
    }
    val (res, _)  = SubstConstants.eval(scriptBytes.toArray, positions.toArray, constants)(validationSettings)
    Colls.fromArray(res)
  }

  override def decodePoint(encoded: Coll[Byte]): GroupElement = {
    val r = SigmaSerializer.startReader(encoded.toArray)
    val p = GroupElementSerializer.parse(r)
    this.GroupElement(p)
  }

  override def fromBigEndianBytes[T](bytes: Coll[Byte])(implicit cT: RType[T]): T = {
    cT match {
      case sigma.ByteType => if (bytes.length != 1) {
        throw new IllegalArgumentException("To deserialize SByte with fromBigEndianBytes, exactly one byte should be provided")
      } else {
        bytes.apply(0).asInstanceOf[T]
      }
      case sigma.ShortType => if (bytes.length != 2) {
        throw new IllegalArgumentException("To deserialize SShort with fromBigEndianBytes, exactly two bytes should be provided")
      } else {
        val b0 = bytes(0)
        val b1 = bytes(1)
        ((b0 & 0xFF) << 8 | (b1 & 0xFF)).toShort.asInstanceOf[T]
      }
      case sigma.IntType => if (bytes.length != 4) {
        throw new IllegalArgumentException("To deserialize SInt with fromBigEndianBytes, exactly four bytes should be provided")
      } else {
        Ints.fromByteArray(bytes.toArray).asInstanceOf[T]
      }
      case sigma.LongType => if (bytes.length != 8) {
        throw new IllegalArgumentException("To deserialize SLong with fromBigEndianBytes, exactly eight bytes should be provided")
      } else {
        Longs.fromByteArray(bytes.toArray).asInstanceOf[T]
      }
      case sigma.BigIntRType =>
        if (bytes.length > SBigInt.MaxSizeInBytes) {
          throw SerializerException(s"BigInt value doesn't not fit into ${SBigInt.MaxSizeInBytes} bytes in fromBigEndianBytes")
        }
        CBigInt(new BigInteger(bytes.toArray).to256BitValueExact).asInstanceOf[T]
      // todo: UnsignedBitInt
      case _ => throw new IllegalArgumentException("Unsupported type provided in fromBigEndianBytes")
    }
  }

  /** Serializes the given `value` into bytes using the default serialization format. */
  override def serialize[T](value: T)(implicit cT: RType[T]): Coll[Byte] = {
    val tpe = Evaluation.rtypeToSType(cT)
    val w = SigmaSerializer.startWriter()
    DataSerializer.serialize(value.asInstanceOf[SType#WrappedType], tpe, w)
    Colls.fromArray(w.toBytes)
  }

  override def some[T](value: T)(implicit cT: RType[T]): Option[T] = {
    Some(value)
  }

  override def none[T]()(implicit cT: RType[T]): Option[T] = {
    None
  }
}

/** Default singleton instance of Global object, which implements global ErgoTree functions. */
object CSigmaDslBuilder extends CSigmaDslBuilder