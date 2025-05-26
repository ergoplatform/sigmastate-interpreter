package org.ergoplatform

import org.ergoplatform.ErgoBox.{AdditionalRegisters, Token}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.util._
import scorex.util.encode.Base16
import scorex.utils.{Ints, Shorts}
import sigma.Extensions.ArrayOps
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.AnyOps
import sigma.data.{Digest32Coll, SigmaConstants}
import sigma.ast._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}
import sigma._
import sigma.util.CollectionUtil

/**
  * Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency. In Bitcoin, such an object
  * is associated with some monetary value (arbitrary, but with predefined precision, so we use integer arithmetic to
  * work with the value), and also a guarding script (aka proposition) to protect the box from unauthorized opening.
  *
  * In other way, a box is a state element locked by some proposition (ErgoTree).
  *
  * In Ergo, box is just a collection of registers, some with mandatory types and semantics, others could be used by
  * applications in any way.
  * We add additional fields in addition to amount and proposition~(which stored in the registers R0 and R1). Namely,
  * register R2 contains additional tokens (a sequence of pairs (token identifier, value)). Register R3 contains height
  * when block got included into the blockchain and also transaction identifier and box index in the transaction outputs.
  * Registers R4-R9 are free for arbitrary usage.
  *
  *
  * A transaction is unsealing a box. As a box can not be open twice, any further valid transaction can not be linked
  * to the same box.
  *
  * Note, private constructor can only be used from within the ErgoBox companion object, e.g. by deserializer.
  *
  * @param value               - amount of money associated with the box
  * @param ergoTree            - guarding script, which should be evaluated to true in order to open this box
  * @param additionalTokens    - secondary tokens the box contains
  * @param additionalRegisters - additional registers the box can carry over
  * @param transactionId       - id of transaction which created the box
  * @param index               - index of the box (from 0 to total number of boxes the transaction with transactionId created - 1)
  * @param creationHeight      - height when a transaction containing the box was created.
  *                            This height is declared by user and should not exceed height of the block,
  *                            containing the transaction with this box.
  * @param _bytes              - serialized bytes of the box when not `null`
  * HOTSPOT: don't beautify the code of this class
  */
class ErgoBox private (
         override val value: Long,
         override val ergoTree: ErgoTree,
         override val additionalTokens: Coll[Token],
         override val additionalRegisters: AdditionalRegisters,
         val transactionId: ModifierId,
         val index: Short,
         override val creationHeight: Int,
         _bytes: Array[Byte]
       ) extends ErgoBoxCandidate(value, ergoTree, creationHeight, additionalTokens, additionalRegisters) {
  /** This is public constructor has the same parameters as the private primary constructor, except bytes. */
  def this(value: Long,
           ergoTree: ErgoTree,
           additionalTokens: Coll[Token] = Colls.emptyColl[Token],
           additionalRegisters: AdditionalRegisters = Map.empty,
           transactionId: ModifierId,
           index: Short,
           creationHeight: Int) =
    this(value, ergoTree, additionalTokens, additionalRegisters, transactionId, index, creationHeight, null)

  import ErgoBox._

  /** Blake2b256 hash of the serialized `bytes`. */
  lazy val id: BoxId = ADKey @@@ Blake2b256.hash(bytes)

  override def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ReferenceRegId =>
        val tupleVal = (creationHeight, CollectionUtil.concatArrays(transactionId.toBytes, Shorts.toByteArray(index)).toColl)
        Some(Constant(tupleVal.asWrappedType, SReferenceRegType))
      case _ => super.get(identifier)
    }
  }

  /** Serialized content of this box.
    * @see [[ErgoBox.sigmaSerializer]]
    */
  lazy val bytes: Array[Byte] = {
    if (_bytes != null)
      _bytes // bytes provided by deserializer
    else
      ErgoBox.sigmaSerializer.toBytes(this)
  }

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBox => java.util.Arrays.equals(id, x.id)
    case _ => false
  }

  override def hashCode(): Int = Ints.fromByteArray(id)

  /** Convert this box to [[ErgoBoxCandidate]] by forgetting transaction reference data
   * (transactionId, index).
   */
  def toCandidate: ErgoBoxCandidate =
    new ErgoBoxCandidate(value, ergoTree, creationHeight, additionalTokens, additionalRegisters)

  override def toString: String = s"ErgoBox(${Base16.encode(id)},$value,$ergoTree," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1.toArray) + ":" + t._2)}), $transactionId, " +
    s"$index, $additionalRegisters, $creationHeight)"
}

object ErgoBox {
  type BoxId = ADKey
  object BoxId {
    /** Size in bytes of the box identifier. */
    val size: Short = 32
  }

  /** Token id is tagged collection of bytes. */
  type TokenId = Digest32Coll
  object TokenId {
    val size: Short = 32
  }
  /** Helper synonym for a token with a value attached. */
  type Token = (Digest32Coll, Long)

  val MaxBoxSize: Int = SigmaConstants.MaxBoxSize.value

  val STokenType = STuple(SByteArray, SLong)
  val STokensRegType = SCollection(STokenType)
  val SReferenceRegType: STuple = ExtractCreationInfo.ResultType

  type Amount = Long

  /** Represents id of a [[ErgoBox]] register. */
  sealed trait RegisterId {
    /** Zero-based register index in [0, 9] range. */
    val number: Byte

    /** Returns zero-based register index in [0, 9] range. */
    def asIndex: Int = number.toInt

    override def toString: String = "R" + number
  }

  /** Represents id of pre-defined mandatory registers of a box. */
  sealed abstract class MandatoryRegisterId(override val number: Byte, val purpose: String) extends RegisterId

  /** Represents id of optional registers of a box. */
  sealed abstract class NonMandatoryRegisterId(override val number: Byte) extends RegisterId

  type AdditionalRegisters = scala.collection.Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]

  object R0 extends MandatoryRegisterId(0, "Monetary value, in Ergo tokens")
  object R1 extends MandatoryRegisterId(1, "Guarding script")
  object R2 extends MandatoryRegisterId(2, "Secondary tokens")
  object R3 extends MandatoryRegisterId(3, "Reference to transaction and output id where the box was created")
  object R4 extends NonMandatoryRegisterId(4)
  object R5 extends NonMandatoryRegisterId(5)
  object R6 extends NonMandatoryRegisterId(6)
  object R7 extends NonMandatoryRegisterId(7)
  object R8 extends NonMandatoryRegisterId(8)
  object R9 extends NonMandatoryRegisterId(9)

  val ValueRegId: MandatoryRegisterId = R0
  val ScriptRegId: MandatoryRegisterId = R1
  val TokensRegId: MandatoryRegisterId = R2
  val ReferenceRegId: MandatoryRegisterId = R3

  val MaxTokens: Int = SigmaConstants.MaxTokens.value

  val maxRegisters: Int = SigmaConstants.MaxRegisters.value

  /** HOTSPOT: don't beautify the code in this companion */
  private val _mandatoryRegisters: Array[MandatoryRegisterId] = Array(R0, R1, R2, R3)
  val mandatoryRegisters: Seq[MandatoryRegisterId] = _mandatoryRegisters

  private val _nonMandatoryRegisters: Array[NonMandatoryRegisterId] = Array(R4, R5, R6, R7, R8, R9)
  val nonMandatoryRegisters: Seq[NonMandatoryRegisterId] = _nonMandatoryRegisters

  val startingNonMandatoryIndex: Byte = nonMandatoryRegisters.head.number
    .ensuring(_ == mandatoryRegisters.last.number + 1)

  val allRegisters: Seq[RegisterId] =
    CollectionUtil.concatArrays[RegisterId](
      CollectionUtil.castArray(_mandatoryRegisters): Array[RegisterId],
      CollectionUtil.castArray(_nonMandatoryRegisters): Array[RegisterId]).ensuring(_.length == maxRegisters)

  val mandatoryRegistersCount: Byte = mandatoryRegisters.size.toByte
  val nonMandatoryRegistersCount: Byte = nonMandatoryRegisters.size.toByte

  val registerByName: Map[String, RegisterId] = allRegisters.map(r => s"R${r.number}" -> r).toMap

  /** HOTSPOT: called from ErgoBox serializer */
  @inline final def registerByIndex(index: Int): RegisterId = allRegisters(index)

  def findRegisterByIndex(i: Int): Option[RegisterId] =
    if (0 <= i && i < maxRegisters) Some(registerByIndex(i)) else None

  val allZerosModifierId: ModifierId = Array.fill[Byte](32)(0.toByte).toModifierId

  object sigmaSerializer extends SigmaSerializer[ErgoBox, ErgoBox] {

    override def serialize(obj: ErgoBox, w: SigmaByteWriter): Unit = {
      ErgoBoxCandidate.serializer.serialize(obj, w)
      val txIdBytes = obj.transactionId.toBytes
      val txIdBytesSize = txIdBytes.length
      assert(txIdBytesSize == ErgoLikeTransaction.TransactionIdBytesSize,
        s"Invalid transaction id size: $txIdBytesSize (expected ${ErgoLikeTransaction.TransactionIdBytesSize})")
      w.putBytes(txIdBytes)
      w.putUShort(obj.index)
    }

    override def parse(r: SigmaByteReader): ErgoBox = {
      val start = r.position
      val c = ErgoBoxCandidate.serializer.parse(r)
      val transactionId = r.getBytes(ErgoLikeTransaction.TransactionIdBytesSize).toModifierId
      val index = r.getUShort()
      val end = r.position
      val len = end - start
      r.position = start
      val boxBytes = r.getBytes(len) // also moves position back to end
      new ErgoBox(c.value, c.ergoTree, c.additionalTokens, c.additionalRegisters,
        transactionId, index.toShort, c.creationHeight, boxBytes)
    }
  }
}
