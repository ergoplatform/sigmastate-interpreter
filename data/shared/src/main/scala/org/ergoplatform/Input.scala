package org.ergoplatform

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}
import sigma.util.CollectionUtil

/**
  * Inputs, that are used to enrich script context, but won't be spent by the transaction
  *
  * @param boxId - id of a box to add into context (should be in UTXO)
  */
case class DataInput(boxId: BoxId) {
  override def toString: String = s"DataInput(${Base16.encode(boxId)})"

  override def equals(obj: Any): Boolean = obj match {
    case x: DataInput => java.util.Arrays.equals(boxId, x.boxId)
    case _ => false
  }

  override def hashCode(): Int = CollectionUtil.deepHashCode(boxId)
}

/**
  * Inputs of formed, but unsigned transaction
  *
  * @param boxId     - id of a box to spent
  * @param extension - user-defined variables to be put into context
  */
class UnsignedInput(val boxId: BoxId, val extension: ContextExtension) {

  def this(boxId: BoxId) = this(boxId, ContextExtension.empty)

  require(boxId.size == BoxId.size, s"incorrect boxId size, expected: $BoxId.size, got: ${boxId.size}")

  // todo check whether it is correct to compare inputs (Input use the same equals) by boxId only?
  override def equals(obj: Any): Boolean = obj match {
    case x: UnsignedInput => java.util.Arrays.equals(boxId, x.boxId)
    case _ => false
  }

  override def hashCode(): Int = CollectionUtil.deepHashCode(boxId)

  /**
    * Input, that should be signed by prover and verified by verifier.
    * Contains all the input data except of signature itself.
    */
  def inputToSign: Input = Input(boxId: BoxId, ProverResult(Array[Byte](), extension))
}

/**
  * Fully signed transaction input
  *
  * @param boxId         - id of a box to spent
  * @param spendingProof - proof of spending correctness
  */
case class Input(override val boxId: BoxId, spendingProof: ProverResult)
  extends UnsignedInput(boxId, spendingProof.extension) {
  override def toString: String = s"Input(${Base16.encode(boxId)},$spendingProof)"
}

object Input {

  object serializer extends SigmaSerializer[Input, Input] {

    override def serialize(obj: Input, w: SigmaByteWriter): Unit = {
      w.putBytes(obj.boxId)
      ProverResult.serializer.serialize(obj.spendingProof, w)
    }

    override def parse(r: SigmaByteReader): Input = {
      val boxId = r.getBytes(BoxId.size)
      val spendingProof = ProverResult.serializer.parse(r)
      Input(ADKey @@ boxId, spendingProof)
    }
  }

}
