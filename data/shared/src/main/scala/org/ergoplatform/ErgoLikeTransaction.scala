package org.ergoplatform

import debox.cfor
import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import scorex.util._
import sigma.Colls
import sigma.ast.syntax.ErgoBoxCandidateRType
import sigma.data.Digest32Coll
import sigma.eval.Extensions.EvalIterableOps
import sigma.interpreter.ProverResult
import sigma.util.safeNewArray
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

import scala.util.Try

trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}

/** Base trait of a real transaction to be used in Ergo network.
  * May be in unsigned (`UnsignedErgoLikeTransaction`) or in signed (`ErgoLikeTransaction`) version.
  */
trait ErgoLikeTransactionTemplate[IT <: UnsignedInput] {
  /** Inputs, that are not going to be spent by transaction, but will be
    * reachable from inputs scripts. `dataInputs` scripts will not be executed,
    * thus their scripts costs are not included in transaction cost and
    * they do not contain spending proofs.
    */
  val dataInputs: IndexedSeq[DataInput]

  /** Inputs, that will be spent by this transaction. */
  val inputs: IndexedSeq[IT]

  /** Box candidates to be created by this transaction.
    * Differ from ordinary ones in that they do not include transaction id and index.
    */
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  /** Identifier of this transaction as state Modifier. */
  val id: ModifierId

  lazy val outputs: IndexedSeq[ErgoBox] =
    outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] = ErgoLikeTransaction.bytesToSign(this)

  lazy val inputIds: IndexedSeq[ADKey] = inputs.map(_.boxId)

}


/**
  * Unsigned version of `ErgoLikeTransactionTemplate`
  */
class UnsignedErgoLikeTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                  override val dataInputs: IndexedSeq[DataInput],
                                  override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[UnsignedInput] {

  override lazy val id: ModifierId = Blake2b256.hash(messageToSign).toModifierId

  def toSigned(proofs: IndexedSeq[ProverResult]): ErgoLikeTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map { case (ui, proof) => Input(ui.boxId, proof) }
    new ErgoLikeTransaction(ins, dataInputs, outputCandidates)
  }

  override def equals(obj: Any): Boolean = obj match {
    case tx: UnsignedErgoLikeTransaction => this.id == tx.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()

  override def toString = s"UnsignedErgoLikeTransaction(dataInputs=$dataInputs, inputs=$inputs, outputCandidates=$outputCandidates)"
}

object UnsignedErgoLikeTransaction {
  def apply(inputs: IndexedSeq[UnsignedInput], dataInputs: IndexedSeq[DataInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, dataInputs, outputCandidates)

  def apply(inputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)
}

/**
  * Signed version of `ErgoLikeTransactionTemplate`
  */
class ErgoLikeTransaction(override val inputs: IndexedSeq[Input],
                          override val dataInputs: IndexedSeq[DataInput],
                          override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[Input] {

  require(outputCandidates.length <= Short.MaxValue, s"${Short.MaxValue} is the maximum number of outputs")

  override lazy val id: ModifierId = Blake2b256.hash(messageToSign).toModifierId

  override def equals(obj: Any): Boolean = obj match {
    //we're ignoring spending proofs here
    case tx: ErgoLikeTransaction => this.id == tx.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
  override def toString = s"ErgoLikeTransaction(dataInputs=$dataInputs, inputs=$inputs, outputCandidates=$outputCandidates)"
}

object ErgoLikeTransactionSerializer extends SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] {

  override def serialize(tx: ErgoLikeTransaction, w: SigmaByteWriter): Unit = {
    // serialize transaction inputs
    w.putUShort(tx.inputs.length)
    for (input <- tx.inputs) {
      Input.serializer.serialize(input, w)
    }
    // serialize transaction data inputs
    w.putUShort(tx.dataInputs.length)
    for (input <- tx.dataInputs) {
      w.putBytes(input.boxId)
    }
    // Serialize distinct ids of tokens in transaction outputs.
    val tokenIds = tx.outputCandidates.toColl
      .flatMap(box => box.additionalTokens.map(t => t._1))

    val distinctTokenIds = tokenIds.distinct // rely on equality of Coll

    w.putUInt(distinctTokenIds.length)
    cfor(0)(_ < distinctTokenIds.length, _ + 1) { i =>
      val tokenId = distinctTokenIds(i)
      w.putBytes(tokenId.toArray)
    }
    // serialize outputs
    val outs = tx.outputCandidates
    w.putUShort(outs.length)
    cfor(0)(_ < outs.length, _ + 1) { i =>
      val out = outs(i)
      ErgoBoxCandidate.serializer.serializeBodyWithIndexedDigests(out, Some(distinctTokenIds), w)
    }
  }

  /** HOTSPOT: don't beautify the code */
  override def parse(r: SigmaByteReader): ErgoLikeTransaction = {
    // parse transaction inputs
    val inputsCount = r.getUShort()
    val inputs = safeNewArray[Input](inputsCount)
    cfor(0)(_ < inputsCount, _ + 1) { i =>
      inputs(i) = Input.serializer.parse(r)
    }

    // parse transaction data inputs
    val dataInputsCount = r.getUShort()
    val dataInputs = safeNewArray[DataInput](dataInputsCount)
    cfor(0)(_ < dataInputsCount, _ + 1) { i =>
      dataInputs(i) = DataInput(ADKey @@ r.getBytes(ErgoBox.BoxId.size))
    }

    // parse distinct ids of tokens in transaction outputs
    val tokensCount = r.getUIntExact
    // NO-FORK: in v5.x getUIntExact may throw Int overflow exception
    // in v4.x r.getUInt().toInt is used and may return negative Int instead of the overflow
    // in which case the array allocation will throw NegativeArraySizeException
    val tokens = safeNewArray[TokenId](tokensCount)
    cfor(0)(_ < tokensCount, _ + 1) { i =>
      tokens(i) = Digest32Coll @@@ Colls.fromArray(r.getBytes(TokenId.size))
    }

    // parse outputs
    val outsCount = r.getUShort()
    val outputCandidates = safeNewArray[ErgoBoxCandidate](outsCount)
    cfor(0)(_ < outsCount, _ + 1) { i =>
      outputCandidates(i) = ErgoBoxCandidate.serializer.parseBodyWithIndexedDigests(tokens, r)
    }

    new ErgoLikeTransaction(inputs, dataInputs, outputCandidates)
  }

}


object ErgoLikeTransaction {

  val TransactionIdBytesSize: Short = 32

  /**
    * Bytes that should be signed by provers.
    * Contains all the transaction bytes except of signatures
    */
  def bytesToSign[IT <: UnsignedInput](tx: ErgoLikeTransactionTemplate[IT]): Array[Byte] = {
    val emptyProofInputs = tx.inputs.map(_.inputToSign)
    val w = SigmaSerializer.startWriter()
    val txWithoutProofs = new ErgoLikeTransaction(emptyProofInputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(txWithoutProofs, w)
    w.toBytes
  }

  /**
    * Creates ErgoLikeTransaction without data inputs
    */
  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)

  // TODO unify serialization approach in Ergo/sigma with BytesSerializable
  val serializer: SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] = ErgoLikeTransactionSerializer
}
