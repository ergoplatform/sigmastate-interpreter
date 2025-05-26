package org.ergoplatform.sdk

import org.ergoplatform.sdk.JavaHelpers.StringExtensions
import org.ergoplatform.{ErgoBox, ErgoLikeTransaction, UnsignedErgoLikeTransaction}
import sigmastate.eval.Extensions.ArrayByteOps
import sigma.serialization.SigmaSerializer

/** Represents a transaction data chat can be reduced to [[ReducedTransaction]].
  *
  * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
  *
  * @param unsignedTx   original unsigned transaction to be reduced (holds messageToSign)
  * @param boxesToSpend input boxes of the transaction
  * @param dataInputs   data inputs of the transaction
  * @param tokensToBurn requested tokens to be burnt in the transaction, if empty no burning allowed
  */
case class UnreducedTransaction(
    unsignedTx: UnsignedErgoLikeTransaction,
    boxesToSpend: IndexedSeq[ExtendedInputBox],
    dataInputs: IndexedSeq[ErgoBox],
    tokensToBurn: IndexedSeq[ErgoToken]
) {
  require(unsignedTx.inputs.length == boxesToSpend.length, "Not enough boxes to spend")
  require(unsignedTx.dataInputs.length == dataInputs.length, "Not enough data boxes")
  checkSameIds(
    "unsignedTx.inputs", unsignedTx.inputs.map(_.boxId),
    "boxesToSpend", boxesToSpend.map(_.box.id),
    "boxesToSpend should have the same box ids as unsignedTx.inputs")
  checkSameIds(
    "unsignedTx.dataInputs", unsignedTx.dataInputs.map(_.boxId),
    "dataInputs", dataInputs.map(_.id),
    "dataInputs should have the same box ids as unsignedTx.dataInputs")

  private def checkSameIds(
      xsName: String, xs: Seq[Array[Byte]],
      ysName: String, ys: Seq[Array[Byte]], msg: => String) = {
    require(xs.zip(ys).forall { case (id1, id2) => java.util.Arrays.equals(id1, id2) }, {
      val xsOnly = xs.diff(ys)
      val ysOnly = ys.diff(xs)
      s"""$msg:
        | only in $xsName: $xsOnly
        | only in $ysName: $ysOnly
        |""".stripMargin
    })
  }
}

/** Represents results for transaction reduction by [[ReducingInterpreter]]. */
case class ReducedTransaction(ergoTx: ReducedErgoLikeTransaction) {
  /** Serialized bytes of this transaction in hex format. */
  def toHex: String = {
    val w = SigmaSerializer.startWriter()
    ReducedErgoLikeTransactionSerializer.serialize(ergoTx, w)
    w.toBytes.toHex
  }
}

object ReducedTransaction {
  /** Creates a [[ReducedTransaction]] from serialized bytes in hex format. */
  def fromHex(hex: String): ReducedTransaction = {
    val r = SigmaSerializer.startReader(hex.toBytes)
    val tx = ReducedErgoLikeTransactionSerializer.parse(r)
    ReducedTransaction(tx)
  }
}

/** Represents results for transaction signing by a prover like [[SigmaProver]]. */
case class SignedTransaction(ergoTx: ErgoLikeTransaction, cost: Int)


