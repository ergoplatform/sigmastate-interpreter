package org.ergoplatform.sdk

import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, UnsignedInput}
import scorex.util.ModifierId
import sigma.interpreter.ContextExtension

/** Input ErgoBox paired with context variables (aka ContextExtensions).
 *
 * @param box       an instance of ErgoBox which is used as an input of the transaction.
 * @param extension a set of context variables necessary to satisfy the box's
 *                  guarding proposition.
 *                  This extension is also saved in the corresponding
 *                  [[org.ergoplatform.Input]] instance of the signed transaction.
 */
case class ExtendedInputBox(
  box: ErgoBox,
  extension: ContextExtension
) {
  def toUnsignedInput: UnsignedInput = new UnsignedInput(box.id, extension)
  def value: Long = box.value
}

case class OutBox(candidate: ErgoBoxCandidate) {
  /**
    * Converts this box candidate into a new instance of {@link ExtendedInputBox} by
    * associating it with the given transaction and output position.
    * This method can be used to create input boxed from scratch, without
    * retrieving them from the UTXOs. Thus created boxes can be indistinguishable from those
    * loaded from blockchain node, and as result can be used to create new transactions.
    * This method can also be used to create chains of transactions in advance
    *
    * @param txId        the id of the transaction of which created the box which will be returned
    * @param outputIndex zero-based position (index) of the box in the outputs of the transaction.
    * @return a new {@link ExtendedInputBox} representing UTXO box as an input of a next transaction.
    */
  def convertToInputWith(txId: String, boxIndex: Short): ExtendedInputBox = {
    val box = candidate.toBox(ModifierId @@ txId, boxIndex)
    ExtendedInputBox(box, ContextExtension.empty)
  }
}

