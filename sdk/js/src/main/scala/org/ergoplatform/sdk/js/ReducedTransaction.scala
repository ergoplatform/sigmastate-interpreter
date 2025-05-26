package org.ergoplatform.sdk.js

import org.ergoplatform.sdk

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sdk.ReducedTransaction]] available from JS. */
@JSExportTopLevel("ReducedTransaction")
class ReducedTransaction(val _tx: sdk.ReducedTransaction) extends js.Object {
  /** Serialized bytes of this transaction in hex format. */
  def toHex(): String = _tx.toHex
}

@JSExportTopLevel("ReducedTransaction$")
object ReducedTransaction extends js.Object {
  /** Creates a [[ReducedTransaction]] from serialized bytes in hex format. */
  def fromHex(hex: String): ReducedTransaction = {
    val tx = sdk.ReducedTransaction.fromHex(hex)
    new ReducedTransaction(tx)
  }
}