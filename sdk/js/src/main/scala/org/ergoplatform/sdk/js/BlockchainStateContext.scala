package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext]] available from JS.
  * @param sigmaLastHeaders    fixed number (10 in Ergo) of last block headers
  * @param previousStateDigest hex of UTXO set digest from a last header (of sigmaLastHeaders)
  * @param sigmaPreHeader      returns pre-header (header without certain fields) of the current block
  */
@JSExportTopLevel("BlockchainStateContext")
class BlockchainStateContext(
    val sigmaLastHeaders: js.Array[Header],
    val previousStateDigest: String,
    val sigmaPreHeader: PreHeader
) extends js.Object
