package org.ergoplatform.sdk.js

import sigma.js.GroupElement

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.PreHeader]] available from JS.
  * Only header fields that can be predicted by a miner.
  */
@JSExportTopLevel("PreHeader")
class PreHeader(
    /** Block version, to be increased on every soft and hardfork. */
    val version: Byte,
    /** Hex of id of parent block */
    val parentId: String,
    /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
    val timestamp: js.BigInt,

    /** Current difficulty in a compressed view.
      * NOTE: actually it is unsigned integer */
    val nBits: js.BigInt,
    /** Block height */
    val height: Int,
    /** Miner public key (hex of EC Point). Should be used to collect block rewards. */
    val minerPk: GroupElement,
    /** Hex of miner votes bytes for changing system parameters. */
    val votes: String
) extends js.Object
