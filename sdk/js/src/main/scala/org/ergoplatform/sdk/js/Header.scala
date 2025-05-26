package org.ergoplatform.sdk.js

import sigma.js.{AvlTree, GroupElement}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.Header]] available from JS.
  * Represents data of the block header available in Sigma propositions.
  */
@JSExportTopLevel("Header")
class Header(
    /** Hex representation of ModifierId of this Header */
    val id: String,
    /** Block version, to be increased on every soft and hardfork. */
    val version: Byte,
    /** Hex representation of ModifierId of the parent block */
    val parentId: String,
    /** Hex hash of ADProofs for transactions in a block */
    val ADProofsRoot: String,
    /** AvlTree of a state after block application */
    val stateRoot: AvlTree,
    /** Hex of root hash (for a Merkle tree) of transactions in a block. */
    val transactionsRoot: String,
    /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
    val timestamp: js.BigInt,
    /** Current difficulty in a compressed view.
      * NOTE: actually it is unsigned Int */
    val nBits: js.BigInt,
    /** Block height */
    val height: Int,
    /** Hex of root hash of extension section */
    val extensionRoot: String,

    /** Miner public key (hex of EC Point). Should be used to collect block rewards.
      * Part of Autolykos solution.
      */
    val minerPk: GroupElement,

    /** One-time public key (hex of EC Point). Prevents revealing of miners secret. */
    val powOnetimePk: GroupElement,

    /** Hex of nonce bytes */
    val powNonce: String,

    /** Distance between pseudo-random number, corresponding to nonce `powNonce` and a secret,
      * corresponding to `minerPk`. The lower `powDistance` is, the harder it was to find this solution. */
    val powDistance: js.BigInt,

    /** Miner votes for changing system parameters. */
    val votes: String
) extends js.Object
