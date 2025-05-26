package org.ergoplatform.sdk.wallet.protocol.context

import sigma.Coll

/** Blockchain context used in tx signing. */
abstract class BlockchainStateContext {
  /** Fixed number (10 in Ergo) of last block headers. */
  def sigmaLastHeaders: Coll[sigma.Header]
  /** UTXO set digest from a last header (of sigmaLastHeaders). */
  def previousStateDigest: Coll[Byte]
  /** Returns pre-header (header without certain fields) of the current block. */
  def sigmaPreHeader: sigma.PreHeader
}

/** Blockchain context used in tx signing. */
case class CBlockchainStateContext(
  sigmaLastHeaders: Coll[sigma.Header],
  previousStateDigest: Coll[Byte],
  sigmaPreHeader: sigma.PreHeader
) extends BlockchainStateContext
