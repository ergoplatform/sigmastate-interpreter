package org.ergoplatform.sdk

import org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext
import sigma.Coll
import sigma.Header

/** Represents a specific context of blockchain for execution
  * of transaction building scenario.
  * It contains methods for accessing blockchain data, current blockchain state,
  * node information etc.
  * An instance of this class can also be used to create new builders
  * for creating new transactions and provers (used for transaction signing).
  */
case class BlockchainContext(
    networkType: NetworkType,
    parameters: BlockchainParameters,
    stateContext: BlockchainStateContext
) {
  def headers: Coll[Header] = stateContext.sigmaLastHeaders

  def height: Int = headers(0).height
}
