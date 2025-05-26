package org.ergoplatform.sdk

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix

/**
  * Enumeration of network types as they are defined by Ergo specification of {@link ErgoAddress}.
  */
abstract class NetworkType {

  /**
    * The network prefix code used in Ergo addresses
    */
  val networkPrefix: NetworkPrefix

  /**
    * verbose name for network type as reported by Node API
    */
  val verboseName: String
}

object NetworkType {
  /** Mainnet network type.
    *
    * @see ErgoAddressEncoder#MainnetNetworkPrefix()
    */
  case object Mainnet extends NetworkType {
    override val networkPrefix: NetworkPrefix = ErgoAddressEncoder.MainnetNetworkPrefix
    override val verboseName = "mainnet"
  }

  /** Testnet network type.
    *
    * @see ErgoAddressEncoder#TestnetNetworkPrefix()
    */
  case object Testnet extends NetworkType {
    override val networkPrefix: NetworkPrefix = ErgoAddressEncoder.TestnetNetworkPrefix
    override val verboseName = "testnet"
  }

  /** @return network type for given verbose name */
  def fromName(name: String): Option[NetworkType] = name match {
    case "mainnet" => Some(Mainnet)
    case "testnet" => Some(Testnet)
    case _ => None
  }

}

