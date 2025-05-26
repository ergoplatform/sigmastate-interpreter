package org.ergoplatform.sdk

/** Blockchain parameters re-adjustable via miners voting and voting-related data.
  * All these parameters are included into extension section of a first block of a voting epoch.
  */
abstract class BlockchainParameters {
  /** Cost of storing 1 byte in UTXO for four years, in nanoErgs. */
  def storageFeeFactor: Int
  /** Cost of a transaction output, in computation unit. */
  def minValuePerByte: Int
  /** Max block size, in bytes. */
  def maxBlockSize: Int
  /** Cost of a token contained in a transaction, in computation unit. */
  def tokenAccessCost: Int
  /** Cost of a transaction input, in computation unit. */
  def inputCost: Int
  /** Cost of a transaction data input, in computation unit. */
  def dataInputCost: Int
  /** Cost of a transaction output, in computation unit. */
  def outputCost: Int
  /** Computation units limit per block. */
  def maxBlockCost: Int
  /** Height when voting for a soft-fork had been started. */
  def softForkStartingHeight: Option[Int]
  /** Votes for soft-fork collected in previous epochs. */
  def softForkVotesCollected: Option[Int]
  /** Protocol version activated on the network. */
  def blockVersion: Byte
}

/** Concete implementation of blockchain parameters. */
case class CBlockchainParameters(
    storageFeeFactor: Int,
    minValuePerByte: Int,
    maxBlockSize: Int,
    tokenAccessCost: Int,
    inputCost: Int,
    dataInputCost: Int,
    outputCost: Int,
    maxBlockCost: Int,
    softForkStartingHeight: Option[Int],
    softForkVotesCollected: Option[Int],
    blockVersion: Byte
) extends BlockchainParameters

/** Global parameters used by SDK */
object BlockchainParameters {
  /** A number of blocks a miner should wait before he/she can spend block reward.
    * This is part of Ergo protocol and cannot be changed.
    */
  val MinerRewardDelay_Mainnet = 720

  val MinerRewardDelay_Testnet = 720

  /** One Erg is 10^9 NanoErg */
  val OneErg: Long = 1000 * 1000 * 1000

  /** Minimum transaction fee in NanoErgs as it is defined in Ergo protocol. */
  val MinFee: Long = 1000 * 1000

  /** Minimum value for a change. It can be used to compute change output value.
    * If computed change is less than this value, it is added to the fee
    * and `change` output in not added to the transaction.
    */
  val MinChangeValue: Long = 1000 * 1000
}