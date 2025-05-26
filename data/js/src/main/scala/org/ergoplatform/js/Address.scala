package org.ergoplatform.js

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.ErgoAddressEncoder
import scorex.util.encode.Base58
import sigma.ast.js.ErgoTree
import sigma.js.{GroupElement, SigmaProp}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{UndefOr, undefined}
import scala.util.{Failure, Success}

/** An exported JavaScript class wrapping the Scala `ErgoAddress` type. */
@JSExportTopLevel("Address")
abstract class Address extends js.Object {
  def ergoAddress: org.ergoplatform.ErgoAddress

  private lazy val _addressBytes: Array[Byte] = {
    ErgoAddressEncoder(ergoAddress.networkPrefix).toBytes(ergoAddress)
  }

  /** Serialize this address to bytes.
    * @see ErgoAddressEncoder.toBytes()
    */
  def addressBytes(): Array[Byte] = _addressBytes

  /** Address type code used to differentiate between pay-to-public-key, pay-to-script,
    * pay-to-script-hash addresses.
    *
    * @see [[P2PKAddress]], [[P2SAddress]], [[P2SHAddress]]
    */
  def addressTypePrefix(): Byte = ergoAddress.addressTypePrefix

  /** First byte is used to encode network type and address type.
    *
    * @see ErgoAddressEncoder
    */
  private def headByte: Byte = _addressBytes(0)

  /** @return true if this address from Ergo mainnet. */
  def isMainnet(): Boolean = Address.isMainnet(headByte)

  /** @return true if this address has Pay-To-Public-Key type. */
  def isP2PK(): Boolean = ergoAddress.isInstanceOf[org.ergoplatform.P2PKAddress]

  /** @return underlying {@link P2PKAddress}.
    * @throws IllegalArgumentException if this instance is not P2PK address
    */
  def asP2PK(): P2PKAddress = {
    require(isP2PK(), s"This instance $this is not P2PKAddress")
    new P2PKAddress(ergoAddress.asInstanceOf[org.ergoplatform.P2PKAddress])
  }

  /** @return true if this address has Pay-To-Script type. */
  def isP2S(): Boolean = ergoAddress.isInstanceOf[org.ergoplatform.Pay2SAddress]

  /** @return underlying {@link P2SAddress}.
    * @throws IllegalArgumentException if this instance is not P2S address
    */
  def asP2S(): P2SAddress = {
    require(isP2S(), s"This instance $this is not P2SAddress")
    new P2SAddress(ergoAddress.asInstanceOf[org.ergoplatform.Pay2SAddress])
  }

  /** @return true if this address has Pay-To-Script-Hash type. */
  def isP2SH(): Boolean = ergoAddress.isInstanceOf[org.ergoplatform.Pay2SHAddress]

  /** @return underlying {@link P2SHAddress}.
    * @throws IllegalArgumentException if this instance is not P2SH address
    */
  def asP2SH(): P2SHAddress = {
    require(isP2SH(), s"This instance $this is not P2SHAddress")
    new P2SHAddress(ergoAddress.asInstanceOf[org.ergoplatform.Pay2SHAddress])
  }

  /** Extracts a [[sigma.js.SigmaProp]] from this address of the underlying ErgoTree if of
    * specific form.
    * @see ErgoTree.toSigmaBooleanOpt()
    */
  def toSigmaPropOpt(): UndefOr[sigma.js.SigmaProp] = {
    ergoAddress.script.toSigmaBooleanOpt match {
      case Some(sb) => new SigmaProp(sb)
      case _ => undefined
    }
  }

  /** ErgoTree which corresponds to the address (depending on the address type).
    *
    * @see [[P2PKAddress]], [[P2SAddress]], [[P2SHAddress]]
    */
  def toErgoTree(): ErgoTree = new ErgoTree(ergoAddress.script)

  /** @return this addresses ErgoTree's proposition bytes. Use this to store this address
    *         on Box registers.
    */
  def toPropositionBytes(): Array[Byte] = ergoAddress.script.bytes

  /** Converts the given [[Address]] to Base58 string. */
  override def toString() = ergoAddress.toString
}

/** An exported JavaScript object providing utility methods for working with Address instances. */
@JSExportTopLevel("Address$")
object Address extends js.Object {
  /** Creates JS wrapper over given [[ErgoAddress]]. */
  def fromErgoAddress(ergoAddress: org.ergoplatform.ErgoAddress): Address = {
    ergoAddress match {
      case p2pk: org.ergoplatform.P2PKAddress =>
        new P2PKAddress(p2pk)
      case p2s: org.ergoplatform.Pay2SAddress =>
        new P2SAddress(p2s)
      case p2sh: org.ergoplatform.Pay2SHAddress =>
        new P2SHAddress(p2sh)
    }
  }

  /** @return true if this address from Ergo mainnet. */
  private def isMainnet(headByte: Byte): Boolean = headByte < ErgoAddressEncoder.TestnetNetworkPrefix

  private def getNetworkType(headByte: Byte): NetworkPrefix = {
    if (isMainnet(headByte)) ErgoAddressEncoder.MainnetNetworkPrefix else ErgoAddressEncoder.TestnetNetworkPrefix
  }

  /** Deserializes an ErgoTree instance from an address string.
    *
    * @param base58String a Base58 string representing the serialized ErgoTree
    */
  def fromString(base58String: String): Address = {
    Base58.decode(base58String) match {
      case Success(bytes) =>
        val headByte = bytes(0)
        val encoder = ErgoAddressEncoder(getNetworkType(headByte))
        encoder.fromBytes(bytes, base58String) match {
          case Success(ergoAddress) =>
            Address.fromErgoAddress(ergoAddress)
          case Failure(t) =>
            throw new RuntimeException(
              "Invalid address encoding, expected base58 string: " + base58String, t)
        }
      case Failure(t) =>
        throw new RuntimeException(
          "Invalid address encoding, expected base58 string: " + base58String, t)
    }
  }

  /** Creates an `Address` instance from an `ErgoTree` and a network prefix.
    *
    * @param ergoTree      The `ErgoTree` instance to be converted into an `Address`.
    * @param networkPrefix The network prefix indicating the network for which the address is valid.
    * @return An `Address` instance corresponding to the given `ErgoTree` and network prefix.
    */
  def fromErgoTree(ergoTree: ErgoTree, networkPrefix: NetworkPrefix): Address = {
    val encoder     = ErgoAddressEncoder(networkPrefix)
    val ergoAddress = encoder.fromProposition(ergoTree.tree).get
    Address.fromErgoAddress(ergoAddress)
  }

  /**
    * Creates an `Address` from a `SigmaProp` and a network prefix.
    *
    * @param sigmaProp     The `SigmaProp` to be converted into an `Address`.
    * @param networkPrefix The network prefix indicating the network for which the address is valid.
    * @return An `Address` instance corresponding to the given `SigmaProp` and network prefix.
    */
  def fromSigmaProp(
      sigmaProp: SigmaProp,
      networkPrefix: NetworkPrefix): Address = {
    val ergoTree = sigma.ast.ErgoTree.fromSigmaBoolean(sigmaProp.sigmaBoolean)
    fromErgoTree(new ErgoTree(ergoTree), networkPrefix)
  }

  /** Creates address from given ergovalue containing an ErgoTree proposition bytes.
    * Use this to convert a box register containing an ErgoTree into its address.
    *
    * @param networkPrefix    mainnet or testnet network
    * @param propositionBytes ErgoTree proposition bytes
    */
  def fromPropositionBytes(networkPrefix: NetworkPrefix, propositionBytes: Array[Byte]): Address = {
    fromErgoTree(ErgoTree.fromBytes(propositionBytes), networkPrefix)
  }

}

/** An exported JavaScript class wrapping the Scala `P2PKAddress` type. */
@JSExportTopLevel("P2PKAddress")
class P2PKAddress(
    override val ergoAddress: org.ergoplatform.P2PKAddress
) extends Address {

  /** Converts this address to the underlying ProveDlog sigma proposition wrapped in SigmaProp. */
  def toSigmaProp(): sigma.js.SigmaProp = new SigmaProp(ergoAddress.pubkey)

  /** Extract the underlying [[sigma.js.GroupElement]] of this address. */
  def getPublicKeyGE(): sigma.js.GroupElement = new GroupElement(ergoAddress.pubkey.value)
}

/** An exported JavaScript class wrapping the Scala `P2SAddress` type. */
@JSExportTopLevel("P2SAddress")
class P2SAddress(
    override val ergoAddress: org.ergoplatform.Pay2SAddress
) extends Address

/** An exported JavaScript class wrapping the Scala `P2SHAddress` type. */
@JSExportTopLevel("P2SHAddress")
class P2SHAddress(
    override val ergoAddress: org.ergoplatform.Pay2SHAddress
) extends Address
