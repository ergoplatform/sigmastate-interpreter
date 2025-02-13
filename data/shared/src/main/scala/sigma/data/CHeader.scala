package sigma.data

import org.ergoplatform.{AutolykosSolution, ErgoHeader, HeaderWithoutPow, HeaderWithoutPowSerializer}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.{bytesToId, idToBytes}
import sigma.pow.Autolykos2PowValidation
import sigma.{AvlTree, BigInt, Coll, Colls, GroupElement, Header}

/** A default implementation of [[Header]] interface.
  *
  * @see [[Header]] for detailed descriptions
  */
class CHeader(val ergoHeader: ErgoHeader) extends Header with WrapperOf[ErgoHeader] {

  /** Bytes representation of ModifierId of this Header */
  override lazy val id: Coll[Byte] = ergoHeader.id

  /** Block version, to be increased on every soft and hardfork. */
  override def version: Byte = ergoHeader.version

  /** Bytes representation of ModifierId of the parent block */
  override def parentId: Coll[Byte] = Colls.fromArray(idToBytes(ergoHeader.parentId))

  /** Hash of ADProofs for transactions in a block */
  override def ADProofsRoot: Coll[Byte] = Colls.fromArray(ergoHeader.ADProofsRoot)

  /** AvlTree of a state after block application */
  override def stateRoot: AvlTree = CAvlTree(AvlTreeData.avlTreeFromDigest(Colls.fromArray(ergoHeader.stateRoot)))

  /** Root hash (for a Merkle tree) of transactions in a block. */
  override def transactionsRoot: Coll[Byte] = Colls.fromArray(ergoHeader.transactionsRoot)

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  override def timestamp: Long = ergoHeader.timestamp

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int */
  override def nBits: Long = ergoHeader.nBits

  /** Block height */
  override def height: Int = ergoHeader.height

  /** Root hash of extension section */
  override def extensionRoot: Coll[Byte] = Colls.fromArray(ergoHeader.extensionRoot)

  /** Miner public key. Should be used to collect block rewards.
    * Part of Autolykos solution. */
  override def minerPk: GroupElement = CGroupElement(ergoHeader.powSolution.pk)

  /** One-time public key. Prevents revealing of miners secret. */
  override def powOnetimePk: GroupElement = CGroupElement(ergoHeader.powSolution.w)

  /** nonce */
  override def powNonce: Coll[Byte] = Colls.fromArray(ergoHeader.powSolution.n)

  /** Distance between pseudo-random number, corresponding to nonce `powNonce` and a secret,
    * corresponding to `minerPk`. The lower `powDistance` is, the harder it was to find this solution. */
  override def powDistance: BigInt = CBigInt(ergoHeader.powSolution.d.bigInteger)

  /** Miner votes for changing system parameters. */
  override def votes: Coll[Byte] = Colls.fromArray(ergoHeader.votes)

  override def unparsedBytes: Coll[Byte] = Colls.fromArray(ergoHeader.unparsedBytes)

  /** The data value wrapped by this wrapper. */
  override def wrappedValue: ErgoHeader = ergoHeader

  override def serializeWithoutPoW: Coll[Byte] = {
    Colls.fromArray(HeaderWithoutPowSerializer.toBytes(ergoHeader))
  }

  override def checkPow: Boolean = {
    if (version == 1) {
      throw new Exception("Autolykos v1 is not supported") //todo: more specific exception?
    } else {
      Autolykos2PowValidation.checkPoWForVersion2(this)
    }
  }

  override def toString: String =
    s"""CHeader(
       |  id: ${id},
       |  version: ${version},
       |  tx proofs hash: ${ADProofsRoot},
       |  state root: ${stateRoot.digest},
       |  transactions root: ${transactionsRoot},
       |  time: $timestamp,
       |  nbits: $nBits,
       |  extension root: $extensionRoot,
       |  miner pubkey: $minerPk,
       |  pow one time pubkey(from AL 1): $powOnetimePk,
       |  pow nonce: $powNonce,
       |  pow distance (from AL 1): $powDistance,
       |  votes: $votes,
       |  unparsed bytes: $unparsedBytes
       |)""".stripMargin

  override def hashCode(): Int = id.hashCode()

  override def equals(other: Any): Boolean = other match {
    case ch: CHeader => ch.id == this.id
    case _ => false
  }

  def copy(): CHeader = new CHeader(ergoHeader.copy()) // used in tests only
}

object CHeader {

  def apply( version: Byte,
             parentId: Coll[Byte],
             ADProofsRoot: Coll[Byte],
             stateRootDigest: Coll[Byte],
             transactionsRoot: Coll[Byte],
             timestamp: Long,
             nBits: Long,
             height: Int,
             extensionRoot: Coll[Byte],
             minerPk: GroupElement,
             powOnetimePk: GroupElement,
             powNonce: Coll[Byte],
             powDistance: BigInt,
             votes: Coll[Byte],
             unparsedBytes: Coll[Byte]): CHeader = {

    val solution = new AutolykosSolution(
      minerPk.asInstanceOf[CGroupElement].wrappedValue,
      powOnetimePk.asInstanceOf[CGroupElement].wrappedValue,
      powNonce.toArray,
      powDistance.asInstanceOf[CBigInt].wrappedValue)

    val h = ErgoHeader(version, bytesToId(parentId.toArray), Digest32 @@ ADProofsRoot.toArray,
      ADDigest @@ stateRootDigest.toArray, Digest32 @@ transactionsRoot.toArray, timestamp, nBits, height,
      Digest32 @@ extensionRoot.toArray, solution, votes.toArray, unparsedBytes.toArray, null)

    new CHeader(h)
  }

  /** Size of of Header.votes array. */
  val VotesSize: Int = SigmaConstants.VotesArraySize.value

  /** Size of nonce array from Autolykos POW solution in Header.powNonce array. */
  val NonceSize: Int = SigmaConstants.AutolykosPowSolutionNonceArraySize.value

}
