package sigmastate.eval

import sigma.data.SigmaConstants
import sigma.{AvlTree, BigInt, Coll, GroupElement, Header}

/** A default implementation of [[Header]] interface.
  *
  * @see [[Header]] for detailed descriptions
  */
case class CHeader(
    id: Coll[Byte],
    version: Byte,
    parentId: Coll[Byte],
    ADProofsRoot: Coll[Byte],
    stateRoot: AvlTree,
    transactionsRoot: Coll[Byte],
    timestamp: Long,
    nBits: Long,
    height: Int,
    extensionRoot: Coll[Byte],
    minerPk: GroupElement,
    powOnetimePk: GroupElement,
    powNonce: Coll[Byte],
    powDistance: BigInt,
    votes: Coll[Byte]
) extends Header

object CHeader {
  /** Size of of Header.votes array. */
  val VotesSize: Int = SigmaConstants.VotesArraySize.value

  /** Size of nonce array from Autolykos POW solution in Header.powNonce array. */
  val NonceSize: Int = SigmaConstants.AutolykosPowSolutionNonceArraySize.value
}