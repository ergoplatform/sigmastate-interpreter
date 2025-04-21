package sigmastate.eval

import sigma.exceptions.SoftFieldAccessException
import sigma.{Coll, GroupElement, PreHeader}

/** A default implementation of [[PreHeader]] interface.
  *
  * @see [[PreHeader]] for detailed descriptions
  */
class CPreHeader(
    val version: Byte,
    val parentId: Coll[Byte],
    _timestamp: Long,
    val nBits: Long,
    val height: Int,
    _minerPk: GroupElement,
    _votes: Coll[Byte],
    val softFieldsAllowed: Boolean
) extends PreHeader {

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  override def timestamp: Long = {
    if(softFieldsAllowed) {
      _timestamp
    } else {
      throw new SoftFieldAccessException("timestamp")
    }
  }

  /** Miner public key. Should be used to collect block rewards. */
  override def minerPk: GroupElement = {
    if(softFieldsAllowed) {
      _minerPk
    } else {
      throw new SoftFieldAccessException("minerPubKey")
    }
  }

  /** Miner votes for changing system parameters. */
  override def votes: Coll[Byte] = {
    if(softFieldsAllowed) {
      _votes
    } else {
      throw new SoftFieldAccessException("votes")
    }
  }

  def withHeight(updHeight: Int): CPreHeader = {
    new CPreHeader(
      version,
      parentId,
      timestamp,
      nBits,
      updHeight,
      minerPk,
      votes,
      softFieldsAllowed
    )
  }
}
