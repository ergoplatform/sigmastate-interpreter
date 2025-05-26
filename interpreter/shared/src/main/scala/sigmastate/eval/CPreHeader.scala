package sigmastate.eval

import sigma.{Coll, GroupElement, PreHeader}

/** A default implementation of [[PreHeader]] interface.
  *
  * @see [[PreHeader]] for detailed descriptions
  */
case class CPreHeader(
    version: Byte,
    parentId: Coll[Byte],
    timestamp: Long,
    nBits: Long,
    height: Int,
    minerPk: GroupElement,
    votes: Coll[Byte]
) extends PreHeader
