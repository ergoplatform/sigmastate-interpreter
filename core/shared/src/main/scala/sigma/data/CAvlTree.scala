package sigma.data

import sigma.{AvlTree, Coll}

/** A default implementation of [[AvlTree]] interface.
  *
  * @see [[AvlTree]] for detailed descriptions
  */
case class CAvlTree(treeData: AvlTreeData) extends AvlTree with WrapperOf[AvlTreeData] {
  override def wrappedValue: AvlTreeData = treeData

  override def keyLength: Int = treeData.keyLength

  override def enabledOperations = treeData.treeFlags.serializeToByte

  override def isInsertAllowed: Boolean = treeData.treeFlags.insertAllowed

  override def isUpdateAllowed: Boolean = treeData.treeFlags.updateAllowed

  override def isRemoveAllowed: Boolean = treeData.treeFlags.removeAllowed

  override def updateOperations(newOperations: Byte): AvlTree = {
    val td = treeData.copy(treeFlags = AvlTreeFlags(newOperations))
    this.copy(treeData = td)
  }

  override def valueLengthOpt: Option[Int] = treeData.valueLengthOpt

  override def digest: Coll[Byte] = treeData.digest

  override def updateDigest(newDigest: Coll[Byte]): AvlTree = {
    val td = treeData.copy(digest = newDigest)
    this.copy(treeData = td)
  }
}

