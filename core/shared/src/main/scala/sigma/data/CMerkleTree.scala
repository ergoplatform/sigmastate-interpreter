package sigma.data

import sigma.{Coll, MerkleTree}

/** Default implementation of [[MerkleTree]] backed by [[MerkleTreeData]].
  *
  * @see [[MerkleTree]] for detailed descriptions
  */
case class CMerkleTree(treeData: MerkleTreeData) extends MerkleTree with WrapperOf[MerkleTreeData] {
  override def wrappedValue: MerkleTreeData = treeData

  override def digest: Coll[Byte] = treeData.digest

  override def updateDigest(newDigest: Coll[Byte]): MerkleTree = {
    val td = treeData.copy(digest = newDigest)
    this.copy(treeData = td)
  }
}
