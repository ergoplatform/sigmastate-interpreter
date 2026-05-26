package sigma.data

import sigma.serialization.{CoreByteReader, CoreByteWriter, CoreSerializer}
import sigma.{Coll, Colls, crypto}

/**
  * Type of data that authenticates membership in a static Merkle tree by storing only
  * its root digest.
  *
  * A `MerkleTreeData` is fully described by its 32-byte Blake2b256 root hash; tree
  * shape and leaf contents are not stored on-chain. Proofs are passed in at
  * verification time (see [[sigma.MerkleTree.containsLeaf]] /
  * [[sigma.MerkleTree.containsLeaves]]).
  *
  * @param digest root hash of the tree (exactly `crypto.hashLength` bytes).
  */
case class MerkleTreeData(digest: Coll[Byte])

object MerkleTreeData {
  /** Size of the root digest in bytes. */
  val DigestSize: Int = crypto.hashLength

  /** A placeholder tree with a zero-filled digest. Useful as a default constant or as
    * the starting point for [[sigma.MerkleTree.updateDigest]].
    */
  val dummy: MerkleTreeData =
    MerkleTreeData(Colls.fromArray(Array.fill(DigestSize)(0: Byte)))

  /** Build a [[MerkleTreeData]] from a raw root digest. */
  def merkleTreeFromDigest(digest: Coll[Byte]): MerkleTreeData = MerkleTreeData(digest)

  object serializer extends CoreSerializer[MerkleTreeData, MerkleTreeData] {
    override def serialize(data: MerkleTreeData, w: CoreByteWriter): Unit = {
      w.putBytes(data.digest.toArray)
    }

    override def parse(r: CoreByteReader): MerkleTreeData = {
      val digest = r.getBytes(DigestSize)
      MerkleTreeData(Colls.fromArray(digest))
    }
  }
}
