package sigmastate.eval

import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Insert, Lookup, Remove, Update}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.data.CAvlTree
import sigma.eval.AvlTreeVerifier
import sigma.{AvlTree, Coll}

import scala.util.Try

/** Implements operations of AVL tree verifier based on
  * [[scorex.crypto.authds.avltree.batch.BatchAVLVerifier]].
  *
  * @see BatchAVLVerifier, CAvlTreeVerifier
  */
class CAvlTreeVerifier private(
    startingDigest: ADDigest,
    proof: SerializedAdProof,
    override val keyLength: Int,
    override val valueLengthOpt: Option[Int])
    extends BatchAVLVerifier[Digest32, Blake2b256.type](
      startingDigest, proof, keyLength, valueLengthOpt) with AvlTreeVerifier {
  def treeHeight: Int = rootNodeHeight

  override def performLookup(key: Array[Byte]): Try[Option[Array[Byte]]] =
    performOneOperation(Lookup(ADKey @@ key))

  override def performInsert(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]] =
    performOneOperation(Insert(ADKey @@ key, ADValue @@ value))

  override def performUpdate(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]] =
    performOneOperation(Update(ADKey @@ key, ADValue @@ value))

  override def performRemove(key: Array[Byte]): Try[Option[Array[Byte]]] =
    performOneOperation(Remove(ADKey @@ key))

  override def digest: Option[ADDigest] = super.digest

  /** Override default logging which outputs stack trace to the console. */
  override protected def logError(t: Throwable): Unit = {}
}

object CAvlTreeVerifier {
  /** Create an instance of [[CAvlTreeVerifier]] for the given tree and proof.
    * Both tree and proof are immutable.
    *
    * @param tree  represents a tree state to verify
    * @param proof proof of tree operations leading to the state digest in the tree
    * @return a new verifier instance
    */
  def apply(tree: AvlTree, proof: Coll[Byte]): CAvlTreeVerifier = {
    val treeData = tree.asInstanceOf[CAvlTree].treeData
    val adProof  = SerializedAdProof @@ proof.toArray
    val bv       = new CAvlTreeVerifier(
      ADDigest @@ treeData.digest.toArray, adProof, treeData.keyLength, treeData.valueLengthOpt)
    bv
  }
}