package sigmastate.eval

import scorex.crypto.authds.merkle.BatchMerkleProof
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.{Coll, MerkleTree}
import sigma.data.CMerkleTree

import scala.util.Try

/** Verifier for static Merkle proofs introduced in v7.0.
  *
  * Wraps scrypto's [[BatchMerkleProof]] so the proof byte format is the same one used
  * by the Ergo node. The verifier:
  *   1. parses the proof bytes via [[BatchMerkleProofSerializer]];
  *   2. computes prefixed leaf hashes from the supplied raw leaf bytes
  *      (`Blake2b256(LeafPrefix || leafBytes)`); and
  *   3. checks both that each computed leaf hash appears in the proof's claimed leaf
  *      set AND that the proof reconstructs to the tree's root digest.
  *
  * Both checks are required: a proof that's `valid` against the root but doesn't
  * actually claim the requested leaves must be rejected. Any malformed proof, leaf
  * mismatch, or root mismatch yields `false` rather than throwing.
  */
class CMerkleTreeVerifier private (
    rootDigest: Array[Byte],
    parsedProof: Try[BatchMerkleProof[Digest32]]) {

  /** Verify single-leaf membership.
    *
    * @param leafData raw leaf bytes (will be domain-separated hashed inside).
    * @return true iff the proof is well-formed, claims exactly this leaf, and
    *         reconstructs to the root digest.
    */
  def containsLeaf(leafData: Coll[Byte]): Boolean = {
    parsedProof.fold(
      _ => false,
      bmp => {
        val expectedHash = leafHash(leafData)
        val claimsThisLeaf =
          bmp.indices.length == 1 &&
            java.util.Arrays.equals(bmp.indices.head._2, expectedHash)
        claimsThisLeaf && Try(bmp.valid(Digest32 @@ rootDigest)).getOrElse(false)
      }
    )
  }

  /** Verify batch membership.
    *
    * @param leaves raw leaf bytes for each element. Order is irrelevant — the verifier
    *               only requires set-equality between the supplied leaves' hashes and
    *               the proof's claimed leaf hashes.
    * @return true iff the proof is well-formed, claims exactly these leaves, and
    *         reconstructs to the root digest.
    */
  def containsLeaves(leaves: Coll[Coll[Byte]]): Boolean = {
    parsedProof.fold(
      _ => false,
      bmp => {
        if (bmp.indices.length != leaves.length) {
          false
        } else {
          val claimedHashes = bmp.indices.map(_._2.toSeq).toSet
          val expectedHashes = (0 until leaves.length).map(i => leafHash(leaves(i)).toSeq).toSet
          claimedHashes == expectedHashes && Try(bmp.valid(Digest32 @@ rootDigest)).getOrElse(false)
        }
      }
    )
  }

  private def leafHash(data: Coll[Byte]): Array[Byte] =
    Blake2b256.prefixedHash(scorex.crypto.authds.merkle.MerkleTree.LeafPrefix, data.toArray)
}

object CMerkleTreeVerifier {
  /** scrypto serializer pinned to Blake2b256/Digest32 — the only hash function used by
    * Ergo's Merkle constructions. */
  private val proofSerializer = new BatchMerkleProofSerializer[Digest32, Blake2b256.type]()(Blake2b256)

  /** Build a verifier for the given tree and proof. Proof bytes are parsed eagerly so
    * verification calls become cheap; parse failure is captured in the returned
    * verifier and surfaces as `false` from `containsLeaf` / `containsLeaves`.
    */
  def apply(tree: MerkleTree, proof: Coll[Byte]): CMerkleTreeVerifier = {
    val treeData = tree.asInstanceOf[CMerkleTree].treeData
    val parsed   = proofSerializer.deserialize(proof.toArray)
    new CMerkleTreeVerifier(treeData.digest.toArray, parsed)
  }
}
