package org.ergo.stark.protocol

import org.ergo.stark.hash.Poseidon1BabyBear

/**
 * Batch Merkle Tree Verifier with tweakable hashing.
 *
 * Verifies multiple Merkle openings simultaneously by deduplicating
 * shared sibling nodes across paths. This is the "Batched Merkle Opening"
 * optimization from EIP-0045's Goldilocks Zone analysis.
 *
 * Tweakable hashing: H(seed ‖ layer ‖ nodeIndex ‖ left ‖ right)
 * This domain separation prevents second-preimage attacks across
 * different tree positions (NIST FIPS 205 / SPHINCS+ standard).
 *
 * Digest size: C = 13 BabyBear elements (403 bits, ≥128-bit PQ collision).
 */
object MerkleVerifier {

  private val DIGEST_LEN = Poseidon1BabyBear.C  // 13

  /**
   * Hash two child digests into a parent digest with domain separation.
   *
   * H(seed ‖ layer ‖ nodeIndex ‖ left ‖ right)
   *
   * @param seed      public randomness (from Fiat-Shamir transcript)
   * @param layer     tree layer (0 = leaves, depth-1 = root)
   * @param nodeIndex index of the parent node in its layer
   * @param left      left child digest (DIGEST_LEN elements)
   * @param right     right child digest (DIGEST_LEN elements)
   * @return parent digest (DIGEST_LEN elements)
   */
  def tweakableHash(
    seed: Array[Int],
    layer: Int,
    nodeIndex: Int,
    left: Array[Int],
    right: Array[Int]
  ): Array[Int] = {
    // Build input: seed ‖ layer ‖ nodeIndex ‖ left ‖ right
    val input = new Array[Int](seed.length + 2 + left.length + right.length)
    System.arraycopy(seed, 0, input, 0, seed.length)
    input(seed.length) = layer
    input(seed.length + 1) = nodeIndex
    System.arraycopy(left, 0, input, seed.length + 2, left.length)
    System.arraycopy(right, 0, input, seed.length + 2 + left.length, right.length)

    Poseidon1BabyBear.hash(input, DIGEST_LEN)
  }

  /**
   * Verify a batch of Merkle openings against a committed root.
   *
   * Algorithm (layer-by-layer, bottom-up):
   *   For each layer, group current nodes by pairs.
   *   If both siblings are in the active set, use them directly (no sibling consumed).
   *   If only one is present, pop the next sibling from the proof stream.
   *   Hash the pair with tweakable hashing.
   *   After all layers, exactly one node (the root) should remain,
   *   and the sibling stream should be exhausted.
   *
   * @param root          committed Merkle root (DIGEST_LEN elements)
   * @param depth         tree depth (log2 of number of leaves)
   * @param leafIndices   sorted array of leaf indices being opened
   * @param leafDigests   corresponding leaf digests (same order as leafIndices)
   * @param siblings      flat array of sibling digests (deduplicated, in layer order)
   * @param seed          public randomness for tweakable hashing
   * @return true if the batch proof is valid
   */
  def verifyBatch(
    root: Array[Int],
    depth: Int,
    leafIndices: Array[Int],
    leafDigests: Array[Array[Int]],
    siblings: Array[Array[Int]],
    seed: Array[Int]
  ): Boolean = {
    require(leafIndices.length == leafDigests.length,
      s"leafIndices (${leafIndices.length}) and leafDigests (${leafDigests.length}) must match")
    require(leafIndices.length > 0, "Must have at least one leaf")

    // Verify leaves are sorted and unique
    var i = 1
    while (i < leafIndices.length) {
      require(leafIndices(i) > leafIndices(i - 1), "leafIndices must be sorted and unique")
      i += 1
    }

    // Current layer: map of nodeIndex → digest
    var currentNodes = scala.collection.mutable.TreeMap.empty[Int, Array[Int]]
    i = 0
    while (i < leafIndices.length) {
      currentNodes(leafIndices(i)) = leafDigests(i)
      i += 1
    }

    var siblingIdx = 0

    // Process layer by layer (bottom-up)
    var layer = 0
    while (layer < depth) {
      val nextNodes = scala.collection.mutable.TreeMap.empty[Int, Array[Int]]

      // Process nodes in sorted order
      val indices = currentNodes.keys.toArray
      var j = 0
      while (j < indices.length) {
        val idx = indices(j)
        val parentIdx = idx / 2
        val siblingIndex = idx ^ 1  // flip LSB to get sibling

        if (!nextNodes.contains(parentIdx)) {
          // Determine left and right children
          val isLeft = (idx % 2) == 0

          val myDigest = currentNodes(idx)
          val sibDigest: Array[Int] = if (currentNodes.contains(siblingIndex)) {
            // Sibling is in the active set — use it directly (no proof consumption)
            currentNodes(siblingIndex)
          } else {
            // Sibling not in active set — consume from proof stream
            if (siblingIdx >= siblings.length) {
              return false  // Proof too short
            }
            val sib = siblings(siblingIdx)
            siblingIdx += 1
            sib
          }

          val (left, right) = if (isLeft) (myDigest, sibDigest) else (sibDigest, myDigest)
          nextNodes(parentIdx) = tweakableHash(seed, layer, parentIdx, left, right)
        }

        j += 1
      }

      currentNodes = nextNodes
      layer += 1
    }

    // After all layers: should have exactly 1 node (the root)
    if (currentNodes.size != 1) return false

    // Sibling stream should be fully consumed
    if (siblingIdx != siblings.length) return false

    // Compare computed root with expected root
    val computedRoot = currentNodes.head._2
    java.util.Arrays.equals(computedRoot, root)
  }

  /**
   * Compute a Merkle root from a complete set of leaves.
   * Utility for testing and proof generation.
   */
  def computeRoot(leaves: Array[Array[Int]], seed: Array[Int]): Array[Int] = {
    require(leaves.length > 0, "Must have at least one leaf")
    require((leaves.length & (leaves.length - 1)) == 0, "Number of leaves must be power of 2")

    var currentLayer = leaves.clone()
    var layer = 0

    while (currentLayer.length > 1) {
      val nextLayer = new Array[Array[Int]](currentLayer.length / 2)
      var i = 0
      while (i < currentLayer.length) {
        nextLayer(i / 2) = tweakableHash(seed, layer, i / 2, currentLayer(i), currentLayer(i + 1))
        i += 2
      }
      currentLayer = nextLayer
      layer += 1
    }

    currentLayer(0)
  }

  /**
   * Generate a batch Merkle proof for a set of leaf indices.
   * Returns the deduplicated sibling array.
   * Utility for testing.
   */
  def generateBatchProof(
    leaves: Array[Array[Int]],
    indices: Array[Int],
    seed: Array[Int]
  ): (Array[Int], Array[Array[Int]], Array[Array[Int]]) = {
    val depth = Integer.numberOfTrailingZeros(leaves.length)

    // Build full tree layer by layer
    val layers = new Array[Array[Array[Int]]](depth + 1)
    layers(0) = leaves.clone()

    var layer = 0
    while (layer < depth) {
      val cur = layers(layer)
      val next = new Array[Array[Int]](cur.length / 2)
      var i = 0
      while (i < cur.length) {
        next(i / 2) = tweakableHash(seed, layer, i / 2, cur(i), cur(i + 1))
        i += 2
      }
      layers(layer + 1) = next
      layer += 1
    }

    val root = layers(depth)(0)
    val sortedIndices = indices.sorted

    // Extract deduplicated siblings
    val siblings = scala.collection.mutable.ArrayBuffer.empty[Array[Int]]
    var currentIndexSet = sortedIndices.toSet

    layer = 0
    while (layer < depth) {
      val nextIndexSet = scala.collection.mutable.Set.empty[Int]
      currentIndexSet.toArray.sorted.foreach { idx =>
        val sibIdx = idx ^ 1
        val parentIdx = idx / 2
        if (!nextIndexSet.contains(parentIdx)) {
          if (!currentIndexSet.contains(sibIdx)) {
            siblings += layers(layer)(sibIdx)
          }
          nextIndexSet += parentIdx
        }
      }
      currentIndexSet = nextIndexSet.toSet
      layer += 1
    }

    val leafDigests = sortedIndices.map(i => leaves(i))
    (root, leafDigests, siblings.toArray)
  }
}
