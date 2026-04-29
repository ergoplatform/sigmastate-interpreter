package org.ergo.stark.protocol

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergo.stark.field.BabyBearField
import org.ergo.stark.hash.Poseidon1BabyBear

class MerkleVerifierSpec extends AnyFlatSpec with Matchers {

  private val C = Poseidon1BabyBear.C  // 13
  private val seed = Array.fill(8)(42)  // 8-element seed for tweakable hashing

  /** Create a dummy leaf digest from an integer */
  private def leaf(v: Int): Array[Int] = {
    Poseidon1BabyBear.hash(Array(v), C)
  }

  // ══════════════════════════════════════════
  // Tweakable Hashing
  // ══════════════════════════════════════════

  "tweakableHash" should "produce C-element digests" in {
    val left = leaf(1)
    val right = leaf(2)
    val result = MerkleVerifier.tweakableHash(seed, 0, 0, left, right)
    result.length shouldBe C
    result.foreach(e => BabyBearField.isValid(e) shouldBe true)
  }

  it should "be non-commutative (left/right order matters)" in {
    val left = leaf(1)
    val right = leaf(2)
    val h1 = MerkleVerifier.tweakableHash(seed, 0, 0, left, right)
    val h2 = MerkleVerifier.tweakableHash(seed, 0, 0, right, left)
    h1 should not be h2
  }

  it should "be sensitive to layer" in {
    val left = leaf(1)
    val right = leaf(2)
    val h1 = MerkleVerifier.tweakableHash(seed, 0, 0, left, right)
    val h2 = MerkleVerifier.tweakableHash(seed, 1, 0, left, right)
    h1 should not be h2
  }

  it should "be sensitive to nodeIndex" in {
    val left = leaf(1)
    val right = leaf(2)
    val h1 = MerkleVerifier.tweakableHash(seed, 0, 0, left, right)
    val h2 = MerkleVerifier.tweakableHash(seed, 0, 1, left, right)
    h1 should not be h2
  }

  // ══════════════════════════════════════════
  // Root Computation
  // ══════════════════════════════════════════

  "computeRoot" should "compute correct root for 2 leaves" in {
    val leaves = Array(leaf(1), leaf(2))
    val root = MerkleVerifier.computeRoot(leaves, seed)
    root.length shouldBe C

    // Manual: root = tweakableHash(seed, 0, 0, leaf(1), leaf(2))
    val expected = MerkleVerifier.tweakableHash(seed, 0, 0, leaves(0), leaves(1))
    root shouldBe expected
  }

  it should "compute correct root for 4 leaves" in {
    val leaves = Array(leaf(10), leaf(20), leaf(30), leaf(40))
    val root = MerkleVerifier.computeRoot(leaves, seed)
    root.length shouldBe C

    // Manual verification:
    val h01 = MerkleVerifier.tweakableHash(seed, 0, 0, leaves(0), leaves(1))
    val h23 = MerkleVerifier.tweakableHash(seed, 0, 1, leaves(2), leaves(3))
    val expected = MerkleVerifier.tweakableHash(seed, 1, 0, h01, h23)
    root shouldBe expected
  }

  it should "be deterministic" in {
    val leaves = (0 until 8).map(i => leaf(i)).toArray
    MerkleVerifier.computeRoot(leaves, seed) shouldBe MerkleVerifier.computeRoot(leaves, seed)
  }

  // ══════════════════════════════════════════
  // Single Opening Verification
  // ══════════════════════════════════════════

  "verifyBatch" should "verify a single leaf opening" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i * 100)).toArray
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(3), seed)

    MerkleVerifier.verifyBatch(root, 3, Array(3), leafDigests, siblings, seed) shouldBe true
  }

  it should "reject wrong leaf value" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i * 100)).toArray
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(3), seed)

    // Corrupt the leaf
    val badLeaf = leafDigests(0).clone()
    badLeaf(0) = BabyBearField.add(badLeaf(0), 1)

    MerkleVerifier.verifyBatch(root, 3, Array(3), Array(badLeaf), siblings, seed) shouldBe false
  }

  it should "reject wrong root" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i * 100)).toArray
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(3), seed)

    val badRoot = root.clone()
    badRoot(0) = BabyBearField.add(badRoot(0), 1)

    MerkleVerifier.verifyBatch(badRoot, 3, Array(3), leafDigests, siblings, seed) shouldBe false
  }

  // ══════════════════════════════════════════
  // Batch Opening Verification
  // ══════════════════════════════════════════

  it should "verify batch opening with 2 leaves (siblings share a parent)" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i + 1)).toArray
    // Open indices 2 and 3 (they are siblings → saves one sibling hash)
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(2, 3), seed)

    MerkleVerifier.verifyBatch(root, 3, Array(2, 3), leafDigests, siblings, seed) shouldBe true
  }

  it should "verify batch opening with non-sibling leaves" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i + 1)).toArray
    // Open indices 1 and 6 (not siblings)
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(1, 6), seed)

    MerkleVerifier.verifyBatch(root, 3, Array(1, 6), leafDigests, siblings, seed) shouldBe true
  }

  it should "verify batch opening with many leaves" in {
    val n = 16
    val leaves = (0 until n).map(i => leaf(i * 7 + 3)).toArray
    val openIndices = Array(0, 3, 5, 7, 12, 15)
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, openIndices, seed)

    MerkleVerifier.verifyBatch(root, 4, openIndices, leafDigests, siblings, seed) shouldBe true
  }

  it should "verify batch with adjacent pairs (maximum sibling sharing)" in {
    val n = 16
    val leaves = (0 until n).map(i => leaf(i)).toArray
    val openIndices = Array(0, 1, 4, 5, 10, 11, 14, 15)  // 4 pairs
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, openIndices, seed)

    MerkleVerifier.verifyBatch(root, 4, openIndices, leafDigests, siblings, seed) shouldBe true
  }

  // ══════════════════════════════════════════
  // Proof Size Reduction
  // ══════════════════════════════════════════

  it should "deduplicate siblings for sibling pairs" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i + 1)).toArray

    // Opening 1 leaf needs log2(8)=3 siblings
    val (_, _, sib1) = MerkleVerifier.generateBatchProof(leaves, Array(2), seed)
    sib1.length shouldBe 3  // Full path

    // Opening sibling pair (2,3) should need fewer siblings
    val (_, _, sib2) = MerkleVerifier.generateBatchProof(leaves, Array(2, 3), seed)
    sib2.length should be < sib1.length * 2  // Savings from deduplication
  }

  // ══════════════════════════════════════════
  // Edge Cases
  // ══════════════════════════════════════════

  it should "verify with minimal tree (2 leaves)" in {
    val leaves = Array(leaf(100), leaf(200))
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(0), seed)

    MerkleVerifier.verifyBatch(root, 1, Array(0), leafDigests, siblings, seed) shouldBe true
  }

  it should "verify when opening ALL leaves (no siblings needed)" in {
    val n = 4
    val leaves = (0 until n).map(i => leaf(i)).toArray
    val openIndices = Array(0, 1, 2, 3)
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, openIndices, seed)

    siblings.length shouldBe 0  // All siblings are in the active set!
    MerkleVerifier.verifyBatch(root, 2, openIndices, leafDigests, siblings, seed) shouldBe true
  }

  it should "reject proof with extra sibling" in {
    val n = 8
    val leaves = (0 until n).map(i => leaf(i)).toArray
    val (root, leafDigests, siblings) = MerkleVerifier.generateBatchProof(leaves, Array(3), seed)

    val extraSiblings = siblings :+ leaf(999)  // Add bogus sibling
    MerkleVerifier.verifyBatch(root, 3, Array(3), leafDigests, extraSiblings, seed) shouldBe false
  }
}
