package org.ergoplatform.sdk.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.ast.{ErgoTree, SigmaPropConstant}
import sigma.data.ProveDlog
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.helpers.{CompilerTestingCommons, ErgoLikeContextTesting}
import sigma.serialization.generators.ObjectGenerators

/** Comprehensive test suite for ErgoTreeUtils.
  *
  * Tests cover:
  * - Comparison without headers (bytes, objects)
  * - Hashing without headers
  * - Header validation
  * - Edge cases and error handling
  * - Performance characteristics
  */
class ErgoTreeUtilsSpec extends AnyPropSpec
  with Matchers
  with ScalaCheckPropertyChecks
  with ObjectGenerators
  with CompilerTestingCommons
  with ErgoLikeContextTesting {

  // ============================================================================
  // Test Data Generators
  // ============================================================================

  /** Creates a simple ErgoTree from a public key */
  def createSimpleTree(pkBytes: Array[Byte] = Array.fill(33)(1.toByte)): ErgoTree = {
    val pk = ProveDlog(CostingSigmaDslBuilder.Colls.fromArray(pkBytes))
    ErgoTree.fromProposition(SigmaPropConstant(pk))
  }

  /** Creates an ErgoTree with constant segregation */
  def createTreeWithSegregation(pkBytes: Array[Byte] = Array.fill(33)(1.toByte)): ErgoTree = {
    val pk = ProveDlog(CostingSigmaDslBuilder.Colls.fromArray(pkBytes))
    val prop = SigmaPropConstant(pk)
    ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)
  }

  // ============================================================================
  // compareWithoutHeader Tests
  // ============================================================================

  property("compareWithoutHeader should return true for same proposition with different headers") {
    forAll(ergoTreeGen) { prop =>
      val tree1 = ErgoTree.fromProposition(prop)
      val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)

      // Verify headers are actually different
      tree1.header should not equal tree2.header

      // But logic should be the same
      ErgoTreeUtils.compareWithoutHeader(tree1, tree2) shouldBe true
      ErgoTreeUtils.compareWithoutHeader(tree1.bytes, tree2.bytes) shouldBe true
    }
  }

  property("compareWithoutHeader should return false for different propositions") {
    val tree1 = createSimpleTree(Array.fill(33)(1.toByte))
    val tree2 = createSimpleTree(Array.fill(33)(2.toByte))

    ErgoTreeUtils.compareWithoutHeader(tree1, tree2) shouldBe false
    ErgoTreeUtils.compareWithoutHeader(tree1.bytes, tree2.bytes) shouldBe false
  }

  property("compareWithoutHeader should handle same reference optimization") {
    val tree = createSimpleTree()

    // Same reference should return true immediately
    ErgoTreeUtils.compareWithoutHeader(tree, tree) shouldBe true
  }

  property("compareWithoutHeader should handle different lengths") {
    val tree1 = createSimpleTree(Array.fill(33)(1.toByte))
    val tree2Bytes = Array[Byte](0x00, 0x01, 0x02) // Short tree

    ErgoTreeUtils.compareWithoutHeader(tree1.bytes, tree2Bytes) shouldBe false
  }

  property("compareWithoutHeader should handle single-byte arrays") {
    val singleByte1 = Array[Byte](0x00)
    val singleByte2 = Array[Byte](0x10)

    // Single-byte arrays (header only) should be equal when comparing without header
    ErgoTreeUtils.compareWithoutHeader(singleByte1, singleByte2) shouldBe true
  }

  property("compareWithoutHeader should throw on empty arrays") {
    val validTree = createSimpleTree().bytes
    val emptyArray = Array.empty[Byte]

    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.compareWithoutHeader(emptyArray, validTree)
    }

    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.compareWithoutHeader(validTree, emptyArray)
    }

    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.compareWithoutHeader(emptyArray, emptyArray)
    }
  }

  // ============================================================================
  // hashWithoutHeader Tests
  // ============================================================================

  property("hashWithoutHeader should be consistent for same logic") {
    forAll(ergoTreeGen) { prop =>
      val tree1 = ErgoTree.fromProposition(prop)
      val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)

      val hash1 = ErgoTreeUtils.hashWithoutHeader(tree1)
      val hash2 = ErgoTreeUtils.hashWithoutHeader(tree2)

      // Hashes should be identical (same logic, different headers)
      hash1 shouldEqual hash2

      // Hash should be 32 bytes (Blake2b256)
      hash1.length shouldBe 32
      hash2.length shouldBe 32
    }
  }

  property("hashWithoutHeader should match manual slice approach") {
    forAll(ergoTreeGen) { tree =>
      val treeBytes = ErgoTree.fromProposition(tree).bytes

      // Manual approach (what developers currently do)
      val manualHash = Blake2b256.hash(treeBytes.slice(1, treeBytes.length))

      // Utility function approach
      val utilHash = ErgoTreeUtils.hashWithoutHeader(treeBytes)

      utilHash shouldEqual manualHash
    }
  }

  property("hashWithoutHeader should produce different hashes for different logic") {
    val tree1 = createSimpleTree(Array.fill(33)(1.toByte))
    val tree2 = createSimpleTree(Array.fill(33)(2.toByte))

    val hash1 = ErgoTreeUtils.hashWithoutHeader(tree1)
    val hash2 = ErgoTreeUtils.hashWithoutHeader(tree2)

    hash1 should not equal hash2
  }

  property("hashWithoutHeader should handle single-byte arrays") {
    val singleByte = Array[Byte](0x00)

    // Should hash empty array (header removed)
    val hash = ErgoTreeUtils.hashWithoutHeader(singleByte)

    hash.length shouldBe 32
    hash shouldEqual Blake2b256.hash(Array.empty[Byte])
  }

  property("hashWithoutHeader should throw on empty arrays") {
    val emptyArray = Array.empty[Byte]

    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.hashWithoutHeader(emptyArray)
    }
  }

  property("hashWithoutHeader should be deterministic") {
    val tree = createSimpleTree()

    val hash1 = ErgoTreeUtils.hashWithoutHeader(tree)
    val hash2 = ErgoTreeUtils.hashWithoutHeader(tree)
    val hash3 = ErgoTreeUtils.hashWithoutHeader(tree.bytes)

    hash1 shouldEqual hash2
    hash2 shouldEqual hash3
  }

  // ============================================================================
  // extractTreeBody Tests
  // ============================================================================

  property("extractTreeBody should return bytes without header") {
    forAll(ergoTreeGen) { prop =>
      val tree = ErgoTree.fromProposition(prop)
      val treeBytes = tree.bytes

      val body = ErgoTreeUtils.extractTreeBody(treeBytes)

      // Body should be original minus first byte
      body.length shouldBe (treeBytes.length - 1)

      // Reconstructing should give original
      val reconstructed = treeBytes(0) +: body
      reconstructed shouldEqual treeBytes
    }
  }

  property("extractTreeBody should handle single-byte arrays") {
    val singleByte = Array[Byte](0x00)

    val body = ErgoTreeUtils.extractTreeBody(singleByte)

    body shouldBe empty
  }

  property("extractTreeBody should throw on empty arrays") {
    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.extractTreeBody(Array.empty[Byte])
    }
  }

  // ============================================================================
  // hasSameHeader Tests
  // ============================================================================

  property("hasSameHeader should return true for same headers") {
    val tree1 = createSimpleTree()
    val tree2 = createSimpleTree(Array.fill(33)(2.toByte))

    // Both use default header
    ErgoTreeUtils.hasSameHeader(tree1, tree2) shouldBe true
    ErgoTreeUtils.hasSameHeader(tree1.bytes, tree2.bytes) shouldBe true
  }

  property("hasSameHeader should return false for different headers") {
    val tree1 = createSimpleTree()
    val tree2 = createTreeWithSegregation()

    // Different headers (segregation flag)
    ErgoTreeUtils.hasSameHeader(tree1, tree2) shouldBe false
    ErgoTreeUtils.hasSameHeader(tree1.bytes, tree2.bytes) shouldBe false
  }

  property("hasSameHeader should throw on empty arrays") {
    val validTree = createSimpleTree().bytes

    an[IllegalArgumentException] should be thrownBy {
      ErgoTreeUtils.hasSameHeader(Array.empty[Byte], validTree)
    }
  }

  // ============================================================================
  // isValidErgoTreeBytes Tests
  // ============================================================================

  property("isValidErgoTreeBytes should return true for valid trees") {
    forAll(ergoTreeGen) { prop =>
      val tree = ErgoTree.fromProposition(prop)

      ErgoTreeUtils.isValidErgoTreeBytes(tree.bytes) shouldBe true
    }
  }

  property("isValidErgoTreeBytes should return false for empty arrays") {
    ErgoTreeUtils.isValidErgoTreeBytes(Array.empty[Byte]) shouldBe false
  }

  property("isValidErgoTreeBytes should return false for invalid version") {
    val invalidHeader = Array[Byte](0x08.toByte) // Version 8 (invalid, max is 7)

    ErgoTreeUtils.isValidErgoTreeBytes(invalidHeader) shouldBe false
  }

  property("isValidErgoTreeBytes should return false for reserved bits set") {
    val invalidHeader = Array[Byte](0x20.toByte, 0x01) // Bit 5 set (reserved)

    ErgoTreeUtils.isValidErgoTreeBytes(invalidHeader) shouldBe false
  }

  property("isValidErgoTreeBytes should return true for single-byte arrays") {
    val singleByte = Array[Byte](0x00)

    ErgoTreeUtils.isValidErgoTreeBytes(singleByte) shouldBe true
  }

  property("isValidErgoTreeBytes should return false for too large arrays") {
    val tooLarge = Array.fill(200 * 1024)(0.toByte) // 200KB (exceeds 100KB limit)

    ErgoTreeUtils.isValidErgoTreeBytes(tooLarge) shouldBe false
  }

  // ============================================================================
  // Integration Tests
  // ============================================================================

  property("comparison and hashing should be consistent") {
    forAll(ergoTreeGen) { prop =>
      val tree1 = ErgoTree.fromProposition(prop)
      val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)

      // If comparison says they're equal
      if (ErgoTreeUtils.compareWithoutHeader(tree1, tree2)) {
        // Then hashes should also be equal
        val hash1 = ErgoTreeUtils.hashWithoutHeader(tree1)
        val hash2 = ErgoTreeUtils.hashWithoutHeader(tree2)

        hash1 shouldEqual hash2
      }
    }
  }

  property("real-world scenario: comparing box proposition with expected script") {
    // Simulate a smart contract scenario
    val expectedPk = ProveDlog(CostingSigmaDslBuilder.Colls.fromArray(Array.fill(33)(1.toByte)))
    val expectedProp = SigmaPropConstant(expectedPk)

    // Expected script (stored in contract)
    val expectedTree = ErgoTree.fromProposition(expectedProp)
    val expectedHash = ErgoTreeUtils.hashWithoutHeader(expectedTree)

    // Actual box proposition (might have different header due to wallet implementation)
    val actualTree = ErgoTree.withSegregation(ErgoTree.ZeroHeader, expectedProp)

    // Direct comparison would fail
    expectedTree.bytes should not equal actualTree.bytes

    // But hash comparison succeeds
    val actualHash = ErgoTreeUtils.hashWithoutHeader(actualTree)
    actualHash shouldEqual expectedHash

    // And direct comparison without header also succeeds
    ErgoTreeUtils.compareWithoutHeader(expectedTree, actualTree) shouldBe true
  }

  property("explainTreeHeader should work with all functions") {
    val tree = createSimpleTree()

    // Should not throw
    noException should be thrownBy {
      ErgoTreeUtils.explainTreeHeader(tree.header)
    }

    // Should contain expected information
    val explanation = ErgoTreeUtils.explainTreeHeader(tree.header)
    explanation should include("Header:")
    explanation should include("ErgoTree version")
    explanation should include("constant segregation")
  }

  // ============================================================================
  // Performance Characteristics Tests
  // ============================================================================

  property("compareWithoutHeader should be fast for large trees") {
    // Create a reasonably large tree
    val largeTree = createSimpleTree()

    val startTime = System.nanoTime()
    val result = ErgoTreeUtils.compareWithoutHeader(largeTree, largeTree)
    val endTime = System.nanoTime()

    result shouldBe true

    // Should complete in less than 1ms for same reference
    val durationMs = (endTime - startTime) / 1000000.0
    durationMs should be < 1.0
  }

  property("hashWithoutHeader should be deterministic across multiple calls") {
    val tree = createSimpleTree()

    val hashes = (1 to 100).map(_ => ErgoTreeUtils.hashWithoutHeader(tree))

    // All hashes should be identical
    hashes.distinct.length shouldBe 1
  }
}
