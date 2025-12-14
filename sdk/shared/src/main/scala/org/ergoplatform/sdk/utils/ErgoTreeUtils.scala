package org.ergoplatform.sdk.utils

import scorex.crypto.hash.Blake2b256
import sigma.ast.ErgoTree
import sigma.ast.ErgoTree.HeaderType
import sigma.util.Extensions.BooleanOps

/** SDK level utilities and helper methods to work with ErgoTrees.
  *
  * This object provides high-level utility functions for common ErgoTree operations,
  * including header analysis, comparison without headers, and cryptographic hashing.
  *
  * @since 5.0.0
  */
object ErgoTreeUtils {

  /** Prints description of the bits in the given ErgoTree header.
    *
    * This is useful for debugging and understanding the structure of ErgoTree headers,
    * showing which flags are set and what version is being used.
    *
    * @param header the ErgoTree header byte to explain
    * @return a formatted string describing each bit in the header
    *
    * @example
    * {{{
    * val tree = ErgoTree.fromProposition(prop)
    * println(ErgoTreeUtils.explainTreeHeader(tree.header))
    * // Output:
    * // Header: 0x00 (0)
    * // Bit 0: 0 \
    * // Bit 1: 0  -- ErgoTree version 0
    * // Bit 2: 0 /
    * // Bit 3: 0  -- size of the whole tree is serialized after the header byte
    * // ...
    * }}}
    */
  def explainTreeHeader(header: HeaderType): String = {
    // convert byte to hex
    val byteToHex = (b: Byte) => f"0x${b & 0xff}%02x"
    val hasSize = ErgoTree.hasSize(header)
    s"""
      |Header: ${byteToHex(header)} (${header.toString})
      |Bit 0: ${header.toByte & 0x01} \\
      |Bit 1: ${(header.toByte & 0x02) >> 1}\t-- ErgoTree version ${ErgoTree.getVersion(header)}
      |Bit 2: ${(header.toByte & 0x04) >> 2} /
      |Bit 3: ${hasSize.toByte} \t-- size of the whole tree is serialized after the header byte
      |Bit 4: ${ErgoTree.isConstantSegregation(header).toByte} \t-- constant segregation is used for this ErgoTree
      |Bit 5: ${header.toByte & 0x20} \t-- reserved (should be 0)
      |Bit 6: ${header.toByte & 0x40} \t-- reserved (should be 0)
      |Bit 7: ${header.toByte & 0x80} \t-- header contains more than 1 byte (default == 0)
      |""".stripMargin
  }

  // ============================================================================
  // ErgoTree Comparison Without Headers
  // ============================================================================

  /** Compares two ErgoTree byte arrays without considering their header bytes.
    *
    * This function skips the first byte (header) of each ErgoTree and compares
    * the remaining bytes, which represent the actual tree logic/proposition.
    *
    * Two ErgoTrees can have identical logic but different headers due to:
    * - Different versions (bits 0-2)
    * - Constant segregation flag (bit 4)
    * - Size serialization flag (bit 3)
    * - Other header flags
    *
    * This function enables semantic comparison of ErgoTrees regardless of
    * these serialization details.
    *
    * '''Performance:''' O(n) where n is the length of the shorter array.
    * Uses optimized `java.util.Arrays.equals` for byte array comparison.
    *
    * @param tree1Bytes serialized bytes of the first ErgoTree (must be non-empty)
    * @param tree2Bytes serialized bytes of the second ErgoTree (must be non-empty)
    * @return `true` if trees have identical logic (ignoring headers), `false` otherwise
    * @throws IllegalArgumentException if either array is empty
    *
    * @example
    * {{{
    * val prop = SigmaPropConstant(pk)
    * val tree1 = ErgoTree.fromProposition(prop)              // header = 0x00
    * val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop) // header = 0x10
    *
    * // Direct comparison fails due to different headers
    * tree1.bytes == tree2.bytes  // false
    *
    * // Comparison without headers succeeds (same logic)
    * ErgoTreeUtils.compareWithoutHeader(tree1.bytes, tree2.bytes)  // true
    * }}}
    *
    * @see [[hashWithoutHeader]] for hash-based comparison
    * @see [[compareWithoutHeader(tree1:sigma\.ast\.ErgoTree,tree2:sigma\.ast\.ErgoTree)* compareWithoutHeader(ErgoTree, ErgoTree)]]
    *      for object-based comparison
    */
  def compareWithoutHeader(tree1Bytes: Array[Byte], tree2Bytes: Array[Byte]): Boolean = {
    require(tree1Bytes.nonEmpty, "First ErgoTree bytes array cannot be empty")
    require(tree2Bytes.nonEmpty, "Second ErgoTree bytes array cannot be empty")

    // Fast path: if lengths differ (excluding header), trees are different
    if (tree1Bytes.length != tree2Bytes.length) {
      return false
    }

    // Fast path: if both arrays are single-byte (header only), they're equal
    if (tree1Bytes.length == 1) {
      return true
    }

    // Skip first byte (header) and compare the rest
    // Using java.util.Arrays.equals for optimized byte array comparison
    val tree1Body = tree1Bytes.slice(1, tree1Bytes.length)
    val tree2Body = tree2Bytes.slice(1, tree2Bytes.length)

    java.util.Arrays.equals(tree1Body, tree2Body)
  }

  /** Compares two ErgoTree instances without considering their header bytes.
    *
    * This is a convenience method that extracts serialized bytes from ErgoTree
    * objects and delegates to [[compareWithoutHeader(tree1Bytes:Array[Byte],tree2Bytes:Array[Byte])* compareWithoutHeader(Array[Byte], Array[Byte])]].
    *
    * '''Performance:''' O(n) where n is the tree size. Includes overhead of
    * accessing `tree.bytes` which may trigger lazy serialization.
    *
    * @param tree1 first ErgoTree instance
    * @param tree2 second ErgoTree instance
    * @return `true` if trees have identical logic (ignoring headers), `false` otherwise
    *
    * @example
    * {{{
    * val prop = SigmaPropConstant(pk)
    * val tree1 = ErgoTree.fromProposition(prop)
    * val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)
    *
    * // Convenient object-based comparison
    * ErgoTreeUtils.compareWithoutHeader(tree1, tree2)  // true
    * }}}
    *
    * @see [[compareWithoutHeader(tree1Bytes:Array[Byte],tree2Bytes:Array[Byte])* compareWithoutHeader(Array[Byte], Array[Byte])]]
    */
  def compareWithoutHeader(tree1: ErgoTree, tree2: ErgoTree): Boolean = {
    // Fast path: if same reference, they're equal
    if (tree1 eq tree2) {
      return true
    }

    compareWithoutHeader(tree1.bytes, tree2.bytes)
  }

  /** Computes the Blake2b256 hash of ErgoTree bytes without the header.
    *
    * This function is useful for:
    * - Storing expected script hashes in smart contracts
    * - Comparing ErgoTrees by their logic hash
    * - Creating deterministic identifiers for ErgoTree logic
    *
    * The hash is computed only on the tree body (excluding the header byte),
    * making it invariant to version changes and header flag modifications.
    *
    * '''Performance:''' O(n) where n is the array length. Uses optimized
    * Blake2b256 implementation from scorex.crypto.
    *
    * @param treeBytes serialized bytes of an ErgoTree (must be non-empty)
    * @return Blake2b256 hash of the tree bytes without the header (32 bytes)
    * @throws IllegalArgumentException if array is empty
    *
    * @example
    * {{{
    * // In a smart contract, store expected script hash
    * val expectedScriptHash = ErgoTreeUtils.hashWithoutHeader(expectedTree.bytes)
    *
    * // Later, verify a box has the expected script
    * val actualHash = ErgoTreeUtils.hashWithoutHeader(box.propositionBytes)
    * require(actualHash == expectedScriptHash, "Invalid script")
    *
    * // Equivalent to manual approach (but safer and more maintainable):
    * // val manualHash = blake2b256(box.propositionBytes.slice(1, box.propositionBytes.size))
    * }}}
    *
    * @see [[hashWithoutHeader(tree:sigma\.ast\.ErgoTree)* hashWithoutHeader(ErgoTree)]]
    *      for object-based hashing
    * @see [[compareWithoutHeader]] for direct comparison without hashing
    */
  def hashWithoutHeader(treeBytes: Array[Byte]): Array[Byte] = {
    require(treeBytes.nonEmpty, "ErgoTree bytes array cannot be empty")

    // Skip first byte (header) and hash the rest
    val treeBody = if (treeBytes.length == 1) {
      // Edge case: single-byte array (header only)
      // Hash empty array to maintain consistency
      Array.empty[Byte]
    } else {
      treeBytes.slice(1, treeBytes.length)
    }

    Blake2b256.hash(treeBody)
  }

  /** Computes the Blake2b256 hash of an ErgoTree without the header.
    *
    * Convenience method for ErgoTree objects. Delegates to
    * [[hashWithoutHeader(treeBytes:Array[Byte])* hashWithoutHeader(Array[Byte])]].
    *
    * '''Performance:''' O(n) where n is the tree size. Includes overhead of
    * accessing `tree.bytes` which may trigger lazy serialization.
    *
    * @param tree ErgoTree instance
    * @return Blake2b256 hash of the tree bytes without the header (32 bytes)
    *
    * @example
    * {{{
    * val tree1 = ErgoTree.fromProposition(prop)
    * val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)
    *
    * val hash1 = ErgoTreeUtils.hashWithoutHeader(tree1)
    * val hash2 = ErgoTreeUtils.hashWithoutHeader(tree2)
    *
    * // Hashes are identical (same logic, different headers)
    * hash1 == hash2  // true
    * }}}
    *
    * @see [[hashWithoutHeader(treeBytes:Array[Byte])* hashWithoutHeader(Array[Byte])]]
    */
  def hashWithoutHeader(tree: ErgoTree): Array[Byte] = {
    hashWithoutHeader(tree.bytes)
  }

  // ============================================================================
  // Advanced Utilities
  // ============================================================================

  /** Extracts the tree body (without header) from ErgoTree bytes.
    *
    * This is a low-level utility that returns the tree body as a separate array.
    * Useful when you need to process the tree body separately from the header.
    *
    * '''Performance:''' O(n) due to array copy. Consider using comparison
    * functions directly if you only need to compare trees.
    *
    * @param treeBytes serialized bytes of an ErgoTree (must be non-empty)
    * @return tree body bytes (everything except the first byte)
    * @throws IllegalArgumentException if array is empty
    *
    * @example
    * {{{
    * val tree = ErgoTree.fromProposition(prop)
    * val header = tree.bytes(0)
    * val body = ErgoTreeUtils.extractTreeBody(tree.bytes)
    *
    * // Reconstruct: header +: body == tree.bytes
    * require((header +: body) sameElements tree.bytes)
    * }}}
    */
  def extractTreeBody(treeBytes: Array[Byte]): Array[Byte] = {
    require(treeBytes.nonEmpty, "ErgoTree bytes array cannot be empty")

    if (treeBytes.length == 1) {
      Array.empty[Byte]
    } else {
      treeBytes.slice(1, treeBytes.length)
    }
  }

  /** Checks if two ErgoTrees have the same header.
    *
    * This is useful when you want to verify that two trees not only have
    * the same logic but also the same serialization format (version, flags).
    *
    * @param tree1 first ErgoTree instance
    * @param tree2 second ErgoTree instance
    * @return `true` if headers are identical, `false` otherwise
    *
    * @example
    * {{{
    * val tree1 = ErgoTree.fromProposition(prop)
    * val tree2 = ErgoTree.withSegregation(ErgoTree.ZeroHeader, prop)
    *
    * ErgoTreeUtils.hasSameHeader(tree1, tree2)  // false (different flags)
    * ErgoTreeUtils.compareWithoutHeader(tree1, tree2)  // true (same logic)
    * }}}
    */
  def hasSameHeader(tree1: ErgoTree, tree2: ErgoTree): Boolean = {
    tree1.header == tree2.header
  }

  /** Checks if two ErgoTree byte arrays have the same header.
    *
    * @param tree1Bytes serialized bytes of the first ErgoTree (must be non-empty)
    * @param tree2Bytes serialized bytes of the second ErgoTree (must be non-empty)
    * @return `true` if headers (first byte) are identical, `false` otherwise
    * @throws IllegalArgumentException if either array is empty
    */
  def hasSameHeader(tree1Bytes: Array[Byte], tree2Bytes: Array[Byte]): Boolean = {
    require(tree1Bytes.nonEmpty, "First ErgoTree bytes array cannot be empty")
    require(tree2Bytes.nonEmpty, "Second ErgoTree bytes array cannot be empty")

    tree1Bytes(0) == tree2Bytes(0)
  }

  /** Validates that an ErgoTree byte array has a valid structure.
    *
    * Performs basic sanity checks on the serialized ErgoTree:
    * - Array is not empty
    * - Header byte is valid
    * - Length is reasonable (not suspiciously small or large)
    *
    * '''Note:''' This is a lightweight validation. For full validation,
    * use `ErgoTreeSerializer.deserializeErgoTree` which will throw if invalid.
    *
    * @param treeBytes serialized bytes to validate
    * @return `true` if basic structure appears valid, `false` otherwise
    *
    * @example
    * {{{
    * val tree = ErgoTree.fromProposition(prop)
    * ErgoTreeUtils.isValidErgoTreeBytes(tree.bytes)  // true
    * ErgoTreeUtils.isValidErgoTreeBytes(Array.empty[Byte])  // false
    * ErgoTreeUtils.isValidErgoTreeBytes(Array(0xFF.toByte))  // false (invalid header)
    * }}}
    */
  def isValidErgoTreeBytes(treeBytes: Array[Byte]): Boolean = {
    if (treeBytes.isEmpty) {
      return false
    }

    val header = treeBytes(0)

    // Check if header has valid version (0-7)
    val version = ErgoTree.getVersion(HeaderType @@ header)
    if (version < 0 || version > 7) {
      return false
    }

    // Check reserved bits are 0 (bits 5-6)
    val reservedBits = header & 0x60
    if (reservedBits != 0) {
      return false
    }

    // Minimum length check: at least header + some content
    // (single-byte trees are technically valid but unusual)
    if (treeBytes.length < 2) {
      // Allow single-byte for edge cases, but it's unusual
      return treeBytes.length == 1
    }

    // Maximum reasonable length check (prevent DoS)
    // ErgoTrees are typically < 10KB, but allow up to 100KB for safety
    if (treeBytes.length > 100 * 1024) {
      return false
    }

    true
  }
}
