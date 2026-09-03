/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2026 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

/** Merkle branch verifier — mirror of risc0-zkp
  * `verify::merkle::MerkleTreeVerifier` with the Poseidon2 hash suite.
  *
  * Version note: risc0-zkp 1.2.6 and 3.0.4 (the version the node's succinct
  * verify path links) are value-identical here — same tree shape, same
  * hashing, same transcript coupling; 3.0.4 additionally validates wire
  * digests (`HashFn::is_digest_valid`, all words `< P`) and returns
  * `ReceiptFormatError` where 1.2.6 panicked. This port follows the 3.0.4
  * semantics.
  *
  * Tree shape (upstream `MerkleTreeParams`): `rowSize` leaves (a power of
  * two), each leaf hashing `colSize` field elements; `topSize = 2^t` where
  * `t` is the largest layer index (`< layers`) with `2^t <= queries` — the
  * layer above which hashes are checked only once. Virtual node `i` has
  * children `2i` / `2i+1`; the root is node 1; the top row occupies nodes
  * `[topSize, 2*topSize)` and comes from the proof stream, nodes
  * `[1, topSize)` are recomputed from it at construction.
  *
  * Transcript protocol (this defines the Fiat-Shamir coupling, ported
  * exactly): construction reads the `topSize` top-row digests from the IOP,
  * folds them up to the root, and commits ONLY the root to the rng.
  * [[verify]] reads the row and path digests without touching the rng.
  *
  * Wire-form decisions (each traced to the upstream flow):
  *  - Digests are held and compared in RAW (Montgomery) word form — upstream
  *    compares `Digest` words directly and never decodes them.
  *  - Node hashing converts deliberately at the hash boundary: upstream
  *    `hash_pair` reinterprets raw digest words as field elements
  *    (`Elem::new_raw`) and sponges them, so [[hashPairRaw]] maps raw ->
  *    canonical ([[BabyBear.fromRaw]]) for [[Poseidon2.hashPair]] and
  *    re-encodes the digest output with [[BabyBear.toRaw]] (upstream
  *    `to_digest` stores `as_u32_montgomery`).
  *  - Leaf hashing consumes the row via `ReadIop.readFieldElemSlice`
  *    (canonical values — upstream `hash_elem_slice` sponges the elements
  *    themselves), then re-encodes the digest to raw form.
  *  - Wire digest words are validated `< P` exactly where risc0-zkp 3.0.4
  *    checks `is_digest_valid` and returns `ReceiptFormatError`: the top row
  *    (validated pairwise before `hash_pair` when `topSize > 1`) and every
  *    path digest in [[verify]]. For the `topSize == 1` corner upstream (both
  *    versions) commits the raw top word to the rng unchecked; an unreduced
  *    word there can never equal a recomputed digest word (hash outputs are
  *    reduced), so every such proof is rejected by upstream too — rejecting
  *    at construction preserves accept/reject parity while keeping the
  *    canonical-form rng state sound.
  *
  * Rejection style: `Either[String, _]`, never throwing on malformed proof
  * bytes; `require` guards only verifier-chosen parameters.
  */
final class MerkleVerifier private (
    val rowSize: Int,
    val colSize: Int,
    topSize: Int,
    top: Array[Array[Int]],
    rest: Array[Array[Int]]
) {
  import MerkleVerifier.{hashPairRaw, hashPairRawObserved, toRawOwned}

  /** Root digest in RAW word form (virtual node 1). */
  def rootRaw: Array[Int] = rootRawOwned.clone()

  /** Trusted internal view used only inside the verifier package. */
  private[stark] def rootRawOwned: Array[Int] =
    if (topSize == 1) top(0) else rest(0)

  /** Verify one branch read from `iop` against row `idx`; returns the
    * CANONICAL row values on success. Mirror of upstream `verify`: rejects
    * an out-of-range index, reads `colSize` elements (leaf), then one
    * "other" digest per level below the top row, ascending with
    * left/right order decided by the index's low bit.
    */
  def verify(iop: ReadIop, idx: Int): Either[String, Array[Int]] = {
    if (idx < 0 || idx >= rowSize)
      return Left(s"merkle query out of range: idx $idx, rows $rowSize")
    iop.readFieldElemSlice(colSize) match {
      case None => Left("merkle branch: bad row data (truncated or word >= P)")
      case Some(row) =>
        var cur = toRawOwned(Poseidon2.unpaddedHash(row))
        var i = idx + rowSize
        var failed: String = null
        while (failed == null && i >= 2 * topSize) {
          iop.readDigestRaw() match {
            case None => failed = "merkle branch: truncated path digest"
            case Some(other) =>
              if (!MerkleVerifier.allReduced(other))
                failed = "merkle branch: unreduced path digest word"
              else {
                val lowBit = i & 1
                i /= 2
                cur = if (lowBit == 1) hashPairRaw(other, cur) else hashPairRaw(cur, other)
              }
          }
        }
        if (failed != null) Left(failed)
        else {
          val present = if (i >= topSize) top(i - topSize) else rest(i - 1)
          if (java.util.Arrays.equals(present, cur)) Right(row)
          else Left("merkle branch: root path mismatch")
        }
    }
  }

  private[stark] def verify(
      iop: ReadIop,
      idx: Int,
      observer: VerifierOperationObserver): Either[String, Array[Int]] = {
    if (observer eq null) return verify(iop, idx)
    if (idx < 0 || idx >= rowSize)
      return Left(s"merkle query out of range: idx $idx, rows $rowSize")
    iop.readFieldElemSlice(colSize) match {
      case None => Left("merkle branch: bad row data (truncated or word >= P)")
      case Some(row) =>
        var cur = toRawOwned(Poseidon2.unpaddedHash(row, observer))
        var i = idx + rowSize
        var failed: String = null
        while (failed == null && i >= 2 * topSize) {
          iop.readDigestRaw() match {
            case None => failed = "merkle branch: truncated path digest"
            case Some(other) =>
              if (!MerkleVerifier.allReduced(other))
                failed = "merkle branch: unreduced path digest word"
              else {
                val lowBit = i & 1
                i /= 2
                cur = if (lowBit == 1)
                  hashPairRawObserved(
                    other,
                    cur,
                    observer,
                    VerifierOperationObserver.MerkleQueryPairHash)
                else
                  hashPairRawObserved(
                    cur,
                    other,
                    observer,
                    VerifierOperationObserver.MerkleQueryPairHash)
              }
          }
        }
        if (failed != null) Left(failed)
        else {
          val present = if (i >= topSize) top(i - topSize) else rest(i - 1)
          if (java.util.Arrays.equals(present, cur)) Right(row)
          else Left("merkle branch: root path mismatch")
        }
    }
  }
}

object MerkleVerifier {

  /** Construct by reading the top row from `iop` and committing the root —
    * upstream `MerkleTreeVerifier::new`. `rowSize` must be a power of two
    * and `colSize`/`queries` positive (verifier parameters, not proof
    * data); a malformed proof stream yields `Left`.
    */
  def create(
      iop: ReadIop,
      rowSize: Int,
      colSize: Int,
      queries: Int): Either[String, MerkleVerifier] = {
    require(rowSize > 0 && (rowSize & (rowSize - 1)) == 0, s"rowSize not a power of 2: $rowSize")
    require(colSize > 0, s"colSize must be positive: $colSize")
    require(queries > 0, s"queries must be positive: $queries")

    // Upstream MerkleTreeParams::new: the top layer is the deepest layer
    // (strictly below the leaves) of size at most `queries`.
    val layers = 31 - Integer.numberOfLeadingZeros(rowSize)
    var topLayer = 0
    var i = 1
    while (i < layers && (1 << i) <= queries) { topLayer = i; i += 1 }
    val topSize = 1 << topLayer

    iop.readPodSlice(topSize) match {
      case None => Left("merkle top row: truncated proof")
      case Some(top) =>
        if (!allDigestsReduced(top))
          Left("merkle top row: unreduced digest word")
        else {
          // Fold the top row up to the root: children of virtual node i are
          // at 2i / 2i+1; rest(i - 1) holds node i for i in [1, topSize).
          val rest = new Array[Array[Int]](topSize - 1)
          var n = topSize - 1
          while (n >= topSize / 2 && n >= 1) {
            rest(n - 1) = hashPairRaw(top(2 * n - topSize), top(2 * n + 1 - topSize))
            n -= 1
          }
          while (n >= 1) {
            rest(n - 1) = hashPairRaw(rest(2 * n - 1), rest(2 * n))
            n -= 1
          }
          val verifier = new MerkleVerifier(rowSize, colSize, topSize, top, rest)
          iop.commit(verifier.rootRawOwned)
          Right(verifier)
        }
    }
  }

  private[stark] def create(
      iop: ReadIop,
      rowSize: Int,
      colSize: Int,
      queries: Int,
      observer: VerifierOperationObserver): Either[String, MerkleVerifier] = {
    if (observer eq null) return create(iop, rowSize, colSize, queries)
    require(rowSize > 0 && (rowSize & (rowSize - 1)) == 0, s"rowSize not a power of 2: $rowSize")
    require(colSize > 0, s"colSize must be positive: $colSize")
    require(queries > 0, s"queries must be positive: $queries")

    val layers = 31 - Integer.numberOfLeadingZeros(rowSize)
    var topLayer = 0
    var i = 1
    while (i < layers && (1 << i) <= queries) { topLayer = i; i += 1 }
    val topSize = 1 << topLayer

    iop.readPodSlice(topSize) match {
      case None => Left("merkle top row: truncated proof")
      case Some(top) =>
        if (!allDigestsReduced(top))
          Left("merkle top row: unreduced digest word")
        else {
          val rest = new Array[Array[Int]](topSize - 1)
          var n = topSize - 1
          while (n >= topSize / 2 && n >= 1) {
            rest(n - 1) = hashPairRawObserved(
              top(2 * n - topSize),
              top(2 * n + 1 - topSize),
              observer,
              VerifierOperationObserver.MerkleTopPairHash)
            n -= 1
          }
          while (n >= 1) {
            rest(n - 1) = hashPairRawObserved(
              rest(2 * n - 1),
              rest(2 * n),
              observer,
              VerifierOperationObserver.MerkleTopPairHash)
            n -= 1
          }
          val verifier = new MerkleVerifier(rowSize, colSize, topSize, top, rest)
          iop.commit(verifier.rootRawOwned, observer)
          Right(verifier)
        }
    }
  }

  /** True iff every word is a reduced residue (`< P` unsigned). */
  private def allReduced(digestRaw: Array[Int]): Boolean = {
    var i = 0
    while (i < digestRaw.length) {
      val w = digestRaw(i)
      if (w < 0 || w >= BabyBear.P) return false
      i += 1
    }
    true
  }

  /** True iff every digest in a fixed top row is reduced. */
  private def allDigestsReduced(digestsRaw: Array[Array[Int]]): Boolean = {
    var i = 0
    while (i < digestsRaw.length) {
      if (!allReduced(digestsRaw(i))) return false
      i += 1
    }
    true
  }

  /** Convert a freshly allocated canonical digest to RAW form in place. */
  private def toRawOwned(digest: Array[Int]): Array[Int] = {
    var i = 0
    while (i < digest.length) {
      digest(i) = BabyBear.toRaw(digest(i))
      i += 1
    }
    digest
  }

  /** Node compression over RAW digests — upstream `hash_pair` reinterprets
    * raw words as elements and sponges them; output re-encoded to raw.
    */
  private def hashPairRaw(aRaw: Array[Int], bRaw: Array[Int]): Array[Int] = {
    require(
      aRaw.length == Poseidon2.CellsOut && bRaw.length == Poseidon2.CellsOut,
      "digests are 8 elements")
    // hashPair is one full 16-element rate block. Decode directly into the
    // 24-cell state, avoiding two map arrays and the ++ concatenation array.
    val state = new Array[Int](Poseidon2Constants.Cells)
    var i = 0
    while (i < Poseidon2.CellsOut) {
      state(i) = BabyBear.fromRaw(aRaw(i))
      state(Poseidon2.CellsOut + i) = BabyBear.fromRaw(bRaw(i))
      i += 1
    }
    Poseidon2.mix(state)
    val out = new Array[Int](Poseidon2.CellsOut)
    i = 0
    while (i < out.length) {
      out(i) = BabyBear.toRaw(state(i))
      i += 1
    }
    out
  }

  private def hashPairRawObserved(
      aRaw: Array[Int],
      bRaw: Array[Int],
      observer: VerifierOperationObserver,
      operationId: Int): Array[Int] = {
    val out = hashPairRaw(aRaw, bRaw)
    observer.onOperation(operationId)
    out
  }
}
