/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2026 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * RISC Zero credits https://github.com/nhukc for the initial implementation.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

import BabyBear.{add => fadd, mul => fmul}
import Poseidon2Constants._

/** The Poseidon2-BabyBear width-24 permutation used by the stock RISC0
  * verifier profile's Merkle commitments (EIP-0045 `verifyStark`).
  *
  * Faithful port of risc0-zkp 1.2.6 `core::hash::poseidon2::poseidon2_mix`:
  * initial external linear layer, `RoundsHalfFull` full rounds (constants,
  * x^7 s-box on every cell, external matrix), `RoundsPartial` partial rounds
  * (single constant, x^7 on cell 0, internal matrix), `RoundsHalfFull` full
  * rounds. The external matrix multiply uses the 4x4-circulant decomposition
  * of the Poseidon2 paper (appendix B); the internal matrix has all-ones
  * off-diagonal with `MIntDiag` on the diagonal. Constants come from
  * [[Poseidon2Constants]], extracted programmatically from risc0-zkp.
  *
  * Correctness is pinned by permutation Known Answer Tests generated from
  * risc0-zkp itself (`stark-kats/poseidon2_perm.tsv`).
  */
object Poseidon2 {

  /** x^7 over BabyBear. */
  private def sbox(x: Int): Int = {
    val x2 = fmul(x, x)
    val x4 = fmul(x2, x2)
    fmul(fmul(x4, x2), x)
  }

  private def multiplyByMExt(cells: Array[Int]): Unit = {
    // Each 4-cell circulant block is independent. It is therefore safe to
    // overwrite that block as soon as its four outputs have been computed;
    // only the four column sums must survive until the second pass. Keeping
    // those sums and outputs in scalars avoids a clone, a Tuple4 and two
    // temporary arrays for every external linear layer.
    var sum0 = 0
    var sum1 = 0
    var sum2 = 0
    var sum3 = 0
    var i = 0
    while (i < Cells / 4) {
      val base = i * 4
      val x0 = cells(base)
      val x1 = cells(base + 1)
      val x2 = cells(base + 2)
      val x3 = cells(base + 3)
      val t0 = fadd(x0, x1)
      val t1 = fadd(x2, x3)
      val t2 = fadd(fmul(2, x1), t1)
      val t3 = fadd(fmul(2, x3), t0)
      val t4 = fadd(fmul(4, t1), t3)
      val t5 = fadd(fmul(4, t0), t2)
      val out0 = fadd(t3, t5)
      val out1 = t5
      val out2 = fadd(t2, t4)
      val out3 = t4

      cells(base) = out0
      cells(base + 1) = out1
      cells(base + 2) = out2
      cells(base + 3) = out3
      sum0 = fadd(sum0, out0)
      sum1 = fadd(sum1, out1)
      sum2 = fadd(sum2, out2)
      sum3 = fadd(sum3, out3)
      i += 1
    }
    i = 0
    while (i < Cells / 4) {
      val base = i * 4
      cells(base) = fadd(cells(base), sum0)
      cells(base + 1) = fadd(cells(base + 1), sum1)
      cells(base + 2) = fadd(cells(base + 2), sum2)
      cells(base + 3) = fadd(cells(base + 3), sum3)
      i += 1
    }
  }

  private def multiplyByMInt(cells: Array[Int]): Unit = {
    var sum = 0
    var i = 0
    while (i < Cells) { sum = fadd(sum, cells(i)); i += 1 }
    i = 0
    while (i < Cells) {
      cells(i) = fadd(sum, fmul(mIntDiag(i), cells(i)))
      i += 1
    }
  }

  private def fullRound(cells: Array[Int], round: Int): Unit = {
    var i = 0
    while (i < Cells) {
      cells(i) = sbox(fadd(cells(i), roundConstant(round * Cells + i)))
      i += 1
    }
    multiplyByMExt(cells)
  }

  private def partialRound(cells: Array[Int], round: Int): Unit = {
    cells(0) = sbox(fadd(cells(0), roundConstant(round * Cells)))
    multiplyByMInt(cells)
  }

  /** Digest length in field elements. */
  final val CellsOut: Int = 8

  /** Sponge rate in field elements. */
  final val CellsRate: Int = 16

  /** Unpadded sponge hash — mirror of risc0-zkp `unpadded_hash`:
    * overwrite-absorb `CellsRate` elements per block, permute, zero-pad the
    * final partial block (also hashing an empty input as one zero block);
    * digest is the first [[CellsOut]] cells. NOTE (as upstream documents):
    * collision resistance holds only among equal-length inputs.
    */
  def unpaddedHash(input: Array[Int]): Array[Int] = {
    val state = new Array[Int](Cells)
    var unmixed = 0
    var i = 0
    while (i < input.length) {
      state(unmixed) = input(i)
      unmixed += 1
      if (unmixed == CellsRate) { mix(state); unmixed = 0 }
      i += 1
    }
    if (unmixed != 0 || input.length == 0) {
      var j = unmixed
      while (j < CellsRate) { state(j) = 0; j += 1 }
      mix(state)
    }
    java.util.Arrays.copyOfRange(state, 0, CellsOut)
  }

  /** Diagnostic variant of [[unpaddedHash]]. The production `null` path
    * delegates to the allocation-stable implementation above. Events are
    * emitted only after the corresponding permutation or complete hash call
    * succeeds; observer exceptions intentionally propagate.
    */
  private[stark] def unpaddedHash(
      input: Array[Int],
      observer: VerifierOperationObserver): Array[Int] = {
    if (observer eq null) return unpaddedHash(input)

    val state = new Array[Int](Cells)
    var unmixed = 0
    var i = 0
    while (i < input.length) {
      state(unmixed) = input(i)
      unmixed += 1
      if (unmixed == CellsRate) {
        mix(state)
        observer.onOperation(VerifierOperationObserver.ContentHashPermutation)
        unmixed = 0
      }
      i += 1
    }
    if (unmixed != 0 || input.length == 0) {
      var j = unmixed
      while (j < CellsRate) { state(j) = 0; j += 1 }
      mix(state)
      observer.onOperation(VerifierOperationObserver.ContentHashPermutation)
    }
    val out = java.util.Arrays.copyOfRange(state, 0, CellsOut)
    observer.onOperation(VerifierOperationObserver.ContentHashCall)
    out
  }

  /** Merkle node compression — `unpadded_hash` of two 8-element digests
    * (RISC0 `Poseidon2HashFn.hash_pair`).
    */
  def hashPair(a: Array[Int], b: Array[Int]): Array[Int] = {
    require(a.length == CellsOut && b.length == CellsOut, "digests are 8 elements")
    // A pair is exactly one full rate block. Build the permutation state
    // directly instead of allocating the concatenated 16-element input.
    val state = new Array[Int](Cells)
    var i = 0
    while (i < CellsOut) {
      state(i) = a(i)
      state(CellsOut + i) = b(i)
      i += 1
    }
    mix(state)
    java.util.Arrays.copyOfRange(state, 0, CellsOut)
  }

  /** The raw sponge mixing function; permutes `cells` (length [[Cells]])
    * in place. Mirror of risc0-zkp `poseidon2_mix`.
    */
  def mix(cells: Array[Int]): Unit = {
    require(cells.length == Cells, s"expected $Cells cells, got ${cells.length}")
    var round = 0
    multiplyByMExt(cells)
    var i = 0
    while (i < RoundsHalfFull) { fullRound(cells, round); round += 1; i += 1 }
    i = 0
    while (i < RoundsPartial) { partialRound(cells, round); round += 1; i += 1 }
    i = 0
    while (i < RoundsHalfFull) { fullRound(cells, round); round += 1; i += 1 }
  }
}
