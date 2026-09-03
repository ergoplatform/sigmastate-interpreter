/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2026 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

/** Verifier-side transcript over a RISC0 proof word stream — mirror of
  * risc0-zkp `verify::read_iop::ReadIOP` specialized to the stock succinct
  * profile (BabyBear + Poseidon2 Fiat-Shamir rng).
  *
  * Version note: risc0-zkp 1.2.6 and 3.0.4 (the version the node's succinct
  * verify path links) are value-identical here — same stream layout, same
  * conversions, same rng; 3.0.4 only turns 1.2.6's panics on malformed
  * input into `Err(VerificationError::ReceiptFormatError)`. This port
  * follows the 3.0.4 rejection semantics (`None`, never throw).
  *
  * The proof is a flat stream of u32 words (stored as bit-identical `Int`s).
  * Reads consume from the front; `commit` mixes a digest into the transcript
  * rng; `random*` draw Fiat-Shamir challenges. Reads never touch the rng and
  * the rng never consumes proof words — exactly upstream's split.
  *
  * Wire-form conversions (the Montgomery raw/canonical boundary, decided per
  * how risc0-zkp actually flows values):
  *  - [[readU32s]] returns wire words untouched (upstream `read_u32s` — no
  *    validation, words may be arbitrary u32 including `>= P`).
  *  - [[readFieldElemSlice]] returns CANONICAL values: upstream
  *    `read_field_elem_slice` is a checked cast that requires each wire word
  *    `< P` and yields the element whose Montgomery residue is that word, so
  *    each word is validated then mapped through [[BabyBear.fromRaw]]. A
  *    word `>= P` fails the cast upstream (1.2.6 panics, 3.0.4 returns
  *    `ReceiptFormatError`) — here it returns `None`.
  *  - [[readPodSlice]] returns RAW digest words with no validation
  *    (upstream `read_pod_slice::<Digest>` is an unchecked cast); digest
  *    words stay in wire form until a hash boundary consumes them.
  *
  * Malformed input (short stream, negative count) is signalled by `None` —
  * upstream 3.0.4 returns `ReceiptFormatError` on a short read (1.2.6
  * panicked); either way the verification rejects.
  */
final class ReadIop(proof: Array[Int]) {
  private[this] val rng = new Poseidon2Rng
  private[this] var pos: Int = 0

  /** Words not yet consumed. */
  def remaining: Int = proof.length - pos

  /** Read `n` raw u32 words with no validation (upstream `read_u32s`). */
  def readU32s(n: Int): Option[Array[Int]] =
    if (n < 0 || n > remaining) None
    else {
      val out = java.util.Arrays.copyOfRange(proof, pos, pos + n)
      pos += n
      Some(out)
    }

  /** Read `n` BabyBear elements, returned as CANONICAL values. Each wire
    * word is a Montgomery residue and must be `< P` (unsigned) — upstream's
    * checked cast; a word `>= P` rejects the read.
    */
  def readFieldElemSlice(n: Int): Option[Array[Int]] =
    if (n < 0 || n > remaining) None
    else {
      val start = pos
      // Preserve readU32s + checked-cast consumption semantics: a complete
      // but unreduced slice advances the proof cursor before it is rejected.
      pos += n
      val out = new Array[Int](n)
      var ok = true
      var i = 0
      while (ok && i < n) {
        val w = proof(start + i)
        // Unsigned check: any word >= 2^31 is negative as an Int and also >= P.
        if (w < 0 || w >= BabyBear.P) ok = false
        else { out(i) = BabyBear.fromRaw(w); i += 1 }
      }
      if (ok) Some(out) else None
    }

  /** Read `n` digests (8 RAW words each) with no validation — upstream
    * `read_pod_slice::<Digest>(n)`, the only pod type the verifier reads.
    */
  def readPodSlice(n: Int): Option[Array[Array[Int]]] =
    if (n < 0 || n > remaining / Poseidon2.CellsOut) None
    else {
      val out = new Array[Array[Int]](n)
      var i = 0
      while (i < n) {
        out(i) = java.util.Arrays.copyOfRange(
          proof,
          pos + i * Poseidon2.CellsOut,
          pos + (i + 1) * Poseidon2.CellsOut
        )
        i += 1
      }
      pos += n * Poseidon2.CellsOut
      Some(out)
    }

  /** Read one RAW digest without the one-element outer array allocated by
    * [[readPodSlice]]. Package-private because only Merkle path verification
    * needs this allocation-minimal specialization.
    */
  private[stark] def readDigestRaw(): Option[Array[Int]] =
    if (remaining < Poseidon2.CellsOut) None
    else {
      val out = java.util.Arrays.copyOfRange(proof, pos, pos + Poseidon2.CellsOut)
      pos += Poseidon2.CellsOut
      Some(out)
    }

  /** Mix a digest (RAW words, as on the wire) into the transcript rng.
    * Words must be reduced (`< P`): every digest this verifier commits is
    * either a Poseidon2 output (reduced by construction) or a wire digest
    * already validated by [[MerkleVerifier]].
    */
  def commit(digestRaw: Array[Int]): Unit = rng.mix(digestRaw)

  private[stark] def commit(
      digestRaw: Array[Int],
      observer: VerifierOperationObserver): Unit = rng.mix(digestRaw, observer)

  /** True iff the entire proof stream has been consumed (upstream
    * `verify_complete` — an assert in 1.2.6, `Err(ReceiptFormatError)` in
    * 3.0.4 when words remain).
    */
  def verifyComplete: Boolean = pos == proof.length

  /** Fiat-Shamir challenge draws — delegation to [[Poseidon2Rng]];
    * canonical values.
    */
  def randomElem(): Int = rng.randomElem()

  private[stark] def randomElem(observer: VerifierOperationObserver): Int =
    rng.randomElem(observer)

  /** Cryptographically uniform `bits`-bit value (upstream `random_bits`). */
  def randomBits(bits: Int): Int = rng.randomBits(bits)

  private[stark] def randomBits(
      bits: Int,
      observer: VerifierOperationObserver): Int = rng.randomBits(bits, observer)

  /** Uniform extension-field challenge (upstream `random_ext_elem`). */
  def randomExtElem(): Ext4 = rng.randomExtElem()

  private[stark] def randomExtElem(observer: VerifierOperationObserver): Ext4 =
    rng.randomExtElem(observer)
}
