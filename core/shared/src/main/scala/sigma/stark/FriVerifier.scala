/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Copyright 2026 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

import BabyBear.{mul => fmul}

/** FRI low-degree-test verifier — mirror of risc0-zkp 3.0.4
  * `verify::fri::fri_verify` / `verify_query` with the Poseidon2 hash
  * suite, plus the 16-point inverse NTT it folds with (`core/ntt.rs`
  * `interpolate_ntt` + `bit_reverse`).
  *
  * Version note: risc0-zkp 1.2.6 and 3.0.4 (the version the node's
  * succinct verify path links) are value-identical here — same stream
  * layout, same fold math, same transcript coupling; 3.0.4 only turns
  * 1.2.6's panics on malformed input into `Err(VerificationError)` and
  * rearranges an internal capacity assert. This port follows the 3.0.4
  * rejection semantics (`Left`, never throw, on proof data).
  *
  * Protocol (upstream `fri_verify`, parameterized by `totCycles` = the
  * degree bound and `queries`):
  *  1. Commit phase: while `degree > FriMinDegree`, read one fold-round
  *     Merkle tree (rows = domain/16, cols = 64) via [[MerkleVerifier]]
  *     (which commits the root to the transcript), then draw the round's
  *     fold mix (`random_ext_elem`), dividing degree and domain by 16.
  *  2. Read the final polynomial (`4 * degree` elements), commit its hash.
  *  3. Query phase, per query: draw `pos = random_bits(log2(origDomain))`,
  *     obtain the DEEP-ALI `goal` from `inner(pos)`, then per round: open
  *     the 64-element column at `pos % rows`, check the goal against the
  *     column, fold (inverse-NTT the 16 ext elements, evaluate at
  *     `mix * RouRev(log2(16 * rows))^group`), and reduce the position;
  *     finally evaluate the final polynomial at
  *     `RouFwd(log2(finalDomain))^pos` and require it equal the goal.
  *
  * `inner` is the caller-supplied per-query opening of the DEEP-ALI
  * quotient (upstream's `InnerFn`); it may read from the same `iop`
  * (branch reads never touch the transcript rng, exactly upstream's
  * split).
  *
  * Wire-form conventions follow [[ReadIop]] / [[MerkleVerifier]]: Merkle
  * column values and final-poly coefficients arrive as validated CANONICAL
  * values (`readFieldElemSlice`); the fold math runs entirely in canonical
  * [[Ext4]]; the final-poly digest is re-encoded RAW ([[BabyBear.toRaw]])
  * at the commit boundary, matching upstream's `hash_elem_slice` +
  * `commit`.
  *
  * Correctness is pinned by KATs generated from risc0-zkp's own FRI
  * prover/verifier (`stark-kats/fri_kat.tsv`, produced by
  * `stark-kat/src/bin/fri_kat.rs`) and by the direct raw-seal end-to-end KAT
  * sourced from a real receipt, per the oracle-parity rule.
  */
object FriVerifier {

  /** risc0-zkp 3.0.4 `src/lib.rs:41,46`: query count of the stock profile
    * and the reciprocal of the coding rate.
    */
  final val Queries: Int = 50
  final val InvRate: Int = 4

  /** risc0-zkp 3.0.4 `src/lib.rs:48-51`: fold factor `16 = 1 << 4`. */
  final val FriFoldPo2: Int = 4
  final val FriFold: Int = 1 << FriFoldPo2

  /** risc0-zkp 3.0.4 `src/lib.rs:54` (private upstream, cited): folding
    * stops once the degree is at most this.
    */
  final val FriMinDegree: Int = 256

  /** risc0-core BabyBear roots of unity (`field/baby_bear.rs`
    * `RootsOfUnity::ROU_FWD/ROU_REV`), CANONICAL values; index = po2,
    * `MAX_ROU_PO2 = 27`. `RouFwd(k)` has order `2^k`;
    * `RouRev(k) = RouFwd(k)^-1`. Asserted equal to the crate's own
    * constants by the `rou_fwd`/`rou_rev` vectors of `fri_kat.tsv`.
    */
  private val RouFwdTable: Array[Int] = Array(
    1, 2013265920, 284861408, 1801542727, 567209306, 740045640, 918899846,
    1881002012, 1453957774, 65325759, 1538055801, 515192888, 483885487,
    157393079, 1695124103, 2005211659, 1540072241, 88064245, 1542985445,
    1269900459, 1461624142, 825701067, 682402162, 1311873874, 1164520853,
    352275361, 18769, 137)

  private val RouRevTable: Array[Int] = Array(
    1, 2013265920, 1728404513, 1592366214, 196396260, 1253260071, 72041623,
    1091445674, 145223211, 1446820157, 1030796471, 2010749425, 1827366325,
    1239938613, 246299276, 596347512, 1893145354, 246074437, 1525739923,
    1194341128, 1463599021, 704606912, 95395244, 15672543, 647517488,
    584175179, 137728885, 749463956)

  /** Package-bounded scalar access keeps the consensus tables immutable
    * while allowing the verifier and authenticated package loader to share
    * the exact compiled values.
    */
  private[stark] def RouFwd(po2: Int): Int = RouFwdTable(po2)
  private[stark] def RouRev(po2: Int): Int = RouRevTable(po2)
  private[stark] def rouFwdLength: Int = RouFwdTable.length
  private[stark] def rouRevLength: Int = RouRevTable.length
  private[stark] def rouFwdSnapshot: Array[Int] = RouFwdTable.clone()
  private[stark] def rouRevSnapshot: Array[Int] = RouRevTable.clone()

  /** One fold round's verification state (upstream `VerifyRoundInfo`):
    * `rows` = the folded domain (upstream's `domain` field), the round's
    * Merkle tree over 64-element columns, and the fold mix drawn AFTER the
    * root commit.
    */
  private final class RoundInfo(val rows: Int, val merkle: MerkleVerifier, val mix: Ext4)

  /** Test-instrumentation hook: called with `(query, round, pos, goal)`
    * immediately before round `round` processes the query
    * (`round == rounds` = the final-polynomial comparison state). The
    * default does nothing; the KAT spec uses it to pin per-round fold
    * checkpoints.
    */
  type Probe = (Int, Int, Int, Ext4) => Unit
  val NoProbe: Probe = (_, _, _, _) => ()

  /** Verify the FRI proof section of `iop` — upstream `fri_verify`.
    *
    * `totCycles` is the degree bound (`2^po2`, a verifier-validated
    * parameter, not raw proof data — the outer verifier bounds po2 before
    * calling); `inner` supplies each query's DEEP-ALI goal and may consume
    * proof words from `iop`. Returns `Left` on any malformed or
    * non-verifying proof; never throws on proof data.
    */
  def friVerify(
      iop: ReadIop,
      totCycles: Int,
      queries: Int,
      inner: Int => Either[String, Ext4],
      probe: Probe = NoProbe): Either[String, Unit] =
    friVerify(iop, totCycles, queries, inner, probe, null)

  private[stark] def friVerify(
      iop: ReadIop,
      totCycles: Int,
      queries: Int,
      inner: Int => Either[String, Ext4],
      probe: Probe,
      operationObserver: VerifierOperationObserver): Either[String, Unit] = {
    require(totCycles > 0 && (totCycles & (totCycles - 1)) == 0,
      s"totCycles not a power of 2: $totCycles")
    require(queries > 0, s"queries must be positive: $queries")

    var degree = totCycles
    val origDomain = InvRate * totCycles
    var domain = origDomain

    // Commit phase: one Merkle tree + fold mix per round.
    var rounds = List.empty[RoundInfo]
    var failed: String = null
    while (failed == null && degree > FriMinDegree) {
      val rows = domain / FriFold
      MerkleVerifier.create(iop, rows, FriFold * 4, queries, operationObserver) match {
        case Left(e) => failed = s"fri round: $e"
        case Right(merkle) =>
          rounds = new RoundInfo(
            rows,
            merkle,
            iop.randomExtElem(operationObserver)) :: rounds
          domain /= FriFold
          degree /= FriFold
      }
    }
    if (failed != null) return Left(failed)
    val roundList = rounds.reverse

    // Final polynomial: read, commit its hash.
    val finalCoeffs = iop.readFieldElemSlice(4 * degree) match {
      case None => return Left("fri final poly: truncated or word >= P")
      case Some(cs) => cs
    }
    iop.commit(
      Poseidon2.unpaddedHash(finalCoeffs, operationObserver).map(BabyBear.toRaw),
      operationObserver)
    // Natural-order ext coefficients (plane-major on the wire).
    val finalExt = new Array[Ext4](degree)
    var i = 0
    while (i < degree) {
      finalExt(i) = Ext4(
        finalCoeffs(i),
        finalCoeffs(degree + i),
        finalCoeffs(2 * degree + i),
        finalCoeffs(3 * degree + i))
      i += 1
    }
    val gen = RouFwd(log2Ceil(domain))

    // Query phase.
    val posBits = log2Ceil(origDomain)
    var q = 0
    while (q < queries) {
      var pos = iop.randomBits(posBits, operationObserver)
      var goal: Ext4 = null
      inner(pos) match {
        case Left(e) => return Left(s"fri query $q inner: $e")
        case Right(g) => goal = g
      }
      var r = 0
      var it = roundList
      while (it.nonEmpty) {
        val round = it.head
        probe(q, r, pos, goal)
        val quot = pos / round.rows
        val group = pos % round.rows
        round.merkle.verify(iop, group, operationObserver) match {
          case Left(e) => return Left(s"fri query $q round $r: $e")
          case Right(row) =>
            val dataExt = new Array[Ext4](FriFold)
            var k = 0
            while (k < FriFold) {
              dataExt(k) = Ext4(row(k), row(FriFold + k), row(2 * FriFold + k), row(3 * FriFold + k))
              k += 1
            }
            if (dataExt(quot) != goal)
              return Left(s"fri query $q round $r: goal mismatch")
            val rootPo2 = log2Ceil(FriFold * round.rows)
            val invWk = BabyBear.pow(RouRev(rootPo2), group.toLong)
            interpolateNtt(dataExt)
            bitReverse(dataExt)
            goal = polyEval(dataExt, scale(round.mix, invWk))
            pos = group
        }
        r += 1
        it = it.tail
      }
      probe(q, r, pos, goal)
      val fx = polyEval(finalExt, Ext4.fromBase(BabyBear.pow(gen, pos.toLong)))
      if (fx != goal) return Left(s"fri query $q: final poly mismatch")
      q += 1
    }
    Right(())
  }

  /** Smallest `r` with `2^r >= v` — upstream `log2_ceil`. */
  def log2Ceil(v: Int): Int = {
    var r = 0
    while ((1 << r) < v) r += 1
    r
  }

  /** Evaluate a polynomial with [[Ext4]] coefficients at `x` — upstream
    * `Verifier::poly_eval` (ascending powers).
    */
  def polyEval(coeffs: Array[Ext4], x: Ext4): Ext4 = {
    var mulX = Ext4.One
    var tot = Ext4.Zero
    var i = 0
    while (i < coeffs.length) {
      tot = tot + (coeffs(i) * mulX)
      mulX = mulX * x
      i += 1
    }
    tot
  }

  /** `e * s` for a base-field scalar `s` (upstream `ExtElem * Elem`). */
  private def scale(e: Ext4, s: Int): Ext4 =
    Ext4(fmul(e.c0, s), fmul(e.c1, s), fmul(e.c2, s), fmul(e.c3, s))

  /** In-place inverse NTT over a power-of-two-sized array — upstream
    * `core/ntt.rs interpolate_ntt` (`rev_butterfly` then divide by the
    * size). Base-field twiddles (`RouRev`), [[Ext4]] values; output
    * coefficients are in bit-reversed order (upstream pairs this with
    * [[bitReverse]]).
    */
  def interpolateNtt(io: Array[Ext4]): Unit = {
    val n = log2Ceil(io.length)
    require(io.length == (1 << n), s"size not a power of 2: ${io.length}")
    revButterfly(io, 0, n)
    val norm = BabyBear.inv(io.length % BabyBear.P)
    var i = 0
    while (i < io.length) {
      io(i) = scale(io(i), norm)
      i += 1
    }
  }

  private def revButterfly(io: Array[Ext4], off: Int, n: Int): Unit =
    if (n > 0) {
      val half = 1 << (n - 1)
      val step = RouRev(n)
      var cur = 1
      var i = 0
      while (i < half) {
        val a = io(off + i)
        val b = io(off + i + half)
        io(off + i) = a + b
        io(off + i + half) = scale(a - b, cur)
        cur = fmul(cur, step)
        i += 1
      }
      revButterfly(io, off, n - 1)
      revButterfly(io, off + half, n - 1)
    }

  /** In-place bit-reversal permutation — upstream `core/ntt.rs
    * bit_reverse`.
    */
  def bitReverse(io: Array[Ext4]): Unit = {
    val n = log2Ceil(io.length)
    require(io.length == (1 << n), s"size not a power of 2: ${io.length}")
    var i = 0
    while (i < io.length) {
      val rev = Integer.reverse(i) >>> (32 - n)
      if (i < rev) {
        val t = io(i)
        io(i) = io(rev)
        io(rev) = t
      }
      i += 1
    }
  }
}
