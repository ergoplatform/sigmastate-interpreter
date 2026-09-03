/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

/** The BabyBear prime field, `p = 15 * 2^27 + 1 = 2013265921` — the base
  * field of the RISC0 STARK verifier profiles proposed for EIP-0045
  * (`verifyStark`).
  *
  * Elements are canonical `Int`s in `[0, P)`; all intermediate arithmetic is
  * carried in `Long`, which cannot overflow for 31-bit operands. This is a
  * straightforward reference implementation: correctness is pinned by
  * Known Answer Tests generated from RISC0's own field implementation
  * (`core/jvm/src/test/resources/stark-kats/babybear_ops.tsv`, produced by
  * `stark-kat/`), per the oracle-parity rule — performance work can come
  * later without touching the vectors.
  */
object BabyBear {
  /** Field modulus, `15 * 2^27 + 1`. */
  final val P: Int = 2013265921

  /** `a + b mod P` for canonical operands. */
  def add(a: Int, b: Int): Int = {
    val s = a.toLong + b
    (if (s >= P) s - P else s).toInt
  }

  /** `a - b mod P` for canonical operands. */
  def sub(a: Int, b: Int): Int = {
    val d = a.toLong - b
    (if (d < 0) d + P else d).toInt
  }

  /** `a * b mod P` for canonical operands. */
  def mul(a: Int, b: Int): Int = ((a.toLong * b) % P).toInt

  /** `-a mod P` for a canonical operand. */
  def neg(a: Int): Int = if (a == 0) 0 else P - a

  /** `a^e mod P` by square-and-multiply; `e >= 0`. */
  def pow(a: Int, e: Long): Int = {
    require(e >= 0, s"negative exponent: $e")
    var base = a
    var exp = e
    var acc = 1
    while (exp > 0) {
      if ((exp & 1L) == 1L) acc = mul(acc, base)
      base = mul(base, base)
      exp >>= 1
    }
    acc
  }

  /** Multiplicative inverse by Fermat: `a^(P-2)`. Undefined for zero. */
  def inv(a: Int): Int = {
    require(a != 0, "zero has no inverse")
    pow(a, P - 2L)
  }

  /** risc0-core stores BabyBear elements in Montgomery form (`R = 2^32`);
    * RISC0 digest WORDS are raw Montgomery residues (`Elem::new_raw` /
    * `as_words`). These convert between that wire form and this
    * implementation's canonical values at the digest boundary.
    */
  final val MontR: Int = ((1L << 32) % P).toInt
  val MontRInv: Int = inv(MontR)

  /** Canonical value of a raw (Montgomery) RISC0 digest word. */
  def fromRaw(w: Int): Int = mul(w, MontRInv)

  /** Raw (Montgomery) RISC0 digest word of a canonical value. */
  def toRaw(x: Int): Int = mul(x, MontR)
}
