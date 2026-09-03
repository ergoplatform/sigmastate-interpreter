/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark

import BabyBear.{P, add => fadd, mul => fmul, neg => fneg, sub => fsub}

/** The degree-4 extension of BabyBear used by the RISC0 STARK verifier
  * profiles (EIP-0045): `F_p[x] / (x^4 + 11)` with coefficients
  * `c0 + c1*x + c2*x^2 + c3*x^3` (matching risc0-core's `BabyBearExtElem`
  * coefficient order).
  *
  * Reduction uses `x^4 = -11 (= P - 11)`. Correctness — including the sign
  * convention of the irreducible polynomial — is pinned by Known Answer
  * Tests generated from risc0-core's own `ExtElem`
  * (`stark-kats/ext4_ops.tsv`); inversion goes through Fermat in the
  * extension (`a^(p^4 - 2)`), a reference-simplicity choice pinned by the
  * same vectors.
  */
final case class Ext4(c0: Int, c1: Int, c2: Int, c3: Int) {

  def +(that: Ext4): Ext4 =
    Ext4(fadd(c0, that.c0), fadd(c1, that.c1), fadd(c2, that.c2), fadd(c3, that.c3))

  def -(that: Ext4): Ext4 =
    Ext4(fsub(c0, that.c0), fsub(c1, that.c1), fsub(c2, that.c2), fsub(c3, that.c3))

  def unary_- : Ext4 = Ext4(fneg(c0), fneg(c1), fneg(c2), fneg(c3))

  def *(that: Ext4): Ext4 = {
    // Schoolbook product then fold degrees 4..6 with x^4 = NBETA (= -11).
    // Keep the same schoolbook accumulation order as the array version, but
    // hold the seven coefficients in scalars. Ext4 multiplication is the
    // verifier's dominant field operation, so avoiding three arrays here is
    // material on both the JVM and Scala.js.
    val p0 = fmul(c0, that.c0)
    val p1 = fadd(fmul(c0, that.c1), fmul(c1, that.c0))
    val p2 = fadd(fadd(fmul(c0, that.c2), fmul(c1, that.c1)), fmul(c2, that.c0))
    val p3 = fadd(
      fadd(fadd(fmul(c0, that.c3), fmul(c1, that.c2)), fmul(c2, that.c1)),
      fmul(c3, that.c0))
    val p4 = fadd(fadd(fmul(c1, that.c3), fmul(c2, that.c2)), fmul(c3, that.c1))
    val p5 = fadd(fmul(c2, that.c3), fmul(c3, that.c2))
    val p6 = fmul(c3, that.c3)

    Ext4(
      fadd(p0, fmul(p4, Ext4.NBeta)),
      fadd(p1, fmul(p5, Ext4.NBeta)),
      fadd(p2, fmul(p6, Ext4.NBeta)),
      p3)
  }

  def isZero: Boolean = c0 == 0 && c1 == 0 && c2 == 0 && c3 == 0

  /** `this^e` by square-and-multiply over a non-negative BigInt exponent. */
  def pow(e: BigInt): Ext4 = {
    require(e >= 0, s"negative exponent: $e")
    var base = this
    var exp = e
    var acc = Ext4.One
    while (exp > 0) {
      if (exp.testBit(0)) acc = acc * base
      base = base * base
      exp >>= 1
    }
    acc
  }

  /** Multiplicative inverse by Fermat in the extension: `a^(p^4 - 2)`.
    * Undefined for zero.
    */
  def inv: Ext4 = {
    require(!isZero, "zero has no inverse")
    pow(Ext4.FermatExp)
  }
}

object Ext4 {
  /** `x^4 = NBeta`, i.e. `-11 mod P` (irreducible `x^4 + 11`). */
  final val NBeta: Int = P - 11

  val Zero: Ext4 = Ext4(0, 0, 0, 0)
  val One: Ext4 = Ext4(1, 0, 0, 0)

  /** `p^4 - 2`, the Fermat inversion exponent of the extension field. */
  val FermatExp: BigInt = BigInt(P).pow(4) - 2

  def fromBase(a: Int): Ext4 = Ext4(a, 0, 0, 0)
}
