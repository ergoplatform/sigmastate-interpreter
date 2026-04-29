package org.ergo.stark.field

/**
 * Quadratic Tower Extension of BabyBear to degree 16.
 *
 * Construction (each level is a quadratic extension):
 *   Ext2:  BabyBear[x]  / (x² - 11)     where x = T^8
 *   Ext4:  Ext2[y]      / (y² - x)      where y = T^4
 *   Ext8:  Ext4[z]      / (z² - y)      where z = T^2
 *   Ext16: Ext8[w]      / (w² - z)      where w = T^1
 *
 * The full extension is isomorphic to F_p[T]/(T^16 - 11).
 *
 * Multiplication uses Karatsuba at each level (3 sub-muls instead of 4).
 * Multiplication by the non-residue constant uses dedicated O(1) functions,
 * ensuring the true cost is 121 base field multiplications per Ext16 mul
 * (3 variable muls per level × 4 levels + 1 constant-11 mul per mulByNonResidue).
 *
 * IMPORTANT - Coefficient ordering:
 *   The tower stores coefficients in "bit-reversed" order relative to the
 *   monomial basis (T^0, T^1, ..., T^15). Tower tuple index i corresponds
 *   to monomial degree bitrev_4(i). Use fromMonomial/toMonomial for
 *   interop with Rust/Plonky3 which use monomial basis.
 */
object QuadraticTower {

  import BabyBearField.{add => bAdd, sub => bSub, mul => bMul, neg => bNeg, inv => bInv, ZERO, ONE}

  // ═══════════════════════════════════════════════════════════════
  // Ext2: BabyBear[x] / (x² - 11)
  // Element: (a0, a1) represents a0 + a1·x
  // ═══════════════════════════════════════════════════════════════

  /** Irreducible polynomial residue for Ext2: x² = 11 */
  val EXT2_W: Int = 11

  /** Ext2 element: 2 BabyBear coefficients */
  type Ext2 = (Int, Int)

  val EXT2_ZERO: Ext2 = (ZERO, ZERO)
  val EXT2_ONE: Ext2 = (ONE, ZERO)

  def ext2Add(a: Ext2, b: Ext2): Ext2 =
    (bAdd(a._1, b._1), bAdd(a._2, b._2))

  def ext2Sub(a: Ext2, b: Ext2): Ext2 =
    (bSub(a._1, b._1), bSub(a._2, b._2))

  def ext2Neg(a: Ext2): Ext2 =
    (bNeg(a._1), bNeg(a._2))

  /**
   * Ext2 multiplication using Karatsuba:
   *   (a0 + a1·x)(b0 + b1·x) = (a0·b0 + 11·a1·b1) + (a0·b1 + a1·b0)·x
   *
   * Cost: 3 base muls + 1 mul by constant 11.
   */
  def ext2Mul(a: Ext2, b: Ext2): Ext2 = {
    val p0 = bMul(a._1, b._1)           // a0·b0
    val p1 = bMul(a._2, b._2)           // a1·b1
    val p2 = bMul(bAdd(a._1, a._2), bAdd(b._1, b._2))  // (a0+a1)(b0+b1)

    val c0 = bAdd(p0, bMul(EXT2_W, p1)) // a0·b0 + 11·a1·b1
    val c1 = bSub(bSub(p2, p0), p1)     // a0·b1 + a1·b0

    (c0, c1)
  }

  /**
   * Multiply by the non-residue x (the generator of Ext2 over BabyBear).
   * (a0 + a1·x) · x = a1·x² + a0·x = 11·a1 + a0·x
   * Cost: 1 base mul (by constant 11). NOT a full Karatsuba.
   */
  def ext2MulByNonResidue(a: Ext2): Ext2 =
    (bMul(EXT2_W, a._2), a._1)

  /** Scalar multiplication: s * (a0, a1) */
  def ext2ScalarMul(s: Int, a: Ext2): Ext2 =
    (bMul(s, a._1), bMul(s, a._2))

  /**
   * Ext2 inverse: (a0 + a1·x)^(-1) = (a0 - a1·x) / (a0² - 11·a1²)
   */
  def ext2Inv(a: Ext2): Ext2 = {
    val norm = bSub(bMul(a._1, a._1), bMul(EXT2_W, bMul(a._2, a._2)))
    val normInv = bInv(norm)
    (bMul(a._1, normInv), bMul(bNeg(a._2), normInv))
  }

  // ═══════════════════════════════════════════════════════════════
  // Ext4: Ext2[y] / (y² - x)
  // Non-residue W = x in Ext2 = (0, 1)
  // MulByNonResidue: (a0, a1) · W = (ext2MulByNonResidue(a1), a0)
  // ═══════════════════════════════════════════════════════════════

  type Ext4 = (Ext2, Ext2)

  val EXT4_ZERO: Ext4 = (EXT2_ZERO, EXT2_ZERO)
  val EXT4_ONE: Ext4 = (EXT2_ONE, EXT2_ZERO)

  /**
   * Multiply Ext4 element by the non-residue (which is x, the Ext2 generator).
   * (a0 + a1·y) · y = a1·y² + a0·y = a1·x + a0·y
   * = (ext2MulByNonResidue(a1), a0)    [since y² = x means a1·y² = a1·x]
   * Wait — more carefully: y² = x (the non-residue of Ext2).
   * So multiplying the hi part a1 by y² gives a1 * x = ext2MulByNonResidue(a1) in Ext2.
   * Cost: 1 base mul (propagated from ext2MulByNonResidue).
   */
  def ext4MulByNonResidue(a: Ext4): Ext4 =
    (ext2MulByNonResidue(a._2), a._1)

  def ext4Add(a: Ext4, b: Ext4): Ext4 =
    (ext2Add(a._1, b._1), ext2Add(a._2, b._2))

  def ext4Sub(a: Ext4, b: Ext4): Ext4 =
    (ext2Sub(a._1, b._1), ext2Sub(a._2, b._2))

  def ext4Neg(a: Ext4): Ext4 =
    (ext2Neg(a._1), ext2Neg(a._2))

  /** Scalar multiplication: s * Ext4 (only base field muls, no Karatsuba needed) */
  def ext4ScalarMul(s: Int, a: Ext4): Ext4 =
    (ext2ScalarMul(s, a._1), ext2ScalarMul(s, a._2))

  /** Ext4 Karatsuba multiplication. Cost: 3 Ext2 muls + 1 mulByNonResidue = 13 base muls. */
  def ext4Mul(a: Ext4, b: Ext4): Ext4 = {
    val p0 = ext2Mul(a._1, b._1)
    val p1 = ext2Mul(a._2, b._2)
    val p2 = ext2Mul(ext2Add(a._1, a._2), ext2Add(b._1, b._2))

    val c0 = ext2Add(p0, ext2MulByNonResidue(p1))  // a0·b0 + W·a1·b1
    val c1 = ext2Sub(ext2Sub(p2, p0), p1)           // a0·b1 + a1·b0

    (c0, c1)
  }

  def ext4Inv(a: Ext4): Ext4 = {
    val a0sq = ext2Mul(a._1, a._1)
    val a1sq = ext2Mul(a._2, a._2)
    val norm = ext2Sub(a0sq, ext2MulByNonResidue(a1sq))
    val normInv = ext2Inv(norm)
    (ext2Mul(a._1, normInv), ext2Neg(ext2Mul(a._2, normInv)))
  }

  // ═══════════════════════════════════════════════════════════════
  // Ext8: Ext4[z] / (z² - y)
  // Non-residue is y (the Ext4 generator)
  // ═══════════════════════════════════════════════════════════════

  type Ext8 = (Ext4, Ext4)

  val EXT8_ZERO: Ext8 = (EXT4_ZERO, EXT4_ZERO)
  val EXT8_ONE: Ext8 = (EXT4_ONE, EXT4_ZERO)

  /**
   * Multiply Ext8 element by non-residue y.
   * (a0 + a1·z) · z = a1·z² + a0·z = a1·y + a0·z
   * = (ext4MulByNonResidue(a1), a0)
   */
  def ext8MulByNonResidue(a: Ext8): Ext8 =
    (ext4MulByNonResidue(a._2), a._1)

  def ext8Add(a: Ext8, b: Ext8): Ext8 =
    (ext4Add(a._1, b._1), ext4Add(a._2, b._2))

  def ext8Sub(a: Ext8, b: Ext8): Ext8 =
    (ext4Sub(a._1, b._1), ext4Sub(a._2, b._2))

  def ext8Neg(a: Ext8): Ext8 =
    (ext4Neg(a._1), ext4Neg(a._2))

  /** Scalar multiplication: s * Ext8 */
  def ext8ScalarMul(s: Int, a: Ext8): Ext8 =
    (ext4ScalarMul(s, a._1), ext4ScalarMul(s, a._2))

  /** Ext8 Karatsuba multiplication. Cost: 3 Ext4 muls + 1 mulByNonResidue = 40 base muls. */
  def ext8Mul(a: Ext8, b: Ext8): Ext8 = {
    val p0 = ext4Mul(a._1, b._1)
    val p1 = ext4Mul(a._2, b._2)
    val p2 = ext4Mul(ext4Add(a._1, a._2), ext4Add(b._1, b._2))

    val c0 = ext4Add(p0, ext4MulByNonResidue(p1))
    val c1 = ext4Sub(ext4Sub(p2, p0), p1)

    (c0, c1)
  }

  def ext8Inv(a: Ext8): Ext8 = {
    val a0sq = ext4Mul(a._1, a._1)
    val a1sq = ext4Mul(a._2, a._2)
    val norm = ext4Sub(a0sq, ext4MulByNonResidue(a1sq))
    val normInv = ext4Inv(norm)
    (ext4Mul(a._1, normInv), ext4Neg(ext4Mul(a._2, normInv)))
  }

  // ═══════════════════════════════════════════════════════════════
  // Ext16: Ext8[w] / (w² - z)
  // Non-residue is z (the Ext8 generator)
  // ═══════════════════════════════════════════════════════════════

  type Ext16 = (Ext8, Ext8)

  val EXT16_ZERO: Ext16 = (EXT8_ZERO, EXT8_ZERO)
  val EXT16_ONE: Ext16 = (EXT8_ONE, EXT8_ZERO)

  /**
   * Multiply Ext16 element by non-residue z.
   * (a0 + a1·w) · w = a1·w² + a0·w = a1·z + a0·w
   * = (ext8MulByNonResidue(a1), a0)
   */
  def ext16MulByNonResidue(a: Ext16): Ext16 =
    (ext8MulByNonResidue(a._2), a._1)

  def ext16Add(a: Ext16, b: Ext16): Ext16 =
    (ext8Add(a._1, b._1), ext8Add(a._2, b._2))

  def ext16Sub(a: Ext16, b: Ext16): Ext16 =
    (ext8Sub(a._1, b._1), ext8Sub(a._2, b._2))

  def ext16Neg(a: Ext16): Ext16 =
    (ext8Neg(a._1), ext8Neg(a._2))

  /** Scalar multiplication: s * Ext16 (16 base muls, no Karatsuba) */
  def ext16ScalarMul(s: Int, a: Ext16): Ext16 =
    (ext8ScalarMul(s, a._1), ext8ScalarMul(s, a._2))

  /** Ext16 Karatsuba multiplication. Cost: 3 Ext8 muls + 1 mulByNonResidue = 121 base muls. */
  def ext16Mul(a: Ext16, b: Ext16): Ext16 = {
    val p0 = ext8Mul(a._1, b._1)
    val p1 = ext8Mul(a._2, b._2)
    val p2 = ext8Mul(ext8Add(a._1, a._2), ext8Add(b._1, b._2))

    val c0 = ext8Add(p0, ext8MulByNonResidue(p1))
    val c1 = ext8Sub(ext8Sub(p2, p0), p1)

    (c0, c1)
  }

  def ext16Inv(a: Ext16): Ext16 = {
    val a0sq = ext8Mul(a._1, a._1)
    val a1sq = ext8Mul(a._2, a._2)
    val norm = ext8Sub(a0sq, ext8MulByNonResidue(a1sq))
    val normInv = ext8Inv(norm)
    (ext8Mul(a._1, normInv), ext8Neg(ext8Mul(a._2, normInv)))
  }

  /**
   * Batch inversion of Ext16 elements using Montgomery's trick.
   * Computes all N inverses using only 1 Ext16 inversion + 2(N-1) Ext16 muls.
   * Critical for DEEP-ALI: reduces 70 inversions to 1.
   */
  def ext16BatchInv(elems: Array[Ext16]): Array[Ext16] = {
    val n = elems.length
    if (n == 0) return Array.empty
    if (n == 1) return Array(ext16Inv(elems(0)))

    // Forward pass: prefix products
    val prefixes = new Array[Ext16](n)
    prefixes(0) = elems(0)
    var i = 1
    while (i < n) {
      prefixes(i) = ext16Mul(prefixes(i - 1), elems(i))
      i += 1
    }

    // Single inversion of total product
    var invProduct = ext16Inv(prefixes(n - 1))

    // Backward pass: extract individual inverses
    val result = new Array[Ext16](n)
    i = n - 1
    while (i > 0) {
      result(i) = ext16Mul(invProduct, prefixes(i - 1))
      invProduct = ext16Mul(invProduct, elems(i))
      i -= 1
    }
    result(0) = invProduct

    result
  }

  // ═══════════════════════════════════════════════════════════════
  // Coefficient Ordering: Tower ↔ Monomial Basis
  //
  // The tower stores coefficients in bit-reversed order relative to
  // the monomial basis T^0, T^1, ..., T^15.
  //
  // Tower structure: w=T, z=T², y=T⁴, x=T⁸
  //   Tuple index i (4-bit) → monomial degree = bitrev₄(i)
  //
  // Internal fromFlat/toFlat use TOWER ordering (sequential tuple fill).
  // fromMonomial/toMonomial use MONOMIAL ordering (Rust/Plonky3 interop).
  // ═══════════════════════════════════════════════════════════════

  /**
   * 4-bit reversal permutation table.
   * bitReverseMap(i) = the monomial degree stored at tower tuple index i.
   * i.e., tower[i] holds the coefficient of T^{bitReverseMap(i)}.
   */
  private val bitReverseMap: Array[Int] = Array(
    0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15
  )

  /** Inverse permutation: monomialToTower(d) = tower index for monomial degree d */
  private val monomialToTower: Array[Int] = {
    val inv = new Array[Int](16)
    var i = 0
    while (i < 16) { inv(bitReverseMap(i)) = i; i += 1 }
    inv
  }

  /** Convert tower-ordered flat array to nested Ext16 (internal use) */
  def fromFlat(coeffs: Array[Int]): Ext16 = {
    require(coeffs.length == 16, s"Expected 16 coefficients, got ${coeffs.length}")
    val lo = (
      ((coeffs(0), coeffs(1)), (coeffs(2), coeffs(3))),
      ((coeffs(4), coeffs(5)), (coeffs(6), coeffs(7)))
    )
    val hi = (
      ((coeffs(8), coeffs(9)), (coeffs(10), coeffs(11))),
      ((coeffs(12), coeffs(13)), (coeffs(14), coeffs(15)))
    )
    (lo, hi)
  }

  /** Convert nested Ext16 to tower-ordered flat array (internal use) */
  def toFlat(e: Ext16): Array[Int] = {
    val ((((c0, c1), (c2, c3)), ((c4, c5), (c6, c7))),
         (((c8, c9), (c10, c11)), ((c12, c13), (c14, c15)))) = e
    Array(c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
  }

  /**
   * Convert from MONOMIAL basis (Rust/Plonky3 ordering: c0 + c1·T + c2·T² + ... + c15·T^15)
   * to nested Ext16 tower representation.
   * Applies bit-reversal permutation.
   */
  def fromMonomial(coeffs: Array[Int]): Ext16 = {
    require(coeffs.length == 16, s"Expected 16 coefficients, got ${coeffs.length}")
    val tower = new Array[Int](16)
    var i = 0
    while (i < 16) { tower(monomialToTower(i)) = coeffs(i); i += 1 }
    fromFlat(tower)
  }

  /**
   * Convert from nested Ext16 tower representation to MONOMIAL basis
   * (Rust/Plonky3 ordering: c0 + c1·T + c2·T² + ... + c15·T^15).
   * Applies bit-reversal permutation.
   */
  def toMonomial(e: Ext16): Array[Int] = {
    val tower = toFlat(e)
    val mono = new Array[Int](16)
    var i = 0
    while (i < 16) { mono(bitReverseMap(i)) = tower(i); i += 1 }
    mono
  }

  /** Embed a BabyBear scalar into Ext16 (T^0 coefficient only — same in both orderings) */
  def fromScalar(s: Int): Ext16 = {
    val flat = new Array[Int](16)
    flat(0) = s
    fromFlat(flat)
  }
}
