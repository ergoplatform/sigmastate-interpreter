package org.ergo.stark.field

/**
 * BabyBear prime field: p = 2^31 - 2^27 + 1 = 2,013,265,921
 *
 * Properties:
 *   - 31-bit prime, fits in a signed 32-bit Int
 *   - NTT-friendly: p - 1 = 2^27 × 15, so roots of unity exist up to order 2^27
 *   - Used by RISC Zero, Plonky3, SP1
 *
 * All arithmetic is performed in Long to avoid overflow during multiplication,
 * then reduced mod p. Elements are stored as Int in [0, p-1].
 */
object BabyBearField {

  /** The prime modulus: 2^31 - 2^27 + 1 */
  val P: Long = 2013265921L

  /** P as Int for storage */
  val P_INT: Int = P.toInt

  /** A generator of the multiplicative group (primitive root mod p) */
  val GENERATOR: Int = 31

  /** Zero element */
  val ZERO: Int = 0

  /** One element (multiplicative identity) */
  val ONE: Int = 1

  /**
   * Reduce a Long value to canonical form in [0, p-1].
   * Handles negative values from subtraction.
   */
  def reduce(x: Long): Int = {
    val r = x % P
    if (r < 0) (r + P).toInt else r.toInt
  }

  /** Addition: (a + b) mod p */
  def add(a: Int, b: Int): Int = {
    val sum = a.toLong + b.toLong
    if (sum >= P) (sum - P).toInt else sum.toInt
  }

  /** Subtraction: (a - b) mod p */
  def sub(a: Int, b: Int): Int = {
    val diff = a.toLong - b.toLong
    if (diff < 0) (diff + P).toInt else diff.toInt
  }

  /** Negation: (-a) mod p */
  def neg(a: Int): Int = {
    if (a == 0) 0 else (P - a.toLong).toInt
  }

  /** Multiplication: (a * b) mod p */
  def mul(a: Int, b: Int): Int = {
    reduce(a.toLong * b.toLong)
  }

  /**
   * Modular exponentiation by squaring: a^exp mod p.
   * exp must be non-negative.
   */
  def pow(a: Int, exp: Long): Int = {
    require(exp >= 0, s"Exponent must be non-negative, got $exp")
    if (exp == 0) return ONE
    if (a == 0) return ZERO

    var base = a.toLong
    var e = exp
    var result = 1L

    while (e > 0) {
      if ((e & 1) == 1) {
        result = result * base % P
      }
      base = base * base % P
      e >>= 1
    }
    result.toInt
  }

  /**
   * Multiplicative inverse using Fermat's little theorem: a^(p-2) mod p.
   * Returns 0 if a == 0 (caller should check).
   */
  def inv(a: Int): Int = {
    require(a != 0, "Cannot invert zero")
    pow(a, P - 2)
  }

  /** Division: a / b mod p */
  def div(a: Int, b: Int): Int = {
    mul(a, inv(b))
  }

  /**
   * Batch inversion using Montgomery's trick.
   * Computes inverses of all elements in the array using only ONE modular inversion
   * and O(n) multiplications. Critical for FRI folding performance.
   *
   * @param elems array of field elements (none should be zero)
   * @return array of their inverses
   */
  def batchInv(elems: Array[Int]): Array[Int] = {
    val n = elems.length
    if (n == 0) return Array.empty

    // Forward pass: compute prefix products
    val prefixes = new Array[Int](n)
    prefixes(0) = elems(0)
    var i = 1
    while (i < n) {
      prefixes(i) = mul(prefixes(i - 1), elems(i))
      i += 1
    }

    // Single inversion of the total product
    var invProduct = inv(prefixes(n - 1))

    // Backward pass: extract individual inverses
    val result = new Array[Int](n)
    i = n - 1
    while (i > 0) {
      result(i) = mul(invProduct, prefixes(i - 1))
      invProduct = mul(invProduct, elems(i))
      i -= 1
    }
    result(0) = invProduct

    result
  }

  /**
   * Returns a primitive root of unity of order 2^k.
   * Exists for k <= 27 because p - 1 = 2^27 × 15.
   */
  def rootOfUnity(k: Int): Int = {
    require(k >= 0 && k <= 27, s"Root of unity order 2^$k not supported (max 2^27)")
    // g^((p-1) / 2^k) where g is a generator of the multiplicative group
    pow(GENERATOR, (P - 1) >> k)
  }

  /** Check if element is in canonical form [0, p-1] */
  def isValid(a: Int): Boolean = a >= 0 && a.toLong < P

  /** Convert to string representation */
  def show(a: Int): String = s"BabyBear($a)"
}
