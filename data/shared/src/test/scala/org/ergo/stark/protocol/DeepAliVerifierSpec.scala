package org.ergo.stark.protocol

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergo.stark.field.{BabyBearField, QuadraticTower}
import org.ergo.stark.field.QuadraticTower.Ext16
import org.ergo.stark.protocol.air.{AirRegistry, FibonacciAir}

class DeepAliVerifierSpec extends AnyFlatSpec with Matchers {

  // ══════════════════════════════════════════
  // ext16BatchInv
  // ══════════════════════════════════════════

  "ext16BatchInv" should "produce same results as individual inversions" in {
    val elems = Array.tabulate(5) { i =>
      val flat = Array.tabulate(16)(j => BabyBearField.mul(i + 1, j + 3))
      QuadraticTower.fromFlat(flat)
    }

    val batchResult = QuadraticTower.ext16BatchInv(elems)
    val individualResult = elems.map(QuadraticTower.ext16Inv)

    batchResult.length shouldBe individualResult.length
    (0 until 5).foreach { i =>
      batchResult(i) shouldBe individualResult(i)
    }
  }

  it should "handle single element" in {
    val e = QuadraticTower.fromScalar(42)
    val result = QuadraticTower.ext16BatchInv(Array(e))
    result(0) shouldBe QuadraticTower.ext16Inv(e)
  }

  it should "handle empty array" in {
    QuadraticTower.ext16BatchInv(Array.empty).length shouldBe 0
  }

  it should "satisfy a * a^(-1) = 1 for all batch elements" in {
    val elems = Array.tabulate(10) { i =>
      val flat = Array.tabulate(16)(j => BabyBearField.mul(i * 7 + 1, j + 1))
      QuadraticTower.fromFlat(flat)
    }
    val invs = QuadraticTower.ext16BatchInv(elems)

    (0 until 10).foreach { i =>
      QuadraticTower.ext16Mul(elems(i), invs(i)) shouldBe QuadraticTower.EXT16_ONE
    }
  }

  // ══════════════════════════════════════════
  // AirRegistry & FibonacciAir
  // ══════════════════════════════════════════

  "AirRegistry" should "return FibonacciAir for vmType 0" in {
    val air = AirRegistry.getEvaluator(0)
    air shouldBe a[FibonacciAir]
    air.numColumns shouldBe 1
  }

  it should "reject unknown vmType" in {
    an[IllegalArgumentException] should be thrownBy AirRegistry.getEvaluator(99)
  }

  "FibonacciAir" should "accept valid constraint (doubling)" in {
    val air = new FibonacciAir()
    val domainSize = 8

    // For the doubling constraint: nextTrace[0] = 2 * trace[0]
    // C(z) = nextTrace - 2*trace
    // If C(z) == H(z) * Z_H(z), then it's valid.

    // Pick z as a scalar for simplicity
    val z = QuadraticTower.fromScalar(5)
    val traceZ = QuadraticTower.fromScalar(10)      // T(z) = 10
    val nextTraceZ = QuadraticTower.fromScalar(20)   // T(z·g) = 20 = 2 * 10 ✓

    // C(z) = 20 - 2*10 = 0
    // H(z) * Z_H(z) must also be 0 → any H(z) works if Z_H(z) != 0, but C(z)=0
    // So we need H(z) such that H(z) * Z_H(z) = 0
    // Since Z_H(z) = z^8 - 1 = 5^8 - 1 mod p, which is non-zero,
    // H(z) must be 0.
    val quotientZ = QuadraticTower.EXT16_ZERO

    air.evaluateConstraints(
      z, Array(traceZ), Array(nextTraceZ), quotientZ, domainSize
    ) shouldBe true
  }

  it should "reject invalid constraint" in {
    val air = new FibonacciAir()
    val z = QuadraticTower.fromScalar(5)
    val traceZ = QuadraticTower.fromScalar(10)
    val nextTraceZ = QuadraticTower.fromScalar(21)  // 21 ≠ 2*10 = 20
    val quotientZ = QuadraticTower.EXT16_ZERO

    air.evaluateConstraints(
      z, Array(traceZ), Array(nextTraceZ), quotientZ, 8
    ) shouldBe false
  }

  // ══════════════════════════════════════════
  // DEEP-ALI Composition
  // ══════════════════════════════════════════

  "DeepAliVerifier.composeDeepValues" should "produce correct composition for single column" in {
    val transcript = FiatShamirTranscript("deep-test")

    // Setup: 1 trace column, 2 queries
    val numCols = 1
    val z = QuadraticTower.fromScalar(7)
    val zg = QuadraticTower.fromScalar(BabyBearField.mul(7, 3)) // arbitrary g=3

    val traceOod = Array(QuadraticTower.fromScalar(100))     // T(z)
    val nextTraceOod = Array(QuadraticTower.fromScalar(200))  // T(z·g)
    val quotientOod = QuadraticTower.fromScalar(50)           // H(z)

    val queryPoints = Array(11, 23)
    val traceAtQuery = Array(
      Array(QuadraticTower.fromScalar(110)),  // T(x_0)
      Array(QuadraticTower.fromScalar(230))   // T(x_1)
    )
    val quotientAtQuery = Array(
      QuadraticTower.fromScalar(55),   // H(x_0)
      QuadraticTower.fromScalar(65)    // H(x_1)
    )

    val result = DeepAliVerifier.composeDeepValues(
      transcript, numCols, z, zg,
      traceOod, nextTraceOod, quotientOod,
      queryPoints, traceAtQuery, quotientAtQuery
    )

    result.length shouldBe 2

    // Verify manually using a fresh transcript with same label
    val t2 = FiatShamirTranscript("deep-test")
    val alpha0 = t2.squeezeExt16()
    val beta0 = t2.squeezeExt16()
    val gamma = t2.squeezeExt16()

    // For query 0 (x=11):
    val x0Ext = QuadraticTower.fromScalar(11)
    val invXZ_0 = QuadraticTower.ext16Inv(QuadraticTower.ext16Sub(x0Ext, z))
    val invXZG_0 = QuadraticTower.ext16Inv(QuadraticTower.ext16Sub(x0Ext, zg))

    val term1 = QuadraticTower.ext16Mul(alpha0,
      QuadraticTower.ext16Mul(
        QuadraticTower.ext16Sub(traceAtQuery(0)(0), traceOod(0)),
        invXZ_0
      ))
    val term2 = QuadraticTower.ext16Mul(beta0,
      QuadraticTower.ext16Mul(
        QuadraticTower.ext16Sub(traceAtQuery(0)(0), nextTraceOod(0)),
        invXZG_0
      ))
    val term3 = QuadraticTower.ext16Mul(gamma,
      QuadraticTower.ext16Mul(
        QuadraticTower.ext16Sub(quotientAtQuery(0), quotientOod),
        invXZ_0
      ))

    val expected0 = QuadraticTower.ext16Add(term1, QuadraticTower.ext16Add(term2, term3))
    result(0) shouldBe expected0
  }

  it should "produce non-zero values for non-trivial inputs" in {
    val transcript = FiatShamirTranscript("deep-nonzero")
    val z = QuadraticTower.fromScalar(13)
    val zg = QuadraticTower.fromScalar(BabyBearField.mul(13, 5))

    val result = DeepAliVerifier.composeDeepValues(
      transcript, 1, z, zg,
      Array(QuadraticTower.fromScalar(42)),
      Array(QuadraticTower.fromScalar(84)),
      QuadraticTower.fromScalar(7),
      Array(17),
      Array(Array(QuadraticTower.fromScalar(99))),
      Array(QuadraticTower.fromScalar(33))
    )

    result.length shouldBe 1
    result(0) should not be QuadraticTower.EXT16_ZERO
  }
}
