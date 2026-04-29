package org.ergo.stark.protocol.air

import org.ergo.stark.field.QuadraticTower
import org.ergo.stark.field.QuadraticTower.Ext16

/**
 * AIR (Algebraic Intermediate Representation) constraint evaluator interface.
 *
 * Each zkVM type implements this trait to define its transition constraints.
 * The verifier calls evaluateConstraints at the OOD point z to check that
 * the prover's claimed trace satisfies the computation integrity relation:
 *
 *   C(trace(z), trace(z·g)) == quotient(z) * Z_H(z)
 *
 * where Z_H is the vanishing polynomial of the execution domain.
 */
trait AirConstraintEvaluator {

  /** Number of trace columns (registers) */
  def numColumns: Int

  /**
   * Evaluate all AIR constraints at the OOD point.
   *
   * @param z            out-of-domain evaluation point (Ext16)
   * @param traceOod     trace evaluations at z: T_0(z), T_1(z), ...
   * @param nextTraceOod trace evaluations at z·g: T_0(z·g), T_1(z·g), ...
   * @param quotientOod  combined quotient polynomial evaluation H(z)
   * @param domainSize   size of the execution domain (for Z_H computation)
   * @return true if the constraint relation holds
   */
  def evaluateConstraints(
    z: Ext16,
    traceOod: Array[Ext16],
    nextTraceOod: Array[Ext16],
    quotientOod: Ext16,
    domainSize: Int
  ): Boolean
}

/**
 * AIR Registry: maps vmType identifiers to their constraint evaluators.
 *
 * In the EIP-0045 opcode, vmType is the first byte of the proof blob,
 * telling the verifier which constraint system to use.
 */
object AirRegistry {
  def getEvaluator(vmType: Int): AirConstraintEvaluator = vmType match {
    case 0 => new FibonacciAir()
    case _ => throw new IllegalArgumentException(s"Unsupported vmType: $vmType")
  }
}

/**
 * Fibonacci AIR: the "Hello World" of STARK proofs.
 *
 * Single trace column with the transition constraint:
 *   T(z·g) - T(z) - T(z·g^{-1}) = 0
 *
 * Simplified version for testing: uses a simpler constraint
 *   T(z·g) = T(z) + publicStep
 * where publicStep is embedded in the constraint check.
 *
 * For our integration test, we use the simplest possible AIR:
 *   Constraint: nextTrace[0] - trace[0] - trace[0] = 0
 *   i.e., T(z·g) = 2 * T(z) (doubling sequence)
 *
 * The constraint polynomial C(z) must equal H(z) * Z_H(z).
 */
class FibonacciAir extends AirConstraintEvaluator {

  override val numColumns: Int = 1

  override def evaluateConstraints(
    z: Ext16,
    traceOod: Array[Ext16],
    nextTraceOod: Array[Ext16],
    quotientOod: Ext16,
    domainSize: Int
  ): Boolean = {
    require(traceOod.length >= numColumns, s"Need >= $numColumns trace columns")
    require(nextTraceOod.length >= numColumns, s"Need >= $numColumns next trace columns")

    // Constraint: nextTrace[0] - 2 * trace[0] = 0
    // C(z) = nextTraceOod[0] - 2 * traceOod[0]
    val doubled = QuadraticTower.ext16ScalarMul(2, traceOod(0))
    val constraintEval = QuadraticTower.ext16Sub(nextTraceOod(0), doubled)

    // Vanishing polynomial: Z_H(z) = z^domainSize - 1
    val zPow = ext16PowBySquaring(z, domainSize)
    val zH = QuadraticTower.ext16Sub(zPow, QuadraticTower.EXT16_ONE)

    // Check: C(z) == H(z) * Z_H(z)
    val rhs = QuadraticTower.ext16Mul(quotientOod, zH)

    constraintEval == rhs
  }

  /** Ext16 exponentiation by squaring (for z^n where n is a positive integer) */
  private def ext16PowBySquaring(base: Ext16, exp: Int): Ext16 = {
    if (exp == 0) return QuadraticTower.EXT16_ONE
    var result = QuadraticTower.EXT16_ONE
    var b = base
    var e = exp
    while (e > 0) {
      if ((e & 1) == 1) result = QuadraticTower.ext16Mul(result, b)
      b = QuadraticTower.ext16Mul(b, b)
      e >>= 1
    }
    result
  }
}
