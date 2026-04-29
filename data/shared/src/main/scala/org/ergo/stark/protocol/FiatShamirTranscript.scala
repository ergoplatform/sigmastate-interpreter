package org.ergo.stark.protocol

import scorex.crypto.hash.Blake2b256
import org.ergo.stark.field.BabyBearField
import org.ergo.stark.field.QuadraticTower

/**
 * Fiat-Shamir Transcript (non-interactive Challenger).
 *
 * Converts an interactive STARK proof into a non-interactive one
 * by deriving all verifier challenges from a Blake2b-256 hash chain.
 *
 * Security model: QROM (Quantum Random Oracle Model).
 * All squeezed field elements use rejection sampling to eliminate
 * modular bias, ensuring uniform distribution over F_p.
 *
 * Blake2b-256 provided by scorex.crypto.hash.Blake2b256
 * (cross-platform: JVM + ScalaJS, zero BouncyCastle dependency).
 */
class FiatShamirTranscript(label: String) {

  private val P: Long = BabyBearField.P

  // Internal state: 32-byte Blake2b digest
  private var state: Array[Byte] = {
    val labelBytes = label.getBytes("UTF-8")
    Blake2b256.hash(labelBytes)
  }

  /**
   * Absorb arbitrary bytes into the transcript.
   * state = Blake2b(state ‖ data)
   */
  def absorb(data: Array[Byte]): Unit = {
    val combined = new Array[Byte](state.length + data.length)
    System.arraycopy(state, 0, combined, 0, state.length)
    System.arraycopy(data, 0, combined, state.length, data.length)
    state = Blake2b256.hash(combined)
  }

  /**
   * Absorb a single BabyBear field element (4 bytes, big-endian).
   */
  def absorbFieldElement(e: Int): Unit = {
    absorb(Array(
      ((e >> 24) & 0xFF).toByte,
      ((e >> 16) & 0xFF).toByte,
      ((e >> 8) & 0xFF).toByte,
      (e & 0xFF).toByte
    ))
  }

  /**
   * Absorb an array of BabyBear field elements.
   */
  def absorbFieldElements(elems: Array[Int]): Unit = {
    val buf = new Array[Byte](elems.length * 4)
    var i = 0
    while (i < elems.length) {
      buf(i * 4) = ((elems(i) >> 24) & 0xFF).toByte
      buf(i * 4 + 1) = ((elems(i) >> 16) & 0xFF).toByte
      buf(i * 4 + 2) = ((elems(i) >> 8) & 0xFF).toByte
      buf(i * 4 + 3) = (elems(i) & 0xFF).toByte
      i += 1
    }
    absorb(buf)
  }

  /**
   * Squeeze raw bytes from the transcript.
   * Each call advances the state: state = Blake2b(state ‖ "squeeze" ‖ counter)
   */
  private var squeezeCounter: Long = 0

  private def squeezeBytes(n: Int): Array[Byte] = {
    val result = new Array[Byte](n)
    var offset = 0
    while (offset < n) {
      // Domain separation for squeeze
      val tag = "squeeze".getBytes("UTF-8")
      // 8-byte counter
      val ctr = squeezeCounter
      val ctrBytes = Array(
        ((ctr >> 56) & 0xFF).toByte, ((ctr >> 48) & 0xFF).toByte,
        ((ctr >> 40) & 0xFF).toByte, ((ctr >> 32) & 0xFF).toByte,
        ((ctr >> 24) & 0xFF).toByte, ((ctr >> 16) & 0xFF).toByte,
        ((ctr >> 8) & 0xFF).toByte, (ctr & 0xFF).toByte
      )

      // Blake2b(state ‖ tag ‖ counter)
      val input = new Array[Byte](state.length + tag.length + 8)
      System.arraycopy(state, 0, input, 0, state.length)
      System.arraycopy(tag, 0, input, state.length, tag.length)
      System.arraycopy(ctrBytes, 0, input, state.length + tag.length, 8)
      val hash = Blake2b256.hash(input)

      squeezeCounter += 1

      // Update state
      state = hash.clone()

      // Copy usable bytes
      val toCopy = math.min(32, n - offset)
      System.arraycopy(hash, 0, result, offset, toCopy)
      offset += toCopy
    }
    result
  }

  /**
   * Squeeze a single BabyBear field element with rejection sampling.
   *
   * Reads 4 bytes (big-endian u32), masks MSB (bit 31) to get [0, 2^31),
   * then rejects if >= P. This eliminates modular bias entirely.
   */
  def squeezeFieldElement(): Int = {
    while (true) {
      val bytes = squeezeBytes(4)
      val raw = ((bytes(0) & 0xFF).toLong << 24) |
                ((bytes(1) & 0xFF).toLong << 16) |
                ((bytes(2) & 0xFF).toLong << 8) |
                (bytes(3) & 0xFF).toLong
      // Mask bit 31 to get [0, 2^31)
      val masked = raw & 0x7FFFFFFFL
      if (masked < P) {
        return masked.toInt
      }
      // Reject and try again (probability of rejection: ~6.25%)
    }
    0 // unreachable
  }

  /**
   * Squeeze an Ext16 element: 16 BabyBear elements assembled
   * into tower representation via fromMonomial.
   */
  def squeezeExt16(): QuadraticTower.Ext16 = {
    val coeffs = new Array[Int](16)
    var i = 0
    while (i < 16) {
      coeffs(i) = squeezeFieldElement()
      i += 1
    }
    QuadraticTower.fromMonomial(coeffs)
  }

  /**
   * Squeeze Q unique query indices in [0, domainSize).
   *
   * Uses rejection sampling with a Set to guarantee uniqueness.
   * Anti-grinding: re-squeezes until exactly Q distinct indices obtained.
   *
   * @param q          number of unique indices to generate
   * @param domainSize size of the domain (must be a power of 2)
   * @return sorted array of Q unique indices
   */
  def squeezeQueryIndices(q: Int, domainSize: Int): Array[Int] = {
    require(q > 0, s"q must be positive, got $q")
    require(domainSize > q, s"domainSize ($domainSize) must be > q ($q)")
    require((domainSize & (domainSize - 1)) == 0, s"domainSize must be power of 2")

    val mask = domainSize - 1
    val indices = scala.collection.mutable.LinkedHashSet.empty[Int]

    while (indices.size < q) {
      val bytes = squeezeBytes(4)
      val raw = ((bytes(0) & 0xFF) << 24) |
                ((bytes(1) & 0xFF) << 16) |
                ((bytes(2) & 0xFF) << 8) |
                (bytes(3) & 0xFF)
      val idx = raw & mask
      indices += idx
    }

    indices.toArray.sorted
  }
}

object FiatShamirTranscript {
  def apply(label: String): FiatShamirTranscript = new FiatShamirTranscript(label)
}
