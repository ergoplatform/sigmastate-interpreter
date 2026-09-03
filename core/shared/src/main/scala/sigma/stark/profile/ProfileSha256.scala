/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

/** Small one-shot SHA-256 implementation for profile-bound host data.
  *
  * Keeping this primitive in shared code avoids a provider-dependent JVM path
  * and a separate Scala.js implementation. The function never mutates its
  * input and owns every mutable buffer used by the compression function.
  */
private[profile] object ProfileSha256 {
  final val DigestBytes: Int = 32
  private final val BlockBytes = 64

  private val InitialState = Array(
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19)

  private val RoundConstants = Array(
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)

  /** Compute SHA-256 over the exact supplied byte string. */
  def hash(input: Array[Byte]): Array[Byte] = {
    if (input == null) throw new NullPointerException("SHA-256 input")

    val state = InitialState.clone()
    val schedule = new Array[Int](64)
    var offset = 0
    while (input.length - offset >= BlockBytes) {
      compress(state, input, offset, schedule)
      offset += BlockBytes
    }

    val remaining = input.length - offset
    val tailLength = if (remaining <= 55) BlockBytes else 2 * BlockBytes
    val tail = new Array[Byte](tailLength)
    copy(input, offset, tail, 0, remaining)
    tail(remaining) = 0x80.toByte
    writeU64Be(tail, tail.length - 8, input.length.toLong * 8L)

    compress(state, tail, 0, schedule)
    if (tail.length == 2 * BlockBytes)
      compress(state, tail, BlockBytes, schedule)

    val output = new Array[Byte](DigestBytes)
    var i = 0
    while (i < state.length) {
      writeU32Be(output, i * 4, state(i))
      i += 1
    }
    output
  }

  private def compress(
      state: Array[Int],
      block: Array[Byte],
      offset: Int,
      schedule: Array[Int]): Unit = {
    var i = 0
    while (i < 16) {
      schedule(i) = readU32Be(block, offset + i * 4)
      i += 1
    }
    while (i < 64) {
      val x = schedule(i - 15)
      val y = schedule(i - 2)
      val sigma0 = rotateRight(x, 7) ^ rotateRight(x, 18) ^ (x >>> 3)
      val sigma1 = rotateRight(y, 17) ^ rotateRight(y, 19) ^ (y >>> 10)
      schedule(i) = schedule(i - 16) + sigma0 + schedule(i - 7) + sigma1
      i += 1
    }

    var a = state(0)
    var b = state(1)
    var c = state(2)
    var d = state(3)
    var e = state(4)
    var f = state(5)
    var g = state(6)
    var h = state(7)

    i = 0
    while (i < 64) {
      val bigSigma1 = rotateRight(e, 6) ^ rotateRight(e, 11) ^ rotateRight(e, 25)
      val choose = (e & f) ^ (~e & g)
      val temp1 = h + bigSigma1 + choose + RoundConstants(i) + schedule(i)
      val bigSigma0 = rotateRight(a, 2) ^ rotateRight(a, 13) ^ rotateRight(a, 22)
      val majority = (a & b) ^ (a & c) ^ (b & c)
      val temp2 = bigSigma0 + majority

      h = g
      g = f
      f = e
      e = d + temp1
      d = c
      c = b
      b = a
      a = temp1 + temp2
      i += 1
    }

    state(0) += a
    state(1) += b
    state(2) += c
    state(3) += d
    state(4) += e
    state(5) += f
    state(6) += g
    state(7) += h
  }

  private def rotateRight(value: Int, bits: Int): Int =
    (value >>> bits) | (value << (32 - bits))

  private def readU32Be(input: Array[Byte], offset: Int): Int =
    ((input(offset) & 0xff) << 24) |
      ((input(offset + 1) & 0xff) << 16) |
      ((input(offset + 2) & 0xff) << 8) |
      (input(offset + 3) & 0xff)

  private def writeU32Be(output: Array[Byte], offset: Int, value: Int): Unit = {
    output(offset) = (value >>> 24).toByte
    output(offset + 1) = (value >>> 16).toByte
    output(offset + 2) = (value >>> 8).toByte
    output(offset + 3) = value.toByte
  }

  private def writeU64Be(output: Array[Byte], offset: Int, value: Long): Unit = {
    var i = 0
    while (i < 8) {
      output(offset + i) = (value >>> (56 - 8 * i)).toByte
      i += 1
    }
  }

  private def copy(
      source: Array[Byte],
      sourceOffset: Int,
      target: Array[Byte],
      targetOffset: Int,
      length: Int): Unit = {
    var i = 0
    while (i < length) {
      target(targetOffset + i) = source(sourceOffset + i)
      i += 1
    }
  }
}
