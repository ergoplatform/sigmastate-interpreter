/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

/** Small, platform-neutral BLAKE2b-256 implementation used only while an
  * activated STARK profile package is authenticated at node startup.
  *
  * `core` deliberately has no dependency on `scrypto`; keeping this one-shot
  * implementation in shared code avoids a JVM-only provider and preserves the
  * same bytes on Scala.js. Inputs are bounded by [[Risc0ProfilePackageLoader]]
  * before reaching this function. The implementation follows RFC 7693 with an
  * unkeyed 32-byte digest (`fanout = 1`, `depth = 1`).
  */
private[sigma] object ProfileBlake2b256 {
  final val DigestBytes: Int = 32
  private final val BlockBytes = 128

  private val Iv = Array(
    0x6a09e667f3bcc908L,
    0xbb67ae8584caa73bL,
    0x3c6ef372fe94f82bL,
    0xa54ff53a5f1d36f1L,
    0x510e527fade682d1L,
    0x9b05688c2b3e6c1fL,
    0x1f83d9abfb41bd6bL,
    0x5be0cd19137e2179L)

  private val Sigma = Array(
    Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    Array(14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3),
    Array(11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4),
    Array(7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8),
    Array(9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13),
    Array(2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9),
    Array(12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11),
    Array(13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10),
    Array(6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5),
    Array(10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0),
    Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    Array(14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3))

  def hash(input: Array[Byte]): Array[Byte] = {
    if (input == null) throw new NullPointerException("BLAKE2b input")
    val h = Iv.clone()
    // digest_length=32, key_length=0, fanout=1, depth=1.
    h(0) ^= 0x01010020L

    var offset = 0
    var count = 0L
    while (input.length - offset > BlockBytes) {
      count += BlockBytes
      compress(h, input, offset, BlockBytes, count, last = false)
      offset += BlockBytes
    }
    val remaining = input.length - offset
    count += remaining
    compress(h, input, offset, remaining, count, last = true)

    val result = new Array[Byte](DigestBytes)
    var i = 0
    while (i < DigestBytes / 8) {
      writeU64Le(result, i * 8, h(i))
      i += 1
    }
    result
  }

  private def compress(
      h: Array[Long],
      input: Array[Byte],
      offset: Int,
      length: Int,
      count: Long,
      last: Boolean): Unit = {
    val m = new Array[Long](16)
    var i = 0
    while (i < length) {
      m(i >>> 3) |= (input(offset + i) & 0xffL) << ((i & 7) * 8)
      i += 1
    }

    val v = new Array[Long](16)
    i = 0
    while (i < 8) {
      v(i) = h(i)
      v(i + 8) = Iv(i)
      i += 1
    }
    v(12) ^= count
    // All loader inputs are below 2^63 bytes, so the high counter word is 0.
    if (last) v(14) = ~v(14)

    var round = 0
    while (round < 12) {
      val s = Sigma(round)
      g(v, 0, 4, 8, 12, m(s(0)), m(s(1)))
      g(v, 1, 5, 9, 13, m(s(2)), m(s(3)))
      g(v, 2, 6, 10, 14, m(s(4)), m(s(5)))
      g(v, 3, 7, 11, 15, m(s(6)), m(s(7)))
      g(v, 0, 5, 10, 15, m(s(8)), m(s(9)))
      g(v, 1, 6, 11, 12, m(s(10)), m(s(11)))
      g(v, 2, 7, 8, 13, m(s(12)), m(s(13)))
      g(v, 3, 4, 9, 14, m(s(14)), m(s(15)))
      round += 1
    }

    i = 0
    while (i < 8) {
      h(i) ^= v(i) ^ v(i + 8)
      i += 1
    }
  }

  private def g(
      v: Array[Long],
      a: Int,
      b: Int,
      c: Int,
      d: Int,
      x: Long,
      y: Long): Unit = {
    v(a) = v(a) + v(b) + x
    v(d) = rotateRight(v(d) ^ v(a), 32)
    v(c) += v(d)
    v(b) = rotateRight(v(b) ^ v(c), 24)
    v(a) = v(a) + v(b) + y
    v(d) = rotateRight(v(d) ^ v(a), 16)
    v(c) += v(d)
    v(b) = rotateRight(v(b) ^ v(c), 63)
  }

  private def rotateRight(value: Long, bits: Int): Long =
    (value >>> bits) | (value << (64 - bits))

  private def writeU64Le(output: Array[Byte], offset: Int, value: Long): Unit = {
    var i = 0
    while (i < 8) {
      output(offset + i) = (value >>> (8 * i)).toByte
      i += 1
    }
  }
}
