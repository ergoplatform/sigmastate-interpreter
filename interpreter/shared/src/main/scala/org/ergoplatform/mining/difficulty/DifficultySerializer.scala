package org.ergoplatform.mining.difficulty

import java.math.BigInteger

/** Conversion between the nBits compact difficulty encoding (as stored in block
  * headers, see `Header.nBits`) and BigInt difficulty values.
  *
  * The "compact" format represents a whole number N using an unsigned 32 bit number
  * similar to a floating point format. The most significant 8 bits are the unsigned
  * exponent of base 256 (can be thought of as "number of bytes of N").
  * The lower 23 bits are the mantissa. Bit 24 (0x800000) represents the sign of N.
  * Therefore, N = (-1^sign) * mantissa * 256^(exponent-3).
  *
  * Having difficulty as a big integer (rather than the compact nBits Long) makes it
  * suitable for comparisons, e.g. in trustless mining derivatives contracts.
  *
  * Ported from the Ergo node codebase
  * (org.ergoplatform.mining.difficulty.DifficultySerializer).
  */
object DifficultySerializer {

  /** Convert nBits Long representation (as in block headers) to BigInt difficulty. */
  def fromNBits(nBits: Long): BigInt = decodeCompactBits(nBits)

  /** Convert BigInt difficulty to nBits Long representation (as in block headers). */
  def toNBits(difficulty: BigInt): Long = encodeCompactBits(difficulty)

  /** Decode nBits compact representation into a BigInt difficulty value. */
  def decodeCompactBits(compact: Long): BigInt = {
    val size: Int = (compact >> 24).toInt & 0xFF
    val bytes: Array[Byte] = new Array[Byte](4 + size)
    bytes(3) = size.toByte
    if (size >= 1) bytes(4) = ((compact >> 16) & 0xFF).toByte
    if (size >= 2) bytes(5) = ((compact >> 8) & 0xFF).toByte
    if (size >= 3) bytes(6) = (compact & 0xFF).toByte
    decodeMPI(bytes)
  }

  /** Encode a BigInt difficulty value into nBits compact representation. */
  def encodeCompactBits(requiredDifficulty: BigInt): Long = {
    val value = requiredDifficulty.bigInteger
    var result: Long = 0L
    var size: Int = value.toByteArray.length
    if (size <= 3) {
      result = value.longValue << 8 * (3 - size)
    } else {
      result = value.shiftRight(8 * (size - 3)).longValue
    }
    // The 0x00800000 bit denotes the sign.
    // Thus, if it is already set, divide the mantissa by 256 and increase the exponent.
    if ((result & 0x00800000L) != 0) {
      result >>= 8
      size += 1
    }
    result |= size << 24
    val a: Int = if (value.signum == -1) 0x00800000 else 0
    result |= a
    result
  }

  /** Parse 4 bytes of the byte array (starting at the offset) as unsigned 32-bit
    * integer in big endian format. */
  def readUint32BE(bytes: Array[Byte]): Long =
    ((bytes(0) & 0xffL) << 24) | ((bytes(1) & 0xffL) << 16) | ((bytes(2) & 0xffL) << 8) | (bytes(3) & 0xffL)

  /** MPI encoded numbers are produced by the OpenSSL BN_bn2mpi function. They consist of
    * a 4 byte big endian length field, followed by the stated number of bytes representing
    * the number in big endian format (with a sign bit).
    */
  private def decodeMPI(mpi: Array[Byte]): BigInteger = {
    val length: Int = readUint32BE(mpi).toInt
    val buf = new Array[Byte](length)
    System.arraycopy(mpi, 4, buf, 0, length)
    if (buf.length == 0) {
      BigInteger.ZERO
    } else {
      val isNegative: Boolean = (buf(0) & 0x80) == 0x80
      if (isNegative) buf(0) = (buf(0) & 0x7f).toByte
      val result: BigInteger = new BigInteger(buf)
      if (isNegative) result.negate else result
    }
  }
}
