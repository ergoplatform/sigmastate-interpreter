/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile

import sigma.stark.BabyBear

/** Strict transport and output-prefix decoder for the first EIP-0045 RISC0
  * raw-seal profile.
  *
  * The proof is read directly from its four Ergo collection chunks. No
  * concatenated proof byte array, input-sized collection, or platform-specific
  * I/O object is created. All accepted words fit in a positive Scala `Int`:
  * word 32 is the literal outer exponent, and every other word is a reduced
  * BabyBear residue.
  */
object RawSealV1Decoder {
  final val WordCount: Int = 55667
  final val ByteCount: Int = 222668
  final val ChunkCount: Int = 4
  final val ExpectedOuterPo2: Int = 18

  /** Canonical transport partition. Chunk boundaries intentionally split
    * little-endian words after three, two, and one bytes respectively. Keep
    * the consensus copy private so callers cannot mutate it.
    */
  private val CanonicalChunkLengths: Array[Int] = Array(65535, 65535, 65535, 26063)

  /** Internal immutable-metadata bridge used by the trusted opcode runtime. */
  private[sigma] def canonicalChunkLengths: Array[Int] = CanonicalChunkLengths.clone()

  /** A stable, typed rejection reason. Consensus code must branch on these
    * values, rather than exception text.
    */
  sealed trait Failure extends Product with Serializable {
    def code: String
  }

  case object NullChunks extends Failure {
    override val code: String = "raw-seal-null-chunks"
  }

  final case class WrongChunkCount(actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-chunk-count"
  }

  final case class NullChunk(index: Int) extends Failure {
    override val code: String = "raw-seal-null-chunk"
  }

  final case class WrongChunkLength(index: Int, expected: Int, actual: Int) extends Failure {
    override val code: String = "raw-seal-wrong-chunk-length"
  }

  final case class WrongTotalLength(expected: Int, actual: Long) extends Failure {
    override val code: String = "raw-seal-wrong-total-length"
  }

  final case class UnexpectedTransportEof(byteOffset: Int) extends Failure {
    override val code: String = "raw-seal-unexpected-transport-eof"
  }

  final case class TrailingTransportBytes(byteOffset: Int) extends Failure {
    override val code: String = "raw-seal-trailing-transport-bytes"
  }

  /** `value` is the unsigned little-endian u32 value, represented as a Long. */
  final case class WordNotReduced(wordIndex: Int, value: Long) extends Failure {
    override val code: String = "raw-seal-word-not-reduced"
  }

  final case class WrongOuterPo2(expected: Int, actual: Long) extends Failure {
    override val code: String = "raw-seal-wrong-outer-po2"
  }

  final case class NonZeroRootPadding(wordIndex: Int, value: Int) extends Failure {
    override val code: String = "raw-seal-nonzero-root-padding"
  }

  final case class ClaimHalfwordOutOfRange(wordIndex: Int, value: Int) extends Failure {
    override val code: String = "raw-seal-claim-halfword-out-of-range"
  }

  /** Successfully decoded fixed-size raw seal. The words remain in their wire
    * representation; only the two recursion-output fields are Montgomery
    * decoded into byte strings.
    */
  final class Decoded private[profile] (
      sourceWords: Array[Int],
      sourceInnerControlRoot: Array[Byte],
      sourceClaimDigest: Array[Byte]) {
    private[profile] val wordsSnapshot: Array[Int] = sourceWords
    private[profile] val innerControlRootSnapshot: Array[Byte] = sourceInnerControlRoot
    private[profile] val claimDigestSnapshot: Array[Byte] = sourceClaimDigest

    /** Defensive review accessor. Consensus verification uses the owned
      * package-private snapshot directly and therefore performs no second
      * 222,668-byte copy.
      */
    def words: Array[Int] = wordsSnapshot.clone()

    def innerControlRoot: Array[Byte] = innerControlRootSnapshot.clone()

    def claimDigest: Array[Byte] = claimDigestSnapshot.clone()
  }

  /** Decode and validate the exact four-chunk raw-seal transport.
    *
    * Malformed and null inputs always produce a typed `Left`. The method does
    * not catch fatal errors and does not use exceptions for input rejection.
    */
  def decode(chunks: Array[Array[Byte]]): Either[Failure, Decoded] = {
    if (chunks == null) return Left(NullChunks)
    if (chunks.length != ChunkCount) return Left(WrongChunkCount(chunks.length))

    // Snapshot the four validated references so mutation of the outer array
    // cannot change the transport after its shape gate.
    val checkedChunks = new Array[Array[Byte]](ChunkCount)
    var total = 0L
    var chunkIndex = 0
    while (chunkIndex < ChunkCount) {
      val chunk = chunks(chunkIndex)
      if (chunk == null) return Left(NullChunk(chunkIndex))
      val expectedLength = CanonicalChunkLengths(chunkIndex)
      if (chunk.length != expectedLength)
        return Left(WrongChunkLength(chunkIndex, expectedLength, chunk.length))
      total += chunk.length.toLong
      checkedChunks(chunkIndex) = chunk
      chunkIndex += 1
    }
    if (total != ByteCount.toLong) return Left(WrongTotalLength(ByteCount, total))

    val cursor = new ChunkCursor(checkedChunks)
    val words = new Array[Int](WordCount)
    var wordIndex = 0
    while (wordIndex < WordCount) {
      val b0 = cursor.readUnsignedByte()
      if (b0 < 0) return Left(UnexpectedTransportEof(cursor.byteOffset))
      val b1 = cursor.readUnsignedByte()
      if (b1 < 0) return Left(UnexpectedTransportEof(cursor.byteOffset))
      val b2 = cursor.readUnsignedByte()
      if (b2 < 0) return Left(UnexpectedTransportEof(cursor.byteOffset))
      val b3 = cursor.readUnsignedByte()
      if (b3 < 0) return Left(UnexpectedTransportEof(cursor.byteOffset))

      val unsignedWord =
        b0.toLong |
        (b1.toLong << 8) |
        (b2.toLong << 16) |
        (b3.toLong << 24)

      if (wordIndex == 32) {
        if (unsignedWord != ExpectedOuterPo2.toLong)
          return Left(WrongOuterPo2(ExpectedOuterPo2, unsignedWord))
      } else if (unsignedWord >= BabyBear.P.toLong) {
        return Left(WordNotReduced(wordIndex, unsignedWord))
      }

      words(wordIndex) = unsignedWord.toInt
      wordIndex += 1
    }

    if (!cursor.atEnd) return Left(TrailingTransportBytes(cursor.byteOffset))

    val innerRoot = new Array[Byte](32)
    var rootWord = 0
    while (rootWord < 8) {
      val evenIndex = rootWord * 2
      val paddingIndex = evenIndex + 1
      if (words(paddingIndex) != 0)
        return Left(NonZeroRootPadding(paddingIndex, words(paddingIndex)))
      putU32Le(innerRoot, rootWord * 4, BabyBear.fromRaw(words(evenIndex)))
      rootWord += 1
    }

    val claim = new Array[Byte](32)
    var claimWord = 0
    while (claimWord < 16) {
      val wordIndex = claimWord + 16
      val decoded = BabyBear.fromRaw(words(wordIndex))
      if (decoded > 0xffff)
        return Left(ClaimHalfwordOutOfRange(wordIndex, decoded))
      putU16Le(claim, claimWord * 2, decoded)
      claimWord += 1
    }

    Right(new Decoded(words, innerRoot, claim))
  }

  private def putU32Le(target: Array[Byte], offset: Int, value: Int): Unit = {
    target(offset) = value.toByte
    target(offset + 1) = (value >>> 8).toByte
    target(offset + 2) = (value >>> 16).toByte
    target(offset + 3) = (value >>> 24).toByte
  }

  private def putU16Le(target: Array[Byte], offset: Int, value: Int): Unit = {
    target(offset) = value.toByte
    target(offset + 1) = (value >>> 8).toByte
  }

  /** Cursor over the four immutable-length chunk references. */
  private final class ChunkCursor(chunks: Array[Array[Byte]]) {
    private var chunkIndex = 0
    private var offsetInChunk = 0
    private var consumed = 0

    def byteOffset: Int = consumed

    def readUnsignedByte(): Int = {
      advancePastBoundary()
      if (chunkIndex == chunks.length) -1
      else {
        val value = chunks(chunkIndex)(offsetInChunk) & 0xff
        offsetInChunk += 1
        consumed += 1
        value
      }
    }

    def atEnd: Boolean = {
      advancePastBoundary()
      chunkIndex == chunks.length
    }

    private def advancePastBoundary(): Unit = {
      while (chunkIndex < chunks.length && offsetInChunk == chunks(chunkIndex).length) {
        chunkIndex += 1
        offsetInChunk = 0
      }
    }
  }
}
