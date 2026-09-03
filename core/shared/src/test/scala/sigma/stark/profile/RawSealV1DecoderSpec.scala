package sigma.stark.profile

import sigma.BaseTests
import sigma.stark.BabyBear
import sigma.stark.profile.RawSealV1Decoder._

class RawSealV1DecoderSpec extends BaseTests {
  private val CanonicalChunkLengths = Array(65535, 65535, 65535, 26063)

  test("canonical four-chunk shape decodes words across every split boundary") {
    val words = canonicalWords()
    // The three 65,535-byte boundaries divide these words 3+1, 2+2, 1+3.
    words(16383) = 0x01020304
    words(32767) = 0x11121314
    words(49151) = 0x21222324

    val chunks = encodeChunks(words)
    chunks.map(_.length) shouldBe Array(65535, 65535, 65535, 26063)

    val decoded = right(decode(chunks))
    decoded.words.length shouldBe WordCount
    decoded.words(16383) shouldBe 0x01020304
    decoded.words(32767) shouldBe 0x11121314
    decoded.words(49151) shouldBe 0x21222324
  }

  test("every chunk length variation and every trailing transport alternative is rejected") {
    val canonical = encodeChunks(canonicalWords())
    var index = 0
    while (index < ChunkCount) {
      Seq(-1, 1).foreach { delta =>
        val changed = copyChunkRefs(canonical)
        changed(index) = resized(canonical(index), canonical(index).length + delta)
        left(decode(changed)) shouldBe
          WrongChunkLength(
            index,
            CanonicalChunkLengths(index),
            CanonicalChunkLengths(index) + delta)
      }
      index += 1
    }

    val repartitioned = copyChunkRefs(canonical)
    repartitioned(0) = resized(canonical(0), canonical(0).length - 1)
    repartitioned(1) = resized(canonical(1), canonical(1).length + 1)
    left(decode(repartitioned)) shouldBe WrongChunkLength(0, 65535, 65534)

    val fifthChunk = new Array[Array[Byte]](5)
    index = 0
    while (index < ChunkCount) {
      fifthChunk(index) = canonical(index)
      index += 1
    }
    fifthChunk(4) = Array.empty[Byte]
    left(decode(fifthChunk)) shouldBe WrongChunkCount(5)
  }

  test("null outer and inner chunk references have canonical failures") {
    left(decode(null)) shouldBe NullChunks

    val chunks = encodeChunks(canonicalWords())
    chunks(2) = null
    left(decode(chunks)) shouldBe NullChunk(2)
  }

  test("little-endian words and Montgomery recursion outputs decode to exact bytes") {
    val words = canonicalWords()
    val expectedBytes = new Array[Byte](32)
    var i = 0
    while (i < expectedBytes.length) {
      expectedBytes(i) = (i + 1).toByte
      i += 1
    }

    i = 0
    while (i < 8) {
      val value =
        (expectedBytes(i * 4) & 0xff) |
        ((expectedBytes(i * 4 + 1) & 0xff) << 8) |
        ((expectedBytes(i * 4 + 2) & 0xff) << 16) |
        ((expectedBytes(i * 4 + 3) & 0xff) << 24)
      words(i * 2) = BabyBear.toRaw(value)
      i += 1
    }

    i = 0
    while (i < 16) {
      val value =
        (expectedBytes(i * 2) & 0xff) |
        ((expectedBytes(i * 2 + 1) & 0xff) << 8)
      words(16 + i) = BabyBear.toRaw(value)
      i += 1
    }
    words(33) = 0x04030201

    val decoded = right(decode(encodeChunks(words)))
    decoded.words(33) shouldBe 0x04030201
    decoded.words(0) shouldBe BabyBear.toRaw(0x04030201)
    decoded.innerControlRoot.sameElements(expectedBytes) shouldBe true
    decoded.claimDigest.sameElements(expectedBytes) shouldBe true
  }

  test("decoded review accessors cannot mutate the verifier-owned snapshots") {
    val decoded = right(decode(encodeChunks(canonicalWords())))
    val exposedWords = decoded.words
    val exposedRoot = decoded.innerControlRoot
    val exposedClaim = decoded.claimDigest

    exposedWords(32) = 99
    exposedRoot(0) = 99
    exposedClaim(0) = 99

    decoded.words(32) shouldBe ExpectedOuterPo2
    decoded.innerControlRoot(0) shouldBe 0
    decoded.claimDigest(0) shouldBe 0
  }

  test("modulus and unsigned high-bit words are rejected before Int conversion") {
    val modulus = canonicalWords()
    modulus(33) = BabyBear.P
    left(decode(encodeChunks(modulus))) shouldBe WordNotReduced(33, BabyBear.P.toLong)

    val unsigned = encodeChunks(canonicalWords())
    writeUnsignedWord(unsigned, 33, 0xffffffffL)
    left(decode(unsigned)) shouldBe WordNotReduced(33, 0xffffffffL)
  }

  test("outer po2 is a literal u32, never a Montgomery field element") {
    val words = canonicalWords()
    words(32) = BabyBear.toRaw(ExpectedOuterPo2)
    val failure = left(decode(encodeChunks(words)))
    failure shouldBe WrongOuterPo2(ExpectedOuterPo2, BabyBear.toRaw(ExpectedOuterPo2).toLong)
  }

  test("odd Poseidon2 output padding must be raw zero") {
    val words = canonicalWords()
    words(7) = BabyBear.toRaw(1)
    left(decode(encodeChunks(words))) shouldBe NonZeroRootPadding(7, BabyBear.toRaw(1))
  }

  test("a Montgomery-decoded claim halfword cannot exceed u16") {
    val words = canonicalWords()
    words(16) = BabyBear.toRaw(65536)
    left(decode(encodeChunks(words))) shouldBe ClaimHalfwordOutOfRange(16, 65536)
  }

  private def canonicalWords(): Array[Int] = {
    val words = new Array[Int](WordCount)
    words(32) = ExpectedOuterPo2
    words
  }

  private def encodeChunks(words: Array[Int]): Array[Array[Byte]] = {
    require(words.length == WordCount)
    val bytes = new Array[Byte](ByteCount)
    var wordIndex = 0
    while (wordIndex < words.length) {
      val value = words(wordIndex)
      val byteOffset = wordIndex * 4
      bytes(byteOffset) = value.toByte
      bytes(byteOffset + 1) = (value >>> 8).toByte
      bytes(byteOffset + 2) = (value >>> 16).toByte
      bytes(byteOffset + 3) = (value >>> 24).toByte
      wordIndex += 1
    }

    val chunks = new Array[Array[Byte]](ChunkCount)
    var sourceOffset = 0
    var chunkIndex = 0
    while (chunkIndex < ChunkCount) {
      val chunk = new Array[Byte](CanonicalChunkLengths(chunkIndex))
      var i = 0
      while (i < chunk.length) {
        chunk(i) = bytes(sourceOffset + i)
        i += 1
      }
      chunks(chunkIndex) = chunk
      sourceOffset += chunk.length
      chunkIndex += 1
    }
    chunks
  }

  private def writeUnsignedWord(chunks: Array[Array[Byte]], wordIndex: Int, value: Long): Unit = {
    var byteIndex = 0
    while (byteIndex < 4) {
      setTransportByte(chunks, wordIndex * 4 + byteIndex, (value >>> (byteIndex * 8)).toByte)
      byteIndex += 1
    }
  }

  private def setTransportByte(chunks: Array[Array[Byte]], absoluteOffset: Int, value: Byte): Unit = {
    var remaining = absoluteOffset
    var chunkIndex = 0
    while (remaining >= chunks(chunkIndex).length) {
      remaining -= chunks(chunkIndex).length
      chunkIndex += 1
    }
    chunks(chunkIndex)(remaining) = value
  }

  private def resized(input: Array[Byte], length: Int): Array[Byte] = {
    val result = new Array[Byte](length)
    val copyLength = math.min(input.length, length)
    var i = 0
    while (i < copyLength) {
      result(i) = input(i)
      i += 1
    }
    result
  }

  private def copyChunkRefs(input: Array[Array[Byte]]): Array[Array[Byte]] = {
    val result = new Array[Array[Byte]](input.length)
    var i = 0
    while (i < input.length) {
      result(i) = input(i)
      i += 1
    }
    result
  }

  private def right(result: Either[Failure, Decoded]): Decoded = result match {
    case Right(value) => value
    case Left(error) => fail("expected successful raw-seal decode, got " + error)
  }

  private def left(result: Either[Failure, Decoded]): Failure = result match {
    case Left(error) => error
    case Right(_) => fail("expected raw-seal rejection")
  }
}
