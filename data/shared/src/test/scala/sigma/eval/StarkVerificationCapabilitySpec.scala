package sigma.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StarkVerificationCapabilitySpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

  private def digest(first: Int): Array[Byte] = {
    val out = new Array[Byte](DigestBytes)
    out(0) = first.toByte
    out
  }

  private def stubRuntime(
      id: Array[Byte],
      minBytes: Int = 100,
      maxBytes: Int = 100,
      maxPayload: Int = 16384): StarkProfileRuntime = new StarkProfileRuntime {
    override private[sigma] def profileId: Array[Byte] = id
    override private[sigma] def minProofBytes: Int = minBytes
    override private[sigma] def maxProofBytes: Int = maxBytes
    override private[sigma] def maxApplicationPayloadBytes: Int = maxPayload
    override private[sigma] def verify(
        statement: Array[Byte],
        programId: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = true
  }

  test("validates snapshot generation, transitionId, sorted entries, and overflow") {
    val id1 = digest(1)
    val id2 = digest(2)
    val chain = digest(0xaa)
    val txId = digest(0xbb)

    val Right(e1) = active(stubRuntime(id1), fixedJit = 500)
    val Right(e2) = quarantined(id2)

    val Right(snap) = snapshot(
      chain,
      starkGeneration = 2,
      maxSupportedStarkGeneration = 2,
      transitionId = txId,
      HistoricalBlockValidation,
      dispatchJit = 100,
      Vector(e1, e2))
    snap.isFutureGeneration shouldBe false
    snap.starkGeneration shouldBe 2
    snap.maxSupportedStarkGeneration shouldBe 2
    snap.transitionId shouldBe txId
    snap.isKnownProfile(id1) shouldBe true
    snap.isKnownProfile(id2) shouldBe true
    snap.isKnownProfile(digest(3)) shouldBe false

    val Right(futureSnap) = snapshot(
      chain,
      starkGeneration = 3,
      maxSupportedStarkGeneration = 2,
      transitionId = txId,
      HistoricalBlockValidation,
      dispatchJit = 100,
      Vector(e1))
    futureSnap.isFutureGeneration shouldBe true

    // Unsigned byte-order across the signed Byte boundary: 0x7f (127) < 0x80 (-128 signed).
    val id7f = digest(0x7f)
    val id80 = digest(0x80)
    val Right(e7f) = active(stubRuntime(id7f), fixedJit = 200)
    val Right(e80) = quarantined(id80)
    val Right(unsignedSnap) = snapshot(chain, 1, txId, HistoricalBlockValidation, 100, Vector(e7f, e80))
    unsignedSnap.dispatchJit shouldBe sigma.ast.JitCost(100)
    unsignedSnap.isKnownProfile(id7f) shouldBe true
    unsignedSnap.isKnownProfile(id80) shouldBe true
    e7f.isActive shouldBe true
    e7f.isQuarantined shouldBe false
    e7f.fixedJitOpt shouldBe Some(sigma.ast.JitCost(200))
    e80.isActive shouldBe false
    e80.isQuarantined shouldBe true
    e80.fixedJitOpt shouldBe None
    snapshot(chain, 1, txId, HistoricalBlockValidation, 100, Vector(e80, e7f)) shouldBe
      Left(EntriesNotStrictlySorted(1))

    // Unsorted or duplicate entries rejected.
    snapshot(chain, 1, txId, HistoricalBlockValidation, 100, Vector(e2, e1)) shouldBe
      Left(EntriesNotStrictlySorted(1))
    snapshot(chain, 1, txId, HistoricalBlockValidation, 100, Vector(e1, e1)) shouldBe
      Left(EntriesNotStrictlySorted(1))

    // Zero/negative generations and wrong transitionId rejected.
    snapshot(chain, 0, txId, HistoricalBlockValidation, 100, Vector(e1)) shouldBe
      Left(InvalidStarkGeneration("stark-generation", 0))
    snapshot(chain, 1, new Array[Byte](31), HistoricalBlockValidation, 100, Vector(e1)) shouldBe
      Left(WrongDigestLength("transition-id", 32, 31))

    // dispatchJit + fixedJit overflow rejected at snapshot construction.
    val Right(maxEntry) = active(stubRuntime(id1), fixedJit = Int.MaxValue)
    snapshot(chain, 1, txId, HistoricalBlockValidation, dispatchJit = 1, Vector(maxEntry)) shouldBe
      Left(JitCostSumOverflow(1, Int.MaxValue))
  }

  test("supports both fixed-size seals and bounded-size multiproof chunk partitioning") {
    // Fixed-size seal: 222,668 bytes => [65535, 65535, 65535, 26063]
    val Right(fixedEntry) = active(stubRuntime(digest(1), minBytes = 222668, maxBytes = 222668), 100)
    val fixedLc = fixedEntry.lifecycle.asInstanceOf[ActiveLifecycle]
    fixedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 26063)) shouldBe true
    fixedLc.isCanonicalChunkPartition(Array(65535, 65535, 65534, 26064)) shouldBe false
    fixedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 26062)) shouldBe false

    // Bounded multiproof (e.g., 128-bit STIR/WHIR 200..253,000 bytes)
    val Right(boundedEntry) = active(stubRuntime(digest(2), minBytes = 200000, maxBytes = 253000), 100)
    val boundedLc = boundedEntry.lifecycle.asInstanceOf[ActiveLifecycle]
    boundedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 40000)) shouldBe true
    boundedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 56395)) shouldBe true
    boundedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 56396)) shouldBe false
    boundedLc.isCanonicalChunkPartition(Array(65535, 65535, 65535, 3394)) shouldBe false
    boundedLc.isCanonicalChunkPartition(Array(65534, 65535, 65535, 40001)) shouldBe false
    boundedLc.isCanonicalChunkPartition(Array.emptyIntArray) shouldBe false
  }

  test("ProverReduction purpose has a distinct stable code") {
    Set(
      HistoricalBlockValidation.code,
      AdmissionValidation.code,
      CandidateConstruction.code,
      ProverReduction.code
    ).size shouldBe 4
  }
}
