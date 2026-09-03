package sigma.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StarkVerificationCapabilitySpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

  private final class TestRuntime(
      sourceProfileId: Array[Byte],
      sourceChunkLengths: Array[Int],
      override private[sigma] val exactProofBytes: Int,
      override private[sigma] val maxApplicationPayloadBytes: Int)
      extends StarkProfileRuntime {
    private val id = sourceProfileId
    private val chunks = sourceChunkLengths
    override private[sigma] def profileId: Array[Byte] = id
    override private[sigma] def canonicalProofChunkLengths: Array[Int] = chunks
    override private[sigma] def verify(
        chainDomainId: Array[Byte],
        programId: Array[Byte],
        contractId: Array[Byte],
        applicationPayload: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = true
  }

  private def id(first: Int): Array[Byte] = {
    val result = new Array[Byte](ProfileIdBytes)
    result(0) = first.toByte
    result
  }

  private def right[A](value: Either[ConstructionFailure, A]): A = value match {
    case Right(result) => result
    case Left(failure) => fail("unexpected capability rejection: " + failure)
  }

  test("active entry snapshots all mutable runtime shape metadata") {
    val runtimeId = id(1)
    val lengths = Array(2, 3)
    val runtime = new TestRuntime(runtimeId, lengths, 5, 7)
    val entry = right(active(runtime, fixedJit = 200))
    runtimeId(0) = 9
    lengths(0) = 4

    entry.profileId shouldBe id(1)
    val lifecycle = entry.lifecycle.asInstanceOf[ActiveLifecycle]
    lifecycle.canonicalProofChunkLengths shouldBe Array(2, 3)
    val exposed = lifecycle.canonicalProofChunkLengths
    exposed(0) = 99
    lifecycle.canonicalProofChunkLengths shouldBe Array(2, 3)
    lifecycle.maxApplicationPayloadBytes shouldBe 7
    lifecycle.fixedJit.value shouldBe 200
  }

  test("runtime metadata is bounded and internally consistent") {
    active(new TestRuntime(id(1), Array(2, 3), 4, 7), 1) shouldBe
      Left(RuntimeMetadataRejected(
        "exact-proof-bytes",
        "does not equal the canonical chunk-length sum"))
    active(new TestRuntime(id(1), Array(0), 1, 7), 1) shouldBe
      Left(RuntimeMetadataRejected(
        "canonical-proof-chunk-lengths[0]",
        "chunk length must be in 1..65535"))
    active(new TestRuntime(id(1), Array(1), 1, -1), 1) shouldBe
      Left(RuntimeMetadataRejected(
        "max-application-payload-bytes",
        "must be nonnegative"))
    active(new TestRuntime(
      id(1),
      Array(1),
      1,
      MaxApplicationPayloadBytes + 1), 1) shouldBe
      Left(RuntimeMetadataRejected(
        "max-application-payload-bytes",
        s"must not exceed $MaxApplicationPayloadBytes"))
    right(active(new TestRuntime(
      id(1),
      Array(1),
      1,
      MaxApplicationPayloadBytes), 1))
      .lifecycle.asInstanceOf[ActiveLifecycle]
      .maxApplicationPayloadBytes shouldBe MaxApplicationPayloadBytes
    active(new TestRuntime(Array[Byte](1), Array(1), 1, 0), 1) shouldBe
      Left(WrongDigestLength("runtime-profile-id", ProfileIdBytes, 1))
    active(new TestRuntime(id(1), Array(1), 1, 0), 0) shouldBe
      Left(InvalidJitCost("fixed-jit", 0))
  }

  test("snapshot rejects duplicate, unsorted, invalid, and overflowing activation data") {
    val low = right(active(new TestRuntime(id(0), Array(1), 1, 0), 10))
    val high = right(active(new TestRuntime(id(0xff), Array(1), 1, 0), 10))
    val chain = id(7)

    snapshot(chain, 1, HistoricalBlockValidation, 10, Vector(high, low)) shouldBe
      Left(EntriesNotStrictlySorted(1))
    snapshot(chain, 1, HistoricalBlockValidation, 10, Vector(low, low)) shouldBe
      Left(EntriesNotStrictlySorted(1))
    snapshot(chain, -1, HistoricalBlockValidation, 10, Vector(low)) shouldBe
      Left(InvalidProtocolGeneration(-1))
    snapshot(chain, 1, HistoricalBlockValidation, 0, Vector(low)) shouldBe
      Left(InvalidJitCost("dispatch-jit", 0))

    val expensive = right(active(
      new TestRuntime(id(1), Array(1), 1, 0),
      Int.MaxValue))
    snapshot(chain, 1, HistoricalBlockValidation, 1, Vector(expensive)) shouldBe
      Left(JitCostSumOverflow(1, Int.MaxValue))
  }

  test("snapshot rejects an unencodable transition entry count before traversal") {
    val oversized = new IndexedSeq[ProfileEntry] {
      override def length: Int = MaxTransitionEntries + 1
      override def apply(index: Int): ProfileEntry =
        fail("oversized transition entries must not be traversed")
    }

    snapshot(
      id(7),
      protocolGeneration = 1,
      HistoricalBlockValidation,
      dispatchJit = 10,
      oversized) shouldBe Left(
        TooManyEntries(MaxTransitionEntries + 1, MaxTransitionEntries))
  }

  test("snapshot uses unsigned ordering and owns chain and entry identity") {
    val low = right(active(new TestRuntime(id(0x7f), Array(1), 1, 0), 10))
    val high = right(quarantined(id(0x80)))
    val chain = id(3)
    val selected = right(snapshot(
      chain,
      protocolGeneration = 12,
      CandidateConstruction,
      dispatchJit = 20,
      Vector(low, high)))
    chain(0) = 99

    selected.chainDomainId shouldBe id(3)
    selected.protocolGeneration shouldBe 12
    selected.validationPurpose shouldBe CandidateConstruction
    selected.lookup(id(0x7f)).get.lifecycle shouldBe a[ActiveLifecycle]
    selected.lookup(id(0x80)).get.lifecycle shouldBe QuarantinedLifecycle
    selected.lookup(id(1)) shouldBe None

    val exposedChain = selected.chainDomainId
    exposedChain(0) = 0
    selected.chainDomainId shouldBe id(3)
    val exposedEntryId = selected.entries.head.profileId
    exposedEntryId(0) = 0
    selected.entries.head.profileId shouldBe id(0x7f)
  }
}
