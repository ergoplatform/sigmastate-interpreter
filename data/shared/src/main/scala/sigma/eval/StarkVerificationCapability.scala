/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.eval

import sigma.ast.JitCost
import sigma.exceptions.StarkProfileRuntimeException
import sigma.stark.VerifierOperationObserver
import sigma.stark.profile.{RawSealV1Decoder, Risc0ClaimBuilder, Risc0ProfilePackageLoader, StarkPreVerifierObserver}

/** Trusted, invocation-specific host capability for EIP-0045 verification.
  *
  * Script values never construct or modify this object. A node supplies the
  * immutable snapshot selected from authenticated chain state for the exact
  * validation purpose. The default is [[StarkVerificationCapability.Unavailable]],
  * which preserves fail-closed pre-activation behavior.
  */
sealed trait StarkVerificationCapability

object StarkVerificationCapability {
  final val ProfileIdBytes: Int = 32
  final val MaxProofChunkBytes: Int = 65535
  final val MaxTransitionEntries: Int = 65535
  /** V1 arithmetic ceiling; each activated profile may require a lower limit. */
  final val MaxApplicationPayloadBytes: Int =
    Risc0ClaimBuilder.MaxApplicationPayloadBytes

  /** Validation-only census hooks for authenticated profile dispatch. The
    * callbacks carry no profile data and callers must not retain observers in
    * production capability state.
    */
  private[sigma] trait DispatchLookupObserver {
    def onEntryComparison(): Unit
    def onByteComparison(): Unit
  }

  /** No applicable transition snapshot exists for this invocation. */
  case object Unavailable extends StarkVerificationCapability

  /** Why the containing node is evaluating the transaction. The selected
    * snapshot is purpose-specific; evaluators must not substitute a tip or
    * historical context belonging to another purpose.
    */
  sealed trait ValidationPurpose extends Product with Serializable {
    def code: Byte
  }
  case object HistoricalBlockValidation extends ValidationPurpose {
    override val code: Byte = 1
  }
  case object AdmissionValidation extends ValidationPurpose {
    override val code: Byte = 2
  }
  case object CandidateConstruction extends ValidationPurpose {
    override val code: Byte = 3
  }

  /** Stable startup/construction rejection taxonomy. These failures concern
    * trusted activation data or compiled runtime metadata, never proof bytes.
    */
  sealed trait ConstructionFailure extends Product with Serializable {
    def code: String
  }
  final case class NullInput(name: String) extends ConstructionFailure {
    override val code: String = "stark-capability-null-input"
  }
  final case class WrongDigestLength(name: String, expected: Int, actual: Int)
      extends ConstructionFailure {
    override val code: String = "stark-capability-wrong-digest-length"
  }
  final case class InvalidProtocolGeneration(actual: Int) extends ConstructionFailure {
    override val code: String = "stark-capability-invalid-protocol-generation"
  }
  final case class InvalidJitCost(name: String, actual: Int) extends ConstructionFailure {
    override val code: String = "stark-capability-invalid-jit-cost"
  }
  final case class JitCostSumOverflow(dispatchJit: Int, fixedJit: Int)
      extends ConstructionFailure {
    override val code: String = "stark-capability-jit-cost-sum-overflow"
  }
  final case class EntriesNotStrictlySorted(index: Int) extends ConstructionFailure {
    override val code: String = "stark-capability-entries-not-strictly-sorted"
  }
  final case class TooManyEntries(actual: Int, maximum: Int)
      extends ConstructionFailure {
    override val code: String = "stark-capability-too-many-entries"
  }
  final case class RuntimeMetadataRejected(field: String, detail: String)
      extends ConstructionFailure {
    override val code: String = "stark-capability-runtime-metadata-rejected"
  }

  private[sigma] sealed trait Lifecycle

  private[sigma] final class ActiveLifecycle private[StarkVerificationCapability] (
      val fixedJit: JitCost,
      val runtime: StarkProfileRuntime,
      val maxApplicationPayloadBytes: Int,
      sourceChunkLengths: Array[Int]) extends Lifecycle {
    private val chunkLengthsSnapshot = sourceChunkLengths.clone()
    def canonicalProofChunkLengths: Array[Int] = chunkLengthsSnapshot.clone()
  }

  private[sigma] case object QuarantinedLifecycle extends Lifecycle

  /** One exact profile entry from a full transition snapshot. */
  final class ProfileEntry private[StarkVerificationCapability] (
      sourceProfileId: Array[Byte],
      private[sigma] val lifecycle: Lifecycle) {
    private val profileIdSnapshot = sourceProfileId.clone()
    def profileId: Array[Byte] = profileIdSnapshot.clone()
    private[StarkVerificationCapability] def profileIdForComparison: Array[Byte] =
      profileIdSnapshot
  }

  /** Applicable immutable transition snapshot for one chain, generation and
    * validation purpose.
    */
  final class Snapshot private[StarkVerificationCapability] (
      sourceChainDomainId: Array[Byte],
      val protocolGeneration: Int,
      val validationPurpose: ValidationPurpose,
      private[sigma] val dispatchJit: JitCost,
      sourceEntries: Array[ProfileEntry]) extends StarkVerificationCapability {
    private val chainDomainIdSnapshot = sourceChainDomainId.clone()
    private val entriesSnapshot = sourceEntries.clone()

    def chainDomainId: Array[Byte] = chainDomainIdSnapshot.clone()
    def entries: IndexedSeq[ProfileEntry] = entriesSnapshot.toVector

    /** Exact unsigned-lexicographic profile lookup. The input is already
      * length-checked by the opcode adapter.
      */
    private[sigma] def lookup(profileId: Array[Byte]): Option[ProfileEntry] =
      lookupObserved(profileId, null)

    /** Validation-only route through the production lookup loop. */
    private[sigma] def lookupObserved(
        profileId: Array[Byte],
        observer: DispatchLookupObserver): Option[ProfileEntry] = {
      var low = 0
      var high = entriesSnapshot.length - 1
      while (low <= high) {
        val mid = low + ((high - low) >>> 1)
        val comparison = compareUnsignedObserved(
          profileId,
          entriesSnapshot(mid).profileIdForComparison,
          observer)
        if (observer ne null) observer.onEntryComparison()
        if (comparison == 0) return Some(entriesSnapshot(mid))
        if (comparison < 0) high = mid - 1 else low = mid + 1
      }
      None
    }
  }

  /** Construct an active entry from a compiled immutable runtime and a
    * positive profile schedule. Runtime-owned shape metadata is snapshotted
    * here so it cannot change between lifecycle lookup and proof evaluation.
    */
  def active(
      runtime: StarkProfileRuntime,
      fixedJit: Int): Either[ConstructionFailure, ProfileEntry] = {
    if (runtime == null) return Left(NullInput("runtime"))
    if (fixedJit <= 0) return Left(InvalidJitCost("fixed-jit", fixedJit))

    val profileId = runtime.profileId
    validateDigest("runtime-profile-id", profileId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    val maximum = runtime.maxApplicationPayloadBytes
    if (maximum < 0)
      return Left(RuntimeMetadataRejected(
        "max-application-payload-bytes",
        "must be nonnegative"))
    if (maximum > MaxApplicationPayloadBytes)
      return Left(RuntimeMetadataRejected(
        "max-application-payload-bytes",
        s"must not exceed $MaxApplicationPayloadBytes"))
    val exactProofBytes = runtime.exactProofBytes
    if (exactProofBytes <= 0)
      return Left(RuntimeMetadataRejected("exact-proof-bytes", "must be positive"))
    val chunkLengths = runtime.canonicalProofChunkLengths
    if (chunkLengths == null)
      return Left(NullInput("runtime-canonical-proof-chunk-lengths"))
    if (chunkLengths.isEmpty || chunkLengths.length > MaxProofChunkBytes)
      return Left(RuntimeMetadataRejected(
        "canonical-proof-chunk-lengths",
        "chunk count must be in 1..65535"))

    var total = 0L
    var i = 0
    while (i < chunkLengths.length) {
      val length = chunkLengths(i)
      if (length <= 0 || length > MaxProofChunkBytes)
        return Left(RuntimeMetadataRejected(
          "canonical-proof-chunk-lengths[" + i + "]",
          "chunk length must be in 1..65535"))
      total += length.toLong
      i += 1
    }
    if (total != exactProofBytes.toLong)
      return Left(RuntimeMetadataRejected(
        "exact-proof-bytes",
        "does not equal the canonical chunk-length sum"))

    Right(new ProfileEntry(
      profileId.clone(),
      new ActiveLifecycle(
        JitCost(fixedJit),
        runtime,
        maximum,
        chunkLengths.clone())))
  }

  /** Construct a quarantined entry. Irreversibility is enforced when an
    * authenticated capability snapshot transition is validated, not by this
    * stateless constructor alone.
    */
  def quarantined(profileId: Array[Byte]): Either[ConstructionFailure, ProfileEntry] = {
    validateDigest("quarantined-profile-id", profileId) match {
      case Some(failure) => Left(failure)
      case None => Right(new ProfileEntry(profileId.clone(), QuarantinedLifecycle))
    }
  }

  /** Construct one applicable full snapshot. Entries must already be unique
    * and strictly sorted by unsigned raw profile ID, exactly like the
    * transition-manifest grammar.
    */
  def snapshot(
      chainDomainId: Array[Byte],
      protocolGeneration: Int,
      validationPurpose: ValidationPurpose,
      dispatchJit: Int,
      entries: IndexedSeq[ProfileEntry]): Either[ConstructionFailure, Snapshot] = {
    validateDigest("chain-domain-id", chainDomainId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    if (protocolGeneration < 0)
      return Left(InvalidProtocolGeneration(protocolGeneration))
    if (validationPurpose == null) return Left(NullInput("validation-purpose"))
    if (dispatchJit <= 0) return Left(InvalidJitCost("dispatch-jit", dispatchJit))
    if (entries == null) return Left(NullInput("entries"))

    val entryCount = entries.length
    if (entryCount > MaxTransitionEntries)
      return Left(TooManyEntries(entryCount, MaxTransitionEntries))

    val copied = new Array[ProfileEntry](entryCount)
    var i = 0
    while (i < entryCount) {
      val entry = entries(i)
      if (entry == null) return Left(NullInput("entries[" + i + "]"))
      copied(i) = entry
      if (i > 0 && compareUnsigned(
          copied(i - 1).profileIdForComparison,
          entry.profileIdForComparison) >= 0)
        return Left(EntriesNotStrictlySorted(i))
      entry.lifecycle match {
        case active: ActiveLifecycle =>
          try JitCost(dispatchJit) + active.fixedJit
          catch {
            case _: ArithmeticException =>
              return Left(JitCostSumOverflow(dispatchJit, active.fixedJit.value))
          }
        case QuarantinedLifecycle => ()
      }
      i += 1
    }

    Right(new Snapshot(
      chainDomainId.clone(),
      protocolGeneration,
      validationPurpose,
      JitCost(dispatchJit),
      copied))
  }

  private def validateDigest(
      name: String,
      value: Array[Byte]): Option[ConstructionFailure] =
    if (value == null) Some(NullInput(name))
    else if (value.length != ProfileIdBytes)
      Some(WrongDigestLength(name, ProfileIdBytes, value.length))
    else None

  private def compareUnsigned(left: Array[Byte], right: Array[Byte]): Int =
    compareUnsignedObserved(left, right, null)

  private def compareUnsignedObserved(
      left: Array[Byte],
      right: Array[Byte],
      observer: DispatchLookupObserver): Int = {
    var i = 0
    while (i < left.length && i < right.length) {
      val a = left(i) & 0xff
      val b = right(i) & 0xff
      if (observer ne null) observer.onByteComparison()
      if (a != b) return if (a < b) -1 else 1
      i += 1
    }
    if (left.length < right.length) -1
    else if (left.length > right.length) 1
    else 0
  }
}

/** Compiled verifier runtime for one immutable profile. Implementations are
  * trusted release code, not script-provided plugins. The capability factory
  * snapshots and validates all resource-shape metadata before use.
  */
trait StarkProfileRuntime {
  private[sigma] def profileId: Array[Byte]
  private[sigma] def exactProofBytes: Int
  private[sigma] def maxApplicationPayloadBytes: Int
  private[sigma] def canonicalProofChunkLengths: Array[Int]

  /** Return false only for a typed proof/claim rejection. Host invariant,
    * allocation, instrumentation and fatal VM failures must propagate.
    */
  private[sigma] def verify(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      proofChunks: Array[Array[Byte]]): Boolean
}

/** Package-bounded opt-in for runtimes that expose the validation census. */
private[sigma] trait ObservedStarkProfileRuntime extends StarkProfileRuntime {
  private[sigma] def verifyObserved(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      proofChunks: Array[Array[Byte]],
      observer: StarkPreVerifierObserver): Boolean
}

/** Initial stock RISC0 v3 runtime backed only by an authenticated B1/B2/B3
  * package. It derives ErgoStatementV1 and the expected OK ReceiptClaim
  * internally before invoking the raw-seal verifier.
  */
final class Risc0StockProfileRuntime private (
    loadedProfile: Risc0ProfilePackageLoader.LoadedProfile)
    extends ObservedStarkProfileRuntime {
  override private[sigma] def profileId: Array[Byte] = loadedProfile.profileId
  override private[sigma] def exactProofBytes: Int = loadedProfile.exactProofBytes
  override private[sigma] def maxApplicationPayloadBytes: Int =
    loadedProfile.maxApplicationPayloadBytes
  override private[sigma] def canonicalProofChunkLengths: Array[Int] =
    RawSealV1Decoder.canonicalChunkLengths

  override private[sigma] def verify(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      proofChunks: Array[Array[Byte]]): Boolean =
    verifyImpl(
      chainDomainId,
      programId,
      contractId,
      applicationPayload,
      proofChunks,
      null)

  override private[sigma] def verifyObserved(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      proofChunks: Array[Array[Byte]],
      observer: StarkPreVerifierObserver): Boolean =
    Risc0ClaimBuilder.buildObserved(
      loadedProfile,
      chainDomainId,
      programId,
      contractId,
      applicationPayload,
      observer) match {
      case Left(failure) =>
        throw new StarkProfileRuntimeException(
          "Authenticated RISC0 runtime rejected host-bound claim input: " + failure.code)
      case Right(binding) =>
        if (observer ne null) observer.onRawVerifierEntered()
        observer match {
          case operationObserver: VerifierOperationObserver =>
            loadedProfile.verifier.verifyObservedOperations(
              proofChunks,
              binding.expectedClaim,
              operationObserver).isRight
          case _ =>
            loadedProfile.verifier.verify(proofChunks, binding.expectedClaim).isRight
        }
    }

  private def verifyImpl(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      applicationPayload: Array[Byte],
      proofChunks: Array[Array[Byte]],
      observer: StarkPreVerifierObserver): Boolean =
    Risc0ClaimBuilder.buildObserved(
      loadedProfile,
      chainDomainId,
      programId,
      contractId,
      applicationPayload,
      observer) match {
      case Left(failure) =>
        throw new StarkProfileRuntimeException(
          "Authenticated RISC0 runtime rejected host-bound claim input: " + failure.code)
      case Right(binding) =>
        if (observer ne null) observer.onRawVerifierEntered()
        loadedProfile.verifier.verify(proofChunks, binding.expectedClaim).isRight
    }
}

object Risc0StockProfileRuntime {
  def fromLoadedProfile(
      loadedProfile: Risc0ProfilePackageLoader.LoadedProfile)
      : Either[StarkVerificationCapability.ConstructionFailure, Risc0StockProfileRuntime] = {
    import StarkVerificationCapability._
    if (loadedProfile == null) return Left(NullInput("loaded-profile"))
    if (loadedProfile.exactProofBytes != RawSealV1Decoder.ByteCount)
      return Left(RuntimeMetadataRejected(
        "exact-proof-bytes",
        "stock RISC0 runtime requires " + RawSealV1Decoder.ByteCount))
    Right(new Risc0StockProfileRuntime(loadedProfile))
  }
}
