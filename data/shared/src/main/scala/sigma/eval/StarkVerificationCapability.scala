/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.eval

import sigma.ast.JitCost
import sigma.stark.ErgoStarkStatement

/** Trusted, invocation-specific host capability for EIP-0045 verification.
  *
  * Script values never construct or modify this object. A node or wallet supplies
  * the immutable snapshot selected from authenticated chain state for the exact
  * validation purpose. The default is [[StarkVerificationCapability.Unavailable]],
  * which preserves fail-closed pre-activation behavior.
  */
sealed trait StarkVerificationCapability

object StarkVerificationCapability {
  final val DigestBytes: Int = 32
  final val ProfileIdBytes: Int = DigestBytes
  final val MaxProofChunkBytes: Int = 65535
  final val MaxTransitionEntries: Int = 65535

  /** V1 arithmetic ceiling; each activated profile may require a lower limit. */
  final val MaxApplicationPayloadBytes: Int =
    ErgoStarkStatement.MaxApplicationPayloadBytes

  /** No applicable transition snapshot exists for this invocation. */
  case object Unavailable extends StarkVerificationCapability

  /** Why the caller is evaluating the transaction. The selected snapshot is
    * purpose-specific; evaluators must not substitute a tip or historical
    * context belonging to another purpose.
    */
  sealed trait ValidationPurpose extends Product with Serializable {
    def code: Byte
  }

  /** Full received-block or historical replay validation. */
  case object HistoricalBlockValidation extends ValidationPurpose {
    override val code: Byte = 1
  }

  /** Unconfirmed mempool or submission-API admission validation. */
  case object AdmissionValidation extends ValidationPurpose {
    override val code: Byte = 2
  }

  /** Local miner block-candidate construction. */
  case object CandidateConstruction extends ValidationPurpose {
    override val code: Byte = 3
  }

  /** Off-node wallet, SDK, or EIP-19 reduced-transaction prover reduction. */
  case object ProverReduction extends ValidationPurpose {
    override val code: Byte = 4
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
  final case class InvalidStarkGeneration(name: String, actual: Int) extends ConstructionFailure {
    override val code: String = "stark-capability-invalid-stark-generation"
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
      val minProofBytes: Int,
      val maxProofBytes: Int) extends Lifecycle {

    /** Maximum number of canonical 65,535-byte chunks an admissible proof may contain. */
    val maxChunkCount: Int = ((maxProofBytes - 1) / MaxProofChunkBytes) + 1

    /** Validates canonical 65,535-byte chunk partitioning and profile byte bounds:
      * - chunkCount in 1..maxChunkCount
      * - every non-final chunk has length == 65,535
      * - final chunk has length in 1..65,535
      * - total length in [minProofBytes, maxProofBytes]
      */
    def isCanonicalChunkPartition(chunkLengths: Array[Int]): Boolean = {
      if (chunkLengths == null || chunkLengths.isEmpty || chunkLengths.length > maxChunkCount) return false
      var total = 0L
      val lastIdx = chunkLengths.length - 1
      var i = 0
      while (i < lastIdx) {
        if (chunkLengths(i) != MaxProofChunkBytes) return false
        total += MaxProofChunkBytes.toLong
        i += 1
      }
      val lastLen = chunkLengths(lastIdx)
      if (lastLen <= 0 || lastLen > MaxProofChunkBytes) return false
      total += lastLen.toLong
      total >= minProofBytes.toLong && total <= maxProofBytes.toLong
    }
  }

  private[sigma] case object QuarantinedLifecycle extends Lifecycle

  /** One exact profile entry from a full transition snapshot. */
  final class ProfileEntry private[StarkVerificationCapability] (
      sourceProfileId: Array[Byte],
      private[sigma] val lifecycle: Lifecycle) {
    private val profileIdSnapshot = sourceProfileId.clone()
    def profileId: Array[Byte] = profileIdSnapshot.clone()
    def isActive: Boolean = lifecycle.isInstanceOf[ActiveLifecycle]
    def isQuarantined: Boolean = lifecycle == QuarantinedLifecycle
    def fixedJitOpt: Option[JitCost] = lifecycle match {
      case active: ActiveLifecycle => Some(active.fixedJit)
      case QuarantinedLifecycle    => None
    }
    private[StarkVerificationCapability] def profileIdForComparison: Array[Byte] =
      profileIdSnapshot
  }

  /** Applicable immutable transition snapshot for one chain, STARK generation,
    * on-chain committed transition digest, and validation purpose.
    *
    * `starkGeneration` is decoupled from Ergo `BlockVersion` / 3-bit ErgoTree
    * versions so new profiles, nondecreasing repricing, and emergency quarantine
    * never consume ErgoTree language versions. When `starkGeneration >
    * maxSupportedStarkGeneration`, this snapshot represents the binary's last
    * known transition `Tmax` evaluated under future-generation stale-node rules.
    */
  final class Snapshot private[StarkVerificationCapability] (
      sourceChainDomainId: Array[Byte],
      val starkGeneration: Int,
      val maxSupportedStarkGeneration: Int,
      sourceTransitionId: Array[Byte],
      val validationPurpose: ValidationPurpose,
      val dispatchJit: JitCost,
      sourceEntries: Array[ProfileEntry]) extends StarkVerificationCapability {
    private val chainDomainIdSnapshot = sourceChainDomainId.clone()
    private val transitionIdSnapshot = sourceTransitionId.clone()
    private val entriesSnapshot = sourceEntries.clone()

    def chainDomainId: Array[Byte] = chainDomainIdSnapshot.clone()
    def transitionId: Array[Byte] = transitionIdSnapshot.clone()
    def entries: IndexedSeq[ProfileEntry] = entriesSnapshot.toVector

    /** True when the authenticated network STARK generation exceeds the highest
      * generation compiled into this binary (`G > Gmax`).
      */
    def isFutureGeneration: Boolean = starkGeneration > maxSupportedStarkGeneration

    /** Exact unsigned-lexicographic profile lookup. Input length is checked
      * by the caller before invoking lookup.
      */
    private[sigma] def lookup(profileId: Array[Byte]): Option[ProfileEntry] = {
      var low = 0
      var high = entriesSnapshot.length - 1
      while (low <= high) {
        val mid = low + ((high - low) >>> 1)
        val comparison = compareUnsigned(
          profileId,
          entriesSnapshot(mid).profileIdForComparison)
        if (comparison == 0) return Some(entriesSnapshot(mid))
        if (comparison < 0) high = mid - 1 else low = mid + 1
      }
      None
    }

    /** True when `profileId` is explicitly present (`Active` or `Quarantined`) in this snapshot. */
    def isKnownProfile(profileId: Array[Byte]): Boolean =
      profileId != null && profileId.length == ProfileIdBytes && lookup(profileId).isDefined
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
      return Left(RuntimeMetadataRejected("max-application-payload-bytes", "must be nonnegative"))
    if (maximum > MaxApplicationPayloadBytes)
      return Left(RuntimeMetadataRejected(
        "max-application-payload-bytes",
        s"must not exceed $MaxApplicationPayloadBytes"))

    val minBytes = runtime.minProofBytes
    val maxBytes = runtime.maxProofBytes
    if (minBytes <= 0)
      return Left(RuntimeMetadataRejected("min-proof-bytes", "must be positive"))
    if (maxBytes < minBytes)
      return Left(RuntimeMetadataRejected("max-proof-bytes", "must be >= min-proof-bytes"))
    val maxChunkCount = (maxBytes.toLong + MaxProofChunkBytes - 1L) / MaxProofChunkBytes
    if (maxChunkCount > MaxProofChunkBytes.toLong)
      return Left(RuntimeMetadataRejected("max-proof-bytes", "exceeds representable chunk count"))

    Right(new ProfileEntry(
      profileId.clone(),
      new ActiveLifecycle(
        JitCost(fixedJit),
        runtime,
        maximum,
        minBytes,
        maxBytes)))
  }

  /** Construct a quarantined entry. Irreversibility is enforced when an
    * authenticated capability snapshot transition is validated, not by this
    * stateless constructor alone.
    */
  def quarantined(profileId: Array[Byte]): Either[ConstructionFailure, ProfileEntry] = {
    validateDigest("quarantined-profile-id", profileId) match {
      case Some(failure) => Left(failure)
      case None          => Right(new ProfileEntry(profileId.clone(), QuarantinedLifecycle))
    }
  }

  /** Construct one applicable full snapshot. Entries must already be unique
    * and strictly sorted by unsigned raw profile ID.
    */
  def snapshot(
      chainDomainId: Array[Byte],
      starkGeneration: Int,
      maxSupportedStarkGeneration: Int,
      transitionId: Array[Byte],
      validationPurpose: ValidationPurpose,
      dispatchJit: Int,
      entries: IndexedSeq[ProfileEntry]): Either[ConstructionFailure, Snapshot] = {
    validateDigest("chain-domain-id", chainDomainId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
    if (starkGeneration <= 0)
      return Left(InvalidStarkGeneration("stark-generation", starkGeneration))
    if (maxSupportedStarkGeneration <= 0)
      return Left(InvalidStarkGeneration(
        "max-supported-stark-generation",
        maxSupportedStarkGeneration))
    validateDigest("transition-id", transitionId) match {
      case Some(failure) => return Left(failure)
      case None          => ()
    }
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
      starkGeneration,
      maxSupportedStarkGeneration,
      transitionId.clone(),
      validationPurpose,
      JitCost(dispatchJit),
      copied))
  }

  /** Convenience constructor when the snapshot's generation is supported (`G <= Gmax`). */
  def snapshot(
      chainDomainId: Array[Byte],
      starkGeneration: Int,
      transitionId: Array[Byte],
      validationPurpose: ValidationPurpose,
      dispatchJit: Int,
      entries: IndexedSeq[ProfileEntry]): Either[ConstructionFailure, Snapshot] =
    snapshot(
      chainDomainId,
      starkGeneration,
      starkGeneration,
      transitionId,
      validationPurpose,
      dispatchJit,
      entries)

  private def validateDigest(
      name: String,
      value: Array[Byte]): Option[ConstructionFailure] =
    if (value == null) Some(NullInput(name))
    else if (value.length != DigestBytes)
      Some(WrongDigestLength(name, DigestBytes, value.length))
    else None

  private def compareUnsigned(left: Array[Byte], right: Array[Byte]): Int = {
    var i = 0
    while (i < left.length && i < right.length) {
      val a = left(i) & 0xff
      val b = right(i) & 0xff
      if (a != b) return if (a < b) -1 else 1
      i += 1
    }
    if (left.length < right.length) -1
    else if (left.length > right.length) 1
    else 0
  }
}

/** Compiled verifier runtime for one immutable EIP-0045 profile.
  *
  * Implementations are vetted release code, never script-provided plugins.
  * Each mainnet profile must satisfy the EIP-0045 security bar (>= 128-bit
  * provable classical soundness verified under `soundcalc`).
  *
  * Both fixed-size seals (`minProofBytes == maxProofBytes`) and bounded-size
  * multiproof formats (`1 <= minProofBytes <= maxProofBytes`, e.g., STIR/WHIR
  * with Merkle path sharing) are supported under one canonical 65,535-byte
  * chunk partition.
  */
trait StarkProfileRuntime {
  private[sigma] def profileId: Array[Byte]
  private[sigma] def minProofBytes: Int
  private[sigma] def maxProofBytes: Int
  private[sigma] def maxApplicationPayloadBytes: Int

  /** Verify a canonically partitioned proof against the host-constructed
    * `ErgoStatementV1` bytes and `programId`.
    *
    * Return `false` only for a typed proof, canonical-format, or claim rejection.
    * Host invariant, allocation, and fatal VM failures must propagate.
    */
  private[sigma] def verify(
      statement: Array[Byte],
      programId: Array[Byte],
      proofChunks: Array[Array[Byte]]): Boolean
}
