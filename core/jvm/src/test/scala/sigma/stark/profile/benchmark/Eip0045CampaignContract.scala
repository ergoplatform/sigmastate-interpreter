/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}

import scala.collection.mutable

import sigma.stark.profile.benchmark.Eip0045BenchmarkSupport._

/** Canonical, dependency-free data contract for a bounded EIP-0045 B5 JVM
  * benchmark campaign. This remains in JVM test sources and has no consensus
  * or production-runtime role.
  */
private[benchmark] object Eip0045CampaignContract {
  final val ManifestSchema: String = "eip-0045-b5-campaign-manifest-v3"
  final val ManifestCanonicalization: String =
    "utf8-fixed-field-order-no-internal-whitespace-single-terminal-lf-v3"
  final val ArchiveIndexSchema: String =
    "eip-0045-b5-campaign-archive-index-v1"
  final val ArchiveIndexCanonicalization: String =
    "utf8-fixed-field-order-no-internal-whitespace-single-terminal-lf-v1"
  final val ExpectedProfileId: String =
    "23c4a123ffb33a1c8db89436fe0e7972bd8e4e289459ee5fd71be5440607d383"
  final val ExpectedVerifierEntryPoint: String =
    "sigma.stark.profile.Risc0RawSealVerifier.verify"
  final val ExpectedThreadAllocationMeter: String =
    "com.sun.management.ThreadMXBean.getThreadAllocatedBytes(currentThread)"
  final val MaxEvidenceBytes: Int = 16 * 1024 * 1024
  final val MaxArchiveIndexBytes: Int = 1024 * 1024
  final val MaxEnvironmentPolicies: Int = 64
  final val MaxJvmArgumentPolicies: Int = 64
  final val MaxCells: Int = 64
  final val MaxRuns: Int = 256
  final val MaxReplicatesPerCell: Int = 16
  final val MaxPublicStringCharacters: Int = 4096
  final val MaxJsonDepth: Int = 32
  final val MaxJsonArrayItems: Int = 100000
  final val MaxJsonObjectMembers: Int = 128
  final val MaxJsonValues: Int = 500000

  final case class EvidenceContract(
      schema: String,
      digestAlgorithm: String,
      digestDomain: String,
      canonicalization: String,
      clock: String,
      schedule: String,
      percentileMethod: String,
      timedScope: String,
      allocationScope: String,
      garbageCollectionScope: String,
      memoryPoolScope: String)

  final case class ScenarioPolicy(
      id: String,
      expectedOutcome: String,
      validationQueryCheckpoints: Int,
      validationBoundary: String,
      lastVerifierCheckpoint: String)

  final case class EnvironmentPolicy(
      id: String,
      javaRuntimeName: String,
      javaRuntimeVersion: String,
      javaVmName: String,
      javaVmVendor: String,
      javaVmVersion: String,
      javaVmInfo: String,
      scalaVersion: String,
      osName: String,
      osVersion: String,
      osArch: String,
      availableProcessors: Int,
      maxHeapBytes: Long,
      jitCompiler: String,
      garbageCollectors: Vector[String],
      memoryPoolIdentities: Vector[MemoryPoolIdentity],
      threadAllocationMeter: String,
      cpuModel: String,
      cpuModelSource: String)

  final case class JvmArgumentPolicy(
      id: String,
      argumentCount: Int,
      argumentsSha256: String)

  final case class CampaignCell(
      id: String,
      environmentPolicyId: String,
      jvmArgumentPolicyId: String,
      replicateCount: Int)

  final case class CampaignRun(
      id: String,
      cellId: String,
      replicate: Int)

  final case class CampaignManifestV3(
      campaignId: String,
      profileId: String,
      implementationRevision: String,
      verifierEntryPoint: String,
      evidenceContract: EvidenceContract,
      resources: Vector[ResourceMetadata],
      warmupRounds: Int,
      sampleRounds: Int,
      scenarios: Vector[ScenarioPolicy],
      environmentPolicies: Vector[EnvironmentPolicy],
      jvmArgumentPolicies: Vector[JvmArgumentPolicy],
      cells: Vector[CampaignCell],
      runs: Vector[CampaignRun])

  /** One immutable interpretation of the exact manifest byte snapshot. The
    * parser and SHA-256 calculation consume the same defensive copy.
    */
  final case class ExactCampaignManifest private (
      manifest: CampaignManifestV3,
      byteLength: Int,
      sha256: String)

  /** Fully joined policy selected for one declared campaign run. */
  final case class ResolvedRunPolicy(
      run: CampaignRun,
      cell: CampaignCell,
      environmentPolicy: EnvironmentPolicy,
      jvmArgumentPolicy: JvmArgumentPolicy,
      campaignBinding: CampaignBinding)

  final case class ArchiveEntry(
      runId: String,
      evidenceByteLength: Int,
      evidenceSha256: String,
      evidenceDigest: String)

  final case class ArchiveIndex(
      campaignId: String,
      manifestByteLength: Int,
      manifestSha256: String,
      entries: Vector[ArchiveEntry])

  val ExpectedEvidenceContract: EvidenceContract = EvidenceContract(
    Schema,
    DigestAlgorithm,
    DigestDomain,
    Canonicalization,
    "System.nanoTime",
    "deterministic-round-robin-rotation",
    "nearest-rank",
    "Risc0RawSealVerifier.verify; fixture and profile loading excluded",
    "current benchmark thread around each timed verifier invocation",
    "process-wide collector deltas across the complete sampling phase",
    "sequential per-pool MemoryPoolMXBean.resetPeakUsage/getPeakUsage/getUsage boundary calls around the complete sampling phase; runner and JVM-management overhead included")

  val ExpectedResources: Vector[ResourceMetadata] = Vector(
    ResourceMetadata(
      "profile-algorithm",
      "classpath:/stark-kats/eip0045-profile-package/algorithm.txt",
      29773,
      "90a884da420a09f2c1108d7388c2ac74db8dbdb195de704206e2bf8ec1ad0bee"),
    ResourceMetadata(
      "profile-constants",
      "classpath:/stark-kats/eip0045-profile-package/constants.bin",
      65119,
      "8c4a92b7d354890481eefdef233d4ca43f6bcd9f7cb00e4dd9e709da47789ef3"),
    ResourceMetadata(
      "profile-manifest",
      "classpath:/stark-kats/eip0045-profile-package/manifest.bin",
      458,
      "deffb2cb231f98a348cbd166d5f1c43315661ccd8bd212099f16f238d0fe8946"),
    ResourceMetadata(
      "profile-id",
      "classpath:/stark-kats/eip0045-profile-package/profile-id.bin",
      32,
      "aa144c74a0cb52b3c5a9827f10a264f320820190da14a9bf82dcf3466f41aae1"),
    ResourceMetadata(
      "raw-seal",
      "classpath:/stark-kats/eip0045-direct/po2-15-raw-seal.bin",
      222668,
      "088e6a306c7143f5a3e057924c42f63a6eb58dd3c30686a8ade5082fac4b386e"),
    ResourceMetadata(
      "claim-digest",
      "classpath:/stark-kats/eip0045-direct/po2-15-claim-digest.bin",
      32,
      "df9df2763693f97b85acd9d3cda4b13e2421b3dc052a6f91ab995c89ae75ee3c"),
    ResourceMetadata(
      "fixture-manifest",
      "classpath:/stark-kats/eip0045-direct/fixture-manifest.json",
      1559,
      "7df5d428210065e9480b44d273db298b85c692e3eafbbe4db76fb00ba3bd84a3"),
    ResourceMetadata(
      "po2-16-raw-seal",
      "classpath:/stark-kats/eip0045-arkadia-independent/raw-seal.bin",
      222668,
      "d7bdef7d0b3759a6d8ba43c9b531b017112b07e42af2761fbe654a596d759d79"),
    ResourceMetadata(
      "po2-16-claim-digest",
      "classpath:/stark-kats/eip0045-arkadia-independent/claim-digest.bin",
      32,
      "e8b4b5217ae717e000f8fb3e36a510aeef5ed5c79d04f2c600b74997b5858cfc"),
    ResourceMetadata(
      "po2-16-image-id",
      "classpath:/stark-kats/eip0045-arkadia-independent/image-id.bin",
      32,
      "0e3a24e2345c1d8e4c3ef2e769aeb3c15465df56d9355c377c4de54cec97fa69"),
    ResourceMetadata(
      "po2-16-journal",
      "classpath:/stark-kats/eip0045-arkadia-independent/journal.bin",
      67,
      "be21ccbca9266d302b484d9a4dd01247f6d2cba5e4dc30474a7238f2033ba2e8"),
    ResourceMetadata(
      "po2-16-fixture-manifest",
      "classpath:/stark-kats/eip0045-arkadia-independent/fixture-manifest.json",
      1840,
      "6d30977a0b13a18131688a133cac4dd977c3fd6d109f549e8ca5ac29395584b7"))

  val ExpectedScenarios: Vector[ScenarioPolicy] = Vector(
    ScenarioPolicy(
      "valid-proof", "verified:1:15", 50, "verification-complete", "query"),
    ScenarioPolicy(
      "early-transport-rejection",
      "raw-seal-transport-rejected",
      0,
      "transport-chunk-shape",
      "none"),
    ScenarioPolicy(
      "early-canonical-cryptographic-rejection",
      "raw-seal-control-id-not-allowed",
      0,
      "terminal-control-allowlist",
      "group_root_code"),
    ScenarioPolicy(
      "late-cryptographic-mutation",
      "raw-seal-malformed-proof",
      50,
      "fri",
      "query"),
    ScenarioPolicy(
      "late-claim-mismatch",
      "raw-seal-claim-mismatch",
      50,
      "expected-claim-comparison",
      "query"),
    ScenarioPolicy(
      "valid-independent-po2-16",
      "verified:1:16",
      50,
      "verification-complete",
      "query"))

  val ExpectedCampaignLimitations: Vector[String] = Vector(
    "This run measures one JVM process on one host and cannot close B5 by itself.",
    "The evidence digest binds content but is not an operator signature or execution attestation.",
    "The harness does not choose, infer, or recommend a consensus fixedJit value.",
    "Profile loading, ErgoTree preflight, transaction parsing, and node admission are outside the timed and allocation scope.",
    "Allocation samples cover only the current benchmark thread; process-wide or native allocations are outside their scope.",
    "Garbage-collector deltas are process-wide observations and cannot be attributed to one scenario.",
    "Memory-pool phase envelopes are JVM MXBean observations; they do not measure native memory, prove object liveness, attribute a peak to one scenario, or bound a node process.",
    "Campaign memory-pool evidence requires a dedicated JVM with no other benchmark, Java agent, or JMX client resetting pool peaks.",
    "Pre-reset and post-sampling topology checks do not detect transient topology changes or a non-inverting external peak reset.",
    "The JVM input-argument digest binds ordered RuntimeMXBean strings but does not disclose or interpret them.",
    "CPU scheduling, frequency scaling, thermal state, and concurrent host load are not controlled by the harness.",
    "Validation boundaries and last verifier checkpoints are probe-only observations; they are not operation counts, cost bounds, or timed-path measurements.",
    "Resource identities cover the complete run; the scenario-to-fixture mapping is fixed by the exact benchmark source and is not encoded separately in the manifest.",
    "The po2-16 case replays checked-in bytes from an independent source; it does not reproduce or attest their upstream generation.",
    "Validation invokes every scenario before warmup, so zero warmup rounds are not a cold-start measurement.",
    "Campaign mode parses the exact manifest bytes and matches the selected run, implementation revision, rounds, environment policy, and ordered JVM-input-argument identity before verifier setup.")

  sealed trait JsonValue
  final case class JsonObject(fields: Vector[(String, JsonValue)]) extends JsonValue
  final case class JsonArray(values: Vector[JsonValue]) extends JsonValue
  final case class JsonString(value: String) extends JsonValue
  final case class JsonNumber(value: String) extends JsonValue
  final case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  private[benchmark] final class DecodeFailure(message: String)
      extends IllegalArgumentException(message)

  def renderManifest(manifest: CampaignManifestV3): Either[String, String] =
    validateManifest(manifest) match {
      case Left(detail) => Left(detail)
      case Right(_) =>
        val rendered = renderManifestUnchecked(manifest)
        strictUtf8ByteLength(rendered) match {
          case Left(_) => Left("campaign manifest is not strict UTF-8")
          case Right(length) if length > MaxCampaignManifestBytes =>
            Left("campaign manifest exceeds " + MaxCampaignManifestBytes + " bytes")
          case Right(_) =>
            parseManifest(rendered.getBytes(StandardCharsets.UTF_8)) match {
              case Right(parsed) if parsed == manifest => Right(rendered)
              case _ => Left("campaign manifest does not round-trip through its parser")
            }
        }
    }

  def parseManifest(bytes: Array[Byte]): Either[String, CampaignManifestV3] = {
    parseDocument(bytes, MaxCampaignManifestBytes, "campaign manifest") match {
      case Left(detail) => Left(detail)
      case Right((text, root)) =>
        try {
          val manifest = decodeManifest(root)
          validateManifest(manifest) match {
            case Left(detail) => Left(detail)
            case Right(_) =>
              val rendered = renderManifestUnchecked(manifest)
              if (rendered == text) Right(manifest)
              else Left("campaign manifest is not canonical JSON")
          }
        } catch {
          case error: DecodeFailure => Left(error.getMessage)
        }
    }
  }

  /** Parses and hashes one defensive byte snapshot so canonical semantics and
    * the published manifest identity cannot be derived from different input
    * arrays.
    */
  def parseExactCampaignManifest(
      bytes: Array[Byte]): Either[String, ExactCampaignManifest] = {
    if (bytes == null) return Left("campaign manifest bytes are null")
    val exactBytes = bytes.clone()
    parseManifest(exactBytes) match {
      case Left(detail) => Left(detail)
      case Right(manifest) => Right(ExactCampaignManifest(
        manifest,
        exactBytes.length,
        sha256Hex(exactBytes)))
    }
  }

  /** Producer entry point: parse the exact manifest bytes, derive their
    * identity, select run -> cell -> environment/JVM policies, and require the
    * observed configuration to match before benchmark verifier setup.
    */
  def resolveRunPolicy(
      manifestBytes: Array[Byte],
      runId: String,
      implementationRevision: String,
      warmupRounds: Int,
      sampleRounds: Int,
      environment: EnvironmentMetadata): Either[String, ResolvedRunPolicy] = {
    parseExactCampaignManifest(manifestBytes) match {
      case Left(detail) => Left(detail)
      case Right(exact) =>
        val binding = CampaignBinding(runId, exact.byteLength, exact.sha256)
        resolveRunPolicy(
          exact,
          binding,
          implementationRevision,
          warmupRounds,
          sampleRounds,
          environment)
    }
  }

  /** Consumer entry point: reuse the same joined policy while additionally
    * checking a claimed evidence binding against the exact parsed bytes.
    */
  def resolveRunPolicy(
      exact: ExactCampaignManifest,
      binding: CampaignBinding,
      implementationRevision: String,
      warmupRounds: Int,
      sampleRounds: Int,
      environment: EnvironmentMetadata): Either[String, ResolvedRunPolicy] = {
    if (exact == null) return Left("exact campaign manifest is null")
    if (binding == null) return Left("evidence campaign binding is missing")
    if (environment == null) return Left("evidence environment is missing")
    if (binding.manifestByteLength != exact.byteLength ||
        binding.manifestSha256 != exact.sha256)
      return Left("evidence manifest binding does not match the exact campaign bytes")

    val manifest = exact.manifest
    if (implementationRevision != manifest.implementationRevision)
      return Left("evidence implementation revision does not match the campaign")
    if (warmupRounds != manifest.warmupRounds)
      return Left("evidence warmup rounds do not match the campaign")
    if (sampleRounds != manifest.sampleRounds)
      return Left("evidence sample rounds do not match the campaign")
    val run = manifest.runs.find(_.id == binding.runId) match {
      case Some(value) => value
      case None => return Left("evidence run ID is not declared by the campaign")
    }
    val cell = manifest.cells.find(_.id == run.cellId).get
    val environmentPolicy = manifest.environmentPolicies
      .find(_.id == cell.environmentPolicyId).get
    val jvmArgumentPolicy = manifest.jvmArgumentPolicies
      .find(_.id == cell.jvmArgumentPolicyId).get
    if (!environmentMatches(environment, environmentPolicy))
      return Left("evidence environment does not match the declared cell policy")
    if (environment.jvmInputArgumentCount != jvmArgumentPolicy.argumentCount ||
        environment.jvmInputArgumentsSha256 != jvmArgumentPolicy.argumentsSha256)
      return Left("evidence JVM argument identity does not match the declared cell policy")
    Right(ResolvedRunPolicy(
      run,
      cell,
      environmentPolicy,
      jvmArgumentPolicy,
      CampaignBinding(run.id, exact.byteLength, exact.sha256)))
  }

  def validateManifest(manifest: CampaignManifestV3): Either[String, Unit] = {
    if (manifest == null) return Left("campaign manifest is null")
    if (!isPublicId(manifest.campaignId)) return Left("campaign ID is invalid")
    if (manifest.profileId != ExpectedProfileId)
      return Left("campaign profile ID does not match the EIP-0045 candidate")
    validateImplementationRevision(manifest.implementationRevision) match {
      case Left(_) => return Left("campaign implementation revision is invalid")
      case Right(value) if value != manifest.implementationRevision =>
        return Left("campaign implementation revision is not normalized")
      case Right(_) =>
    }
    if (manifest.implementationRevision == "unrecorded")
      return Left("campaign implementation revision is unrecorded")
    if (!isExactRevisionIdentity(manifest.implementationRevision))
      return Left("campaign implementation revision is not an exact supported identity")
    if (manifest.verifierEntryPoint != ExpectedVerifierEntryPoint)
      return Left("campaign verifier entry point is invalid")
    if (manifest.evidenceContract != ExpectedEvidenceContract)
      return Left("campaign V5 evidence contract is invalid")
    if (manifest.resources != ExpectedResources)
      return Left("campaign resources do not match the frozen benchmark resources")
    if (manifest.warmupRounds < 0 || manifest.warmupRounds > MaxWarmupRounds)
      return Left("campaign warmup rounds are out of bounds")
    if (manifest.sampleRounds <= 0 || manifest.sampleRounds > MaxSampleRounds)
      return Left("campaign sample rounds are out of bounds")
    if (manifest.scenarios != ExpectedScenarios)
      return Left("campaign scenarios do not match the frozen benchmark scenarios")
    if (manifest.environmentPolicies == null ||
        manifest.environmentPolicies.isEmpty ||
        manifest.environmentPolicies.length > MaxEnvironmentPolicies)
      return Left("campaign environment policy count is invalid")
    if (manifest.environmentPolicies.exists(_ == null))
      return Left("campaign environment policy is null")
    if (manifest.jvmArgumentPolicies == null ||
        manifest.jvmArgumentPolicies.isEmpty ||
        manifest.jvmArgumentPolicies.length > MaxJvmArgumentPolicies)
      return Left("campaign JVM argument policy count is invalid")
    if (manifest.jvmArgumentPolicies.exists(_ == null))
      return Left("campaign JVM argument policy is null")
    if (manifest.cells == null || manifest.cells.isEmpty || manifest.cells.length > MaxCells)
      return Left("campaign cell count is invalid")
    if (manifest.cells.exists(_ == null)) return Left("campaign cell is null")
    if (manifest.runs == null || manifest.runs.isEmpty || manifest.runs.length > MaxRuns)
      return Left("campaign run count is invalid")
    if (manifest.runs.exists(_ == null)) return Left("campaign run is null")

    val environmentIds = manifest.environmentPolicies.map(_.id)
    if (!areSortedUniqueIds(environmentIds))
      return Left("campaign environment policy IDs are not sorted and unique")
    var i = 0
    while (i < manifest.environmentPolicies.length) {
      validateEnvironmentPolicy(manifest.environmentPolicies(i)) match {
        case Left(detail) => return Left(detail)
        case Right(_) =>
      }
      i += 1
    }
    if (manifest.environmentPolicies.map(_.copy(id = "")).distinct.length !=
        manifest.environmentPolicies.length)
      return Left("campaign environment policies contain duplicate values")

    val argumentIds = manifest.jvmArgumentPolicies.map(_.id)
    if (!areSortedUniqueIds(argumentIds))
      return Left("campaign JVM argument policy IDs are not sorted and unique")
    i = 0
    while (i < manifest.jvmArgumentPolicies.length) {
      val policy = manifest.jvmArgumentPolicies(i)
      if (!isPublicId(policy.id)) return Left("campaign JVM argument policy ID is invalid")
      if (policy.argumentCount < 0 || policy.argumentCount > MaxJvmInputArguments)
        return Left("campaign JVM argument count is invalid")
      if (!isLowerHexSha256(policy.argumentsSha256))
        return Left("campaign JVM argument digest is invalid")
      i += 1
    }
    if (manifest.jvmArgumentPolicies.map(p => (p.argumentCount, p.argumentsSha256)).distinct.length !=
        manifest.jvmArgumentPolicies.length)
      return Left("campaign JVM argument policies contain duplicate values")

    val cellIds = manifest.cells.map(_.id)
    if (!areSortedUniqueIds(cellIds))
      return Left("campaign cell IDs are not sorted and unique")
    i = 0
    while (i < manifest.cells.length) {
      val cell = manifest.cells(i)
      if (!isPublicId(cell.id)) return Left("campaign cell ID is invalid")
      if (!environmentIds.contains(cell.environmentPolicyId))
        return Left("campaign cell references an unknown environment policy")
      if (!argumentIds.contains(cell.jvmArgumentPolicyId))
        return Left("campaign cell references an unknown JVM argument policy")
      if (cell.replicateCount <= 0 || cell.replicateCount > MaxReplicatesPerCell)
        return Left("campaign cell replicate count is invalid")
      i += 1
    }
    if (manifest.cells.map(c => (c.environmentPolicyId, c.jvmArgumentPolicyId)).distinct.length !=
        manifest.cells.length)
      return Left("campaign cells contain duplicate policy pairs")
    if (manifest.cells.map(_.environmentPolicyId).distinct.sorted != environmentIds)
      return Left("campaign contains an unused environment policy")
    if (manifest.cells.map(_.jvmArgumentPolicyId).distinct.sorted != argumentIds)
      return Left("campaign contains an unused JVM argument policy")

    val runIds = manifest.runs.map(_.id)
    if (!areSortedUniqueIds(runIds))
      return Left("campaign run IDs are not sorted and unique")
    val expectedRunCount = manifest.cells.map(_.replicateCount).sum
    if (manifest.runs.length != expectedRunCount)
      return Left("campaign run count does not match cell replicates")
    val cellsById = manifest.cells.map(cell => cell.id -> cell).toMap
    i = 0
    while (i < manifest.runs.length) {
      val run = manifest.runs(i)
      if (!isValidCampaignRunId(run.id)) return Left("campaign run ID is invalid")
      cellsById.get(run.cellId) match {
        case None => return Left("campaign run references an unknown cell")
        case Some(cell) if run.replicate <= 0 || run.replicate > cell.replicateCount =>
          return Left("campaign run replicate is invalid")
        case Some(_) =>
      }
      i += 1
    }
    val actualSlots = manifest.runs.map(run => (run.cellId, run.replicate))
    if (actualSlots.distinct.length != actualSlots.length)
      return Left("campaign run replicate slots are not unique")
    val expectedSlots = manifest.cells.flatMap { cell =>
      (1 to cell.replicateCount).map(replicate => (cell.id, replicate))
    }.toSet
    if (actualSlots.toSet != expectedSlots)
      return Left("campaign run replicate slots are incomplete")
    Right(())
  }

  def renderArchiveIndex(index: ArchiveIndex): Either[String, String] =
    validateArchiveIndex(index) match {
      case Left(detail) => Left(detail)
      case Right(_) => Right(renderArchiveIndexUnchecked(index))
    }

  def parseArchiveIndex(bytes: Array[Byte]): Either[String, ArchiveIndex] = {
    parseDocument(bytes, MaxArchiveIndexBytes, "archive index") match {
      case Left(detail) => Left(detail)
      case Right((text, root)) =>
        try {
          val fields = exactObject(root, Vector(
            "schema",
            "digestAlgorithm",
            "canonicalization",
            "campaignId",
            "manifestByteLength",
            "manifestSha256",
            "entries"), "archive index")
          requireString(fields("schema"), "archive index schema") match {
            case ArchiveIndexSchema =>
            case _ => fail("archive index schema is invalid")
          }
          requireString(fields("digestAlgorithm"), "archive index digest algorithm") match {
            case DigestAlgorithm =>
            case _ => fail("archive index digest algorithm is invalid")
          }
          requireString(fields("canonicalization"), "archive index canonicalization") match {
            case ArchiveIndexCanonicalization =>
            case _ => fail("archive index canonicalization is invalid")
          }
          val entries = requireArray(fields("entries"), "archive index entries").map { value =>
            val entry = exactObject(value, Vector(
              "runId",
              "evidenceByteLength",
              "evidenceSha256",
              "evidenceDigest"), "archive entry")
            ArchiveEntry(
              requireString(entry("runId"), "archive run ID"),
              requireInt(entry("evidenceByteLength"), "archive evidence byte length"),
              requireString(entry("evidenceSha256"), "archive evidence digest"),
              requireString(entry("evidenceDigest"), "archive payload digest"))
          }
          val index = ArchiveIndex(
            requireString(fields("campaignId"), "archive campaign ID"),
            requireInt(fields("manifestByteLength"), "archive manifest byte length"),
            requireString(fields("manifestSha256"), "archive manifest digest"),
            entries)
          validateArchiveIndex(index) match {
            case Left(detail) => Left(detail)
            case Right(_) =>
              if (renderArchiveIndexUnchecked(index) == text) Right(index)
              else Left("archive index is not canonical JSON")
          }
        } catch {
          case error: DecodeFailure => Left(error.getMessage)
        }
    }
  }

  def validateArchiveIndex(index: ArchiveIndex): Either[String, Unit] = {
    if (index == null) return Left("archive index is null")
    if (!isPublicId(index.campaignId)) return Left("archive campaign ID is invalid")
    if (index.manifestByteLength <= 0 || index.manifestByteLength > MaxCampaignManifestBytes)
      return Left("archive manifest byte length is invalid")
    if (!isLowerHexSha256(index.manifestSha256))
      return Left("archive manifest digest is invalid")
    if (index.entries == null || index.entries.isEmpty || index.entries.length > MaxRuns)
      return Left("archive entry count is invalid")
    if (index.entries.exists(_ == null)) return Left("archive entry is null")
    if (!areSortedUniqueIds(index.entries.map(_.runId)))
      return Left("archive run IDs are not sorted and unique")
    var i = 0
    while (i < index.entries.length) {
      val entry = index.entries(i)
      if (!isValidCampaignRunId(entry.runId)) return Left("archive run ID is invalid")
      if (entry.evidenceByteLength <= 0 || entry.evidenceByteLength > MaxEvidenceBytes)
        return Left("archive evidence byte length is invalid")
      if (!isLowerHexSha256(entry.evidenceSha256))
        return Left("archive evidence file digest is invalid")
      if (!isLowerHexSha256(entry.evidenceDigest))
        return Left("archive evidence payload digest is invalid")
      i += 1
    }
    Right(())
  }

  private def validateEnvironmentPolicy(
      policy: EnvironmentPolicy): Either[String, Unit] = {
    if (policy == null) return Left("campaign environment policy is null")
    if (!isPublicId(policy.id)) return Left("campaign environment policy ID is invalid")
    val strings = Vector(
      policy.javaRuntimeName,
      policy.javaRuntimeVersion,
      policy.javaVmName,
      policy.javaVmVendor,
      policy.javaVmVersion,
      policy.javaVmInfo,
      policy.scalaVersion,
      policy.osName,
      policy.osVersion,
      policy.osArch,
      policy.jitCompiler,
      policy.threadAllocationMeter,
      policy.cpuModel,
      policy.cpuModelSource)
    if (!strings.forall(isPublicText))
      return Left("campaign environment policy contains invalid public text")
    if (policy.availableProcessors <= 0)
      return Left("campaign available processor count is invalid")
    if (policy.maxHeapBytes <= 0L)
      return Left("campaign maximum heap size is invalid")
    if (policy.threadAllocationMeter != ExpectedThreadAllocationMeter)
      return Left("campaign thread allocation meter is invalid")
    if (policy.cpuModelSource != "--cpu-model")
      return Left("campaign CPU model must be explicitly declared")
    if (policy.garbageCollectors == null || policy.garbageCollectors.isEmpty ||
        policy.garbageCollectors.length > 32 ||
        !policy.garbageCollectors.forall(isPublicText) ||
        policy.garbageCollectors.distinct.length != policy.garbageCollectors.length ||
        policy.garbageCollectors != policy.garbageCollectors.sorted)
      return Left("campaign garbage collector policy is invalid")
    validateMemoryPoolIdentities(policy.memoryPoolIdentities) match {
      case Left(_) => return Left("campaign memory pool identity policy is invalid")
      case Right(_) =>
    }
    Right(())
  }

  private[benchmark] def environmentMatches(
      actual: EnvironmentMetadata,
      expected: EnvironmentPolicy): Boolean =
    actual != null && expected != null &&
      actual.javaRuntimeName == expected.javaRuntimeName &&
      actual.javaRuntimeVersion == expected.javaRuntimeVersion &&
      actual.javaVmName == expected.javaVmName &&
      actual.javaVmVendor == expected.javaVmVendor &&
      actual.javaVmVersion == expected.javaVmVersion &&
      actual.javaVmInfo == expected.javaVmInfo &&
      actual.scalaVersion == expected.scalaVersion &&
      actual.osName == expected.osName &&
      actual.osVersion == expected.osVersion &&
      actual.osArch == expected.osArch &&
      actual.availableProcessors == expected.availableProcessors &&
      actual.maxHeapBytes == expected.maxHeapBytes &&
      actual.jitCompiler == expected.jitCompiler &&
      actual.garbageCollectors == expected.garbageCollectors &&
      actual.memoryPoolIdentities == expected.memoryPoolIdentities &&
      actual.threadAllocationMeter == expected.threadAllocationMeter &&
      actual.cpuModel == expected.cpuModel &&
      actual.cpuModelSource == expected.cpuModelSource

  private def isPublicText(value: String): Boolean =
    value != null && value.nonEmpty && value.length <= MaxPublicStringCharacters &&
      !value.exists(Character.isISOControl)

  private def isPublicId(value: String): Boolean =
    isValidCampaignRunId(value)

  private def isExactRevisionIdentity(value: String): Boolean = {
    val commitPrefix = "commit:"
    val treePrefix = "tree-sha256:"
    (value.startsWith(commitPrefix) &&
      isLowerHex(value.substring(commitPrefix.length), 40)) ||
      (value.startsWith(treePrefix) &&
        isLowerHex(value.substring(treePrefix.length), 64))
  }

  private def isLowerHex(value: String, length: Int): Boolean =
    value != null && value.length == length && value.forall { ch =>
      (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f')
    }

  private[benchmark] def isLowerHexSha256(value: String): Boolean =
    isLowerHex(value, 64)

  private def areSortedUniqueIds(values: Vector[String]): Boolean =
    values != null && values.nonEmpty && values.forall(isPublicId) &&
      values == values.sorted && values.distinct.length == values.length

  private def renderManifestUnchecked(manifest: CampaignManifestV3): String = {
    val out = new StringBuilder(8192)
    out.append('{')
    stringField(out, "schema", ManifestSchema)
    comma(out); stringField(out, "canonicalization", ManifestCanonicalization)
    comma(out); stringField(out, "campaignId", manifest.campaignId)
    comma(out); stringField(out, "profileId", manifest.profileId)
    comma(out); stringField(out, "implementationRevision", manifest.implementationRevision)
    comma(out); stringField(out, "verifierEntryPoint", manifest.verifierEntryPoint)
    comma(out); out.append(quote("evidenceContract")).append(':')
    renderEvidenceContract(out, manifest.evidenceContract)
    comma(out); out.append(quote("resources")).append(':')
    renderArray(out, manifest.resources) { (builder, resource) =>
      builder.append('{')
      stringField(builder, "id", resource.id)
      comma(builder); stringField(builder, "classpath", resource.classpath)
      comma(builder); numberField(builder, "byteLength", resource.byteLength.toLong)
      comma(builder); stringField(builder, "sha256", resource.sha256)
      builder.append('}')
    }
    comma(out); out.append(quote("configuration")).append(':').append('{')
    numberField(out, "warmupRounds", manifest.warmupRounds.toLong)
    comma(out); numberField(out, "sampleRounds", manifest.sampleRounds.toLong)
    out.append('}')
    comma(out); out.append(quote("scenarios")).append(':')
    renderArray(out, manifest.scenarios) { (builder, scenario) =>
      builder.append('{')
      stringField(builder, "id", scenario.id)
      comma(builder); stringField(builder, "expectedOutcome", scenario.expectedOutcome)
      comma(builder); numberField(
        builder,
        "validationQueryCheckpoints",
        scenario.validationQueryCheckpoints.toLong)
      comma(builder); stringField(builder, "validationBoundary", scenario.validationBoundary)
      comma(builder); stringField(
        builder,
        "lastVerifierCheckpoint",
        scenario.lastVerifierCheckpoint)
      builder.append('}')
    }
    comma(out); out.append(quote("environmentPolicies")).append(':')
    renderArray(out, manifest.environmentPolicies)(renderEnvironmentPolicy)
    comma(out); out.append(quote("jvmArgumentPolicies")).append(':')
    renderArray(out, manifest.jvmArgumentPolicies) { (builder, policy) =>
      builder.append('{')
      stringField(builder, "id", policy.id)
      comma(builder); numberField(builder, "argumentCount", policy.argumentCount.toLong)
      comma(builder); stringField(builder, "argumentsSha256", policy.argumentsSha256)
      builder.append('}')
    }
    comma(out); out.append(quote("cells")).append(':')
    renderArray(out, manifest.cells) { (builder, cell) =>
      builder.append('{')
      stringField(builder, "id", cell.id)
      comma(builder); stringField(builder, "environmentPolicyId", cell.environmentPolicyId)
      comma(builder); stringField(builder, "jvmArgumentPolicyId", cell.jvmArgumentPolicyId)
      comma(builder); numberField(builder, "replicateCount", cell.replicateCount.toLong)
      builder.append('}')
    }
    comma(out); out.append(quote("runs")).append(':')
    renderArray(out, manifest.runs) { (builder, run) =>
      builder.append('{')
      stringField(builder, "id", run.id)
      comma(builder); stringField(builder, "cellId", run.cellId)
      comma(builder); numberField(builder, "replicate", run.replicate.toLong)
      builder.append('}')
    }
    out.append('}').append('\n')
    out.toString()
  }

  private def renderEvidenceContract(out: StringBuilder, contract: EvidenceContract): Unit = {
    out.append('{')
    stringField(out, "schema", contract.schema)
    comma(out); stringField(out, "digestAlgorithm", contract.digestAlgorithm)
    comma(out); stringField(out, "digestDomain", contract.digestDomain)
    comma(out); stringField(out, "canonicalization", contract.canonicalization)
    comma(out); stringField(out, "clock", contract.clock)
    comma(out); stringField(out, "schedule", contract.schedule)
    comma(out); stringField(out, "percentileMethod", contract.percentileMethod)
    comma(out); stringField(out, "timedScope", contract.timedScope)
    comma(out); stringField(out, "allocationScope", contract.allocationScope)
    comma(out); stringField(out, "garbageCollectionScope", contract.garbageCollectionScope)
    comma(out); stringField(out, "memoryPoolScope", contract.memoryPoolScope)
    out.append('}')
  }

  private def renderEnvironmentPolicy(
      out: StringBuilder,
      policy: EnvironmentPolicy): Unit = {
    out.append('{')
    stringField(out, "id", policy.id)
    comma(out); stringField(out, "javaRuntimeName", policy.javaRuntimeName)
    comma(out); stringField(out, "javaRuntimeVersion", policy.javaRuntimeVersion)
    comma(out); stringField(out, "javaVmName", policy.javaVmName)
    comma(out); stringField(out, "javaVmVendor", policy.javaVmVendor)
    comma(out); stringField(out, "javaVmVersion", policy.javaVmVersion)
    comma(out); stringField(out, "javaVmInfo", policy.javaVmInfo)
    comma(out); stringField(out, "scalaVersion", policy.scalaVersion)
    comma(out); stringField(out, "osName", policy.osName)
    comma(out); stringField(out, "osVersion", policy.osVersion)
    comma(out); stringField(out, "osArch", policy.osArch)
    comma(out); numberField(out, "availableProcessors", policy.availableProcessors.toLong)
    comma(out); numberField(out, "maxHeapBytes", policy.maxHeapBytes)
    comma(out); stringField(out, "jitCompiler", policy.jitCompiler)
    comma(out); out.append(quote("garbageCollectors")).append(':')
    renderStringArray(out, policy.garbageCollectors)
    comma(out); out.append(quote("memoryPoolIdentities")).append(':')
    renderArray(out, policy.memoryPoolIdentities) { (builder, identity) =>
      builder.append('{')
      stringField(builder, "name", identity.name)
      comma(builder); stringField(builder, "memoryType", identity.memoryType)
      builder.append('}')
    }
    comma(out); stringField(out, "threadAllocationMeter", policy.threadAllocationMeter)
    comma(out); stringField(out, "cpuModel", policy.cpuModel)
    comma(out); stringField(out, "cpuModelSource", policy.cpuModelSource)
    out.append('}')
  }

  private def renderArchiveIndexUnchecked(index: ArchiveIndex): String = {
    val out = new StringBuilder(4096)
    out.append('{')
    stringField(out, "schema", ArchiveIndexSchema)
    comma(out); stringField(out, "digestAlgorithm", DigestAlgorithm)
    comma(out); stringField(out, "canonicalization", ArchiveIndexCanonicalization)
    comma(out); stringField(out, "campaignId", index.campaignId)
    comma(out); numberField(out, "manifestByteLength", index.manifestByteLength.toLong)
    comma(out); stringField(out, "manifestSha256", index.manifestSha256)
    comma(out); out.append(quote("entries")).append(':')
    renderArray(out, index.entries) { (builder, entry) =>
      builder.append('{')
      stringField(builder, "runId", entry.runId)
      comma(builder); numberField(builder, "evidenceByteLength", entry.evidenceByteLength.toLong)
      comma(builder); stringField(builder, "evidenceSha256", entry.evidenceSha256)
      comma(builder); stringField(builder, "evidenceDigest", entry.evidenceDigest)
      builder.append('}')
    }
    out.append('}').append('\n')
    out.toString()
  }

  private def decodeManifest(root: JsonValue): CampaignManifestV3 = {
    val fields = exactObject(root, Vector(
      "schema",
      "canonicalization",
      "campaignId",
      "profileId",
      "implementationRevision",
      "verifierEntryPoint",
      "evidenceContract",
      "resources",
      "configuration",
      "scenarios",
      "environmentPolicies",
      "jvmArgumentPolicies",
      "cells",
      "runs"), "campaign manifest")
    if (requireString(fields("schema"), "campaign manifest schema") != ManifestSchema)
      fail("campaign manifest schema is invalid")
    if (requireString(fields("canonicalization"), "campaign manifest canonicalization") !=
        ManifestCanonicalization)
      fail("campaign manifest canonicalization is invalid")

    val contractFields = exactObject(fields("evidenceContract"), Vector(
      "schema",
      "digestAlgorithm",
      "digestDomain",
      "canonicalization",
      "clock",
      "schedule",
      "percentileMethod",
      "timedScope",
      "allocationScope",
      "garbageCollectionScope",
      "memoryPoolScope"), "campaign evidence contract")
    val contract = EvidenceContract(
      requireString(contractFields("schema"), "evidence schema"),
      requireString(contractFields("digestAlgorithm"), "evidence digest algorithm"),
      requireString(contractFields("digestDomain"), "evidence digest domain"),
      requireString(contractFields("canonicalization"), "evidence canonicalization"),
      requireString(contractFields("clock"), "evidence clock"),
      requireString(contractFields("schedule"), "evidence schedule"),
      requireString(contractFields("percentileMethod"), "evidence percentile method"),
      requireString(contractFields("timedScope"), "evidence timed scope"),
      requireString(contractFields("allocationScope"), "evidence allocation scope"),
      requireString(contractFields("garbageCollectionScope"), "evidence GC scope"),
      requireString(contractFields("memoryPoolScope"), "evidence memory pool scope"))

    val resources = requireArray(fields("resources"), "campaign resources").map { value =>
      val item = exactObject(value, Vector("id", "classpath", "byteLength", "sha256"),
        "campaign resource")
      ResourceMetadata(
        requireString(item("id"), "resource ID"),
        requireString(item("classpath"), "resource classpath"),
        requireInt(item("byteLength"), "resource byte length"),
        requireString(item("sha256"), "resource digest"))
    }
    val configuration = exactObject(fields("configuration"),
      Vector("warmupRounds", "sampleRounds"), "campaign configuration")
    val scenarios = requireArray(fields("scenarios"), "campaign scenarios").map { value =>
      val item = exactObject(value,
        Vector(
          "id",
          "expectedOutcome",
          "validationQueryCheckpoints",
          "validationBoundary",
          "lastVerifierCheckpoint"),
        "campaign scenario")
      ScenarioPolicy(
        requireString(item("id"), "scenario ID"),
        requireString(item("expectedOutcome"), "scenario outcome"),
        requireInt(item("validationQueryCheckpoints"), "scenario checkpoints"),
        requireString(item("validationBoundary"), "scenario validation boundary"),
        requireString(item("lastVerifierCheckpoint"), "scenario last verifier checkpoint"))
    }
    val environments = requireArray(
      fields("environmentPolicies"), "campaign environment policies").map(decodeEnvironment)
    val arguments = requireArray(
      fields("jvmArgumentPolicies"), "campaign JVM argument policies").map { value =>
      val item = exactObject(value,
        Vector("id", "argumentCount", "argumentsSha256"),
        "campaign JVM argument policy")
      JvmArgumentPolicy(
        requireString(item("id"), "JVM argument policy ID"),
        requireInt(item("argumentCount"), "JVM argument count"),
        requireString(item("argumentsSha256"), "JVM argument digest"))
    }
    val cells = requireArray(fields("cells"), "campaign cells").map { value =>
      val item = exactObject(value,
        Vector("id", "environmentPolicyId", "jvmArgumentPolicyId", "replicateCount"),
        "campaign cell")
      CampaignCell(
        requireString(item("id"), "cell ID"),
        requireString(item("environmentPolicyId"), "cell environment policy ID"),
        requireString(item("jvmArgumentPolicyId"), "cell JVM argument policy ID"),
        requireInt(item("replicateCount"), "cell replicate count"))
    }
    val runs = requireArray(fields("runs"), "campaign runs").map { value =>
      val item = exactObject(value, Vector("id", "cellId", "replicate"), "campaign run")
      CampaignRun(
        requireString(item("id"), "run ID"),
        requireString(item("cellId"), "run cell ID"),
        requireInt(item("replicate"), "run replicate"))
    }
    CampaignManifestV3(
      requireString(fields("campaignId"), "campaign ID"),
      requireString(fields("profileId"), "campaign profile ID"),
      requireString(fields("implementationRevision"), "implementation revision"),
      requireString(fields("verifierEntryPoint"), "verifier entry point"),
      contract,
      resources,
      requireInt(configuration("warmupRounds"), "warmup rounds"),
      requireInt(configuration("sampleRounds"), "sample rounds"),
      scenarios,
      environments,
      arguments,
      cells,
      runs)
  }

  private def decodeEnvironment(value: JsonValue): EnvironmentPolicy = {
    val item = exactObject(value, Vector(
      "id",
      "javaRuntimeName",
      "javaRuntimeVersion",
      "javaVmName",
      "javaVmVendor",
      "javaVmVersion",
      "javaVmInfo",
      "scalaVersion",
      "osName",
      "osVersion",
      "osArch",
      "availableProcessors",
      "maxHeapBytes",
      "jitCompiler",
      "garbageCollectors",
      "memoryPoolIdentities",
      "threadAllocationMeter",
      "cpuModel",
      "cpuModelSource"), "campaign environment policy")
    EnvironmentPolicy(
      requireString(item("id"), "environment policy ID"),
      requireString(item("javaRuntimeName"), "Java runtime name"),
      requireString(item("javaRuntimeVersion"), "Java runtime version"),
      requireString(item("javaVmName"), "Java VM name"),
      requireString(item("javaVmVendor"), "Java VM vendor"),
      requireString(item("javaVmVersion"), "Java VM version"),
      requireString(item("javaVmInfo"), "Java VM info"),
      requireString(item("scalaVersion"), "Scala version"),
      requireString(item("osName"), "OS name"),
      requireString(item("osVersion"), "OS version"),
      requireString(item("osArch"), "OS architecture"),
      requireInt(item("availableProcessors"), "available processors"),
      requireLong(item("maxHeapBytes"), "maximum heap bytes"),
      requireString(item("jitCompiler"), "JIT compiler"),
      requireStringArray(item("garbageCollectors"), "garbage collectors"),
      requireArray(item("memoryPoolIdentities"), "memory pool identities").map { value =>
        val identity = exactObject(
          value,
          Vector("name", "memoryType"),
          "memory pool identity")
        MemoryPoolIdentity(
          requireString(identity("name"), "memory pool name"),
          requireString(identity("memoryType"), "memory pool type"))
      },
      requireString(item("threadAllocationMeter"), "thread allocation meter"),
      requireString(item("cpuModel"), "CPU model"),
      requireString(item("cpuModelSource"), "CPU model source"))
  }

  private[benchmark] def parseDocument(
      bytes: Array[Byte],
      maximumBytes: Int,
      label: String): Either[String, (String, JsonValue)] = {
    if (bytes == null) return Left(label + " bytes are null")
    if (bytes.isEmpty) return Left(label + " is empty")
    if (bytes.length > maximumBytes) return Left(label + " exceeds " + maximumBytes + " bytes")
    strictUtf8(bytes) match {
      case Left(_) => Left(label + " is not strict UTF-8")
      case Right(text) =>
        try Right((text, new JsonParser(text).parse()))
        catch {
          case _: DecodeFailure => Left(label + " contains invalid bounded JSON")
        }
    }
  }

  private def strictUtf8(bytes: Array[Byte]): Either[String, String] = {
    val decoder = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch {
      case _: CharacterCodingException => Left("invalid UTF-8")
    }
  }

  private def strictUtf8ByteLength(text: String): Either[String, Int] = {
    val encoder = StandardCharsets.UTF_8.newEncoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(encoder.encode(CharBuffer.wrap(text)).remaining())
    catch {
      case _: CharacterCodingException => Left("invalid UTF-8")
    }
  }

  private final class JsonParser(input: String) {
    private var offset = 0
    private var values = 0

    def parse(): JsonValue = {
      skipWhitespace()
      val value = parseValue(0)
      skipWhitespace()
      if (offset != input.length) fail("trailing JSON data")
      value
    }

    private def parseValue(depth: Int): JsonValue = {
      skipWhitespace()
      if (depth > MaxJsonDepth) fail("JSON nesting is too deep")
      values += 1
      if (values > MaxJsonValues) fail("JSON value count is too large")
      if (offset >= input.length) fail("unexpected end of JSON")
      input.charAt(offset) match {
        case '{' => parseObject(depth + 1)
        case '[' => parseArray(depth + 1)
        case '"' => JsonString(parseString())
        case 't' => literal("true", JsonBoolean(true))
        case 'f' => literal("false", JsonBoolean(false))
        case 'n' => literal("null", JsonNull)
        case '-' => JsonNumber(parseNumber())
        case ch if ch >= '0' && ch <= '9' => JsonNumber(parseNumber())
        case _ => fail("invalid JSON token")
      }
    }

    private def parseObject(depth: Int): JsonObject = {
      offset += 1
      val out = Vector.newBuilder[(String, JsonValue)]
      val keys = mutable.HashSet.empty[String]
      var count = 0
      if (take('}')) return JsonObject(Vector.empty)
      while (true) {
        skipWhitespace()
        if (offset >= input.length || input.charAt(offset) != '"') fail("object key is missing")
        val key = parseString()
        if (!keys.add(key)) fail("duplicate object key")
        if (!take(':')) fail("object colon is missing")
        out += key -> parseValue(depth)
        count += 1
        if (count > MaxJsonObjectMembers) fail("JSON object has too many members")
        if (take('}')) return JsonObject(out.result())
        if (!take(',')) fail("object separator is missing")
      }
      JsonObject(Vector.empty)
    }

    private def parseArray(depth: Int): JsonArray = {
      offset += 1
      val out = Vector.newBuilder[JsonValue]
      var count = 0
      if (take(']')) return JsonArray(Vector.empty)
      while (true) {
        out += parseValue(depth)
        count += 1
        if (count > MaxJsonArrayItems) fail("JSON array has too many items")
        if (take(']')) return JsonArray(out.result())
        if (!take(',')) fail("array separator is missing")
      }
      JsonArray(Vector.empty)
    }

    private def parseString(): String = {
      if (!take('"')) fail("string opening quote is missing")
      val out = new StringBuilder
      while (offset < input.length) {
        val ch = input.charAt(offset)
        offset += 1
        if (ch == '"') return out.toString()
        if (ch < ' ') fail("raw control character in JSON string")
        if (ch == '\\') parseEscape(out)
        else if (Character.isHighSurrogate(ch)) {
          if (offset >= input.length || !Character.isLowSurrogate(input.charAt(offset)))
            fail("unpaired surrogate in JSON string")
          out.append(ch).append(input.charAt(offset))
          offset += 1
        } else if (Character.isLowSurrogate(ch)) fail("unpaired surrogate in JSON string")
        else out.append(ch)
        if (out.length > MaxPublicStringCharacters) fail("JSON string is too long")
      }
      fail("unterminated JSON string")
    }

    private def parseEscape(out: StringBuilder): Unit = {
      if (offset >= input.length) fail("unterminated JSON escape")
      val escaped = input.charAt(offset)
      offset += 1
      escaped match {
        case '"' => out.append('"')
        case '\\' => out.append('\\')
        case '/' => out.append('/')
        case 'b' => out.append('\b')
        case 'f' => out.append('\f')
        case 'n' => out.append('\n')
        case 'r' => out.append('\r')
        case 't' => out.append('\t')
        case 'u' =>
          val first = parseHexChar()
          if (Character.isHighSurrogate(first)) {
            if (offset + 1 >= input.length || input.charAt(offset) != '\\' ||
                input.charAt(offset + 1) != 'u') fail("unpaired escaped surrogate")
            offset += 2
            val second = parseHexChar()
            if (!Character.isLowSurrogate(second)) fail("unpaired escaped surrogate")
            out.append(first).append(second)
          } else if (Character.isLowSurrogate(first)) fail("unpaired escaped surrogate")
          else out.append(first)
        case _ => fail("invalid JSON escape")
      }
    }

    private def parseHexChar(): Char = {
      if (offset + 4 > input.length) fail("short Unicode escape")
      var value = 0
      var i = 0
      while (i < 4) {
        val digit = Character.digit(input.charAt(offset + i), 16)
        if (digit < 0) fail("invalid Unicode escape")
        value = (value << 4) | digit
        i += 1
      }
      offset += 4
      value.toChar
    }

    private def parseNumber(): String = {
      val start = offset
      if (input.charAt(offset) == '-') offset += 1
      if (offset >= input.length) fail("short JSON number")
      if (input.charAt(offset) == '0') offset += 1
      else {
        if (input.charAt(offset) < '1' || input.charAt(offset) > '9') fail("invalid JSON number")
        while (offset < input.length && input.charAt(offset) >= '0' && input.charAt(offset) <= '9')
          offset += 1
      }
      if (offset < input.length &&
          (input.charAt(offset) == '.' || input.charAt(offset) == 'e' ||
            input.charAt(offset) == 'E')) fail("non-integer JSON number")
      val value = input.substring(start, offset)
      if (value.length > 20) fail("JSON integer is too long")
      value
    }

    private def literal(value: String, parsed: JsonValue): JsonValue = {
      if (!input.regionMatches(offset, value, 0, value.length)) fail("invalid JSON literal")
      offset += value.length
      parsed
    }

    private def take(expected: Char): Boolean = {
      skipWhitespace()
      if (offset < input.length && input.charAt(offset) == expected) {
        offset += 1
        true
      } else false
    }

    private def skipWhitespace(): Unit =
      while (offset < input.length && {
        val ch = input.charAt(offset)
        ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
      }) offset += 1
  }

  private[benchmark] def exactObject(
      value: JsonValue,
      names: Vector[String],
      label: String): Map[String, JsonValue] = value match {
    case JsonObject(fields) if fields.map(_._1) == names => fields.toMap
    case JsonObject(_) => fail(label + " fields are invalid")
    case _ => fail(label + " is not an object")
  }

  private[benchmark] def requireArray(
      value: JsonValue,
      label: String): Vector[JsonValue] = value match {
    case JsonArray(values) => values
    case _ => fail(label + " is not an array")
  }

  private[benchmark] def requireStringArray(
      value: JsonValue,
      label: String): Vector[String] =
    requireArray(value, label).map(item => requireString(item, label + " item"))

  private[benchmark] def requireString(value: JsonValue, label: String): String = value match {
    case JsonString(text) => text
    case _ => fail(label + " is not a string")
  }

  private[benchmark] def requireLong(value: JsonValue, label: String): Long = value match {
    case JsonNumber(text) =>
      try text.toLong
      catch {
        case _: NumberFormatException => fail(label + " is outside the 64-bit integer range")
      }
    case _ => fail(label + " is not an integer")
  }

  private[benchmark] def requireInt(value: JsonValue, label: String): Int = {
    val parsed = requireLong(value, label)
    if (parsed < Int.MinValue.toLong || parsed > Int.MaxValue.toLong)
      fail(label + " is outside the 32-bit integer range")
    parsed.toInt
  }

  private[benchmark] def fail(message: String): Nothing =
    throw new DecodeFailure(message)

  private def comma(out: StringBuilder): Unit = out.append(',')

  private def stringField(out: StringBuilder, name: String, value: String): Unit =
    out.append(quote(name)).append(':').append(quote(value))

  private def numberField(out: StringBuilder, name: String, value: Long): Unit =
    out.append(quote(name)).append(':').append(value)

  private def renderStringArray(out: StringBuilder, values: Vector[String]): Unit = {
    out.append('[')
    var i = 0
    while (i < values.length) {
      if (i > 0) comma(out)
      out.append(quote(values(i)))
      i += 1
    }
    out.append(']')
  }

  private def renderArray[A](
      out: StringBuilder,
      values: Vector[A])(
      render: (StringBuilder, A) => Unit): Unit = {
    out.append('[')
    var i = 0
    while (i < values.length) {
      if (i > 0) comma(out)
      render(out, values(i))
      i += 1
    }
    out.append(']')
  }
}
