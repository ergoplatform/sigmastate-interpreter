/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import java.nio.CharBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.security.MessageDigest

/** Deterministic, dependency-free model and encoder for EIP-0045 B5 JVM
  * timing and resource evidence. It lives in JVM test sources so it cannot
  * enter consensus jars or the normal test run unless a focused support test
  * explicitly exercises it.
  */
private[benchmark] object Eip0045BenchmarkSupport {
  final val Schema: String = "eip-0045-jvm-verifier-benchmark-v5"
  final val DigestAlgorithm: String = "SHA-256"
  final val DigestDomain: String = "Ergo.EIP0045.B5.Evidence.v5"
  final val JvmInputArgumentsDigestDomain: String =
    "Ergo.EIP0045.B5.JvmInputArguments.v1"
  final val Canonicalization: String = "utf8-fixed-field-order-no-whitespace-v5"
  final val DefaultWarmupRounds: Int = 15
  final val DefaultSampleRounds: Int = 100
  final val NoProbeVerifierRoute: String = "no-probe"
  final val OperationOnlyVerifierRoute: String = "operation-only"
  final val MaxWarmupRounds: Int = 10000
  final val MaxSampleRounds: Int = 10000
  final val MaxCampaignManifestBytes: Int = 1024 * 1024
  final val MaxCampaignRunIdCharacters: Int = 128
  final val MaxDiagnosticScenarioIdCharacters: Int = 64
  final val MaxJvmInputArguments: Int = 1024
  final val MaxJvmInputArgumentBytes: Int = 64 * 1024
  final val MaxJvmInputArgumentsTotalBytes: Int = 1024 * 1024
  final val MaxMemoryPools: Int = 256

  final case class Config(
      warmupRounds: Int,
      sampleRounds: Int,
      outputPath: Option[String],
      declaredCpuModel: Option[String],
      implementationRevision: String,
      campaignManifestPath: Option[String],
      campaignRunId: Option[String],
      diagnosticScenario: Option[String] = None,
      verifierRoute: String = NoProbeVerifierRoute,
      pairedAllocation: Boolean = false)

  final case class ResourceMetadata(
      id: String,
      classpath: String,
      byteLength: Int,
      sha256: String)

  final case class MemoryPoolIdentity(
      name: String,
      memoryType: String)

  final case class MemoryUsageEvidence(
      usedBytes: Long,
      committedBytes: Long,
      maxBytes: Long)

  final case class MemoryPoolPhaseEnvelope(
      identity: MemoryPoolIdentity,
      afterResetPeakUsage: MemoryUsageEvidence,
      endUsage: MemoryUsageEvidence,
      finalPeakUsage: MemoryUsageEvidence)

  final case class EnvironmentMetadata(
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
      jvmInputArgumentCount: Int,
      jvmInputArgumentsSha256: String,
      cpuModel: String,
      cpuModelSource: String)

  final case class CampaignBinding(
      runId: String,
      manifestByteLength: Int,
      manifestSha256: String)

  final case class JvmInputArgumentsIdentity(
      argumentCount: Int,
      argumentsSha256: String)

  final case class TimingStatistics(
      sampleCount: Int,
      p50Ns: Long,
      p95Ns: Long,
      p99Ns: Long,
      maxNs: Long)

  final case class AllocationStatistics(
      sampleCount: Int,
      p50Bytes: Long,
      p95Bytes: Long,
      p99Bytes: Long,
      maxBytes: Long)

  final case class GarbageCollectorSnapshot(
      name: String,
      collectionCount: Long,
      collectionTimeMs: Long)

  final case class GarbageCollectorDelta(
      name: String,
      collections: Long,
      collectionTimeMs: Long)

  final case class ScenarioEvidence(
      id: String,
      expectedOutcome: String,
      validationQueryCheckpoints: Int,
      validationBoundary: String,
      lastVerifierCheckpoint: String,
      samplesNs: Vector[Long],
      statistics: TimingStatistics,
      allocatedBytes: Vector[Long],
      allocationStatistics: AllocationStatistics)

  final case class EvidencePayload(
      startedAtUtc: String,
      benchmarkDurationNs: Long,
      profileId: String,
      implementationRevision: String,
      verifierEntryPoint: String,
      resources: Vector[ResourceMetadata],
      warmupRounds: Int,
      sampleRounds: Int,
      campaignBinding: Option[CampaignBinding],
      environment: EnvironmentMetadata,
      scenarios: Vector[ScenarioEvidence],
      garbageCollectorDeltas: Vector[GarbageCollectorDelta],
      memoryPoolPhaseEnvelopes: Vector[MemoryPoolPhaseEnvelope],
      limitations: Vector[String])

  val Usage: String =
    """EIP-0045 JVM verifier benchmark (opt-in; never run by the test suite)
      |Requires current-thread allocation and garbage-collector counters; emits no evidence if unavailable.
      |Usage: Eip0045VerifierBenchmark [options]
      |  --warmup-rounds N   Complete round-robin warmup rounds (default: 15)
      |  --sample-rounds N   Timed samples per scenario (default: 100)
      |  --output FILE       Write one UTF-8 JSON evidence file; refuses overwrite
      |  --cpu-model TEXT    Explicit public CPU description for cross-host evidence
      |  --implementation-revision TEXT
      |                      Exact public commit or source-tree digest (default: unrecorded)
      |  --campaign-manifest FILE
      |                      Enforce an exact manifest run policy; requires --campaign-run-id
      |  --campaign-run-id ID
      |                      Public manifest run ID; requires --campaign-manifest
      |  --diagnostic-scenario ID
      |                      Measure one named scenario; incompatible with campaign mode
      |  --verifier-route ROUTE
      |                      Diagnostic verifier route: no-probe (default) or operation-only
      |  --paired-allocation Run the route-exact preflight and allocation-count gate
      |  --help              Show this help
      |""".stripMargin

  def parseArgs(args: Array[String]): Either[String, Config] = {
    if (args == null) return Left("argument array is null")

    var warmupRounds = DefaultWarmupRounds
    var sampleRounds = DefaultSampleRounds
    var outputPath: Option[String] = None
    var cpuModel: Option[String] = None
    var implementationRevision = "unrecorded"
    var campaignManifestPath: Option[String] = None
    var campaignRunId: Option[String] = None
    var diagnosticScenario: Option[String] = None
    var verifierRoute = NoProbeVerifierRoute
    var pairedAllocation = false
    var seenWarmup = false
    var seenSamples = false
    var seenOutput = false
    var seenCpu = false
    var seenRevision = false
    var seenCampaignManifest = false
    var seenCampaignRunId = false
    var seenDiagnosticScenario = false
    var seenVerifierRoute = false
    var seenPairedAllocation = false
    var i = 0
    while (i < args.length) {
      val option = args(i)
      if (option == null) return Left("argument at index " + i + " is null")
      option match {
        case "--warmup-rounds" =>
          if (seenWarmup) return Left("duplicate option --warmup-rounds")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          parseBoundedNonNegativeInt(option, value, MaxWarmupRounds) match {
            case Right(v) => warmupRounds = v
            case Left(e)  => return Left(e)
          }
          seenWarmup = true
          i += 2
        case "--sample-rounds" =>
          if (seenSamples) return Left("duplicate option --sample-rounds")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          parseBoundedPositiveInt(option, value, MaxSampleRounds) match {
            case Right(v) => sampleRounds = v
            case Left(e)  => return Left(e)
          }
          seenSamples = true
          i += 2
        case "--output" =>
          if (seenOutput) return Left("duplicate option --output")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          if (value.trim.isEmpty) return Left("--output must not be empty")
          outputPath = Some(value)
          seenOutput = true
          i += 2
        case "--cpu-model" =>
          if (seenCpu) return Left("duplicate option --cpu-model")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          val normalized = value.trim
          if (normalized.isEmpty) return Left("--cpu-model must not be empty")
          if (normalized.length > 256)
            return Left("--cpu-model must be at most 256 characters")
          if (normalized.exists(ch => ch < ' '))
            return Left("--cpu-model must not contain control characters")
          cpuModel = Some(normalized)
          seenCpu = true
          i += 2
        case "--implementation-revision" =>
          if (seenRevision) return Left("duplicate option --implementation-revision")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          validateImplementationRevision(value) match {
            case Right(normalized) => implementationRevision = normalized
            case Left(detail) => return Left("--implementation-revision " + detail)
          }
          seenRevision = true
          i += 2
        case "--campaign-manifest" =>
          if (seenCampaignManifest) return Left("duplicate option --campaign-manifest")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          if (value.trim.isEmpty) return Left("--campaign-manifest must not be empty")
          campaignManifestPath = Some(value)
          seenCampaignManifest = true
          i += 2
        case "--campaign-run-id" =>
          if (seenCampaignRunId) return Left("duplicate option --campaign-run-id")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          val normalized = value.trim
          if (!isValidCampaignRunId(normalized))
            return Left(
              "--campaign-run-id must be 1-" + MaxCampaignRunIdCharacters +
                " characters using only ASCII letters, digits, '.', '_', ':', or '-'")
          campaignRunId = Some(normalized)
          seenCampaignRunId = true
          i += 2
        case "--diagnostic-scenario" =>
          if (seenDiagnosticScenario)
            return Left("duplicate option --diagnostic-scenario")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          if (!isValidDiagnosticScenarioId(value))
            return Left(
              "--diagnostic-scenario must be 1-" +
                MaxDiagnosticScenarioIdCharacters +
                " characters using only lowercase ASCII letters, digits, or '-'")
          diagnosticScenario = Some(value)
          seenDiagnosticScenario = true
          i += 2
        case "--verifier-route" =>
          if (seenVerifierRoute)
            return Left("duplicate option --verifier-route")
          val value = nextValue(args, i, option) match {
            case Right(v) => v
            case Left(e)  => return Left(e)
          }
          value match {
            case NoProbeVerifierRoute | OperationOnlyVerifierRoute =>
              verifierRoute = value
            case _ =>
              return Left(
                "--verifier-route must be no-probe or operation-only")
          }
          seenVerifierRoute = true
          i += 2
        case "--paired-allocation" =>
          if (seenPairedAllocation)
            return Left("duplicate option --paired-allocation")
          pairedAllocation = true
          seenPairedAllocation = true
          i += 1
        case "--help" =>
          return Left("--help must be handled before argument parsing")
        case _ =>
          return Left("unknown option at argument index " + i)
      }
    }
    if (diagnosticScenario.isDefined &&
        (campaignManifestPath.isDefined || campaignRunId.isDefined))
      return Left("--diagnostic-scenario cannot be combined with campaign options")
    if (verifierRoute == OperationOnlyVerifierRoute && diagnosticScenario.isEmpty)
      return Left(
        "--verifier-route operation-only requires --diagnostic-scenario")
    if (pairedAllocation && diagnosticScenario.isEmpty)
      return Left("--paired-allocation requires --diagnostic-scenario")
    if (campaignManifestPath.isDefined != campaignRunId.isDefined)
      return Left("--campaign-manifest and --campaign-run-id must be supplied together")
    if (campaignManifestPath.isDefined && implementationRevision == "unrecorded")
      return Left(
        "campaign-bound runs require a recorded --implementation-revision")

    Right(Config(
      warmupRounds,
      sampleRounds,
      outputPath,
      cpuModel,
      implementationRevision,
      campaignManifestPath,
      campaignRunId,
      diagnosticScenario,
      verifierRoute,
      pairedAllocation))
  }

  private def nextValue(
      args: Array[String],
      optionIndex: Int,
      option: String): Either[String, String] = {
    val valueIndex = optionIndex + 1
    if (valueIndex >= args.length) Left("missing value for " + option)
    else if (args(valueIndex) == null) Left("null value for " + option)
    else Right(args(valueIndex))
  }

  private def parseBoundedNonNegativeInt(
      option: String,
      value: String,
      maximum: Int): Either[String, Int] = {
    parseInt(option, value) match {
      case Right(parsed) if parsed >= 0 && parsed <= maximum => Right(parsed)
      case Right(parsed) if parsed > maximum => Left(option + " must be at most " + maximum)
      case Right(_) => Left(option + " must be non-negative")
      case Left(e)  => Left(e)
    }
  }

  private def parseBoundedPositiveInt(
      option: String,
      value: String,
      maximum: Int): Either[String, Int] = {
    parseInt(option, value) match {
      case Right(parsed) if parsed > 0 && parsed <= maximum => Right(parsed)
      case Right(parsed) if parsed > maximum => Left(option + " must be at most " + maximum)
      case Right(_) => Left(option + " must be positive")
      case Left(e)  => Left(e)
    }
  }

  private def parseInt(option: String, value: String): Either[String, Int] = {
    try Right(value.toInt)
    catch {
      case _: NumberFormatException => Left(option + " must be a base-10 32-bit integer")
    }
  }

  private[benchmark] def isValidCampaignRunId(value: String): Boolean = {
    if (value == null || value.isEmpty || value.length > MaxCampaignRunIdCharacters)
      false
    else value.forall { ch =>
      (ch >= 'a' && ch <= 'z') ||
      (ch >= 'A' && ch <= 'Z') ||
      (ch >= '0' && ch <= '9') ||
      ch == '.' || ch == '_' || ch == ':' || ch == '-'
    }
  }

  private[benchmark] def isValidDiagnosticScenarioId(value: String): Boolean = {
    if (value == null || value.isEmpty ||
        value.length > MaxDiagnosticScenarioIdCharacters) false
    else value.forall { ch =>
      (ch >= 'a' && ch <= 'z') ||
      (ch >= '0' && ch <= '9') ||
      ch == '-'
    }
  }

  private[benchmark] def validateImplementationRevision(
      value: String): Either[String, String] = {
    if (value == null) Left("is null")
    else if (value.length > 256) Left("must be at most 256 characters")
    else if (value.exists(ch => Character.isISOControl(ch)))
      Left("must not contain control characters")
    else {
      val normalized = value.trim
      if (normalized.isEmpty) Left("must not be empty")
      else Right(normalized)
    }
  }

  def campaignBindingFromBytes(
      runId: String,
      manifestBytes: Array[Byte]): Either[String, CampaignBinding] = {
    if (!isValidCampaignRunId(runId))
      Left("campaign run ID is invalid")
    else if (manifestBytes == null)
      Left("campaign manifest bytes are null")
    else if (manifestBytes.isEmpty)
      Left("campaign manifest is empty")
    else if (manifestBytes.length > MaxCampaignManifestBytes)
      Left("campaign manifest exceeds " + MaxCampaignManifestBytes + " bytes")
    else Right(CampaignBinding(
      runId,
      manifestBytes.length,
      sha256Hex(manifestBytes)))
  }

  /** Identity of the exact ordered JVM input-argument strings reported by
    * RuntimeMXBean. Each strict UTF-8 value is prefixed with its unsigned
    * 32-bit byte length, and the sequence begins with its element count.
    * Raw arguments are never retained in the evidence envelope.
    */
  def jvmInputArgumentsIdentity(
      arguments: Seq[String]): Either[String, JvmInputArgumentsIdentity] = {
    if (arguments == null) return Left("JVM input arguments are null")
    if (arguments.length > MaxJvmInputArguments)
      return Left("JVM input argument count exceeds " + MaxJvmInputArguments)

    val digest = MessageDigest.getInstance(DigestAlgorithm)
    digest.update(JvmInputArgumentsDigestDomain.getBytes(StandardCharsets.US_ASCII))
    digest.update(0.toByte)
    updateUint32(digest, arguments.length)
    var totalBytes = 0L
    var i = 0
    while (i < arguments.length) {
      val argument = arguments(i)
      if (argument == null)
        return Left("JVM input argument at index " + i + " is null")
      if (argument.length > MaxJvmInputArgumentBytes)
        return Left(
          "JVM input argument at index " + i + " exceeds " +
            MaxJvmInputArgumentBytes + " characters before UTF-8 encoding")
      val encoded = strictUtf8(argument) match {
        case Right(value) => value
        case Left(detail) => return Left(
          "JVM input argument at index " + i + " " + detail)
      }
      if (encoded.length > MaxJvmInputArgumentBytes)
        return Left(
          "JVM input argument at index " + i + " exceeds " +
            MaxJvmInputArgumentBytes + " UTF-8 bytes")
      totalBytes += encoded.length.toLong
      if (totalBytes > MaxJvmInputArgumentsTotalBytes.toLong)
        return Left(
          "JVM input arguments exceed " + MaxJvmInputArgumentsTotalBytes +
            " total UTF-8 bytes")
      updateUint32(digest, encoded.length)
      digest.update(encoded)
      i += 1
    }
    Right(JvmInputArgumentsIdentity(arguments.length, hex(digest.digest())))
  }

  private def strictUtf8(value: String): Either[String, Array[Byte]] = {
    val encoder = StandardCharsets.UTF_8.newEncoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try {
      val buffer = encoder.encode(CharBuffer.wrap(value))
      val out = new Array[Byte](buffer.remaining())
      buffer.get(out)
      Right(out)
    } catch {
      case _: CharacterCodingException => Left("is not valid Unicode")
    }
  }

  private def updateUint32(digest: MessageDigest, value: Int): Unit = {
    require(value >= 0, "framed value is negative")
    digest.update(((value >>> 24) & 0xff).toByte)
    digest.update(((value >>> 16) & 0xff).toByte)
    digest.update(((value >>> 8) & 0xff).toByte)
    digest.update((value & 0xff).toByte)
  }

  /** Nearest-rank percentiles. Raw samples remain in the evidence so every
    * summary can be independently recomputed.
    */
  def statistics(samples: Seq[Long]): Either[String, TimingStatistics] = {
    if (samples == null) return Left("samples are null")
    if (samples.isEmpty) return Left("samples are empty")
    if (samples.exists(_ < 0L)) return Left("samples contain a negative duration")
    val sorted = samples.sorted
    Right(TimingStatistics(
      sorted.length,
      nearestRank(sorted, 50),
      nearestRank(sorted, 95),
      nearestRank(sorted, 99),
      sorted.last))
  }

  def allocationStatistics(samples: Seq[Long]): Either[String, AllocationStatistics] = {
    validatedSamples(samples, "allocation") match {
      case Left(detail) => Left(detail)
      case Right(sorted) =>
        Right(AllocationStatistics(
          sorted.length,
          nearestRank(sorted, 50),
          nearestRank(sorted, 95),
          nearestRank(sorted, 99),
          sorted.last))
    }
  }

  def allocatedBytesDelta(before: Long, after: Long): Either[String, Long] = {
    if (before < 0L) Left("thread allocation counter before sample is unavailable")
    else if (after < 0L) Left("thread allocation counter after sample is unavailable")
    else if (after < before) Left("thread allocation counter moved backwards")
    else Right(after - before)
  }

  def garbageCollectorDeltas(
      before: Seq[GarbageCollectorSnapshot],
      after: Seq[GarbageCollectorSnapshot]): Either[String, Vector[GarbageCollectorDelta]] = {
    if (before == null) return Left("garbage collector snapshot before sampling is null")
    if (after == null) return Left("garbage collector snapshot after sampling is null")
    if (before.isEmpty || after.isEmpty)
      return Left("garbage collector snapshot is empty")
    if (before.exists(_ == null) || after.exists(_ == null))
      return Left("garbage collector snapshot contains a null entry")
    if (before.exists(item => item.name == null || item.name.isEmpty) ||
        after.exists(item => item.name == null || item.name.isEmpty))
      return Left("garbage collector snapshot contains an empty name")
    if (before.map(_.name).distinct.length != before.length)
      return Left("garbage collector snapshot before sampling contains duplicate names")
    if (after.map(_.name).distinct.length != after.length)
      return Left("garbage collector snapshot after sampling contains duplicate names")
    if (before.map(_.name) != after.map(_.name))
      return Left("garbage collector set or order changed during sampling")

    val out = Vector.newBuilder[GarbageCollectorDelta]
    var i = 0
    while (i < before.length) {
      val start = before(i)
      val end = after(i)
      if (start.collectionCount < 0L || end.collectionCount < 0L)
        return Left("garbage collector " + start.name + " does not expose collection counts")
      if (start.collectionTimeMs < 0L || end.collectionTimeMs < 0L)
        return Left("garbage collector " + start.name + " does not expose collection time")
      if (end.collectionCount < start.collectionCount)
        return Left("garbage collector " + start.name + " collection count moved backwards")
      if (end.collectionTimeMs < start.collectionTimeMs)
        return Left("garbage collector " + start.name + " collection time moved backwards")
      out += GarbageCollectorDelta(
        start.name,
        end.collectionCount - start.collectionCount,
        end.collectionTimeMs - start.collectionTimeMs)
      i += 1
    }
    Right(out.result())
  }

  private[benchmark] def sortMemoryPoolIdentities(
      identities: Vector[MemoryPoolIdentity]): Vector[MemoryPoolIdentity] =
    identities.sortBy(identity => (identity.name, identity.memoryType))

  def validateMemoryPoolIdentities(
      identities: Vector[MemoryPoolIdentity]): Either[String, Unit] = {
    if (identities == null) return Left("memory pool identities are null")
    if (identities.isEmpty) return Left("memory pool identities are empty")
    if (identities.length > MaxMemoryPools)
      return Left("memory pool identity count exceeds " + MaxMemoryPools)
    if (identities.exists(_ == null)) return Left("memory pool identities contain null")
    var i = 0
    while (i < identities.length) {
      val identity = identities(i)
      if (identity.name == null || identity.name.isEmpty ||
          identity.name.length > 4096 || identity.name.exists(Character.isISOControl))
        return Left("memory pool identity name is invalid")
      strictUtf8(identity.name) match {
        case Left(_) => return Left("memory pool identity name is invalid")
        case Right(_) =>
      }
      if (identity.memoryType != "HEAP" && identity.memoryType != "NON_HEAP")
        return Left("memory pool identity type is invalid")
      i += 1
    }
    if (identities.distinct.length != identities.length)
      return Left("memory pool identities contain duplicates")
    if (identities != sortMemoryPoolIdentities(identities))
      return Left("memory pool identities are not sorted")
    Right(())
  }

  def validateMemoryUsageEvidence(
      usage: MemoryUsageEvidence,
      label: String): Either[String, Unit] = {
    if (usage == null) return Left(label + " is null")
    if (usage.usedBytes < 0L) return Left(label + " used bytes are negative")
    if (usage.committedBytes < 0L) return Left(label + " committed bytes are negative")
    if (usage.maxBytes < -1L) return Left(label + " maximum bytes are invalid")
    if (usage.usedBytes > usage.committedBytes)
      return Left(label + " used bytes exceed committed bytes")
    if (usage.maxBytes >= 0L && usage.committedBytes > usage.maxBytes)
      return Left(label + " committed bytes exceed maximum bytes")
    Right(())
  }

  def validateMemoryPoolPhaseEnvelopes(
      identities: Vector[MemoryPoolIdentity],
      envelopes: Vector[MemoryPoolPhaseEnvelope]): Either[String, Unit] = {
    validateMemoryPoolIdentities(identities) match {
      case Left(detail) => return Left(detail)
      case Right(_) =>
    }
    if (envelopes == null) return Left("memory pool phase envelopes are null")
    if (envelopes.isEmpty) return Left("memory pool phase envelopes are empty")
    if (envelopes.exists(_ == null))
      return Left("memory pool phase envelopes contain null")
    if (envelopes.map(_.identity) != identities)
      return Left("memory pool phase envelope identities do not match the environment")
    var i = 0
    while (i < envelopes.length) {
      val envelope = envelopes(i)
      val prefix = "memory pool " + envelope.identity.name
      validateMemoryUsageEvidence(
        envelope.afterResetPeakUsage,
        prefix + " after-reset peak usage") match {
        case Left(detail) => return Left(detail)
        case Right(_) =>
      }
      validateMemoryUsageEvidence(envelope.endUsage, prefix + " end usage") match {
        case Left(detail) => return Left(detail)
        case Right(_) =>
      }
      validateMemoryUsageEvidence(envelope.finalPeakUsage, prefix + " final peak usage") match {
        case Left(detail) => return Left(detail)
        case Right(_) =>
      }
      if (envelope.finalPeakUsage.usedBytes < envelope.afterResetPeakUsage.usedBytes)
        return Left(prefix + " final peak used bytes are below after-reset peak used bytes")
      if (envelope.finalPeakUsage.usedBytes < envelope.endUsage.usedBytes)
        return Left(prefix + " final peak used bytes are below end used bytes")
      i += 1
    }
    Right(())
  }

  private def validatedSamples(
      samples: Seq[Long],
      label: String): Either[String, Seq[Long]] = {
    if (samples == null) Left(label + " samples are null")
    else if (samples.isEmpty) Left(label + " samples are empty")
    else if (samples.exists(_ < 0L)) Left(label + " samples contain a negative value")
    else Right(samples.sorted)
  }

  private def nearestRank(sorted: Seq[Long], percentile: Int): Long = {
    val numerator = sorted.length.toLong * percentile.toLong
    val oneBased = (numerator + 99L) / 100L
    sorted((oneBased - 1L).toInt)
  }

  def sha256Hex(bytes: Array[Byte]): String =
    hex(MessageDigest.getInstance(DigestAlgorithm).digest(bytes))

  def evidenceDigest(payloadJson: String): String = {
    val digest = MessageDigest.getInstance(DigestAlgorithm)
    digest.update(DigestDomain.getBytes(StandardCharsets.US_ASCII))
    digest.update(0.toByte)
    digest.update(payloadJson.getBytes(StandardCharsets.UTF_8))
    hex(digest.digest())
  }

  def renderEnvelope(payload: EvidencePayload): String = {
    validatePayload(payload)
    val payloadJson = renderPayload(payload)
    val digest = evidenceDigest(payloadJson)
    val out = new StringBuilder(payloadJson.length + 320)
    out.append('{')
    field(out, "schema", Schema)
    out.append(',')
    field(out, "digestAlgorithm", DigestAlgorithm)
    out.append(',')
    field(out, "digestDomain", DigestDomain)
    out.append(',')
    field(out, "canonicalization", Canonicalization)
    out.append(',')
    field(out, "evidenceDigest", digest)
    out.append(',').append(quote("payload")).append(':').append(payloadJson)
    out.append('}').append('\n')
    out.toString()
  }

  private def validatePayload(payload: EvidencePayload): Unit = {
    require(payload != null, "evidence payload is null")
    require(payload.warmupRounds >= 0, "warmupRounds is negative")
    require(payload.sampleRounds > 0, "sampleRounds is not positive")
    require(payload.benchmarkDurationNs >= 0L, "benchmarkDurationNs is negative")
    require(payload.campaignBinding != null, "campaignBinding option is null")
    require(payload.environment != null, "environment is null")
    require(payload.scenarios != null, "scenarios are null")
    require(payload.garbageCollectorDeltas != null, "garbageCollectorDeltas is null")
    require(payload.memoryPoolPhaseEnvelopes != null,
      "memoryPoolPhaseEnvelopes is null")
    require(payload.scenarios.nonEmpty, "scenarios are empty")
    validateImplementationRevision(payload.implementationRevision) match {
      case Left(detail) =>
        throw new IllegalArgumentException(
          "requirement failed: implementationRevision " + detail)
      case Right(normalized) =>
        require(normalized == payload.implementationRevision,
          "implementationRevision has leading or trailing whitespace")
    }
    payload.campaignBinding.foreach { binding =>
      require(binding != null, "campaignBinding contains null")
      require(isValidCampaignRunId(binding.runId), "campaign run ID is invalid")
      require(
        binding.manifestByteLength > 0 &&
          binding.manifestByteLength <= MaxCampaignManifestBytes,
        "campaign manifest byte length is invalid")
      require(isLowerHexSha256(binding.manifestSha256),
        "campaign manifest SHA-256 is invalid")
      require(payload.implementationRevision != "unrecorded",
        "campaign-bound evidence has an unrecorded implementation revision")
    }
    require(
      payload.environment.jvmInputArgumentCount >= 0 &&
        payload.environment.jvmInputArgumentCount <= MaxJvmInputArguments,
      "JVM input argument count is invalid")
    require(isLowerHexSha256(payload.environment.jvmInputArgumentsSha256),
      "JVM input arguments SHA-256 is invalid")
    validateMemoryPoolPhaseEnvelopes(
      payload.environment.memoryPoolIdentities,
      payload.memoryPoolPhaseEnvelopes) match {
      case Left(detail) => throw new IllegalArgumentException("requirement failed: " + detail)
      case Right(_) =>
    }
    require(
      payload.scenarios.forall(item => item != null && item.id != null && item.id.nonEmpty),
      "scenarios contain an invalid entry")
    require(
      payload.scenarios.map(_.id).distinct.length == payload.scenarios.length,
      "scenario IDs are not unique")
    payload.scenarios.foreach { scenario =>
      require(
        scenario.validationBoundary != null && scenario.validationBoundary.nonEmpty,
        scenario.id + " validation boundary is empty")
      require(
        scenario.lastVerifierCheckpoint != null && scenario.lastVerifierCheckpoint.nonEmpty,
        scenario.id + " last verifier checkpoint is empty")
      require(
        scenario.samplesNs.length == payload.sampleRounds,
        scenario.id + " timing sample count does not match sampleRounds")
      require(
        scenario.allocatedBytes.length == payload.sampleRounds,
        scenario.id + " allocation sample count does not match sampleRounds")
      require(
        statistics(scenario.samplesNs) == Right(scenario.statistics),
        scenario.id + " timing statistics do not match raw samples")
      require(
        allocationStatistics(scenario.allocatedBytes) == Right(scenario.allocationStatistics),
        scenario.id + " allocation statistics do not match raw samples")
    }
    require(
      payload.garbageCollectorDeltas.forall(item =>
        item != null && item.name != null && item.name.nonEmpty &&
          item.collections >= 0L && item.collectionTimeMs >= 0L),
      "garbage collector deltas contain an invalid value")
    require(
      payload.garbageCollectorDeltas.map(_.name).distinct.length ==
        payload.garbageCollectorDeltas.length,
      "garbage collector delta names are not unique")
    require(
      payload.environment.garbageCollectors == payload.garbageCollectorDeltas.map(_.name),
      "garbage collector metadata does not match sampled deltas")
  }

  private[benchmark] def renderPayload(payload: EvidencePayload): String = {
    val out = new StringBuilder(8192)
    out.append('{')
    field(out, "startedAtUtc", payload.startedAtUtc)
    out.append(',')
    numberField(out, "benchmarkDurationNs", payload.benchmarkDurationNs)
    out.append(',')
    field(out, "profileId", payload.profileId)
    out.append(',')
    field(out, "implementationRevision", payload.implementationRevision)
    out.append(',')
    field(out, "verifierEntryPoint", payload.verifierEntryPoint)
    out.append(',').append(quote("resources")).append(':')
    renderArray(out, payload.resources) { (builder, resource) =>
      builder.append('{')
      field(builder, "id", resource.id)
      builder.append(',')
      field(builder, "classpath", resource.classpath)
      builder.append(',')
      numberField(builder, "byteLength", resource.byteLength.toLong)
      builder.append(',')
      field(builder, "sha256", resource.sha256)
      builder.append('}')
    }
    out.append(',').append(quote("campaignBinding")).append(':')
    payload.campaignBinding match {
      case Some(binding) =>
        out.append('{')
        field(out, "runId", binding.runId)
        out.append(',')
        numberField(out, "manifestByteLength", binding.manifestByteLength.toLong)
        out.append(',')
        field(out, "manifestSha256", binding.manifestSha256)
        out.append('}')
      case None => out.append("null")
    }
    out.append(',').append(quote("configuration")).append(':').append('{')
    numberField(out, "warmupRounds", payload.warmupRounds.toLong)
    out.append(',')
    numberField(out, "sampleRounds", payload.sampleRounds.toLong)
    out.append(',')
    field(out, "clock", "System.nanoTime")
    out.append(',')
    field(out, "schedule", "deterministic-round-robin-rotation")
    out.append(',')
    field(out, "percentileMethod", "nearest-rank")
    out.append(',')
    field(
      out,
      "timedScope",
      if (payload.verifierEntryPoint ==
          "sigma.stark.profile.Risc0RawSealVerifier.verifyObservedOperations")
        "Risc0RawSealVerifier.verifyObservedOperations; fixture and profile loading excluded"
      else
        "Risc0RawSealVerifier.verify; fixture and profile loading excluded")
    out.append(',')
    field(out, "allocationScope", "current benchmark thread around each timed verifier invocation")
    out.append(',')
    field(out, "garbageCollectionScope", "process-wide collector deltas across the complete sampling phase")
    out.append(',')
    field(
      out,
      "memoryPoolScope",
      "sequential per-pool MemoryPoolMXBean.resetPeakUsage/getPeakUsage/getUsage boundary calls around the complete sampling phase; runner and JVM-management overhead included")
    out.append('}')
    out.append(',').append(quote("environment")).append(':')
    renderEnvironment(out, payload.environment)
    out.append(',').append(quote("scenarios")).append(':')
    renderArray(out, payload.scenarios) { (builder, scenario) =>
      builder.append('{')
      field(builder, "id", scenario.id)
      builder.append(',')
      field(builder, "expectedOutcome", scenario.expectedOutcome)
      builder.append(',')
      numberField(builder, "validationQueryCheckpoints", scenario.validationQueryCheckpoints.toLong)
      builder.append(',')
      field(builder, "validationBoundary", scenario.validationBoundary)
      builder.append(',')
      field(builder, "lastVerifierCheckpoint", scenario.lastVerifierCheckpoint)
      builder.append(',').append(quote("samplesNs")).append(':')
      renderLongArray(builder, scenario.samplesNs)
      builder.append(',').append(quote("statistics")).append(':').append('{')
      numberField(builder, "sampleCount", scenario.statistics.sampleCount.toLong)
      builder.append(',')
      numberField(builder, "p50Ns", scenario.statistics.p50Ns)
      builder.append(',')
      numberField(builder, "p95Ns", scenario.statistics.p95Ns)
      builder.append(',')
      numberField(builder, "p99Ns", scenario.statistics.p99Ns)
      builder.append(',')
      numberField(builder, "maxNs", scenario.statistics.maxNs)
      builder.append('}')
      builder.append(',').append(quote("allocatedBytes")).append(':')
      renderLongArray(builder, scenario.allocatedBytes)
      builder.append(',').append(quote("allocationStatistics")).append(':').append('{')
      numberField(builder, "sampleCount", scenario.allocationStatistics.sampleCount.toLong)
      builder.append(',')
      numberField(builder, "p50Bytes", scenario.allocationStatistics.p50Bytes)
      builder.append(',')
      numberField(builder, "p95Bytes", scenario.allocationStatistics.p95Bytes)
      builder.append(',')
      numberField(builder, "p99Bytes", scenario.allocationStatistics.p99Bytes)
      builder.append(',')
      numberField(builder, "maxBytes", scenario.allocationStatistics.maxBytes)
      builder.append('}').append('}')
    }
    out.append(',').append(quote("garbageCollectorDeltas")).append(':')
    renderArray(out, payload.garbageCollectorDeltas) { (builder, collector) =>
      builder.append('{')
      field(builder, "name", collector.name)
      builder.append(',')
      numberField(builder, "collections", collector.collections)
      builder.append(',')
      numberField(builder, "collectionTimeMs", collector.collectionTimeMs)
      builder.append('}')
    }
    out.append(',').append(quote("memoryPoolPhaseEnvelopes")).append(':')
    renderArray(out, payload.memoryPoolPhaseEnvelopes) { (builder, envelope) =>
      builder.append('{')
      builder.append(quote("identity")).append(':')
      renderMemoryPoolIdentity(builder, envelope.identity)
      builder.append(',').append(quote("afterResetPeakUsage")).append(':')
      renderMemoryUsage(builder, envelope.afterResetPeakUsage)
      builder.append(',').append(quote("endUsage")).append(':')
      renderMemoryUsage(builder, envelope.endUsage)
      builder.append(',').append(quote("finalPeakUsage")).append(':')
      renderMemoryUsage(builder, envelope.finalPeakUsage)
      builder.append('}')
    }
    out.append(',').append(quote("limitations")).append(':')
    renderStringArray(out, payload.limitations)
    out.append('}')
    out.toString()
  }

  private def renderEnvironment(out: StringBuilder, environment: EnvironmentMetadata): Unit = {
    out.append('{')
    field(out, "javaRuntimeName", environment.javaRuntimeName)
    out.append(',')
    field(out, "javaRuntimeVersion", environment.javaRuntimeVersion)
    out.append(',')
    field(out, "javaVmName", environment.javaVmName)
    out.append(',')
    field(out, "javaVmVendor", environment.javaVmVendor)
    out.append(',')
    field(out, "javaVmVersion", environment.javaVmVersion)
    out.append(',')
    field(out, "javaVmInfo", environment.javaVmInfo)
    out.append(',')
    field(out, "scalaVersion", environment.scalaVersion)
    out.append(',')
    field(out, "osName", environment.osName)
    out.append(',')
    field(out, "osVersion", environment.osVersion)
    out.append(',')
    field(out, "osArch", environment.osArch)
    out.append(',')
    numberField(out, "availableProcessors", environment.availableProcessors.toLong)
    out.append(',')
    numberField(out, "maxHeapBytes", environment.maxHeapBytes)
    out.append(',')
    field(out, "jitCompiler", environment.jitCompiler)
    out.append(',').append(quote("garbageCollectors")).append(':')
    renderStringArray(out, environment.garbageCollectors)
    out.append(',').append(quote("memoryPoolIdentities")).append(':')
    renderArray(out, environment.memoryPoolIdentities)(renderMemoryPoolIdentity)
    out.append(',')
    field(out, "threadAllocationMeter", environment.threadAllocationMeter)
    out.append(',')
    numberField(out, "jvmInputArgumentCount", environment.jvmInputArgumentCount.toLong)
    out.append(',')
    field(out, "jvmInputArgumentsSha256", environment.jvmInputArgumentsSha256)
    out.append(',')
    field(out, "cpuModel", environment.cpuModel)
    out.append(',')
    field(out, "cpuModelSource", environment.cpuModelSource)
    out.append('}')
  }

  private def renderMemoryPoolIdentity(
      out: StringBuilder,
      identity: MemoryPoolIdentity): Unit = {
    out.append('{')
    field(out, "name", identity.name)
    out.append(',')
    field(out, "memoryType", identity.memoryType)
    out.append('}')
  }

  private def renderMemoryUsage(
      out: StringBuilder,
      usage: MemoryUsageEvidence): Unit = {
    out.append('{')
    numberField(out, "usedBytes", usage.usedBytes)
    out.append(',')
    numberField(out, "committedBytes", usage.committedBytes)
    out.append(',')
    numberField(out, "maxBytes", usage.maxBytes)
    out.append('}')
  }

  private def renderLongArray(out: StringBuilder, values: Seq[Long]): Unit = {
    out.append('[')
    var i = 0
    while (i < values.length) {
      if (i > 0) out.append(',')
      out.append(values(i))
      i += 1
    }
    out.append(']')
  }

  private def renderStringArray(out: StringBuilder, values: Seq[String]): Unit = {
    out.append('[')
    var i = 0
    while (i < values.length) {
      if (i > 0) out.append(',')
      out.append(quote(values(i)))
      i += 1
    }
    out.append(']')
  }

  private def renderArray[A](
      out: StringBuilder,
      values: Seq[A])(
      render: (StringBuilder, A) => Unit): Unit = {
    out.append('[')
    var i = 0
    while (i < values.length) {
      if (i > 0) out.append(',')
      render(out, values(i))
      i += 1
    }
    out.append(']')
  }

  private def field(out: StringBuilder, name: String, value: String): Unit =
    out.append(quote(name)).append(':').append(quote(nullSafe(value)))

  private def numberField(out: StringBuilder, name: String, value: Long): Unit =
    out.append(quote(name)).append(':').append(value)

  private def nullSafe(value: String): String = if (value == null) "" else value

  private def isLowerHexSha256(value: String): Boolean =
    value != null && value.length == 64 && value.forall { ch =>
      (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f')
    }

  private[benchmark] def quote(value: String): String = {
    val out = new StringBuilder(value.length + 2)
    out.append('"')
    var i = 0
    while (i < value.length) {
      value.charAt(i) match {
        case '"' => out.append("\\\"")
        case '\\' => out.append("\\\\")
        case '\b' => out.append("\\b")
        case '\f' => out.append("\\f")
        case '\n' => out.append("\\n")
        case '\r' => out.append("\\r")
        case '\t' => out.append("\\t")
        case ch if ch < ' ' =>
          out.append("\\u")
          val rendered = Integer.toHexString(ch.toInt)
          var padding = rendered.length
          while (padding < 4) {
            out.append('0')
            padding += 1
          }
          out.append(rendered)
        case ch => out.append(ch)
      }
      i += 1
    }
    out.append('"')
    out.toString()
  }

  private def hex(bytes: Array[Byte]): String = {
    val out = new StringBuilder(bytes.length * 2)
    var i = 0
    while (i < bytes.length) {
      out.append(String.format("%02x", Integer.valueOf(bytes(i) & 0xff)))
      i += 1
    }
    out.toString()
  }
}
