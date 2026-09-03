/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import java.io.{ByteArrayOutputStream, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths, StandardOpenOption}
import java.time.Instant
import java.util.UUID

import sigma.stark.profile.benchmark.Eip0045BenchmarkSupport._
import sigma.stark.profile.benchmark.Eip0045CampaignContract._

/** Bounded validator and path-free archive indexer for EIP-0045 B5 campaign
  * evidence. This opt-in test-source tool validates already generated files;
  * it neither runs the benchmark nor recommends a consensus charge.
  */
object Eip0045CampaignValidator {
  private[benchmark] final val ArchiveTemporaryFilePrefix = ".eip0045-b5-index-"
  private final val MaxTemporaryCreateAttempts = 16

  private final case class CliConfig(
      manifestPath: Path,
      evidencePaths: Vector[Path],
      expectedIndexPath: Option[Path],
      outputPath: Option[Path])

  private sealed trait CliOption {
    def token: String
    def pathLabel: String
  }

  private case object ManifestCliOption extends CliOption {
    override val token: String = "--manifest"
    override val pathLabel: String = "manifest"
  }

  private case object EvidenceCliOption extends CliOption {
    override val token: String = "--evidence"
    override val pathLabel: String = "evidence"
  }

  private case object ExpectedIndexCliOption extends CliOption {
    override val token: String = "--expected-index"
    override val pathLabel: String = "expected archive index"
  }

  private case object OutputCliOption extends CliOption {
    override val token: String = "--output"
    override val pathLabel: String = "archive output"
  }

  private final case class ManifestContext(
      exact: ExactCampaignManifest) {
    def manifest: CampaignManifestV3 = exact.manifest
    def byteLength: Int = exact.byteLength
    def sha256: String = exact.sha256
  }

  private final case class DecodedEvidence(
      runId: String,
      evidenceDigest: String)

  private[benchmark] trait EvidenceReadObserver {
    def beforeRead(index: Int, path: Path): Unit
  }

  private object NoEvidenceReadObserver extends EvidenceReadObserver {
    override def beforeRead(index: Int, path: Path): Unit = ()
  }

  private[benchmark] trait TemporaryOpenObserver {
    def beforeOpen(attempt: Int, candidate: Path, finalPath: Path): Unit
  }

  private object NoTemporaryOpenObserver extends TemporaryOpenObserver {
    override def beforeOpen(attempt: Int, candidate: Path, finalPath: Path): Unit = ()
  }

  private final case class OwnedTemporary(path: Path, channel: FileChannel)

  val Usage: String =
    """EIP-0045 B5 campaign archive validator (opt-in test-source tool)
      |Usage: Eip0045CampaignValidator --manifest FILE --evidence FILE [--evidence FILE ...] [options]
      |  --expected-index FILE  Recheck an existing canonical path-free index
      |  --output FILE          Create a canonical path-free index; refuses overwrite
      |  --help                 Show this help
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    if (args != null && args.contains("--help")) {
      System.out.print(Usage)
      return
    }
    val config = parseArgs(args) match {
      case Right(value) => value
      case Left(detail) => throw new IllegalArgumentException(detail)
    }
    validateFiles(
      config.manifestPath,
      config.evidencePaths,
      config.expectedIndexPath,
      config.outputPath) match {
      case Left(detail) => throw new IllegalArgumentException(detail)
      case Right(indexJson) if config.outputPath.isEmpty => System.out.print(indexJson)
      case Right(_) =>
    }
  }

  /** Validates in-memory exact file bytes and returns the canonical archive
    * index. If an expected index is supplied, its canonical content must equal
    * the index recomputed from the complete manifest and evidence files.
    */
  private[benchmark] def validateArchiveBytes(
      manifestBytes: Array[Byte],
      evidenceFiles: Vector[Array[Byte]],
      expectedIndexBytes: Option[Array[Byte]] = None): Either[String, String] = {
    if (evidenceFiles == null || evidenceFiles.isEmpty || evidenceFiles.length > MaxRuns)
      return Left("evidence file count is invalid")
    if (evidenceFiles.exists(_ == null)) return Left("evidence file bytes are null")
    if (expectedIndexBytes == null) return Left("expected archive index option is null")

    val context = prepareManifest(manifestBytes, evidenceFiles.length) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }

    val entries = Vector.newBuilder[ArchiveEntry]
    var i = 0
    while (i < evidenceFiles.length) {
      validateEvidenceEntry(context, evidenceFiles(i)) match {
        case Left(detail) => return Left(detail)
        case Right(entry) => entries += entry
      }
      i += 1
    }
    val index = buildArchiveIndex(context, entries.result()) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }
    renderAndCheckArchiveIndex(index, expectedIndexBytes)
  }

  private[benchmark] def validateFiles(
      manifestPath: Path,
      evidencePaths: Vector[Path],
      expectedIndexPath: Option[Path],
      outputPath: Option[Path]): Either[String, String] =
    validateFilesInternal(
      manifestPath,
      evidencePaths,
      expectedIndexPath,
      outputPath,
      NoEvidenceReadObserver)

  private[benchmark] def validateFilesWithObserverForTest(
      manifestPath: Path,
      evidencePaths: Vector[Path],
      expectedIndexPath: Option[Path],
      outputPath: Option[Path],
      observer: EvidenceReadObserver): Either[String, String] =
    validateFilesInternal(
      manifestPath,
      evidencePaths,
      expectedIndexPath,
      outputPath,
      observer)

  private def validateFilesInternal(
      manifestPath: Path,
      evidencePaths: Vector[Path],
      expectedIndexPath: Option[Path],
      outputPath: Option[Path],
      observer: EvidenceReadObserver): Either[String, String] = {
    if (manifestPath == null) return Left("campaign manifest path is null")
    if (evidencePaths == null || evidencePaths.isEmpty || evidencePaths.length > MaxRuns)
      return Left("evidence path count is invalid")
    if (evidencePaths.exists(_ == null)) return Left("evidence path is null")
    if (expectedIndexPath == null) return Left("expected archive index path option is null")
    if (outputPath == null) return Left("archive output path option is null")
    if (expectedIndexPath.exists(_ == null))
      return Left("expected archive index path is null")
    if (outputPath.exists(_ == null)) return Left("archive output path is null")
    if (observer == null) return Left("evidence read observer is null")

    val manifestBytes = readBoundedRegularFile(
      manifestPath,
      MaxCampaignManifestBytes,
      "campaign manifest") match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }
    val context = prepareManifest(manifestBytes, evidencePaths.length) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }

    // The file route retains only one bounded evidence byte array at a time.
    // The helper frame holding bytes and decoded payload returns before the
    // next path is observed; the builder keeps only ArchiveEntry metadata.
    val entries = Vector.newBuilder[ArchiveEntry]
    var i = 0
    while (i < evidencePaths.length) {
      observer.beforeRead(i, evidencePaths(i))
      readAndValidateEvidencePath(context, evidencePaths(i)) match {
        case Right(entry) => entries += entry
        case Left(detail) => return Left(detail)
      }
      i += 1
    }
    val index = buildArchiveIndex(context, entries.result()) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }

    // Expected-index I/O happens only after every declared evidence file has
    // passed semantic validation and exact run coverage has closed.
    val expectedBytes = expectedIndexPath match {
      case Some(path) =>
        readBoundedRegularFile(path, MaxArchiveIndexBytes, "archive index") match {
          case Right(value) => Some(value)
          case Left(detail) => return Left(detail)
      }
      case None => None
    }
    val indexJson = renderAndCheckArchiveIndex(index, expectedBytes) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }
    outputPath match {
      case Some(path) =>
        publishCreateNew(
          path,
          indexJson.getBytes(StandardCharsets.UTF_8),
          None,
          simulatePostPublicationCleanupFailure = false,
          temporaryOpenObserver = NoTemporaryOpenObserver) match {
          case Left(detail) => Left(detail)
          case Right(_) => Right(indexJson)
        }
      case None => Right(indexJson)
    }
  }

  private def readAndValidateEvidencePath(
      context: ManifestContext,
      path: Path): Either[String, ArchiveEntry] =
    readBoundedRegularFile(path, MaxEvidenceBytes, "evidence file") match {
      case Right(bytes) => validateEvidenceEntry(context, bytes)
      case Left(detail) => Left(detail)
    }

  private def prepareManifest(
      manifestBytes: Array[Byte],
      evidenceCount: Int): Either[String, ManifestContext] = {
    val exact = parseExactCampaignManifest(manifestBytes) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }
    if (evidenceCount != exact.manifest.runs.length)
      return Left("evidence file count does not match declared runs")
    Right(ManifestContext(exact))
  }

  private def validateEvidenceEntry(
      context: ManifestContext,
      bytes: Array[Byte]): Either[String, ArchiveEntry] = {
    if (bytes.isEmpty) return Left("evidence file is empty")
    if (bytes.length > MaxEvidenceBytes)
      return Left("evidence file exceeds " + MaxEvidenceBytes + " bytes")
    decodeAndValidateEvidence(context, bytes) match {
      case Left(detail) => Left(detail)
      case Right(decoded) => Right(ArchiveEntry(
        decoded.runId,
        bytes.length,
        sha256Hex(bytes),
        decoded.evidenceDigest))
    }
  }

  private def buildArchiveIndex(
      context: ManifestContext,
      entries: Vector[ArchiveEntry]): Either[String, ArchiveIndex] = {
    val runIds = entries.map(_.runId)
    if (runIds.distinct.length != runIds.length)
      return Left("more than one evidence file claims the same run ID")
    if (runIds.toSet != context.manifest.runs.map(_.id).toSet)
      return Left("evidence run IDs do not exactly cover declared runs")
    Right(ArchiveIndex(
      context.manifest.campaignId,
      context.byteLength,
      context.sha256,
      entries.sortBy(_.runId)))
  }

  private def renderAndCheckArchiveIndex(
      index: ArchiveIndex,
      expectedIndexBytes: Option[Array[Byte]]): Either[String, String] = {
    val indexJson = renderArchiveIndex(index) match {
      case Right(value) => value
      case Left(detail) => return Left(detail)
    }
    expectedIndexBytes match {
      case Some(bytes) =>
        parseArchiveIndex(bytes) match {
          case Left(detail) => return Left(detail)
          case Right(expected) if expected != index =>
            return Left("archive index does not match the validated full files")
          case Right(_) =>
        }
      case None =>
    }
    Right(indexJson)
  }

  private def decodeAndValidateEvidence(
      context: ManifestContext,
      evidenceBytes: Array[Byte]): Either[String, DecodedEvidence] = {
    parseDocument(evidenceBytes, MaxEvidenceBytes, "evidence file") match {
      case Left(detail) => Left(detail)
      case Right((text, root)) =>
        try {
          val top = exactObject(root, Vector(
            "schema",
            "digestAlgorithm",
            "digestDomain",
            "canonicalization",
            "evidenceDigest",
            "payload"), "evidence envelope")
          if (requireString(top("schema"), "evidence schema") != Schema)
            fail("evidence schema is invalid")
          if (requireString(top("digestAlgorithm"), "evidence digest algorithm") !=
              DigestAlgorithm) fail("evidence digest algorithm is invalid")
          if (requireString(top("digestDomain"), "evidence digest domain") != DigestDomain)
            fail("evidence digest domain is invalid")
          if (requireString(top("canonicalization"), "evidence canonicalization") !=
              Canonicalization) fail("evidence canonicalization is invalid")
          val recordedDigest = requireString(top("evidenceDigest"), "evidence digest")
          if (!isLowerHexSha256(recordedDigest)) fail("evidence digest is invalid")
          val payload = decodePayload(top("payload"))
          val payloadJson = renderPayload(payload)
          if (evidenceDigest(payloadJson) != recordedDigest)
            fail("evidence payload digest does not match the payload")
          val rerendered = try renderEnvelope(payload)
          catch {
            case _: IllegalArgumentException => fail("evidence payload semantics are invalid")
          }
          if (rerendered != text) fail("evidence file is not canonical V5 JSON")
          validateAgainstManifest(context, payload) match {
            case Left(detail) => Left(detail)
            case Right(runId) => Right(DecodedEvidence(runId, recordedDigest))
          }
        } catch {
          case error: DecodeFailure => Left(error.getMessage)
          case _: IllegalArgumentException => Left("evidence payload semantics are invalid")
        }
    }
  }

  private def decodePayload(value: JsonValue): EvidencePayload = {
    val payload = exactObject(value, Vector(
      "startedAtUtc",
      "benchmarkDurationNs",
      "profileId",
      "implementationRevision",
      "verifierEntryPoint",
      "resources",
      "campaignBinding",
      "configuration",
      "environment",
      "scenarios",
      "garbageCollectorDeltas",
      "memoryPoolPhaseEnvelopes",
      "limitations"), "evidence payload")
    val resources = requireArray(payload("resources"), "evidence resources").map { itemValue =>
      val item = exactObject(itemValue,
        Vector("id", "classpath", "byteLength", "sha256"), "evidence resource")
      ResourceMetadata(
        requireString(item("id"), "evidence resource ID"),
        requireString(item("classpath"), "evidence resource classpath"),
        requireInt(item("byteLength"), "evidence resource byte length"),
        requireString(item("sha256"), "evidence resource digest"))
    }
    val binding = payload("campaignBinding") match {
      case JsonNull => fail("evidence campaign binding is missing")
      case other =>
        val item = exactObject(other,
          Vector("runId", "manifestByteLength", "manifestSha256"),
          "evidence campaign binding")
        Some(CampaignBinding(
          requireString(item("runId"), "evidence run ID"),
          requireInt(item("manifestByteLength"), "evidence manifest byte length"),
          requireString(item("manifestSha256"), "evidence manifest digest")))
    }
    val configuration = exactObject(payload("configuration"), Vector(
      "warmupRounds",
      "sampleRounds",
      "clock",
      "schedule",
      "percentileMethod",
      "timedScope",
      "allocationScope",
      "garbageCollectionScope",
      "memoryPoolScope"), "evidence configuration")
    requireExactConfigurationConstants(configuration)
    val environment = decodeEnvironment(payload("environment"))
    val scenarios = requireArray(payload("scenarios"), "evidence scenarios").map { value =>
      val item = exactObject(value, Vector(
        "id",
        "expectedOutcome",
        "validationQueryCheckpoints",
        "validationBoundary",
        "lastVerifierCheckpoint",
        "samplesNs",
        "statistics",
        "allocatedBytes",
        "allocationStatistics"), "evidence scenario")
      val timing = exactObject(item("statistics"), Vector(
        "sampleCount", "p50Ns", "p95Ns", "p99Ns", "maxNs"),
        "evidence timing statistics")
      val allocation = exactObject(item("allocationStatistics"), Vector(
        "sampleCount", "p50Bytes", "p95Bytes", "p99Bytes", "maxBytes"),
        "evidence allocation statistics")
      ScenarioEvidence(
        requireString(item("id"), "evidence scenario ID"),
        requireString(item("expectedOutcome"), "evidence scenario outcome"),
        requireInt(item("validationQueryCheckpoints"), "evidence scenario checkpoints"),
        requireString(item("validationBoundary"), "evidence scenario validation boundary"),
        requireString(
          item("lastVerifierCheckpoint"),
          "evidence scenario last verifier checkpoint"),
        requireLongArray(item("samplesNs"), "evidence timing samples"),
        TimingStatistics(
          requireInt(timing("sampleCount"), "timing sample count"),
          requireLong(timing("p50Ns"), "timing p50"),
          requireLong(timing("p95Ns"), "timing p95"),
          requireLong(timing("p99Ns"), "timing p99"),
          requireLong(timing("maxNs"), "timing maximum")),
        requireLongArray(item("allocatedBytes"), "evidence allocation samples"),
        AllocationStatistics(
          requireInt(allocation("sampleCount"), "allocation sample count"),
          requireLong(allocation("p50Bytes"), "allocation p50"),
          requireLong(allocation("p95Bytes"), "allocation p95"),
          requireLong(allocation("p99Bytes"), "allocation p99"),
          requireLong(allocation("maxBytes"), "allocation maximum")))
    }
    val collectors = requireArray(
      payload("garbageCollectorDeltas"), "evidence collector deltas").map { value =>
      val item = exactObject(value,
        Vector("name", "collections", "collectionTimeMs"),
        "evidence collector delta")
      GarbageCollectorDelta(
        requireString(item("name"), "collector name"),
        requireLong(item("collections"), "collector count"),
        requireLong(item("collectionTimeMs"), "collector time"))
    }
    val memoryPools = requireArray(
      payload("memoryPoolPhaseEnvelopes"), "memory pool phase envelopes").map { value =>
      val item = exactObject(value, Vector(
        "identity",
        "afterResetPeakUsage",
        "endUsage",
        "finalPeakUsage"), "memory pool phase envelope")
      MemoryPoolPhaseEnvelope(
        decodeMemoryPoolIdentity(item("identity")),
        decodeMemoryUsage(item("afterResetPeakUsage"), "after-reset peak usage"),
        decodeMemoryUsage(item("endUsage"), "end usage"),
        decodeMemoryUsage(item("finalPeakUsage"), "final peak usage"))
    }
    EvidencePayload(
      requireString(payload("startedAtUtc"), "evidence start time"),
      requireLong(payload("benchmarkDurationNs"), "benchmark duration"),
      requireString(payload("profileId"), "evidence profile ID"),
      requireString(payload("implementationRevision"), "evidence implementation revision"),
      requireString(payload("verifierEntryPoint"), "evidence verifier entry point"),
      resources,
      requireInt(configuration("warmupRounds"), "evidence warmup rounds"),
      requireInt(configuration("sampleRounds"), "evidence sample rounds"),
      binding,
      environment,
      scenarios,
      collectors,
      memoryPools,
      requireStringArray(payload("limitations"), "evidence limitations"))
  }

  private def decodeEnvironment(value: JsonValue): EnvironmentMetadata = {
    val item = exactObject(value, Vector(
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
      "jvmInputArgumentCount",
      "jvmInputArgumentsSha256",
      "cpuModel",
      "cpuModelSource"), "evidence environment")
    EnvironmentMetadata(
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
      requireArray(item("memoryPoolIdentities"), "memory pool identities")
        .map(decodeMemoryPoolIdentity),
      requireString(item("threadAllocationMeter"), "thread allocation meter"),
      requireInt(item("jvmInputArgumentCount"), "JVM input argument count"),
      requireString(item("jvmInputArgumentsSha256"), "JVM input argument digest"),
      requireString(item("cpuModel"), "CPU model"),
      requireString(item("cpuModelSource"), "CPU model source"))
  }

  private def decodeMemoryPoolIdentity(value: JsonValue): MemoryPoolIdentity = {
    val item = exactObject(
      value,
      Vector("name", "memoryType"),
      "memory pool identity")
    MemoryPoolIdentity(
      requireString(item("name"), "memory pool name"),
      requireString(item("memoryType"), "memory pool type"))
  }

  private def decodeMemoryUsage(
      value: JsonValue,
      label: String): MemoryUsageEvidence = {
    val item = exactObject(
      value,
      Vector("usedBytes", "committedBytes", "maxBytes"),
      "memory pool " + label)
    MemoryUsageEvidence(
      requireLong(item("usedBytes"), label + " used bytes"),
      requireLong(item("committedBytes"), label + " committed bytes"),
      requireLong(item("maxBytes"), label + " maximum bytes"))
  }

  private def requireExactConfigurationConstants(
      fields: Map[String, JsonValue]): Unit = {
    val expected = ExpectedEvidenceContract
    if (requireString(fields("clock"), "evidence clock") != expected.clock)
      fail("evidence clock is invalid")
    if (requireString(fields("schedule"), "evidence schedule") != expected.schedule)
      fail("evidence schedule is invalid")
    if (requireString(fields("percentileMethod"), "evidence percentile method") !=
        expected.percentileMethod) fail("evidence percentile method is invalid")
    if (requireString(fields("timedScope"), "evidence timed scope") != expected.timedScope)
      fail("evidence timed scope is invalid")
    if (requireString(fields("allocationScope"), "evidence allocation scope") !=
        expected.allocationScope) fail("evidence allocation scope is invalid")
    if (requireString(fields("garbageCollectionScope"), "evidence GC scope") !=
        expected.garbageCollectionScope) fail("evidence GC scope is invalid")
    if (requireString(fields("memoryPoolScope"), "evidence memory pool scope") !=
        expected.memoryPoolScope) fail("evidence memory pool scope is invalid")
  }

  private def validateAgainstManifest(
      context: ManifestContext,
      payload: EvidencePayload): Either[String, String] = {
    val manifest = context.manifest
    if (payload.profileId != manifest.profileId)
      return Left("evidence profile ID does not match the campaign")
    if (payload.verifierEntryPoint != manifest.verifierEntryPoint)
      return Left("evidence verifier entry point does not match the campaign")
    if (payload.resources != manifest.resources)
      return Left("evidence resources do not match the campaign")
    val scenarioPolicies = payload.scenarios.map { scenario =>
      ScenarioPolicy(
        scenario.id,
        scenario.expectedOutcome,
        scenario.validationQueryCheckpoints,
        scenario.validationBoundary,
        scenario.lastVerifierCheckpoint)
    }
    if (scenarioPolicies != manifest.scenarios)
      return Left("evidence scenarios do not match the campaign")
    if (payload.limitations != ExpectedCampaignLimitations)
      return Left("evidence limitations do not match campaign-mode V5")
    try {
      if (Instant.parse(payload.startedAtUtc).toString != payload.startedAtUtc)
        return Left("evidence start time is not canonical UTC")
    } catch {
      case _: RuntimeException => return Left("evidence start time is invalid")
    }
    val binding = payload.campaignBinding match {
      case Some(value) => value
      case None => return Left("evidence campaign binding is missing")
    }
    resolveRunPolicy(
      context.exact,
      binding,
      payload.implementationRevision,
      payload.warmupRounds,
      payload.sampleRounds,
      payload.environment) match {
      case Left(detail) => Left(detail)
      case Right(policy) => Right(policy.run.id)
    }
  }

  private def requireLongArray(value: JsonValue, label: String): Vector[Long] =
    requireArray(value, label).map(item => requireLong(item, label + " item"))

  private def parseArgs(args: Array[String]): Either[String, CliConfig] = {
    if (args == null) return Left("argument array is null")
    var manifest: Option[Path] = None
    val evidence = Vector.newBuilder[Path]
    var evidenceCount = 0
    var expectedIndex: Option[Path] = None
    var output: Option[Path] = None
    var i = 0
    while (i < args.length) {
      val option = args(i)
      if (option == null) return Left("argument at index " + i + " is null")
      val classified = option match {
        case ManifestCliOption.token => ManifestCliOption
        case EvidenceCliOption.token => EvidenceCliOption
        case ExpectedIndexCliOption.token => ExpectedIndexCliOption
        case OutputCliOption.token => OutputCliOption
        case _ => return Left("unknown option at argument index " + i)
      }
      if (i + 1 >= args.length) return Left("missing value for " + classified.token)
      val path = parsePath(args(i + 1), classified.pathLabel) match {
        case Right(value) => value
        case Left(detail) => return Left(detail)
      }
      classified match {
        case ManifestCliOption =>
          if (manifest.isDefined) return Left("duplicate option --manifest")
          manifest = Some(path)
        case EvidenceCliOption =>
          evidence += path
          evidenceCount += 1
          if (evidenceCount > MaxRuns) return Left("too many --evidence options")
        case ExpectedIndexCliOption =>
          if (expectedIndex.isDefined) return Left("duplicate option --expected-index")
          expectedIndex = Some(path)
        case OutputCliOption =>
          if (output.isDefined) return Left("duplicate option --output")
          output = Some(path)
      }
      i += 2
    }
    if (manifest.isEmpty) return Left("missing option --manifest")
    if (evidenceCount == 0) return Left("at least one --evidence file is required")
    Right(CliConfig(manifest.get, evidence.result(), expectedIndex, output))
  }

  private[benchmark] def parseArgsForTest(args: Array[String]): Either[String, Unit] =
    parseArgs(args) match {
      case Left(detail) => Left(detail)
      case Right(_) => Right(())
    }

  private def parsePath(value: String, option: String): Either[String, Path] = {
    if (value == null || value.trim.isEmpty) return Left(option + " path is empty")
    try Right(Paths.get(value))
    catch {
      case _: RuntimeException => Left(option + " path is invalid")
    }
  }

  private def readBoundedRegularFile(
      path: Path,
      maximumBytes: Int,
      label: String): Either[String, Array[Byte]] = {
    try {
      if (!Files.isRegularFile(path)) return Left(label + " is not a regular file")
      val declaredSize = Files.size(path)
      if (declaredSize <= 0L) return Left(label + " is empty")
      if (declaredSize > maximumBytes.toLong)
        return Left(label + " exceeds " + maximumBytes + " bytes")
      val input = Files.newInputStream(path)
      val output = new ByteArrayOutputStream(math.min(declaredSize.toInt, 8192))
      val buffer = new Array[Byte](8192)
      try {
        var read = input.read(buffer)
        while (read >= 0) {
          if (read > 0) {
            if (output.size().toLong + read.toLong > maximumBytes.toLong)
              return Left(label + " exceeds " + maximumBytes + " bytes")
            output.write(buffer, 0, read)
          }
          read = input.read(buffer)
        }
        val bytes = output.toByteArray
        if (bytes.isEmpty) Left(label + " is empty") else Right(bytes)
      } finally {
        input.close()
        output.close()
      }
    } catch {
      case _: IOException | _: SecurityException => Left(label + " could not be read")
    }
  }

  /** Test-only fault injection for the adjacent-temp publication boundary.
    * A positive failure offset writes exactly that many bytes to the temporary
    * file, then fails before force, close, and final-name publication.
    */
  private[benchmark] def publishArchiveBytesForTest(
      path: Path,
      bytes: Array[Byte],
      failAfterBytes: Option[Int],
      simulatePostPublicationCleanupFailure: Boolean = false,
      temporaryOpenObserver: TemporaryOpenObserver = NoTemporaryOpenObserver): Either[String, Unit] =
    publishCreateNew(
      path,
      bytes,
      failAfterBytes,
      simulatePostPublicationCleanupFailure,
      temporaryOpenObserver)

  /** Publishes through an adjacent, fully written and forced temporary file.
    * Files.createLink is the final atomic fail-if-exists operation: it cannot
    * replace an existing destination. Cleanup ever targets only the temporary
    * name; after publication, the complete final hard link is never removed.
    * This name-based operation assumes a trusted, non-shared parent whose
    * directory entries cannot be concurrently changed by another principal.
    */
  private def publishCreateNew(
      path: Path,
      bytes: Array[Byte],
      failAfterBytes: Option[Int],
      simulatePostPublicationCleanupFailure: Boolean,
      temporaryOpenObserver: TemporaryOpenObserver): Either[String, Unit] = {
    if (path == null || bytes == null || failAfterBytes == null ||
        temporaryOpenObserver == null)
      return Left("archive output could not be created")
    failAfterBytes match {
      case Some(offset) if offset <= 0 || offset >= bytes.length =>
        return Left("archive output fault injection offset is invalid")
      case _ =>
    }
    val finalPath = path.toAbsolutePath.normalize()
    val parent = finalPath.getParent
    if (parent == null || !Files.isDirectory(parent))
      return Left("archive output parent directory does not exist")
    try {
      if (containsSymbolicLink(parent))
        return Left("archive output parent directory must not contain symbolic links")
    } catch {
      case _: SecurityException => return Left("archive output could not be created")
    }

    var temporaryPath: Path = null
    var channel: FileChannel = null
    var published = false
    try {
      openOwnedTemporary(parent, finalPath, temporaryOpenObserver) match {
        case Right(owned) =>
          // Ownership begins only after this exact CREATE_NEW succeeds. A
          // colliding name observed during retry is never cleaned up here.
          temporaryPath = owned.path
          channel = owned.channel
        case Left(detail) => return Left(detail)
      }
      writeAndForce(channel, bytes, failAfterBytes)
      channel.close()
      channel = null

      // Unlike an ATOMIC_MOVE with implementation-dependent target handling,
      // createLink atomically creates one new directory entry and is required
      // to fail when that entry already exists.
      Files.createLink(finalPath, temporaryPath)
      published = true

      // Failure to remove the now-redundant temporary link must never roll
      // back or delete the complete final link.
      if (!simulatePostPublicationCleanupFailure &&
          deleteTemporaryBestEffort(temporaryPath)) temporaryPath = null
      Right(())
    } catch {
      case _: FileAlreadyExistsException =>
        Left("archive output could not be created")
      case _: IOException | _: UnsupportedOperationException | _: SecurityException =>
        Left("archive output could not be created")
    } finally {
      if (channel != null) {
        try channel.close()
        catch { case _: IOException => }
      }
      if (temporaryPath != null &&
          (!published || !simulatePostPublicationCleanupFailure)) {
        // Before publication this removes the failed partial. After
        // publication it removes only the redundant temporary hard link.
        deleteTemporaryBestEffort(temporaryPath)
      }
      // Deliberately no final-path cleanup. A complete published final is
      // success even if a redundant temporary name could not be removed.
    }
  }

  private def openOwnedTemporary(
      parent: Path,
      finalPath: Path,
      observer: TemporaryOpenObserver): Either[String, OwnedTemporary] = {
    var attempt = 0
    while (attempt < MaxTemporaryCreateAttempts) {
      val candidate = parent.resolve(
        ArchiveTemporaryFilePrefix +
          UUID.randomUUID().toString.replace("-", "") +
          ".tmp")
      try {
        observer.beforeOpen(attempt, candidate, finalPath)
        val channel = FileChannel.open(
          candidate,
          StandardOpenOption.CREATE_NEW,
          StandardOpenOption.WRITE)
        return Right(OwnedTemporary(candidate, channel))
      } catch {
        case _: FileAlreadyExistsException =>
          // The colliding directory entry belongs to somebody else. Retry
          // with a fresh name and never delete or otherwise mutate it.
          attempt += 1
        case _: IOException | _: UnsupportedOperationException | _: SecurityException =>
          return Left("archive output could not be created")
      }
    }
    Left("archive output could not be created")
  }

  private def containsSymbolicLink(path: Path): Boolean = {
    val absolute = path.toAbsolutePath.normalize()
    var current = absolute.getRoot
    if (current != null && Files.isSymbolicLink(current)) return true
    val components = absolute.iterator()
    while (components.hasNext) {
      val component = components.next()
      current = if (current == null) component else current.resolve(component)
      if (Files.isSymbolicLink(current)) return true
    }
    false
  }

  private def writeAndForce(
      channel: FileChannel,
      bytes: Array[Byte],
      failAfterBytes: Option[Int]): Unit = {
    val stop = failAfterBytes.getOrElse(bytes.length)
    var offset = 0
    while (offset < bytes.length) {
      if (offset == stop) throw new IOException("injected archive output failure")
      val length = math.min(8192, math.min(bytes.length - offset, stop - offset))
      val buffer = ByteBuffer.wrap(bytes, offset, length)
      while (buffer.hasRemaining) channel.write(buffer)
      offset += length
    }
    channel.force(true)
  }

  private def deleteTemporaryBestEffort(path: Path): Boolean =
    try {
      Files.deleteIfExists(path)
      true
    }
    catch {
      case _: IOException | _: SecurityException => false
    }
}
