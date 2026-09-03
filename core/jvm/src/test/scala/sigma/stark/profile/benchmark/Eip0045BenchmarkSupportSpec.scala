/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.profile.benchmark.Eip0045BenchmarkSupport._
import sigma.stark.profile.benchmark.Eip0045CampaignContract.ExpectedScenarios

import scala.collection.mutable.ArrayBuffer

class Eip0045BenchmarkSupportSpec extends AnyFunSuite with Matchers {
  test("argument parsing is bounded, explicit, and deterministic") {
    parseArgs(Array.empty) shouldBe
      Right(Config(15, 100, None, None, "unrecorded", None, None))
    parseArgs(Array(
      "--sample-rounds", "7",
      "--warmup-rounds", "0",
      "--cpu-model", "Reference CPU",
      "--implementation-revision", "commit:0123456789abcdef",
      "--campaign-manifest", "campaign.json",
      "--campaign-run-id", "host-a:run-01",
      "--output", "evidence.json")) shouldBe
      Right(Config(
        0,
        7,
        Some("evidence.json"),
        Some("Reference CPU"),
        "commit:0123456789abcdef",
        Some("campaign.json"),
        Some("host-a:run-01")))

    parseArgs(Array("--sample-rounds", "0")) shouldBe
      Left("--sample-rounds must be positive")
    parseArgs(Array("--warmup-rounds", "-1")) shouldBe
      Left("--warmup-rounds must be non-negative")
    parseArgs(Array("--sample-rounds", "NaN")) shouldBe
      Left("--sample-rounds must be a base-10 32-bit integer")
    parseArgs(Array("--sample-rounds", "10001")) shouldBe
      Left("--sample-rounds must be at most 10000")
    parseArgs(Array("--warmup-rounds", "10001")) shouldBe
      Left("--warmup-rounds must be at most 10000")
    parseArgs(Array("--output")) shouldBe Left("missing value for --output")
    parseArgs(Array("--cpu-model", "x", "--cpu-model", "y")) shouldBe
      Left("duplicate option --cpu-model")
    parseArgs(Array("--implementation-revision", " ")) shouldBe
      Left("--implementation-revision must not be empty")
    parseArgs(Array("--implementation-revision", "x" * 257)) shouldBe
      Left("--implementation-revision must be at most 256 characters")
    parseArgs(Array("--implementation-revision", "commit:\n00")) shouldBe
      Left("--implementation-revision must not contain control characters")
    parseArgs(Array("--implementation-revision", "commit:\u007f00")) shouldBe
      Left("--implementation-revision must not contain control characters")
    parseArgs(Array("--campaign-manifest", "campaign.json")) shouldBe
      Left("--campaign-manifest and --campaign-run-id must be supplied together")
    parseArgs(Array("--campaign-run-id", "run-01")) shouldBe
      Left("--campaign-manifest and --campaign-run-id must be supplied together")
    parseArgs(Array(
      "--campaign-manifest", "campaign.json",
      "--campaign-run-id", "run-01")) shouldBe
      Left("campaign-bound runs require a recorded --implementation-revision")
    parseArgs(Array(
      "--implementation-revision", "commit:00",
      "--campaign-manifest", "campaign.json",
      "--campaign-run-id", "contains space")) shouldBe
      Left("--campaign-run-id must be 1-128 characters using only ASCII letters, digits, '.', '_', ':', or '-'")
    parseArgs(Array(
      "--implementation-revision", "commit:00",
      "--campaign-manifest", "campaign.json",
      "--campaign-manifest", "other.json")) shouldBe
      Left("duplicate option --campaign-manifest")
    parseArgs(Array(
      "--implementation-revision", "commit:00",
      "--campaign-manifest", "campaign.json",
      "--campaign-run-id", "run-01",
      "--campaign-run-id", "run-02")) shouldBe
      Left("duplicate option --campaign-run-id")
    parseArgs(Array("--unknown")) shouldBe Left("unknown option at argument index 0")
  }

  test("argument parsing accepts one diagnostic scenario selector") {
    parseArgs(Array("--diagnostic-scenario", "valid-proof")) shouldBe Right(Config(
      15,
      100,
      None,
      None,
      "unrecorded",
      None,
      None,
      Some("valid-proof")))
    parseArgs(Array("--diagnostic-scenario")) shouldBe
      Left("missing value for --diagnostic-scenario")
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--diagnostic-scenario", "late-claim-mismatch")) shouldBe
      Left("duplicate option --diagnostic-scenario")
    parseArgs(Array("--diagnostic-scenario", "Valid-Proof")) shouldBe
      Left("--diagnostic-scenario must be 1-64 characters using only lowercase ASCII letters, digits, or '-'")
    Vector(" valid-proof", "valid-proof ", "", "valid\nproof", "a" * 65).foreach { value =>
      parseArgs(Array("--diagnostic-scenario", value)) shouldBe
        Left("--diagnostic-scenario must be 1-64 characters using only lowercase ASCII letters, digits, or '-'")
    }
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--campaign-manifest", "campaign.json")) shouldBe
      Left("--diagnostic-scenario cannot be combined with campaign options")
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--campaign-run-id", "run-01")) shouldBe
      Left("--diagnostic-scenario cannot be combined with campaign options")
  }

  test("operation-only measurement is explicit and diagnostic-only") {
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--verifier-route", "operation-only")) shouldBe Right(Config(
      15,
      100,
      None,
      None,
      "unrecorded",
      None,
      None,
      Some("valid-proof"),
      OperationOnlyVerifierRoute))
    parseArgs(Array("--verifier-route")) shouldBe
      Left("missing value for --verifier-route")
    parseArgs(Array("--verifier-route", "capturing")) shouldBe
      Left("--verifier-route must be no-probe or operation-only")
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--verifier-route", "operation-only",
      "--verifier-route", "no-probe")) shouldBe
      Left("duplicate option --verifier-route")
    parseArgs(Array("--verifier-route", "operation-only")) shouldBe
      Left("--verifier-route operation-only requires --diagnostic-scenario")
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--verifier-route", "operation-only",
      "--paired-allocation")) shouldBe Right(Config(
      15,
      100,
      None,
      None,
      "unrecorded",
      None,
      None,
      Some("valid-proof"),
      OperationOnlyVerifierRoute,
      pairedAllocation = true))
    parseArgs(Array("--paired-allocation")) shouldBe
      Left("--paired-allocation requires --diagnostic-scenario")
    parseArgs(Array(
      "--diagnostic-scenario", "valid-proof",
      "--paired-allocation",
      "--paired-allocation")) shouldBe
      Left("duplicate option --paired-allocation")
  }

  test("paired-allocation help names the supported verifier routes") {
    Usage should include ("--verifier-route ROUTE")
    Usage should include ("no-probe (default) or operation-only")
    Usage should include (
      "--paired-allocation Run the route-exact preflight and allocation-count gate")
  }

  test("unknown benchmark options never echo secret-shaped or path-shaped tokens") {
    val secretShaped = "--unknown=C:\\private\\credential-token-marker"
    val result = parseArgs(Array(secretShaped))
    result shouldBe Left("unknown option at argument index 0")
    result.toString should not include secretShaped
    result.toString should not include "credential-token-marker"
  }

  test("campaign binding hashes exact bounded bytes without retaining a path") {
    val first = campaignBindingFromBytes("host-a:run-01", "manifest\n".getBytes(StandardCharsets.UTF_8))
    val changed = campaignBindingFromBytes("host-a:run-01", "manifesU\n".getBytes(StandardCharsets.UTF_8))
    first shouldBe Right(CampaignBinding(
      "host-a:run-01",
      9,
      "7021e610a5f62eefd01830fea68e5fa180e8cf017c08ea0890c326b2854ebc96"))
    changed should not be first
    changed match {
      case Right(value) => value.manifestByteLength shouldBe 9
      case Left(detail) => fail(detail)
    }
    campaignBindingFromBytes("bad run", Array[Byte](1)) shouldBe
      Left("campaign run ID is invalid")
    campaignBindingFromBytes("run-01", Array.empty[Byte]) shouldBe
      Left("campaign manifest is empty")
    campaignBindingFromBytes(
      "run-01",
      new Array[Byte](MaxCampaignManifestBytes + 1)) shouldBe
      Left("campaign manifest exceeds 1048576 bytes")
  }

  test("JVM input-argument identity binds order and element boundaries") {
    val identity = jvmInputArgumentsIdentity(Vector("-Xms4g", "-XX:+UseG1GC"))
    identity shouldBe Right(JvmInputArgumentsIdentity(
      2,
      "5c04a86bfc85463c5a6b66a6b0e51fb54d0c80cc09946b47ae03da88ce73bca3"))
    jvmInputArgumentsIdentity(Vector("-XX:+UseG1GC", "-Xms4g")) should not be identity
    jvmInputArgumentsIdentity(Vector("ab", "c")) should not be
      jvmInputArgumentsIdentity(Vector("a", "bc"))
    jvmInputArgumentsIdentity(Vector("ok", null)) shouldBe
      Left("JVM input argument at index 1 is null")
    jvmInputArgumentsIdentity(Vector.fill(MaxJvmInputArguments + 1)("")) shouldBe
      Left("JVM input argument count exceeds 1024")
    jvmInputArgumentsIdentity(Vector("x" * (MaxJvmInputArgumentBytes + 1))) shouldBe
      Left("JVM input argument at index 0 exceeds 65536 characters before UTF-8 encoding")
    jvmInputArgumentsIdentity(Vector("é" * 32769)) shouldBe
      Left("JVM input argument at index 0 exceeds 65536 UTF-8 bytes")
    jvmInputArgumentsIdentity(Vector.fill(17)("x" * MaxJvmInputArgumentBytes)) shouldBe
      Left("JVM input arguments exceed 1048576 total UTF-8 bytes")
    val unpairedSurrogate = new String(Array(0xd800.toChar))
    jvmInputArgumentsIdentity(Vector(unpairedSurrogate)) shouldBe
      Left("JVM input argument at index 0 is not valid Unicode")
  }

  test("nearest-rank p50 p95 p99 and max are reproducible from raw samples") {
    val samples = (1L to 100L).reverse
    statistics(samples) shouldBe Right(TimingStatistics(
      sampleCount = 100,
      p50Ns = 50,
      p95Ns = 95,
      p99Ns = 99,
      maxNs = 100))
    statistics(Seq(9L)) shouldBe Right(TimingStatistics(1, 9, 9, 9, 9))
    statistics(Seq.empty) shouldBe Left("samples are empty")
    statistics(Seq(1L, -1L)) shouldBe Left("samples contain a negative duration")
  }

  test("allocation summaries preserve raw samples and reject unavailable values") {
    allocationStatistics((1L to 100L).reverse) shouldBe Right(AllocationStatistics(
      sampleCount = 100,
      p50Bytes = 50,
      p95Bytes = 95,
      p99Bytes = 99,
      maxBytes = 100))
    allocationStatistics(Seq.empty) shouldBe Left("allocation samples are empty")
    allocationStatistics(Seq(1L, -1L)) shouldBe
      Left("allocation samples contain a negative value")

    allocatedBytesDelta(100L, 175L) shouldBe Right(75L)
    allocatedBytesDelta(-1L, 175L) shouldBe
      Left("thread allocation counter before sample is unavailable")
    allocatedBytesDelta(100L, -1L) shouldBe
      Left("thread allocation counter after sample is unavailable")
    allocatedBytesDelta(175L, 100L) shouldBe
      Left("thread allocation counter moved backwards")
  }

  test("garbage collector deltas fail closed on unsupported or unstable counters") {
    val before = Vector(
      GarbageCollectorSnapshot("old", 3, 20),
      GarbageCollectorSnapshot("young", 8, 11))
    val after = Vector(
      GarbageCollectorSnapshot("old", 4, 27),
      GarbageCollectorSnapshot("young", 10, 16))
    garbageCollectorDeltas(before, after) shouldBe Right(Vector(
      GarbageCollectorDelta("old", 1, 7),
      GarbageCollectorDelta("young", 2, 5)))

    garbageCollectorDeltas(
      before,
      after.reverse) shouldBe Left("garbage collector set or order changed during sampling")
    garbageCollectorDeltas(Vector.empty, Vector.empty) shouldBe
      Left("garbage collector snapshot is empty")
    garbageCollectorDeltas(
      Vector(GarbageCollectorSnapshot("old", -1, 20)),
      Vector(GarbageCollectorSnapshot("old", -1, 20))) shouldBe
      Left("garbage collector old does not expose collection counts")
    garbageCollectorDeltas(
      Vector(GarbageCollectorSnapshot("old", 4, -1)),
      Vector(GarbageCollectorSnapshot("old", 4, -1))) shouldBe
      Left("garbage collector old does not expose collection time")
    garbageCollectorDeltas(
      Vector(GarbageCollectorSnapshot("old", 4, 20)),
      Vector(GarbageCollectorSnapshot("old", 3, 21))) shouldBe
      Left("garbage collector old collection count moved backwards")
    garbageCollectorDeltas(
      Vector(GarbageCollectorSnapshot("old", 4, 20)),
      Vector(GarbageCollectorSnapshot("old", 5, 19))) shouldBe
      Left("garbage collector old collection time moved backwards")
  }

  test("memory pool phase envelopes enforce snapshot invariants without freezing committed or max") {
    val identities = Vector(
      MemoryPoolIdentity("heap", "HEAP"),
      MemoryPoolIdentity("metaspace", "NON_HEAP"))
    val envelopes = Vector(
      MemoryPoolPhaseEnvelope(
        identities(0),
        MemoryUsageEvidence(10L, 20L, -1L),
        MemoryUsageEvidence(15L, 40L, 80L),
        MemoryUsageEvidence(20L, 30L, 60L)),
      MemoryPoolPhaseEnvelope(
        identities(1),
        MemoryUsageEvidence(3L, 8L, 10L),
        MemoryUsageEvidence(4L, 6L, -1L),
        MemoryUsageEvidence(5L, 9L, 12L)))

    validateMemoryPoolPhaseEnvelopes(identities, envelopes) shouldBe Right(())
    validateMemoryPoolIdentities(null) shouldBe
      Left("memory pool identities are null")
    validateMemoryPoolIdentities(Vector.empty) shouldBe
      Left("memory pool identities are empty")
    validateMemoryPoolIdentities(Vector.fill(MaxMemoryPools + 1)(identities.head)) shouldBe
      Left("memory pool identity count exceeds " + MaxMemoryPools)
    validateMemoryPoolIdentities(Vector(null)) shouldBe
      Left("memory pool identities contain null")
    validateMemoryPoolIdentities(identities.reverse) shouldBe
      Left("memory pool identities are not sorted")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity("heap", "OTHER"))) shouldBe
      Left("memory pool identity type is invalid")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity("", "HEAP"))) shouldBe
      Left("memory pool identity name is invalid")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity(null, "HEAP"))) shouldBe
      Left("memory pool identity name is invalid")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity("x" * 4097, "HEAP"))) shouldBe
      Left("memory pool identity name is invalid")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity("bad\nname", "HEAP"))) shouldBe
      Left("memory pool identity name is invalid")
    validateMemoryPoolIdentities(Vector(MemoryPoolIdentity(
      new String(Array(0xd800.toChar)),
      "HEAP"))) shouldBe Left("memory pool identity name is invalid")
    validateMemoryPoolIdentities(Vector(identities.head, identities.head)) shouldBe
      Left("memory pool identities contain duplicates")
    validateMemoryUsageEvidence(null, "snapshot") shouldBe
      Left("snapshot is null")
    validateMemoryUsageEvidence(
      MemoryUsageEvidence(-1L, 0L, -1L),
      "snapshot") shouldBe Left("snapshot used bytes are negative")
    validateMemoryUsageEvidence(
      MemoryUsageEvidence(0L, -1L, -1L),
      "snapshot") shouldBe Left("snapshot committed bytes are negative")
    validateMemoryUsageEvidence(
      MemoryUsageEvidence(0L, 0L, -2L),
      "snapshot") shouldBe Left("snapshot maximum bytes are invalid")
    validateMemoryUsageEvidence(
      MemoryUsageEvidence(21L, 20L, 30L),
      "snapshot") shouldBe Left("snapshot used bytes exceed committed bytes")
    validateMemoryUsageEvidence(
      MemoryUsageEvidence(10L, 20L, 19L),
      "snapshot") shouldBe Left("snapshot committed bytes exceed maximum bytes")
    validateMemoryPoolPhaseEnvelopes(
      identities,
      envelopes.updated(
        0,
        envelopes.head.copy(finalPeakUsage = MemoryUsageEvidence(14L, 30L, 60L)))) shouldBe
      Left("memory pool heap final peak used bytes are below end used bytes")
    validateMemoryPoolPhaseEnvelopes(
      identities,
      envelopes.updated(
        0,
        envelopes.head.copy(finalPeakUsage = MemoryUsageEvidence(9L, 30L, 60L)))) shouldBe
      Left("memory pool heap final peak used bytes are below after-reset peak used bytes")
    validateMemoryPoolPhaseEnvelopes(identities, envelopes.dropRight(1)) shouldBe
      Left("memory pool phase envelope identities do not match the environment")
    validateMemoryPoolPhaseEnvelopes(identities, envelopes.reverse) shouldBe
      Left("memory pool phase envelope identities do not match the environment")
    validateMemoryPoolPhaseEnvelopes(identities, null) shouldBe
      Left("memory pool phase envelopes are null")
    validateMemoryPoolPhaseEnvelopes(identities, Vector.empty) shouldBe
      Left("memory pool phase envelopes are empty")
    validateMemoryPoolPhaseEnvelopes(identities, Vector(null)) shouldBe
      Left("memory pool phase envelopes contain null")
  }

  test("JSON quoting is complete for evidence-controlled strings") {
    quote("a\"b\\c\n\t" + 1.toChar) shouldBe "\"a\\\"b\\\\c\\n\\t\\u0001\""
  }

  test("domain-separated evidence digest has an independent golden value") {
    evidenceDigest("{\"a\":1}") shouldBe
      "cf1aea685225c7a477c259f727950cb96f124be2dc6764bbea7a7916484ca062"
  }

  test("evidence envelope is canonical and binds raw samples and metadata") {
    val payload = samplePayload(Vector(10L, 20L))
    val rendered = renderEnvelope(payload)
    rendered should startWith("{\"schema\":\"eip-0045-jvm-verifier-benchmark-v5\"")
    rendered should endWith("}\n")
    rendered should include("\"campaignBinding\":{\"runId\":\"host-a:run-01\",\"manifestByteLength\":9")
    rendered should include("\"samplesNs\":[10,20]")
    rendered should include("\"p99Ns\":20")
    rendered should include("\"allocatedBytes\":[100,200]")
    rendered should include("\"p99Bytes\":200")
    rendered should include("\"jvmInputArgumentCount\":2")
    rendered should include(
      "\"memoryPoolIdentities\":[{\"name\":\"heap\",\"memoryType\":\"HEAP\"}]")
    rendered should include(
      "\"afterResetPeakUsage\":{\"usedBytes\":10,\"committedBytes\":20,\"maxBytes\":-1}")
    rendered should include("\"validationBoundary\":\"verification-complete\"")
    rendered should include("\"lastVerifierCheckpoint\":\"query\"")
    rendered should include("\"garbageCollectorDeltas\":[{\"name\":\"gc\",\"collections\":1,\"collectionTimeMs\":2}]")

    val changed = renderEnvelope(samplePayload(Vector(10L, 21L)))
    val changedManifest = renderEnvelope(payload.copy(
      campaignBinding = Some(payload.campaignBinding.get.copy(
        manifestSha256 = "b" * 64))))
    val changedRun = renderEnvelope(payload.copy(
      campaignBinding = Some(payload.campaignBinding.get.copy(runId = "host-a:run-02"))))
    val changedJvmArguments = renderEnvelope(payload.copy(
      environment = payload.environment.copy(jvmInputArgumentsSha256 = "c" * 64)))
    val changedMemory = renderEnvelope(payload.copy(
      memoryPoolPhaseEnvelopes = payload.memoryPoolPhaseEnvelopes.map { envelope =>
        envelope.copy(finalPeakUsage = envelope.finalPeakUsage.copy(usedBytes = 21L))
      }))
    digestFrom(rendered) shouldBe evidenceDigest(renderPayload(payload))
    digestFrom(rendered) should not be digestFrom(changed)
    digestFrom(rendered) should not be digestFrom(changedManifest)
    digestFrom(rendered) should not be digestFrom(changedRun)
    digestFrom(rendered) should not be digestFrom(changedJvmArguments)
    digestFrom(rendered) should not be digestFrom(changedMemory)
    renderEnvelope(payload) shouldBe rendered
  }

  test("evidence rendering rejects allocation and GC metadata drift") {
    val payload = samplePayload(Vector(10L, 20L))
    val shortAllocation = payload.scenarios.head.copy(
      allocatedBytes = Vector(100L),
      allocationStatistics = AllocationStatistics(1, 100, 100, 100, 100))
    val allocationError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(scenarios = Vector(shortAllocation)))
    }
    allocationError.getMessage should include(
      "allocation sample count does not match sampleRounds")

    val gcError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(
        garbageCollectorDeltas = Vector(GarbageCollectorDelta("other", 0, 0))))
    }
    gcError.getMessage should include("garbage collector metadata does not match sampled deltas")

    val manifestError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(
        campaignBinding = Some(payload.campaignBinding.get.copy(
          manifestSha256 = "A" * 64))))
    }
    manifestError.getMessage should include("campaign manifest SHA-256 is invalid")

    val revisionError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(implementationRevision = "unrecorded"))
    }
    revisionError.getMessage should include(
      "campaign-bound evidence has an unrecorded implementation revision")

    val nullRevisionError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(
        implementationRevision = null,
        campaignBinding = None))
    }
    nullRevisionError.getMessage shouldBe
      "requirement failed: implementationRevision is null"

    val emptyRevisionError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(
        implementationRevision = "",
        campaignBinding = None))
    }
    emptyRevisionError.getMessage shouldBe
      "requirement failed: implementationRevision must not be empty"

    val argumentsError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(
        environment = payload.environment.copy(jvmInputArgumentCount = -1)))
    }
    argumentsError.getMessage should include("JVM input argument count is invalid")

    val emptyBoundaryError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(scenarios = Vector(
        payload.scenarios.head.copy(validationBoundary = ""))))
    }
    emptyBoundaryError.getMessage should include("validation boundary is empty")

    val nullCheckpointError = intercept[IllegalArgumentException] {
      renderEnvelope(payload.copy(scenarios = Vector(
        payload.scenarios.head.copy(lastVerifierCheckpoint = null))))
    }
    nullCheckpointError.getMessage should include("last verifier checkpoint is empty")
  }

  test("failed campaign manifest reads use constant errors and create no output") {
    val directory = Files.createTempDirectory("eip0045-b5-campaign-negative-")
    val missing = directory.resolve("missing.json")
    val empty = directory.resolve("empty.json")
    val oversized = directory.resolve("oversized.json")
    try {
      Files.createFile(empty)
      Files.write(oversized, new Array[Byte](MaxCampaignManifestBytes + 1))
      failedCampaignMessage(directory, missing, "missing-output.json") shouldBe
        "campaign manifest is not a regular file"
      failedCampaignMessage(directory, empty, "empty-output.json") shouldBe
        "campaign manifest is empty"
      failedCampaignMessage(directory, oversized, "oversized-output.json") shouldBe
        "campaign manifest exceeds 1048576 bytes"
    } finally {
      Files.deleteIfExists(missing)
      Files.deleteIfExists(empty)
      Files.deleteIfExists(oversized)
      Files.deleteIfExists(directory)
    }
  }

  test("benchmark output failures use path-free diagnostics") {
    val directory = Files.createTempDirectory("eip0045-b5-output-diagnostic-")
    val missingParent = directory.resolve("private-credential-marker").resolve("evidence.json")
    try {
      val error = intercept[IllegalArgumentException] {
        Eip0045VerifierBenchmark.main(Array(
          "--warmup-rounds", "0",
          "--sample-rounds", "1",
          "--output", missingParent.toString))
      }
      error.getMessage shouldBe "benchmark output parent directory does not exist"
      error.toString should not include missingParent.toString
      error.toString should not include "private-credential-marker"
      Files.exists(missingParent) shouldBe false
    } finally Files.deleteIfExists(directory)
  }

  test("memory pool handles are sorted and sampled in the fixed phase order") {
    val events = ArrayBuffer.empty[String]

    final class Handle(
        override val identity: MemoryPoolIdentity,
        afterResetPeak: MemoryUsageEvidence,
        end: MemoryUsageEvidence,
        finalPeak: MemoryUsageEvidence)
        extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      private var peakReads = 0
      override def isValid: Boolean = {
        events += ("valid:" + identity.name)
        true
      }
      override def resetPeakUsage(): Unit = events += ("reset:" + identity.name)
      override def usage(): MemoryUsageEvidence = {
        events += ("usage:" + identity.name)
        end
      }
      override def peakUsage(): MemoryUsageEvidence = {
        events += ("peak:" + identity.name)
        peakReads += 1
        if (peakReads == 1) afterResetPeak else finalPeak
      }
    }

    val a = new Handle(
      MemoryPoolIdentity("a-heap", "HEAP"),
      MemoryUsageEvidence(10L, 20L, -1L),
      MemoryUsageEvidence(20L, 40L, 80L),
      MemoryUsageEvidence(25L, 30L, 60L))
    val z = new Handle(
      MemoryPoolIdentity("z-meta", "NON_HEAP"),
      MemoryUsageEvidence(4L, 10L, 20L),
      MemoryUsageEvidence(5L, 8L, -1L),
      MemoryUsageEvidence(8L, 12L, 30L))
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        events += "source"
        Vector(z, a)
      }
    }

    val topology = Eip0045VerifierBenchmark.openMemoryPoolTopology(source)
    topology.identities shouldBe Vector(a.identity, z.identity)
    val measured = Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(
      topology,
      source) {
      events += "sampling"
      "complete"
    }
    measured._1 shouldBe "complete"
    measured._2.map(_.identity) shouldBe Vector(a.identity, z.identity)
    measured._2.head shouldBe MemoryPoolPhaseEnvelope(
      a.identity,
      MemoryUsageEvidence(10L, 20L, -1L),
      MemoryUsageEvidence(20L, 40L, 80L),
      MemoryUsageEvidence(25L, 30L, 60L))
    events.toVector shouldBe Vector(
      "source", "valid:z-meta", "valid:a-heap",
      "source", "valid:z-meta", "valid:a-heap",
      "valid:a-heap", "reset:a-heap",
      "valid:z-meta", "reset:z-meta",
      "valid:a-heap", "peak:a-heap",
      "valid:z-meta", "peak:z-meta",
      "sampling",
      "valid:a-heap", "usage:a-heap",
      "valid:z-meta", "usage:z-meta",
      "valid:a-heap", "peak:a-heap",
      "valid:z-meta", "peak:z-meta",
      "source", "valid:z-meta", "valid:a-heap")
  }

  test("fresh topology checks never replace the policy-bound memory pool handles") {
    val poolIdentity = MemoryPoolIdentity("heap", "HEAP")
    final class OriginalHandle extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override val identity: MemoryPoolIdentity = poolIdentity
      var resets: Int = 0
      var usageReads: Int = 0
      var peakReads: Int = 0
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = resets += 1
      override def usage(): MemoryUsageEvidence = {
        usageReads += 1
        MemoryUsageEvidence(20L, 30L, -1L)
      }
      override def peakUsage(): MemoryUsageEvidence = {
        peakReads += 1
        if (peakReads == 1) MemoryUsageEvidence(10L, 25L, -1L)
        else MemoryUsageEvidence(25L, 35L, -1L)
      }
    }
    final class CheckOnlyHandle extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override val identity: MemoryPoolIdentity = poolIdentity
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit =
        throw new IllegalStateException("fresh topology handle was reset")
      override def usage(): MemoryUsageEvidence =
        throw new IllegalStateException("fresh topology handle usage was read")
      override def peakUsage(): MemoryUsageEvidence =
        throw new IllegalStateException("fresh topology handle peak was read")
    }
    val original = new OriginalHandle
    var reads = 0
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        reads += 1
        if (reads == 1) Vector(original) else Vector(new CheckOnlyHandle)
      }
    }

    val topology = Eip0045VerifierBenchmark.openMemoryPoolTopology(source)
    val measured = Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(
      topology,
      source)("sampled")
    measured shouldBe ("sampled" -> Vector(MemoryPoolPhaseEnvelope(
      poolIdentity,
      MemoryUsageEvidence(10L, 25L, -1L),
      MemoryUsageEvidence(20L, 30L, -1L),
      MemoryUsageEvidence(25L, 35L, -1L))))
    reads shouldBe 3
    original.resets shouldBe 1
    original.usageReads shouldBe 1
    original.peakReads shouldBe 2
  }

  test("memory pool sampling phases are serialized within the runner instance") {
    val firstSamplingEntered = new CountDownLatch(1)
    val releaseFirstSampling = new CountDownLatch(1)
    val secondStarted = new CountDownLatch(1)
    val secondReset = new CountDownLatch(1)
    val failures = new ConcurrentLinkedQueue[Throwable]()

    final class Handle(
        override val identity: MemoryPoolIdentity,
        onReset: () => Unit)
        extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = onReset()
      override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
      override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
    }
    def sourceOf(handle: Eip0045VerifierBenchmark.MemoryPoolHandle) =
      new Eip0045VerifierBenchmark.MemoryPoolSource {
        override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
          Vector(handle)
      }

    val firstSource = sourceOf(new Handle(
      MemoryPoolIdentity("first-heap", "HEAP"),
      () => ()))
    val secondSource = sourceOf(new Handle(
      MemoryPoolIdentity("second-heap", "HEAP"),
      () => secondReset.countDown()))
    val firstTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(firstSource)
    val secondTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(secondSource)

    val first = new Thread(new Runnable {
      override def run(): Unit = try {
        Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(
          firstTopology,
          firstSource) {
          firstSamplingEntered.countDown()
          if (!releaseFirstSampling.await(5L, TimeUnit.SECONDS))
            throw new IllegalStateException("first sampling release timed out")
        }
      } catch {
        case error: Throwable => failures.add(error)
      }
    }, "eip0045-memory-pool-first")
    val second = new Thread(new Runnable {
      override def run(): Unit = try {
        secondStarted.countDown()
        Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(
          secondTopology,
          secondSource)(())
      } catch {
        case error: Throwable => failures.add(error)
      }
    }, "eip0045-memory-pool-second")

    first.start()
    try {
      firstSamplingEntered.await(5L, TimeUnit.SECONDS) shouldBe true
      second.start()
      secondStarted.await(5L, TimeUnit.SECONDS) shouldBe true
      val blockedDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5L)
      while (second.getState != Thread.State.BLOCKED &&
          secondReset.getCount != 0L && System.nanoTime() < blockedDeadline) {
        Thread.`yield`()
      }
      secondReset.getCount shouldBe 1L
      second.getState shouldBe Thread.State.BLOCKED
    } finally {
      releaseFirstSampling.countDown()
      first.join(5000L)
      second.join(5000L)
    }

    first.isAlive shouldBe false
    second.isAlive shouldBe false
    secondReset.await(1L, TimeUnit.SECONDS) shouldBe true
    if (!failures.isEmpty) fail("memory pool worker failed", failures.peek())
  }

  test("successful production order is validation then warmup then reset and sampling") {
    val events = ArrayBuffer.empty[String]
    final class Handle extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override val identity: MemoryPoolIdentity = MemoryPoolIdentity("test-heap", "HEAP")
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = events += "reset"
      override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
      override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
    }
    val handle = new Handle
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
        Vector(handle)
    }
    val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeVerifierSetup(): Unit = events += "setup"
      override def beforeValidationInvocation(): Unit = events += "validation"
      override def beforeWarmupInvocation(): Unit = events += "warmup"
      override def beforeTimedInvocation(): Unit = events += "sample"
      override def beforeOutput(): Unit = events += "output"
    }
    val directory = Files.createTempDirectory("eip0045-memory-pool-order-")
    val output = directory.resolve("evidence.json")
    try {
      Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
        "--warmup-rounds", "1",
        "--sample-rounds", "1",
        "--implementation-revision", "commit:" + ("1" * 40),
        "--cpu-model", "Test CPU",
        "--output", output.toString), observer, source)

      val validation = events.zipWithIndex.collect {
        case ("validation", index) => index
      }
      val warmup = events.zipWithIndex.collect {
        case ("warmup", index) => index
      }
      val samples = events.zipWithIndex.collect {
        case ("sample", index) => index
      }
      validation.length shouldBe ExpectedScenarios.length
      warmup.length shouldBe ExpectedScenarios.length
      samples.length shouldBe ExpectedScenarios.length
      events.indexOf("setup") should be < validation.head
      validation.last should be < warmup.head
      warmup.last should be < events.indexOf("reset")
      events.indexOf("reset") should be < samples.head
      samples.last should be < events.indexOf("output")
      Files.isRegularFile(output) shouldBe true
    } finally {
      Files.deleteIfExists(output)
      Files.deleteIfExists(directory)
    }
  }

  test("diagnostic selection isolates one scenario before every measured phase") {
    val directory = Files.createTempDirectory("eip0045-single-scenario-")
    try {
      ExpectedScenarios.foreach { policy =>
        val events = ArrayBuffer.empty[String]
        final class Handle extends Eip0045VerifierBenchmark.MemoryPoolHandle {
          override val identity: MemoryPoolIdentity = MemoryPoolIdentity("test-heap", "HEAP")
          override def isValid: Boolean = true
          override def resetPeakUsage(): Unit = events += "reset"
          override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
          override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
        }
        val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
          override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
            Vector(new Handle)
        }
        val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
          override def beforeVerifierSetup(): Unit = events += "setup"
          override def beforeValidationInvocation(): Unit = events += "validation"
          override def beforeWarmupInvocation(): Unit = events += "warmup"
          override def beforeTimedInvocation(): Unit = events += "sample"
          override def beforeOutput(): Unit = events += "output"
        }
        val output = directory.resolve(policy.id + ".json")
        try {
          Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
            "--diagnostic-scenario", policy.id,
            "--warmup-rounds", "1",
            "--sample-rounds", "1",
            "--implementation-revision", "commit:" + ("1" * 40),
            "--cpu-model", "Test CPU",
            "--output", output.toString), observer, source)

          events.count(_ == "validation") shouldBe 1
          events.count(_ == "warmup") shouldBe 1
          events.count(_ == "sample") shouldBe 1
          events.indexOf("setup") should be < events.indexOf("validation")
          events.indexOf("warmup") should be < events.indexOf("reset")
          events.indexOf("reset") should be < events.indexOf("sample")
          events.indexOf("sample") should be < events.indexOf("output")

          val json = new String(Files.readAllBytes(output), StandardCharsets.UTF_8)
          json should include ("\"id\":\"" + policy.id + "\"")
          ExpectedScenarios.filterNot(_.id == policy.id).foreach { excluded =>
            json should not include ("\"id\":\"" + excluded.id + "\"")
          }
          json should include (
            "Validation invokes the selected scenario before warmup")
          json should not include (
            "Validation invokes every scenario before warmup")
          json should include (
            "Diagnostic scenario selection excludes the other five fixed scenarios")
        } finally {
          Files.deleteIfExists(output)
        }
      }
    } finally {
      Files.deleteIfExists(directory)
    }
  }

  test("paired diagnostics bind and execute each exact verifier route") {
    val events = ArrayBuffer.empty[String]
    final class Handle extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override val identity: MemoryPoolIdentity = MemoryPoolIdentity("test-heap", "HEAP")
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = events += "reset"
      override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
      override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
    }
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
        Vector(new Handle)
    }
    val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeVerifierSetup(): Unit = events += "setup"
      override def beforeValidationInvocation(): Unit = events += "validation"
      override def beforeWarmupInvocation(): Unit = events += "warmup"
      override def beforeTimedInvocation(): Unit = events += "sample"
      override def beforeOutput(): Unit = events += "output"
    }
    val directory = Files.createTempDirectory("eip0045-operation-only-")
    val output = directory.resolve("evidence.json")
    val noProbeOutput = directory.resolve("no-probe-evidence.json")
    try {
      Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
        "--diagnostic-scenario", "valid-proof",
        "--verifier-route", "operation-only",
        "--paired-allocation",
        "--warmup-rounds", "1",
        "--sample-rounds", "1",
        "--implementation-revision", "commit:" + ("2" * 40),
        "--cpu-model", "Test CPU",
        "--output", output.toString), observer, source)

      events.count(_ == "validation") shouldBe 1
      events.count(_ == "warmup") shouldBe 1
      events.count(_ == "sample") shouldBe 1
      events.indexOf("setup") should be < events.indexOf("validation")
      events.indexOf("validation") should be < events.indexOf("warmup")
      events.indexOf("warmup") should be < events.indexOf("reset")
      events.indexOf("reset") should be < events.indexOf("sample")
      events.indexOf("sample") should be < events.indexOf("output")

      val json = new String(Files.readAllBytes(output), StandardCharsets.UTF_8)
      json should include (
        "\"verifierEntryPoint\":\"sigma.stark.profile.Risc0RawSealVerifier.verifyObservedOperations\"")
      json should include (
        "\"timedScope\":\"Risc0RawSealVerifier.verifyObservedOperations; fixture and profile loading excluded\"")
      json should include (
        "The operation-only route uses one preallocated observer with seven primitive counters")
      json should include (
        "The operation-only route is diagnostic-only")
      json should include (
        "\"validationQueryCheckpoints\":0")
      json should include (
        "\"lastVerifierCheckpoint\":\"none\"")
      json should include (
        "This paired-allocation process validates only the selected verifier route before warmup")

      events.clear()
      Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
        "--diagnostic-scenario", "valid-proof",
        "--verifier-route", "no-probe",
        "--paired-allocation",
        "--warmup-rounds", "1",
        "--sample-rounds", "1",
        "--implementation-revision", "commit:" + ("3" * 40),
        "--cpu-model", "Test CPU",
        "--output", noProbeOutput.toString), observer, source)

      events.count(_ == "validation") shouldBe 1
      events.count(_ == "warmup") shouldBe 1
      events.count(_ == "sample") shouldBe 1
      val noProbeJson =
        new String(Files.readAllBytes(noProbeOutput), StandardCharsets.UTF_8)
      noProbeJson should include (
        "\"verifierEntryPoint\":\"sigma.stark.profile.Risc0RawSealVerifier.verify\"")
      noProbeJson should include (
        "\"timedScope\":\"Risc0RawSealVerifier.verify; fixture and profile loading excluded\"")
      noProbeJson should not include ("verifyObservedOperations")
      noProbeJson should include ("\"validationQueryCheckpoints\":0")
    } finally {
      Files.deleteIfExists(output)
      Files.deleteIfExists(noProbeOutput)
      Files.deleteIfExists(directory)
    }
  }

  test("unknown diagnostic selection fails before management and verifier setup") {
    var allocationMeterOpens = 0
    var topologyReads = 0
    var verifierSetups = 0
    var outputAttempts = 0
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        topologyReads += 1
        throw new AssertionError("memory pools must not be read")
      }
    }
    val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeThreadAllocationMeterOpen(): Unit = allocationMeterOpens += 1
      override def beforeVerifierSetup(): Unit = verifierSetups += 1
      override def beforeOutput(): Unit = outputAttempts += 1
    }
    val directory = Files.createTempDirectory("eip0045-unknown-scenario-")
    val output = directory.resolve("must-not-exist.json")
    try {
      val error = intercept[IllegalArgumentException] {
        Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
          "--diagnostic-scenario", "private-token-marker",
          "--sample-rounds", "1",
          "--output", output.toString), observer, source)
      }
      error.getMessage shouldBe "unknown diagnostic scenario"
      error.getMessage should not include "private-token-marker"
      allocationMeterOpens shouldBe 0
      topologyReads shouldBe 0
      verifierSetups shouldBe 0
      outputAttempts shouldBe 0
      Files.exists(output) shouldBe false
    } finally {
      Files.deleteIfExists(output)
      Files.deleteIfExists(directory)
    }
  }

  test("mixed help and diagnostic selectors fail before management and evidence output") {
    val invocations = Vector(
      Array("--diagnostic-scenario", "Valid-Proof", "--help"),
      Array(
        "--diagnostic-scenario", "valid-proof",
        "--diagnostic-scenario", "late-claim-mismatch",
        "--help"),
      Array("--diagnostic-scenario", "private-token-marker", "--help"),
      Array(
        "--diagnostic-scenario", "valid-proof",
        "--campaign-manifest", "campaign.json",
        "--help"))

    invocations.zipWithIndex.foreach { case (args, index) =>
      var allocationMeterOpens = 0
      var topologyReads = 0
      var verifierSetups = 0
      var outputAttempts = 0
      val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
        override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
          topologyReads += 1
          throw new AssertionError("memory pools must not be read")
        }
      }
      val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
        override def beforeThreadAllocationMeterOpen(): Unit = allocationMeterOpens += 1
        override def beforeVerifierSetup(): Unit = verifierSetups += 1
        override def beforeOutput(): Unit = outputAttempts += 1
      }
      val directory = Files.createTempDirectory("eip0045-mixed-help-")
      val output = directory.resolve("must-not-exist-" + index + ".json")
      try {
        intercept[IllegalArgumentException] {
          Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(
            args ++ Array("--output", output.toString),
            observer,
            source)
        }
        allocationMeterOpens shouldBe 0
        topologyReads shouldBe 0
        verifierSetups shouldBe 0
        outputAttempts shouldBe 0
        Files.exists(output) shouldBe false
      } finally {
        Files.deleteIfExists(output)
        Files.deleteIfExists(directory)
      }
    }
  }

  test("complete benchmark runs are serialized before verifier setup") {
    val firstTimedInvocation = new CountDownLatch(1)
    val releaseFirstRun = new CountDownLatch(1)
    val secondStarted = new CountDownLatch(1)
    val secondVerifierSetup = new CountDownLatch(1)
    val firstTimedOnce = new AtomicBoolean(false)
    val failures = new ConcurrentLinkedQueue[Throwable]()

    final class Handle(name: String) extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override val identity: MemoryPoolIdentity = MemoryPoolIdentity(name, "HEAP")
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = ()
      override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
      override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
    }
    def source(name: String) = {
      val handle = new Handle(name)
      new Eip0045VerifierBenchmark.MemoryPoolSource {
        override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
          Vector(handle)
      }
    }
    val firstObserver = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeVerifierSetup(): Unit = ()
      override def beforeTimedInvocation(): Unit = {
        if (firstTimedOnce.compareAndSet(false, true)) {
          firstTimedInvocation.countDown()
          if (!releaseFirstRun.await(10L, TimeUnit.SECONDS))
            throw new IllegalStateException("first benchmark release timed out")
        }
      }
      override def beforeOutput(): Unit = ()
    }
    val secondObserver = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeVerifierSetup(): Unit = secondVerifierSetup.countDown()
      override def beforeOutput(): Unit = ()
    }
    val directory = Files.createTempDirectory("eip0045-complete-run-lock-")
    val firstOutput = directory.resolve("first.json")
    val secondOutput = directory.resolve("second.json")
    def args(output: java.nio.file.Path): Array[String] = Array(
      "--warmup-rounds", "0",
      "--sample-rounds", "1",
      "--implementation-revision", "commit:" + ("1" * 40),
      "--cpu-model", "Test CPU",
      "--output", output.toString)

    val first = new Thread(new Runnable {
      override def run(): Unit = try {
        Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(
          args(firstOutput), firstObserver, source("first-heap"))
      } catch {
        case error: Throwable => failures.add(error)
      }
    }, "eip0045-complete-run-first")
    val second = new Thread(new Runnable {
      override def run(): Unit = try {
        secondStarted.countDown()
        Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(
          args(secondOutput), secondObserver, source("second-heap"))
      } catch {
        case error: Throwable => failures.add(error)
      }
    }, "eip0045-complete-run-second")

    try {
      first.start()
      try {
        firstTimedInvocation.await(15L, TimeUnit.SECONDS) shouldBe true
        second.start()
        secondStarted.await(5L, TimeUnit.SECONDS) shouldBe true
        val blockedDeadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5L)
        while (second.getState != Thread.State.BLOCKED &&
            secondVerifierSetup.getCount != 0L && System.nanoTime() < blockedDeadline) {
          Thread.`yield`()
        }
        secondVerifierSetup.getCount shouldBe 1L
        second.getState shouldBe Thread.State.BLOCKED
      } finally {
        releaseFirstRun.countDown()
        first.join(30000L)
        second.join(30000L)
      }

      first.isAlive shouldBe false
      second.isAlive shouldBe false
      secondVerifierSetup.await(1L, TimeUnit.SECONDS) shouldBe true
      if (!failures.isEmpty) fail("complete benchmark worker failed", failures.peek())
      Files.isRegularFile(firstOutput) shouldBe true
      Files.isRegularFile(secondOutput) shouldBe true
    } finally {
      releaseFirstRun.countDown()
      if (first.isAlive) first.join(30000L)
      if (second.isAlive) second.join(30000L)
      Files.deleteIfExists(firstOutput)
      Files.deleteIfExists(secondOutput)
      Files.deleteIfExists(directory)
    }
  }

  test("memory pool source, reset, read, validity, and topology drift fail closed") {
    final class Handle(
        override val identity: MemoryPoolIdentity,
        valid: () => Boolean = () => true,
        reset: () => Unit = () => (),
        current: () => MemoryUsageEvidence = () => MemoryUsageEvidence(2L, 4L, -1L),
        peak: () => MemoryUsageEvidence = () => MemoryUsageEvidence(3L, 5L, -1L))
        extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override def isValid: Boolean = valid()
      override def resetPeakUsage(): Unit = reset()
      override def usage(): MemoryUsageEvidence = current()
      override def peakUsage(): MemoryUsageEvidence = peak()
    }
    def sourceOf(handle: Eip0045VerifierBenchmark.MemoryPoolHandle) =
      new Eip0045VerifierBenchmark.MemoryPoolSource {
        override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
          Vector(handle)
      }

    val nullTopology = intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(
        new Eip0045VerifierBenchmark.MemoryPoolSource {
          override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = null
        })
    }
    nullTopology.getMessage shouldBe "memory pool topology is null"

    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(
        new Eip0045VerifierBenchmark.MemoryPoolSource {
          override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
            Vector.empty
        })
    }.getMessage shouldBe "memory pool topology is empty"

    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(
        new Eip0045VerifierBenchmark.MemoryPoolSource {
          override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
            Vector(null)
        })
    }.getMessage shouldBe "memory pool topology contains a null handle"

    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(
        new Eip0045VerifierBenchmark.MemoryPoolSource {
          override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] =
            throw new IllegalArgumentException("source failed")
        })
    }.getMessage shouldBe "memory pool topology could not be read"

    val invalid = new Handle(MemoryPoolIdentity("heap", "HEAP"), valid = () => false)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(sourceOf(invalid))
    }.getMessage shouldBe "memory pool heap is invalid"

    val nullIdentity = new Handle(null)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.openMemoryPoolTopology(sourceOf(nullIdentity))
    }.getMessage shouldBe "memory pool identities contain null"

    val resetFailure = new Handle(
      MemoryPoolIdentity("heap", "HEAP"),
      reset = () => throw new IllegalArgumentException("reset failed"))
    val resetSource = sourceOf(resetFailure)
    val resetTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(resetSource)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(resetTopology, resetSource)(())
    }.getMessage shouldBe "memory pool heap peak usage could not be reset"

    val readFailure = new Handle(
      MemoryPoolIdentity("heap", "HEAP"),
      peak = () => null)
    val readSource = sourceOf(readFailure)
    val readTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(readSource)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(readTopology, readSource)(())
    }.getMessage shouldBe "memory pool heap after-reset peak usage is null"

    val stable = new Handle(MemoryPoolIdentity("heap", "HEAP"))
    val changed = new Handle(MemoryPoolIdentity("other", "HEAP"))
    var sourceReads = 0
    val driftingSource = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        sourceReads += 1
        if (sourceReads < 3) Vector(stable) else Vector(changed)
      }
    }
    val driftTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(driftingSource)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(driftTopology, driftingSource)(())
    }.getMessage shouldBe "memory pool topology changed during sampling"

    var preResetReads = 0
    val preResetDrift = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        preResetReads += 1
        if (preResetReads == 1) Vector(stable) else Vector(changed)
      }
    }
    val preResetTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(preResetDrift)
    intercept[IllegalStateException] {
      Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(
        preResetTopology,
        preResetDrift)(())
    }.getMessage shouldBe "memory pool topology changed before peak reset"

    val fatal = new OutOfMemoryError("fatal reset")
    val fatalHandle = new Handle(
      MemoryPoolIdentity("heap", "HEAP"),
      reset = () => throw fatal)
    val fatalSource = sourceOf(fatalHandle)
    val fatalTopology = Eip0045VerifierBenchmark.openMemoryPoolTopology(fatalSource)
    intercept[OutOfMemoryError] {
      Eip0045VerifierBenchmark.captureMemoryPoolPhaseEnvelope(fatalTopology, fatalSource)(())
    } shouldBe fatal
  }

  test("a post-sampling memory pool topology drift creates no producer output") {
    final class Handle(override val identity: MemoryPoolIdentity)
        extends Eip0045VerifierBenchmark.MemoryPoolHandle {
      override def isValid: Boolean = true
      override def resetPeakUsage(): Unit = ()
      override def usage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
      override def peakUsage(): MemoryUsageEvidence = MemoryUsageEvidence(10L, 20L, -1L)
    }
    val stable = new Handle(MemoryPoolIdentity("test-heap", "HEAP"))
    val changed = new Handle(MemoryPoolIdentity("changed-heap", "HEAP"))
    var topologyReads = 0
    val source = new Eip0045VerifierBenchmark.MemoryPoolSource {
      override def handles(): Vector[Eip0045VerifierBenchmark.MemoryPoolHandle] = {
        topologyReads += 1
        if (topologyReads < 3) Vector(stable) else Vector(changed)
      }
    }
    var verifierSetups = 0
    var outputAttempts = 0
    val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
      override def beforeVerifierSetup(): Unit = verifierSetups += 1
      override def beforeOutput(): Unit = outputAttempts += 1
    }
    val directory = Files.createTempDirectory("eip0045-memory-pool-drift-")
    val output = directory.resolve("evidence.json")
    try {
      intercept[IllegalStateException] {
        Eip0045VerifierBenchmark.runWithMemoryPoolSourceForTest(Array(
          "--warmup-rounds", "0",
          "--sample-rounds", "1",
          "--implementation-revision", "commit:" + ("1" * 40),
          "--cpu-model", "Test CPU",
          "--output", output.toString), observer, source)
      }.getMessage shouldBe "memory pool topology changed during sampling"
      verifierSetups shouldBe 1
      outputAttempts shouldBe 0
      Files.exists(output) shouldBe false
    } finally {
      Files.deleteIfExists(output)
      Files.deleteIfExists(directory)
    }
  }

  private def failedCampaignMessage(
      directory: java.nio.file.Path,
      manifest: java.nio.file.Path,
      outputName: String): String = {
    val output = directory.resolve(outputName)
    val error = intercept[IllegalArgumentException] {
      Eip0045VerifierBenchmark.main(Array(
        "--implementation-revision", "commit:00",
        "--campaign-manifest", manifest.toString,
        "--campaign-run-id", "host-a:run-01",
        "--output", output.toString))
    }
    Files.exists(output) shouldBe false
    error.getMessage
  }

  private def digestFrom(envelope: String): String = {
    val marker = "\"evidenceDigest\":\""
    val start = envelope.indexOf(marker) + marker.length
    envelope.substring(start, start + 64)
  }

  private def samplePayload(samples: Vector[Long]): EvidencePayload = {
    val summary = statistics(samples) match {
      case Right(value) => value
      case Left(detail) => fail(detail)
    }
    EvidencePayload(
      startedAtUtc = "2026-07-19T00:00:00Z",
      benchmarkDurationNs = 123,
      profileId = "00",
      implementationRevision = "commit:00",
      verifierEntryPoint = "example.verify",
      resources = Vector(ResourceMetadata("proof", "classpath:/proof.bin", 2, "ab")),
      warmupRounds = 1,
      sampleRounds = samples.length,
      environment = EnvironmentMetadata(
        "runtime",
        "1",
        "vm",
        "vendor",
        "2",
        "mixed mode",
        "2.13.16",
        "os",
        "3",
        "arch",
        4,
        1024,
        "jit",
        Vector("gc"),
        Vector(MemoryPoolIdentity("heap", "HEAP")),
        "com.sun.management.ThreadMXBean.getThreadAllocatedBytes(currentThread)",
        2,
        "d" * 64,
        "cpu",
        "argument"),
      scenarios = Vector(ScenarioEvidence(
        "valid-proof",
        "verified:1:15",
        50,
        "verification-complete",
        "query",
        samples,
        summary,
        Vector(100L, 200L),
        AllocationStatistics(2, 100, 200, 200, 200))),
      garbageCollectorDeltas = Vector(GarbageCollectorDelta("gc", 1, 2)),
      memoryPoolPhaseEnvelopes = Vector(MemoryPoolPhaseEnvelope(
        MemoryPoolIdentity("heap", "HEAP"),
        MemoryUsageEvidence(10L, 20L, -1L),
        MemoryUsageEvidence(15L, 30L, 40L),
        MemoryUsageEvidence(20L, 25L, 50L))),
      campaignBinding = Some(CampaignBinding(
        "host-a:run-01",
        9,
        "7021e610a5f62eefd01830fea68e5fa180e8cf017c08ea0890c326b2854ebc96")),
      limitations = Vector("single host"))
  }
}
