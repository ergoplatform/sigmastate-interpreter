/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemException, Files, Path}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import sigma.stark.profile.benchmark.Eip0045BenchmarkSupport._
import sigma.stark.profile.benchmark.Eip0045CampaignContract._
import sigma.stark.profile.benchmark.Eip0045CampaignValidator._

class Eip0045CampaignValidatorSpec extends AnyFunSuite with Matchers {
  test("V8 freezes schema versions, limitations, and the independent scenario append") {
    Schema shouldBe "eip-0045-jvm-verifier-benchmark-v5"
    DigestDomain shouldBe "Ergo.EIP0045.B5.Evidence.v5"
    Canonicalization shouldBe "utf8-fixed-field-order-no-whitespace-v5"
    ManifestSchema shouldBe "eip-0045-b5-campaign-manifest-v3"
    ManifestCanonicalization shouldBe
      "utf8-fixed-field-order-no-internal-whitespace-single-terminal-lf-v3"
    ArchiveIndexSchema shouldBe "eip-0045-b5-campaign-archive-index-v1"
    ExpectedEvidenceContract.memoryPoolScope shouldBe
      "sequential per-pool MemoryPoolMXBean.resetPeakUsage/getPeakUsage/getUsage boundary calls around the complete sampling phase; runner and JVM-management overhead included"
    val memoryPoolLimitations = Vector(
      "Memory-pool phase envelopes are JVM MXBean observations; they do not measure native memory, prove object liveness, attribute a peak to one scenario, or bound a node process.",
      "Campaign memory-pool evidence requires a dedicated JVM with no other benchmark, Java agent, or JMX client resetting pool peaks.",
      "Pre-reset and post-sampling topology checks do not detect transient topology changes or a non-inverting external peak reset.")
    ExpectedCampaignLimitations.slice(6, 9) shouldBe memoryPoolLimitations
    ExpectedCampaignLimitations.takeRight(4) shouldBe Vector(
      "Resource identities cover the complete run; the scenario-to-fixture mapping is fixed by the exact benchmark source and is not encoded separately in the manifest.",
      "The po2-16 case replays checked-in bytes from an independent source; it does not reproduce or attest their upstream generation.",
      "Validation invokes every scenario before warmup, so zero warmup rounds are not a cold-start measurement.",
      "Campaign mode parses the exact manifest bytes and matches the selected run, implementation revision, rounds, environment policy, and ordered JVM-input-argument identity before verifier setup.")

    val v6Scenarios = Vector(
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
        "query"))

    ExpectedScenarios.take(v6Scenarios.length) shouldBe v6Scenarios
    ExpectedScenarios.drop(v6Scenarios.length) shouldBe Vector(
      ScenarioPolicy(
        "valid-independent-po2-16",
        "verified:1:16",
        50,
        "verification-complete",
        "query"))

    val fixture = campaignFixture()
    val index = ExpectedScenarios.length - 1
    val scenario = ExpectedScenarios(index)
    val mutations = Vector(
      "id" -> ExpectedScenarios.updated(
        index,
        scenario.copy(id = "valid-independent-po2-15")),
      "outcome" -> ExpectedScenarios.updated(
        index,
        scenario.copy(expectedOutcome = "verified:1:15")),
      "query-count" -> ExpectedScenarios.updated(
        index,
        scenario.copy(validationQueryCheckpoints = 49)),
      "boundary" -> ExpectedScenarios.updated(
        index,
        scenario.copy(validationBoundary = "fri")),
      "checkpoint" -> ExpectedScenarios.updated(
        index,
        scenario.copy(lastVerifierCheckpoint = "group_root_code")),
      "removal" -> ExpectedScenarios.dropRight(1),
      "order" -> (scenario +: ExpectedScenarios.dropRight(1)))
    mutations.foreach { case (label, changed) =>
      withClue(label + ": ") {
        renderManifest(fixture.manifest.copy(scenarios = changed)) shouldBe
          Left("campaign scenarios do not match the frozen benchmark scenarios")
      }
    }

    val first = fixture.payloads.head
    val firstNewLimitation = ExpectedCampaignLimitations.length - 4
    val changedLimitations = (0 until 3).map { offset =>
      val index = firstNewLimitation + offset
      "text-" + offset -> ExpectedCampaignLimitations.updated(
        index,
        ExpectedCampaignLimitations(index) + " changed")
    }.toVector ++ Vector(
      "removal" -> ExpectedCampaignLimitations.patch(firstNewLimitation, Nil, 1),
      "order" -> ExpectedCampaignLimitations.updated(
        firstNewLimitation,
        ExpectedCampaignLimitations(firstNewLimitation + 1)).updated(
        firstNewLimitation + 1,
        ExpectedCampaignLimitations(firstNewLimitation)))
    changedLimitations.foreach { case (label, changed) =>
      withClue("limitation " + label + ": ") {
        val payload = first.copy(limitations = changed)
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence limitations do not match campaign-mode V5")
      }
    }
    val changedMemoryPoolLimitations = memoryPoolLimitations.indices.map { offset =>
      val index = 6 + offset
      "text-" + offset -> ExpectedCampaignLimitations.updated(
        index,
        ExpectedCampaignLimitations(index) + " changed")
    }.toVector ++ Vector(
      "removal" -> ExpectedCampaignLimitations.patch(6, Nil, 1),
      "order" -> ExpectedCampaignLimitations.updated(
        6,
        ExpectedCampaignLimitations(7)).updated(
        7,
        ExpectedCampaignLimitations(6)))
    changedMemoryPoolLimitations.foreach { case (label, changed) =>
      withClue("memory-pool limitation " + label + ": ") {
        val payload = first.copy(limitations = changed)
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence limitations do not match campaign-mode V5")
      }
    }
  }

  test("V8 preserves the V7 resource and scenario identities") {
    val v6Resources = Vector(
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
        "7df5d428210065e9480b44d273db298b85c692e3eafbbe4db76fb00ba3bd84a3"))
    val po2_16Resources = Vector(
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

    ExpectedResources.take(v6Resources.length) shouldBe v6Resources
    ExpectedResources.drop(v6Resources.length) shouldBe po2_16Resources

    val fixture = campaignFixture()
    renderManifest(fixture.manifest.copy(resources = v6Resources)) shouldBe
      Left("campaign resources do not match the frozen benchmark resources")
    po2_16Resources.indices.foreach { offset =>
      val index = v6Resources.length + offset
      val resource = ExpectedResources(index)
      val changedDigest = ExpectedResources.updated(
        index,
        resource.copy(sha256 = (if (resource.sha256.head == '0') "1" else "0") +
          resource.sha256.substring(1)))
      val changedId = ExpectedResources.updated(
        index,
        resource.copy(id = resource.id + "-changed"))
      val changedSource = ExpectedResources.updated(
        index,
        resource.copy(classpath = resource.classpath + ".changed"))
      val changedLength = ExpectedResources.updated(
        index,
        resource.copy(byteLength = resource.byteLength + 1))
      val removed = ExpectedResources.patch(index, Nil, 1)
      Vector(
        "digest" -> changedDigest,
        "id" -> changedId,
        "source" -> changedSource,
        "length" -> changedLength,
        "removal" -> removed).foreach { case (label, changed) =>
        withClue(resource.id + " " + label + ": ") {
          renderManifest(fixture.manifest.copy(resources = changed)) shouldBe
            Left("campaign resources do not match the frozen benchmark resources")
          val payload = fixture.payloads.head.copy(resources = changed)
          validateArchiveBytes(
            fixture.manifestBytes,
            fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
            Left("evidence resources do not match the campaign")
        }
      }
    }
    val reordered = ExpectedResources.updated(7, ExpectedResources(8)).updated(
      8,
      ExpectedResources(7))
    renderManifest(fixture.manifest.copy(resources = reordered)) shouldBe
      Left("campaign resources do not match the frozen benchmark resources")
    val reorderedPayload = fixture.payloads.head.copy(resources = reordered)
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(renderEnvelope(reorderedPayload)))) shouldBe
      Left("evidence resources do not match the campaign")
  }

  test("independent image, journal, and retained claim jointly derive the po2-16 claim") {
    val root = "/stark-kats/eip0045-arkadia-independent/"
    val imageId = resourceBytes(root + "image-id.bin")
    val journal = resourceBytes(root + "journal.bin")
    val retainedClaim = resourceBytes(root + "claim-digest.bin")

    val derived = rightValue(Eip0045VerifierBenchmark.deriveIndependentClaim(
      imageId,
      journal,
      retainedClaim))
    derived shouldBe retainedClaim
    (derived eq retainedClaim) shouldBe false
    Eip0045VerifierBenchmark.deriveIndependentClaim(
      imageId.drop(1),
      journal,
      retainedClaim) shouldBe Left("independent claim inputs are invalid")

    val changedImageId = imageId.clone()
    changedImageId(0) = (changedImageId(0) ^ 1).toByte
    Eip0045VerifierBenchmark.deriveIndependentClaim(
      changedImageId,
      journal,
      retainedClaim) shouldBe
      Left("independent claim does not match the retained claim resource")

    val changedJournal = journal.clone()
    changedJournal(0) = (changedJournal(0) ^ 1).toByte
    Eip0045VerifierBenchmark.deriveIndependentClaim(
      imageId,
      changedJournal,
      retainedClaim) shouldBe
      Left("independent claim does not match the retained claim resource")

    val changedClaim = retainedClaim.clone()
    changedClaim(0) = (changedClaim(0) ^ 1).toByte
    Eip0045VerifierBenchmark.deriveIndependentClaim(
      imageId,
      journal,
      changedClaim) shouldBe
      Left("independent claim does not match the retained claim resource")
  }

  test("canonical two-cell four-run campaign validates and produces a path-free index") {
    val fixture = campaignFixture()
    val shuffled = Vector(
      fixture.evidence(2),
      fixture.evidence(0),
      fixture.evidence(3),
      fixture.evidence(1))
    val indexJson = rightValue(validateArchiveBytes(fixture.manifestBytes, shuffled))
    indexJson should startWith("{\"schema\":\"" + ArchiveIndexSchema + "\"")
    ArchiveIndexCanonicalization shouldBe
      "utf8-fixed-field-order-no-internal-whitespace-single-terminal-lf-v1"
    indexJson.count(_ == '\n') shouldBe 1
    indexJson should endWith("\n")
    indexJson should not include "evidence-"
    indexJson should not include "\\"
    val parsed = rightValue(parseArchiveIndex(indexJson.getBytes(StandardCharsets.UTF_8)))
    parsed.campaignId shouldBe fixture.manifest.campaignId
    parsed.entries.map(_.runId) shouldBe fixture.manifest.runs.map(_.id)
    parsed.entries.foreach { entry =>
      entry.evidenceByteLength should be > 0
      entry.evidenceSha256.length shouldBe 64
      entry.evidenceDigest.length shouldBe 64
    }
  }

  test("manifest parsing requires exact canonical bytes and the frozen V5 contract") {
    val fixture = campaignFixture()
    ManifestCanonicalization shouldBe
      "utf8-fixed-field-order-no-internal-whitespace-single-terminal-lf-v3"
    new String(fixture.manifestBytes, StandardCharsets.UTF_8).count(_ == '\n') shouldBe 1
    new String(fixture.manifestBytes, StandardCharsets.UTF_8) should endWith("\n")
    parseManifest(fixture.manifestBytes) shouldBe Right(fixture.manifest)
    val rendered = rightValue(renderManifest(fixture.manifest))
    parseManifest(rendered.getBytes(StandardCharsets.UTF_8)) shouldBe Right(fixture.manifest)
    val nonCanonical = new String(fixture.manifestBytes, StandardCharsets.UTF_8)
      .replace("\"campaignId\":", " \"campaignId\":")
      .getBytes(StandardCharsets.UTF_8)
    parseManifest(nonCanonical) shouldBe Left("campaign manifest is not canonical JSON")
    parseManifest(new String(fixture.manifestBytes, StandardCharsets.UTF_8)
      .replace(ManifestSchema, "eip-0045-b5-campaign-manifest-v1")
      .getBytes(StandardCharsets.UTF_8)) shouldBe
      Left("campaign manifest schema is invalid")

    renderManifest(fixture.manifest.copy(profileId = "0" * 64)) shouldBe
      Left("campaign profile ID does not match the EIP-0045 candidate")
    renderManifest(fixture.manifest.copy(
      evidenceContract = ExpectedEvidenceContract.copy(clock = "wall-clock"))) shouldBe
      Left("campaign V5 evidence contract is invalid")
    renderManifest(fixture.manifest.copy(
      evidenceContract = ExpectedEvidenceContract.copy(memoryPoolScope = "other"))) shouldBe
      Left("campaign V5 evidence contract is invalid")
    renderManifest(fixture.manifest.copy(implementationRevision = "commit:short")) shouldBe
      Left("campaign implementation revision is not an exact supported identity")
    renderManifest(fixture.manifest.copy(resources = ExpectedResources.reverse)) shouldBe
      Left("campaign resources do not match the frozen benchmark resources")
    renderManifest(fixture.manifest.copy(scenarios = ExpectedScenarios.reverse)) shouldBe
      Left("campaign scenarios do not match the frozen benchmark scenarios")
  }

  test("manifest rendering rejects malformed UTF-16 and valid content above the byte cap") {
    val fixture = campaignFixture()
    val malformed = fixture.manifest.copy(environmentPolicies =
      fixture.manifest.environmentPolicies.updated(
        0,
        fixture.manifest.environmentPolicies.head.copy(cpuModel = "\ud800")))
    renderManifest(malformed) shouldBe Left("campaign manifest is not strict UTF-8")

    val padding = "x" * MaxPublicStringCharacters
    val environments = Vector.tabulate(MaxEnvironmentPolicies) { index =>
      val suffix = f"$index%02d"
      fixture.manifest.environmentPolicies.head.copy(
        id = "env-" + suffix,
        javaRuntimeName = padding,
        javaRuntimeVersion = padding,
        javaVmName = padding,
        javaVmVendor = padding,
        javaVmVersion = padding,
        javaVmInfo = padding,
        scalaVersion = padding,
        osName = padding,
        osVersion = suffix + ("x" * (MaxPublicStringCharacters - suffix.length)),
        osArch = padding,
        jitCompiler = padding,
        cpuModel = padding)
    }
    val arguments = Vector.tabulate(MaxJvmArgumentPolicies) { index =>
      val suffix = f"$index%02x"
      JvmArgumentPolicy("args-" + suffix, index, (suffix * 32).take(64))
    }
    val cells = Vector.tabulate(MaxCells) { index =>
      val suffix = f"$index%02d"
      CampaignCell("cell-" + suffix, "env-" + suffix, "args-" + f"$index%02x", 1)
    }
    val runs = cells.map(cell => CampaignRun(cell.id + ":r1", cell.id, 1))
    val oversized = fixture.manifest.copy(
      environmentPolicies = environments,
      jvmArgumentPolicies = arguments,
      cells = cells,
      runs = runs)
    validateManifest(oversized) shouldBe Right(())
    renderManifest(oversized) shouldBe
      Left("campaign manifest exceeds " + MaxCampaignManifestBytes + " bytes")
  }

  test("manifest cells, run IDs, and replicate slots are independently closed") {
    val fixture = campaignFixture()
    val duplicateCell = fixture.manifest.copy(
      cells = fixture.manifest.cells.updated(
        1,
        fixture.manifest.cells(1).copy(id = fixture.manifest.cells.head.id)))
    renderManifest(duplicateCell) shouldBe
      Left("campaign cell IDs are not sorted and unique")

    val duplicateRun = fixture.manifest.copy(
      runs = fixture.manifest.runs.updated(
        1,
        fixture.manifest.runs(1).copy(id = fixture.manifest.runs.head.id)))
    renderManifest(duplicateRun) shouldBe
      Left("campaign run IDs are not sorted and unique")

    val duplicateSlot = fixture.manifest.copy(
      runs = fixture.manifest.runs.updated(
        1,
        fixture.manifest.runs(1).copy(replicate = 1)))
    renderManifest(duplicateSlot) shouldBe
      Left("campaign run replicate slots are not unique")

    val unknownCell = fixture.manifest.copy(
      runs = fixture.manifest.runs.updated(
        0,
        fixture.manifest.runs.head.copy(cellId = "cell-z")))
    renderManifest(unknownCell) shouldBe
      Left("campaign run references an unknown cell")
  }

  test("manifest policy identities and cell joins are unambiguous") {
    val fixture = campaignFixture()
    val firstEnvironment = fixture.manifest.environmentPolicies.head
    val duplicateEnvironmentValue = fixture.manifest.copy(
      environmentPolicies = fixture.manifest.environmentPolicies.updated(
        1,
        firstEnvironment.copy(id = fixture.manifest.environmentPolicies(1).id)))
    renderManifest(duplicateEnvironmentValue) shouldBe
      Left("campaign environment policies contain duplicate values")

    val invalidMemoryPoolType = fixture.manifest.copy(
      environmentPolicies = fixture.manifest.environmentPolicies.updated(
        0,
        firstEnvironment.copy(memoryPoolIdentities =
          firstEnvironment.memoryPoolIdentities.updated(
            0,
            firstEnvironment.memoryPoolIdentities.head.copy(memoryType = "OTHER")))))
    renderManifest(invalidMemoryPoolType) shouldBe
      Left("campaign memory pool identity policy is invalid")

    val unsortedMemoryPools = fixture.manifest.copy(
      environmentPolicies = fixture.manifest.environmentPolicies.updated(
        0,
        firstEnvironment.copy(memoryPoolIdentities =
          firstEnvironment.memoryPoolIdentities.reverse)))
    renderManifest(unsortedMemoryPools) shouldBe
      Left("campaign memory pool identity policy is invalid")

    val firstArguments = fixture.manifest.jvmArgumentPolicies.head
    val duplicateJvmIdentity = fixture.manifest.copy(
      jvmArgumentPolicies = fixture.manifest.jvmArgumentPolicies.updated(
        1,
        firstArguments.copy(id = fixture.manifest.jvmArgumentPolicies(1).id)))
    renderManifest(duplicateJvmIdentity) shouldBe
      Left("campaign JVM argument policies contain duplicate values")

    val firstCell = fixture.manifest.cells.head
    val duplicateCellJoin = fixture.manifest.copy(
      cells = fixture.manifest.cells.updated(
        1,
        firstCell.copy(
          id = fixture.manifest.cells(1).id,
          replicateCount = fixture.manifest.cells(1).replicateCount)))
    renderManifest(duplicateCellJoin) shouldBe
      Left("campaign cells contain duplicate policy pairs")
  }

  test("exactly one evidence file must cover every declared run") {
    val fixture = campaignFixture()
    validateArchiveBytes(fixture.manifestBytes, fixture.evidence.dropRight(1)) shouldBe
      Left("evidence file count does not match declared runs")
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(1, fixture.evidence.head)) shouldBe
      Left("more than one evidence file claims the same run ID")
  }

  test("every evidence file binds the exact manifest length, digest, and declared run") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val badDigest = first.copy(campaignBinding = Some(
      first.campaignBinding.get.copy(manifestSha256 = "f" * 64)))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(renderEnvelope(badDigest)))) shouldBe
      Left("evidence manifest binding does not match the exact campaign bytes")

    val badLength = first.copy(campaignBinding = Some(
      first.campaignBinding.get.copy(manifestByteLength = fixture.manifestBytes.length + 1)))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(renderEnvelope(badLength)))) shouldBe
      Left("evidence manifest binding does not match the exact campaign bytes")

    val badRun = first.copy(campaignBinding = Some(
      first.campaignBinding.get.copy(runId = "cell-a:r9")))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(renderEnvelope(badRun)))) shouldBe
      Left("evidence run ID is not declared by the campaign")
  }

  test("every campaign and runtime join is checked independently") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val resizedScenarios = first.scenarios.map { scenario =>
      val timings = scenario.samplesNs :+ (scenario.samplesNs.last + 1L)
      val allocations = scenario.allocatedBytes :+ (scenario.allocatedBytes.last + 1L)
      scenario.copy(
        samplesNs = timings,
        statistics = rightValue(statistics(timings)),
        allocatedBytes = allocations,
        allocationStatistics = rightValue(allocationStatistics(allocations)))
    }
    val cases = Vector(
      first.copy(profileId = "f" * 64) ->
        "evidence profile ID does not match the campaign",
      first.copy(implementationRevision = "commit:0000000000000000000000000000000000000000") ->
        "evidence implementation revision does not match the campaign",
      first.copy(verifierEntryPoint = "different.verifier.EntryPoint") ->
        "evidence verifier entry point does not match the campaign",
      first.copy(resources = first.resources.updated(
        0,
        first.resources.head.copy(byteLength = first.resources.head.byteLength + 1))) ->
        "evidence resources do not match the campaign",
      first.copy(warmupRounds = first.warmupRounds + 1) ->
        "evidence warmup rounds do not match the campaign",
      first.copy(
        sampleRounds = first.sampleRounds + 1,
        scenarios = resizedScenarios) ->
        "evidence sample rounds do not match the campaign",
      first.copy(scenarios = first.scenarios.updated(
        0,
        first.scenarios.head.copy(expectedOutcome = "raw-seal-malformed-proof"))) ->
        "evidence scenarios do not match the campaign",
      first.copy(limitations = first.limitations :+ "join-negative") ->
        "evidence limitations do not match campaign-mode V5",
      first.copy(startedAtUtc = "2026-08-17T00:00:00.000Z") ->
        "evidence start time is not canonical UTC")
    cases.foreach { case (payload, expected) =>
      validateArchiveBytes(
        fixture.manifestBytes,
        fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe Left(expected)
    }
  }

  test("scenario outcome, query count, boundary, checkpoint, and order are isolated") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val scenario = first.scenarios.head
    val mutations = Vector(
      "outcome" -> first.copy(scenarios = first.scenarios.updated(
        0,
        scenario.copy(expectedOutcome = "raw-seal-malformed-proof"))),
      "query-count" -> first.copy(scenarios = first.scenarios.updated(
        0,
        scenario.copy(validationQueryCheckpoints = scenario.validationQueryCheckpoints + 1))),
      "boundary" -> first.copy(scenarios = first.scenarios.updated(
        0,
        scenario.copy(validationBoundary = "fri"))),
      "checkpoint" -> first.copy(scenarios = first.scenarios.updated(
        0,
        scenario.copy(lastVerifierCheckpoint = "group_root_code"))),
      "order" -> first.copy(scenarios = first.scenarios.reverse))
    mutations.foreach { case (label, payload) =>
      withClue(label + ": ") {
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence scenarios do not match the campaign")
      }
    }
  }

  test("the independent po2-16 evidence row is matched field by field and in order") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val scenarioIndex = first.scenarios.length - 1
    val scenario = first.scenarios(scenarioIndex)
    scenario shouldBe ScenarioEvidence(
      "valid-independent-po2-16",
      "verified:1:16",
      50,
      "verification-complete",
      "query",
      scenario.samplesNs,
      scenario.statistics,
      scenario.allocatedBytes,
      scenario.allocationStatistics)

    val mutations = Vector(
      "id" -> first.copy(scenarios = first.scenarios.updated(
        scenarioIndex,
        scenario.copy(id = "valid-independent-po2-15"))),
      "outcome" -> first.copy(scenarios = first.scenarios.updated(
        scenarioIndex,
        scenario.copy(expectedOutcome = "verified:1:15"))),
      "query-count" -> first.copy(scenarios = first.scenarios.updated(
        scenarioIndex,
        scenario.copy(validationQueryCheckpoints = 49))),
      "boundary" -> first.copy(scenarios = first.scenarios.updated(
        scenarioIndex,
        scenario.copy(validationBoundary = "fri"))),
      "checkpoint" -> first.copy(scenarios = first.scenarios.updated(
        scenarioIndex,
        scenario.copy(lastVerifierCheckpoint = "group_root_code"))),
      "removal" -> first.copy(scenarios = first.scenarios.dropRight(1)),
      "order" -> first.copy(scenarios = scenario +: first.scenarios.dropRight(1)))
    mutations.foreach { case (label, payload) =>
      withClue(label + ": ") {
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence scenarios do not match the campaign")
      }
    }
  }

  test("V4 evidence is rejected by the exact V5 schema gate") {
    val fixture = campaignFixture()
    val oldSchema = new String(fixture.evidence.head, StandardCharsets.UTF_8)
      .replace(Schema, "eip-0045-jvm-verifier-benchmark-v4")
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(oldSchema))) shouldBe
      Left("evidence schema is invalid")
  }

  test("environment and ordered JVM argument identities are precommitted per cell") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val environment = first.environment
    val changedCollectorNames = environment.garbageCollectors.updated(
      0,
      environment.garbageCollectors.head + " changed")
    val changedCollectorDeltas = first.garbageCollectorDeltas.updated(
      0,
      first.garbageCollectorDeltas.head.copy(name = changedCollectorNames.head))
    val changedMemoryPoolIdentities = environment.memoryPoolIdentities.updated(
      0,
      environment.memoryPoolIdentities.head.copy(name = "Different memory pool"))
    val changedMemoryPoolEnvelopes = first.memoryPoolPhaseEnvelopes.updated(
      0,
      first.memoryPoolPhaseEnvelopes.head.copy(identity = changedMemoryPoolIdentities.head))
    val changedMemoryPoolTypes = environment.memoryPoolIdentities.updated(
      0,
      environment.memoryPoolIdentities.head.copy(memoryType =
        (if (environment.memoryPoolIdentities.head.memoryType == "HEAP")
           "NON_HEAP"
         else "HEAP")))
    val changedMemoryPoolTypeEnvelopes = first.memoryPoolPhaseEnvelopes.updated(
      0,
      first.memoryPoolPhaseEnvelopes.head.copy(identity = changedMemoryPoolTypes.head))
    val environmentCases = Vector(
      "javaRuntimeName" -> first.copy(environment = environment.copy(
        javaRuntimeName = "Different Java runtime")),
      "javaRuntimeVersion" -> first.copy(environment = environment.copy(
        javaRuntimeVersion = "different-runtime-version")),
      "javaVmName" -> first.copy(environment = environment.copy(
        javaVmName = "Different VM")),
      "javaVmVendor" -> first.copy(environment = environment.copy(
        javaVmVendor = "Different VM vendor")),
      "javaVmVersion" -> first.copy(environment = environment.copy(
        javaVmVersion = "different-vm-version")),
      "javaVmInfo" -> first.copy(environment = environment.copy(
        javaVmInfo = "different-vm-info")),
      "scalaVersion" -> first.copy(environment = environment.copy(
        scalaVersion = "different-scala-version")),
      "osName" -> first.copy(environment = environment.copy(
        osName = "Different OS")),
      "osVersion" -> first.copy(environment = environment.copy(
        osVersion = "different-os-version")),
      "osArch" -> first.copy(environment = environment.copy(
        osArch = "different-os-arch")),
      "availableProcessors" -> first.copy(environment = environment.copy(
        availableProcessors = environment.availableProcessors + 1)),
      "maxHeapBytes" -> first.copy(environment = environment.copy(
        maxHeapBytes = environment.maxHeapBytes + 1L)),
      "jitCompiler" -> first.copy(environment = environment.copy(
        jitCompiler = "Different JIT")),
      "garbageCollectors" -> first.copy(
        environment = environment.copy(garbageCollectors = changedCollectorNames),
        garbageCollectorDeltas = changedCollectorDeltas),
      "memoryPoolIdentities" -> first.copy(
        environment = environment.copy(memoryPoolIdentities = changedMemoryPoolIdentities),
        memoryPoolPhaseEnvelopes = changedMemoryPoolEnvelopes),
      "memoryPoolIdentityTypes" -> first.copy(
        environment = environment.copy(memoryPoolIdentities = changedMemoryPoolTypes),
        memoryPoolPhaseEnvelopes = changedMemoryPoolTypeEnvelopes),
      "threadAllocationMeter" -> first.copy(environment = environment.copy(
        threadAllocationMeter = "different-thread-allocation-meter")),
      "cpuModel" -> first.copy(environment = environment.copy(
        cpuModel = "Different public CPU")),
      "cpuModelSource" -> first.copy(environment = environment.copy(
        cpuModelSource = "different-cpu-model-source")))
    environmentCases.length shouldBe 19
    environmentCases.foreach { case (field, payload) =>
      withClue(field + ": ") {
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence environment does not match the declared cell policy")
        resolveRunPolicy(
          fixture.manifestBytes,
          payload.campaignBinding.get.runId,
          payload.implementationRevision,
          payload.warmupRounds,
          payload.sampleRounds,
          payload.environment) shouldBe
          Left("evidence environment does not match the declared cell policy")
      }
    }

    Vector(
      "jvmInputArgumentCount" -> first.copy(environment = environment.copy(
        jvmInputArgumentCount = environment.jvmInputArgumentCount + 1)),
      "jvmInputArgumentsSha256" -> first.copy(environment = environment.copy(
        jvmInputArgumentsSha256 = "e" * 64))).foreach { case (field, payload) =>
      withClue(field + ": ") {
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(renderEnvelope(payload)))) shouldBe
          Left("evidence JVM argument identity does not match the declared cell policy")
        resolveRunPolicy(
          fixture.manifestBytes,
          payload.campaignBinding.get.runId,
          payload.implementationRevision,
          payload.warmupRounds,
          payload.sampleRounds,
          payload.environment) shouldBe
          Left("evidence JVM argument identity does not match the declared cell policy")
      }
    }
  }

  test("memory pool identities and every phase field are validated without cross-snapshot capacity rules") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val envelope = first.memoryPoolPhaseEnvelopes.head
    val flexibleCapacities = first.copy(memoryPoolPhaseEnvelopes =
      first.memoryPoolPhaseEnvelopes.updated(
        0,
        envelope.copy(
          afterResetPeakUsage = MemoryUsageEvidence(100L, 150L, 200L),
          endUsage = MemoryUsageEvidence(120L, 120L, -1L),
          finalPeakUsage = MemoryUsageEvidence(130L, 500L, -1L))))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(renderEnvelope(flexibleCapacities)))).isRight shouldBe true

    val invalid = Vector(
      "after-reset-used" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(afterResetPeakUsage =
            envelope.afterResetPeakUsage.copy(usedBytes = -1L)))),
      "after-reset-committed" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(afterResetPeakUsage =
            envelope.afterResetPeakUsage.copy(committedBytes = -1L)))),
      "after-reset-max" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(afterResetPeakUsage =
            envelope.afterResetPeakUsage.copy(maxBytes = -2L)))),
      "end-committed" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(endUsage = envelope.endUsage.copy(
            committedBytes = envelope.endUsage.usedBytes - 1L)))),
      "final-max" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(finalPeakUsage = envelope.finalPeakUsage.copy(
            maxBytes = envelope.finalPeakUsage.committedBytes - 1L)))),
      "peak-coverage" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(finalPeakUsage = envelope.finalPeakUsage.copy(
            usedBytes = envelope.endUsage.usedBytes - 1L)))),
      "after-reset-peak-coverage" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(
            endUsage = envelope.endUsage.copy(
              usedBytes = envelope.afterResetPeakUsage.usedBytes - 20L),
            finalPeakUsage = envelope.finalPeakUsage.copy(
              usedBytes = envelope.afterResetPeakUsage.usedBytes - 1L)))),
      "identity" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.updated(
          0,
          envelope.copy(identity = envelope.identity.copy(name = "other-pool")))),
      "missing-envelope" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.dropRight(1)),
      "extra-envelope" -> first.copy(memoryPoolPhaseEnvelopes =
        (first.memoryPoolPhaseEnvelopes :+ envelope.copy(
          identity = MemoryPoolIdentity("zz-extra-pool", "HEAP")))),
      "reordered-envelopes" -> first.copy(memoryPoolPhaseEnvelopes =
        first.memoryPoolPhaseEnvelopes.reverse))
    invalid.foreach { case (label, payload) =>
      withClue(label + ": ") {
        validateArchiveBytes(
          fixture.manifestBytes,
          fixture.evidence.updated(0, bytes(uncheckedEnvelope(payload)))) shouldBe
          Left("evidence payload semantics are invalid")
      }
    }

    val changedScope = new String(fixture.evidence.head, StandardCharsets.UTF_8)
      .replace(ExpectedEvidenceContract.memoryPoolScope, "different memory pool scope")
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(changedScope))) shouldBe
      Left("evidence memory pool scope is invalid")
  }

  test("shared run-policy resolver selects every run from the exact manifest bytes") {
    val fixture = campaignFixture()
    val exact = rightValue(parseExactCampaignManifest(fixture.manifestBytes))
    exact.manifest shouldBe fixture.manifest
    exact.byteLength shouldBe fixture.manifestBytes.length
    exact.sha256 shouldBe sha256Hex(fixture.manifestBytes)
    val mutableSource = fixture.manifestBytes.clone()
    val frozen = rightValue(parseExactCampaignManifest(mutableSource))
    mutableSource(0) = (mutableSource(0) ^ 1).toByte
    frozen.manifest shouldBe fixture.manifest
    frozen.byteLength shouldBe fixture.manifestBytes.length
    frozen.sha256 shouldBe sha256Hex(fixture.manifestBytes)

    fixture.manifest.runs.zip(fixture.payloads).foreach { case (run, payload) =>
      val binding = payload.campaignBinding.get
      val resolved = rightValue(resolveRunPolicy(
        exact,
        binding,
        payload.implementationRevision,
        payload.warmupRounds,
        payload.sampleRounds,
        payload.environment))
      val expectedCell = fixture.manifest.cells.find(_.id == run.cellId).get
      resolved.run shouldBe run
      resolved.cell shouldBe expectedCell
      resolved.environmentPolicy.id shouldBe expectedCell.environmentPolicyId
      resolved.jvmArgumentPolicy.id shouldBe expectedCell.jvmArgumentPolicyId
      resolved.campaignBinding shouldBe binding

      rightValue(resolveRunPolicy(
        fixture.manifestBytes,
        run.id,
        payload.implementationRevision,
        payload.warmupRounds,
        payload.sampleRounds,
        payload.environment)) shouldBe resolved
    }
  }

  test("run-policy resolver isolates identity, configuration, and cross-cell failures") {
    val fixture = campaignFixture()
    val exact = rightValue(parseExactCampaignManifest(fixture.manifestBytes))
    val first = fixture.payloads.head
    val binding = first.campaignBinding.get

    def resolve(
        selectedBinding: CampaignBinding = binding,
        revision: String = first.implementationRevision,
        warmup: Int = first.warmupRounds,
        samples: Int = first.sampleRounds,
        environment: EnvironmentMetadata = first.environment): Either[String, ResolvedRunPolicy] =
      resolveRunPolicy(
        exact,
        selectedBinding,
        revision,
        warmup,
        samples,
        environment)

    resolve(binding.copy(runId = "cell-z:r1")) shouldBe
      Left("evidence run ID is not declared by the campaign")
    resolve(revision = "commit:0000000000000000000000000000000000000000") shouldBe
      Left("evidence implementation revision does not match the campaign")
    resolve(warmup = first.warmupRounds + 1) shouldBe
      Left("evidence warmup rounds do not match the campaign")
    resolve(samples = first.sampleRounds + 1) shouldBe
      Left("evidence sample rounds do not match the campaign")
    resolve(binding.copy(runId = fixture.manifest.runs(2).id)) shouldBe
      Left("evidence environment does not match the declared cell policy")
    resolve(binding.copy(manifestByteLength = binding.manifestByteLength + 1)) shouldBe
      Left("evidence manifest binding does not match the exact campaign bytes")
    resolve(binding.copy(manifestSha256 = "f" * 64)) shouldBe
      Left("evidence manifest binding does not match the exact campaign bytes")

    val nonCanonical = new String(fixture.manifestBytes, StandardCharsets.UTF_8)
      .replace("\"campaignId\":", " \"campaignId\":")
      .getBytes(StandardCharsets.UTF_8)
    resolveRunPolicy(
      nonCanonical,
      binding.runId,
      first.implementationRevision,
      first.warmupRounds,
      first.sampleRounds,
      first.environment) shouldBe Left("campaign manifest is not canonical JSON")
  }

  test("every benchmark campaign-policy failure precedes verifier setup and output") {
    val fixture = campaignFixture()
    val directory = Files.createTempDirectory("eip0045-campaign-producer-preflight-")
    val manifestPath = directory.resolve("manifest.json")
    val nonCanonicalPath = directory.resolve("noncanonical-manifest.json")
    try {
      Files.write(manifestPath, fixture.manifestBytes)
      val nonCanonical = new String(fixture.manifestBytes, StandardCharsets.UTF_8)
        .replace("\"campaignId\":", " \"campaignId\":")
        .getBytes(StandardCharsets.UTF_8)
      Files.write(nonCanonicalPath, nonCanonical)

      final case class FailureCase(
          id: String,
          selectedManifest: Path,
          runId: String,
          revision: String,
          warmupRounds: Int,
          sampleRounds: Int,
          expected: String)
      val cases = Vector(
        FailureCase(
          "unknown-run",
          manifestPath,
          "cell-z:r1",
          fixture.manifest.implementationRevision,
          fixture.manifest.warmupRounds,
          fixture.manifest.sampleRounds,
          "evidence run ID is not declared by the campaign"),
        FailureCase(
          "revision",
          manifestPath,
          fixture.manifest.runs.head.id,
          "commit:0000000000000000000000000000000000000000",
          fixture.manifest.warmupRounds,
          fixture.manifest.sampleRounds,
          "evidence implementation revision does not match the campaign"),
        FailureCase(
          "warmup",
          manifestPath,
          fixture.manifest.runs.head.id,
          fixture.manifest.implementationRevision,
          fixture.manifest.warmupRounds + 1,
          fixture.manifest.sampleRounds,
          "evidence warmup rounds do not match the campaign"),
        FailureCase(
          "samples",
          manifestPath,
          fixture.manifest.runs.head.id,
          fixture.manifest.implementationRevision,
          fixture.manifest.warmupRounds,
          fixture.manifest.sampleRounds + 1,
          "evidence sample rounds do not match the campaign"),
        FailureCase(
          "environment",
          manifestPath,
          fixture.manifest.runs.head.id,
          fixture.manifest.implementationRevision,
          fixture.manifest.warmupRounds,
          fixture.manifest.sampleRounds,
          "evidence environment does not match the declared cell policy"),
        FailureCase(
          "noncanonical",
          nonCanonicalPath,
          fixture.manifest.runs.head.id,
          fixture.manifest.implementationRevision,
          fixture.manifest.warmupRounds,
          fixture.manifest.sampleRounds,
          "campaign manifest is not canonical JSON"))

      cases.foreach { item =>
        var verifierSetups = 0
        var outputAttempts = 0
        val observer = new Eip0045VerifierBenchmark.ExecutionObserver {
          override def beforeVerifierSetup(): Unit = verifierSetups += 1
          override def beforeOutput(): Unit = outputAttempts += 1
        }
        val outputPath = directory.resolve(item.id + "-evidence.json")
        val error = intercept[IllegalArgumentException] {
          Eip0045VerifierBenchmark.runWithObserverForTest(Array(
            "--warmup-rounds", item.warmupRounds.toString,
            "--sample-rounds", item.sampleRounds.toString,
            "--implementation-revision", item.revision,
            "--cpu-model", "Reference CPU A",
            "--campaign-manifest", item.selectedManifest.toString,
            "--campaign-run-id", item.runId,
            "--output", outputPath.toString), observer)
        }
        withClue(item.id + ": ") {
          error.getMessage shouldBe item.expected
          verifierSetups shouldBe 0
          outputAttempts shouldBe 0
          Files.exists(outputPath) shouldBe false
        }
      }

      var diagnosticVerifierSetups = 0
      var diagnosticOutputAttempts = 0
      val diagnosticObserver = new Eip0045VerifierBenchmark.ExecutionObserver {
        override def beforeVerifierSetup(): Unit = diagnosticVerifierSetups += 1
        override def beforeOutput(): Unit = diagnosticOutputAttempts += 1
      }
      val diagnosticOutput = directory.resolve("diagnostic-evidence.json")
      Eip0045VerifierBenchmark.runWithObserverForTest(Array(
        "--warmup-rounds", "0",
        "--sample-rounds", "1",
        "--cpu-model", "Reference CPU A",
        "--output", diagnosticOutput.toString), diagnosticObserver)
      diagnosticVerifierSetups shouldBe 1
      diagnosticOutputAttempts shouldBe 1
      Files.isRegularFile(diagnosticOutput) shouldBe true
    } finally deleteTree(directory)
  }

  test("real producer evidence creates and replays one campaign archive index") {
    val directory = Files.createTempDirectory("eip0045-campaign-real-e2e-")
    val manifestPath = directory.resolve("manifest.json")
    val evidencePath = directory.resolve("evidence.json")
    val indexPath = directory.resolve("archive-index.json")
    val cpuModel = "EIP-0045 test CPU"
    val revision = "commit:" + ("1" * 40)
    try {
      val environment = Eip0045VerifierBenchmark.currentEnvironmentForTest(cpuModel)
      environment.memoryPoolIdentities should not be empty
      environment.memoryPoolIdentities shouldBe
        sortMemoryPoolIdentities(environment.memoryPoolIdentities)
      val environmentPolicy = environmentPolicyFromMetadata("env-current", environment)
      val argumentPolicy = JvmArgumentPolicy(
        "args-current",
        environment.jvmInputArgumentCount,
        environment.jvmInputArgumentsSha256)
      val manifest = CampaignManifestV3(
        campaignId = "campaign-real-e2e",
        profileId = ExpectedProfileId,
        implementationRevision = revision,
        verifierEntryPoint = ExpectedVerifierEntryPoint,
        evidenceContract = ExpectedEvidenceContract,
        resources = ExpectedResources,
        warmupRounds = 0,
        sampleRounds = 1,
        scenarios = ExpectedScenarios,
        environmentPolicies = Vector(environmentPolicy),
        jvmArgumentPolicies = Vector(argumentPolicy),
        cells = Vector(CampaignCell("cell-current", "env-current", "args-current", 1)),
        runs = Vector(CampaignRun("cell-current:r1", "cell-current", 1)))
      val manifestBytes = bytes(rightValue(renderManifest(manifest)))
      Files.write(manifestPath, manifestBytes)

      Eip0045VerifierBenchmark.main(Array(
        "--warmup-rounds", "0",
        "--sample-rounds", "1",
        "--implementation-revision", revision,
        "--cpu-model", cpuModel,
        "--campaign-manifest", manifestPath.toString,
        "--campaign-run-id", "cell-current:r1",
        "--output", evidencePath.toString))

      val evidenceBytes = Files.readAllBytes(evidencePath)
      val evidenceText = new String(evidenceBytes, StandardCharsets.UTF_8)
      evidenceText should include(
        "\"campaignBinding\":{\"runId\":\"cell-current:r1\"")
      evidenceText should include("\"memoryPoolPhaseEnvelopes\":[{")
      environment.memoryPoolIdentities.foreach { identity =>
        evidenceText should include(
          "\"identity\":{\"name\":" + quote(identity.name) +
            ",\"memoryType\":" + quote(identity.memoryType) + "}")
      }
      evidenceText should include(
        "\"id\":\"valid-independent-po2-16\",\"expectedOutcome\":\"verified:1:16\"," +
          "\"validationQueryCheckpoints\":50,\"validationBoundary\":" +
          "\"verification-complete\",\"lastVerifierCheckpoint\":\"query\"")
      Vector(
        "po2-16-raw-seal",
        "po2-16-claim-digest",
        "po2-16-image-id",
        "po2-16-journal",
        "po2-16-fixture-manifest").foreach { resourceId =>
        evidenceText should include("\"id\":\"" + resourceId + "\"")
      }
      val created = rightValue(validateFiles(
        manifestPath,
        Vector(evidencePath),
        None,
        Some(indexPath)))
      Files.readAllBytes(indexPath) shouldBe bytes(created)
      validateFiles(
        manifestPath,
        Vector(evidencePath),
        Some(indexPath),
        None) shouldBe Right(created)

      val synthetic = campaignFixture()
      val missingBinding = synthetic.payloads.head.copy(campaignBinding = None)
      validateArchiveBytes(
        synthetic.manifestBytes,
        synthetic.evidence.updated(0, bytes(renderEnvelope(missingBinding)))) shouldBe
        Left("evidence campaign binding is missing")
    } finally deleteTree(directory)
  }

  test("payload digest and raw-sample statistics are semantically recomputed") {
    val fixture = campaignFixture()
    val text = new String(fixture.evidence.head, StandardCharsets.UTF_8)
    val marker = "\"evidenceDigest\":\""
    val start = text.indexOf(marker) + marker.length
    val changedDigest = text.substring(0, start) +
      (if (text.charAt(start) == '0') "1" else "0") + text.substring(start + 1)
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(changedDigest))) shouldBe
      Left("evidence payload digest does not match the payload")

    val first = fixture.payloads.head
    val badSummary = first.copy(scenarios = first.scenarios.updated(
      0,
      first.scenarios.head.copy(
        statistics = first.scenarios.head.statistics.copy(p50Ns = 999))))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(uncheckedEnvelope(badSummary)))) shouldBe
      Left("evidence payload semantics are invalid")
  }

  test("collector names and deltas are semantically revalidated") {
    val fixture = campaignFixture()
    val first = fixture.payloads.head
    val badCollectors = first.copy(garbageCollectorDeltas =
      first.garbageCollectorDeltas.updated(
        0,
        first.garbageCollectorDeltas.head.copy(name = "Other GC")))
    validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence.updated(0, bytes(uncheckedEnvelope(badCollectors)))) shouldBe
      Left("evidence payload semantics are invalid")
  }

  test("a frozen full-file index detects coordinated samples, summaries, and GC tampering") {
    val fixture = campaignFixture()
    val frozenIndex = rightValue(validateArchiveBytes(
      fixture.manifestBytes,
      fixture.evidence)).getBytes(StandardCharsets.UTF_8)
    val first = fixture.payloads.head
    val scenario = first.scenarios.head
    val changedSamples = scenario.samplesNs.updated(0, scenario.samplesNs.head + 7L)
    val changedAllocations = scenario.allocatedBytes.updated(
      0,
      scenario.allocatedBytes.head + 11L)
    val coordinated = first.copy(
      scenarios = first.scenarios.updated(
        0,
        scenario.copy(
          samplesNs = changedSamples,
          statistics = rightValue(statistics(changedSamples)),
          allocatedBytes = changedAllocations,
          allocationStatistics = rightValue(allocationStatistics(changedAllocations)))),
      garbageCollectorDeltas = first.garbageCollectorDeltas.updated(
        0,
        first.garbageCollectorDeltas.head.copy(collections =
          first.garbageCollectorDeltas.head.collections + 1L)))
    val changedEvidence = fixture.evidence.updated(0, bytes(renderEnvelope(coordinated)))
    validateArchiveBytes(
      fixture.manifestBytes,
      changedEvidence,
      Some(frozenIndex)) shouldBe
      Left("archive index does not match the validated full files")
  }

  test("bounded regular-file reads fail before optional output is created") {
    val fixture = campaignFixture()
    val directory = Files.createTempDirectory("eip0045-campaign-validator-negative-")
    val manifestPath = directory.resolve("manifest.json")
    val evidencePaths = writeEvidence(directory, fixture)
    val output = directory.resolve("index.json")
    val oversized = directory.resolve("oversized.json")
    try {
      Files.write(manifestPath, fixture.manifestBytes)
      validateFiles(
        manifestPath,
        evidencePaths,
        Some(null.asInstanceOf[Path]),
        None) shouldBe Left("expected archive index path is null")
      validateFiles(
        manifestPath,
        evidencePaths,
        None,
        Some(null.asInstanceOf[Path])) shouldBe Left("archive output path is null")
      Files.exists(output) shouldBe false

      Files.write(oversized, new Array[Byte](MaxCampaignManifestBytes + 1))
      validateFiles(oversized, evidencePaths, None, Some(output)) shouldBe
        Left("campaign manifest exceeds 1048576 bytes")
      Files.exists(output) shouldBe false

      validateFiles(manifestPath, evidencePaths.updated(0, directory), None, Some(output)) shouldBe
        Left("evidence file is not a regular file")
      Files.exists(output) shouldBe false

      val invalidFirst = fixture.payloads.head.copy(
        implementationRevision = "commit:0000000000000000000000000000000000000000")
      Files.write(evidencePaths.head, bytes(renderEnvelope(invalidFirst)))
      Files.delete(evidencePaths(1))
      var observedReads = Vector.empty[Int]
      val observer = new EvidenceReadObserver {
        override def beforeRead(index: Int, path: Path): Unit =
          observedReads = observedReads :+ index
      }
      validateFilesWithObserverForTest(
        manifestPath,
        evidencePaths,
        None,
        Some(output),
        observer) shouldBe
        Left("evidence implementation revision does not match the campaign")
      observedReads shouldBe Vector(0)
      Files.exists(output) shouldBe false
    } finally deleteTree(directory)
  }

  test("optional archive output is create-new and exact") {
    val fixture = campaignFixture()
    val directory = Files.createTempDirectory("eip0045-campaign-validator-output-")
    val manifestPath = directory.resolve("manifest.json")
    val output = directory.resolve("index.json")
    try {
      Files.write(manifestPath, fixture.manifestBytes)
      val evidencePaths = writeEvidence(directory, fixture)
      val expected = rightValue(validateFiles(
        manifestPath,
        evidencePaths,
        None,
        Some(output)))
      val expectedBytes = expected.getBytes(StandardCharsets.UTF_8)
      Files.readAllBytes(output) shouldBe expectedBytes
      archiveTemporaryFiles(directory) shouldBe empty

      val originalBytes = Files.readAllBytes(output)
      publishArchiveBytesForTest(output, bytes("replacement\n"), None) shouldBe
        Left("archive output could not be created")
      Files.readAllBytes(output) shouldBe originalBytes
      archiveTemporaryFiles(directory) shouldBe empty

      val interrupted = directory.resolve("interrupted-index.json")
      publishArchiveBytesForTest(interrupted, expectedBytes, Some(17)) shouldBe
        Left("archive output could not be created")
      Files.exists(interrupted) shouldBe false
      archiveTemporaryFiles(directory) shouldBe empty

      publishArchiveBytesForTest(interrupted, expectedBytes, None) shouldBe Right(())
      Files.readAllBytes(interrupted) shouldBe expectedBytes
      archiveTemporaryFiles(directory) shouldBe empty

      val victim = directory.resolve("collision-victim.bin")
      val victimBytes = bytes("unrelated collision victim\n")
      Files.write(victim, victimBytes)
      val collisionOutput = directory.resolve("collision-index.json")
      var collisionAttempts = Vector.empty[Int]
      var collider: Path = null
      val collisionObserver = new TemporaryOpenObserver {
        override def beforeOpen(
            attempt: Int,
            candidate: Path,
            finalPath: Path): Unit = {
          collisionAttempts = collisionAttempts :+ attempt
          Files.exists(finalPath) shouldBe false
          if (attempt == 0) {
            Files.createLink(candidate, victim)
            collider = candidate
          }
        }
      }
      publishArchiveBytesForTest(
        collisionOutput,
        expectedBytes,
        None,
        simulatePostPublicationCleanupFailure = false,
        temporaryOpenObserver = collisionObserver) shouldBe Right(())
      collisionAttempts shouldBe Vector(0, 1)
      Files.readAllBytes(collisionOutput) shouldBe expectedBytes
      Files.readAllBytes(victim) shouldBe victimBytes
      Files.readAllBytes(collider) shouldBe victimBytes
      archiveTemporaryFiles(directory) shouldBe Vector(collider)
      Files.delete(collider)
      Files.readAllBytes(victim) shouldBe victimBytes
      Files.readAllBytes(collisionOutput) shouldBe expectedBytes
      archiveTemporaryFiles(directory) shouldBe empty

      val cleanupFailure = directory.resolve("cleanup-failure-index.json")
      publishArchiveBytesForTest(
        cleanupFailure,
        expectedBytes,
        None,
        simulatePostPublicationCleanupFailure = true) shouldBe Right(())
      Files.readAllBytes(cleanupFailure) shouldBe expectedBytes
      val retainedTemporary = archiveTemporaryFiles(directory)
      retainedTemporary.length shouldBe 1
      Files.delete(retainedTemporary.head)
      Files.readAllBytes(cleanupFailure) shouldBe expectedBytes
      archiveTemporaryFiles(directory) shouldBe empty
    } finally deleteTree(directory)
  }

  test("archive output rejects a symbolic-link parent without touching its target") {
    val directory = Files.createTempDirectory("eip0045-campaign-validator-symlink-")
    val realParent = Files.createDirectory(directory.resolve("real-parent"))
    val linkedParent = directory.resolve("linked-parent")
    val sentinel = realParent.resolve("sentinel.bin")
    val sentinelBytes = bytes("real-parent sentinel\n")
    val output = linkedParent.resolve("index.json")
    try {
      Files.write(sentinel, sentinelBytes)
      try Files.createSymbolicLink(linkedParent, realParent)
      catch {
        case _: FileSystemException | _: SecurityException |
            _: UnsupportedOperationException
            if System.getProperty("os.name", "").startsWith("Windows") =>
          cancel(
            "symbolic-link negative is unavailable on this Windows test host")
      }

      publishArchiveBytesForTest(output, bytes("validated archive index\n"), None) shouldBe
        Left("archive output parent directory must not contain symbolic links")
      Files.exists(output) shouldBe false
      archiveTemporaryFiles(realParent) shouldBe empty
      Files.readAllBytes(sentinel) shouldBe sentinelBytes
    } finally deleteTree(directory)
  }

  test("temporary-name collision exhaustion preserves every non-owned collider") {
    val directory = Files.createTempDirectory("eip0045-campaign-validator-collisions-")
    val victim = directory.resolve("collision-victim.bin")
    val victimBytes = bytes("collision exhaustion victim\n")
    val output = directory.resolve("index.json")
    val outputBytes = bytes("validated archive index\n")
    var attempts = Vector.empty[Int]
    var colliders = Vector.empty[Path]
    try {
      Files.write(victim, victimBytes)
      val observer = new TemporaryOpenObserver {
        override def beforeOpen(
            attempt: Int,
            candidate: Path,
            finalPath: Path): Unit = {
          attempts = attempts :+ attempt
          Files.exists(finalPath) shouldBe false
          Files.createLink(candidate, victim)
          colliders = colliders :+ candidate
        }
      }

      publishArchiveBytesForTest(
        output,
        outputBytes,
        None,
        simulatePostPublicationCleanupFailure = false,
        temporaryOpenObserver = observer) shouldBe
        Left("archive output could not be created")
      attempts shouldBe (0 until 16).toVector
      colliders.length shouldBe 16
      colliders.distinct.length shouldBe 16
      Files.exists(output) shouldBe false
      Files.readAllBytes(victim) shouldBe victimBytes
      colliders.foreach { collider =>
        Files.exists(collider) shouldBe true
        Files.readAllBytes(collider) shouldBe victimBytes
      }
      archiveTemporaryFiles(directory).toSet shouldBe colliders.toSet

      colliders.foreach(Files.delete)
      archiveTemporaryFiles(directory) shouldBe empty
      Files.readAllBytes(victim) shouldBe victimBytes

      publishArchiveBytesForTest(output, outputBytes, None) shouldBe Right(())
      Files.readAllBytes(output) shouldBe outputBytes
      archiveTemporaryFiles(directory) shouldBe empty
    } finally deleteTree(directory)
  }

  test("strict UTF-8, duplicate keys, and noncanonical archive indexes fail closed") {
    val fixture = campaignFixture()
    parseManifest(Array(0xc3.toByte, 0x28.toByte)) shouldBe
      Left("campaign manifest is not strict UTF-8")
    val duplicateKey = "{\"schema\":\"" + ManifestSchema + "\",\"schema\":\"x\"}\n"
    parseManifest(bytes(duplicateKey)) shouldBe
      Left("campaign manifest contains invalid bounded JSON")

    val index = rightValue(validateArchiveBytes(fixture.manifestBytes, fixture.evidence))
    val nonCanonical = index.replace("\"campaignId\":", " \"campaignId\":")
    parseArchiveIndex(bytes(nonCanonical)) shouldBe
      Left("archive index is not canonical JSON")
  }

  test("CLI diagnostics classify fixed options without disclosing argument tokens") {
    val secretShapedToken = "--unknown=redacted/path-token-marker"
    val expected = "unknown option at argument index 0"
    parseArgsForTest(Array(secretShapedToken)) shouldBe Left(expected)

    val error = intercept[IllegalArgumentException] {
      Eip0045CampaignValidator.main(Array(secretShapedToken))
    }
    error.getMessage shouldBe expected
    error.toString should not include secretShapedToken

    Vector(
      "--manifest" -> "missing value for --manifest",
      "--evidence" -> "missing value for --evidence",
      "--expected-index" -> "missing value for --expected-index",
      "--output" -> "missing value for --output").foreach { case (option, failure) =>
      parseArgsForTest(Array(option)) shouldBe Left(failure)
    }
    parseArgsForTest(Array("--manifest", "\u0000credential-shaped-path")) shouldBe
      Left("manifest path is invalid")
  }

  test("bounded JSON parser rejects every independent structural limit") {
    def zeroArray(itemCount: Int): String =
      if (itemCount == 0) "[]" else "[" + ("0," * (itemCount - 1)) + "0]"

    val legalChildItemCount = 83333
    val valueCountDocument = Vector.fill(6)(zeroArray(legalChildItemCount))
      .mkString("[", ",", "]")
    val totalValueCount = 1 + 6 + 6 * legalChildItemCount
    totalValueCount should be > MaxJsonValues

    val cases = Vector(
      "nesting depth" ->
        (("[" * (MaxJsonDepth + 1)) + "0" + ("]" * (MaxJsonDepth + 1))),
      "object members" ->
        Vector.tabulate(MaxJsonObjectMembers + 1)(index =>
          quote("key-" + index) + ":0").mkString("{", ",", "}"),
      "array items" -> zeroArray(MaxJsonArrayItems + 1),
      "total values" -> valueCountDocument,
      "decoded string length" -> quote("x" * (MaxPublicStringCharacters + 1)),
      "integer token length" -> ("1" * 21))
    val expected = Left("bounded test document contains invalid bounded JSON")
    cases.foreach { case (label, json) =>
      withClue(label + ": ") {
        val encoded = bytes(json)
        encoded.length should be <= MaxCampaignManifestBytes
        parseDocument(encoded, MaxCampaignManifestBytes, "bounded test document") shouldBe expected
      }
    }
  }

  private final class Fixture(
      val manifest: CampaignManifestV3,
      val manifestBytes: Array[Byte],
      val payloads: Vector[EvidencePayload],
      val evidence: Vector[Array[Byte]])

  private def campaignFixture(): Fixture = {
    val environmentA = environmentPolicy("env-a", "Reference CPU A", 4096L)
    val environmentB = environmentPolicy("env-b", "Reference CPU B", 8192L)
    val argumentsA = JvmArgumentPolicy("args-a", 2, "a" * 64)
    val argumentsB = JvmArgumentPolicy("args-b", 3, "b" * 64)
    val manifest = CampaignManifestV3(
      campaignId = "campaign-2026-08",
      profileId = ExpectedProfileId,
      implementationRevision = "commit:dd428dde103d4b69a85456ca176c97fe099f8908",
      verifierEntryPoint = ExpectedVerifierEntryPoint,
      evidenceContract = ExpectedEvidenceContract,
      resources = ExpectedResources,
      warmupRounds = 1,
      sampleRounds = 2,
      scenarios = ExpectedScenarios,
      environmentPolicies = Vector(environmentA, environmentB),
      jvmArgumentPolicies = Vector(argumentsA, argumentsB),
      cells = Vector(
        CampaignCell("cell-a", "env-a", "args-a", 2),
        CampaignCell("cell-b", "env-b", "args-b", 2)),
      runs = Vector(
        CampaignRun("cell-a:r1", "cell-a", 1),
        CampaignRun("cell-a:r2", "cell-a", 2),
        CampaignRun("cell-b:r1", "cell-b", 1),
        CampaignRun("cell-b:r2", "cell-b", 2)))
    val manifestBytes = bytes(rightValue(renderManifest(manifest)))
    val bindingDigest = sha256Hex(manifestBytes)
    val payloads = manifest.runs.zipWithIndex.map { case (run, index) =>
      val cell = manifest.cells.find(_.id == run.cellId).get
      val environment = manifest.environmentPolicies
        .find(_.id == cell.environmentPolicyId).get
      val arguments = manifest.jvmArgumentPolicies
        .find(_.id == cell.jvmArgumentPolicyId).get
      evidencePayload(
        manifest,
        run,
        environment,
        arguments,
        manifestBytes.length,
        bindingDigest,
        index)
    }
    new Fixture(manifest, manifestBytes, payloads, payloads.map(p => bytes(renderEnvelope(p))))
  }

  private def environmentPolicy(
      id: String,
      cpu: String,
      heap: Long): EnvironmentPolicy =
    EnvironmentPolicy(
      id,
      "OpenJDK Runtime Environment",
      "17.0.18+8",
      "OpenJDK 64-Bit Server VM",
      "Vendor",
      "17.0.18+8",
      "mixed mode",
      "2.13.18",
      "Linux",
      "6.8",
      "amd64",
      8,
      heap,
      "HotSpot 64-Bit Tiered Compilers",
      Vector("G1 Old Generation", "G1 Young Generation"),
      Vector(
        MemoryPoolIdentity("G1 Eden Space", "HEAP"),
        MemoryPoolIdentity("Metaspace", "NON_HEAP")),
      ExpectedThreadAllocationMeter,
      cpu,
      "--cpu-model")

  private def environmentPolicyFromMetadata(
      id: String,
      environment: EnvironmentMetadata): EnvironmentPolicy =
    EnvironmentPolicy(
      id,
      environment.javaRuntimeName,
      environment.javaRuntimeVersion,
      environment.javaVmName,
      environment.javaVmVendor,
      environment.javaVmVersion,
      environment.javaVmInfo,
      environment.scalaVersion,
      environment.osName,
      environment.osVersion,
      environment.osArch,
      environment.availableProcessors,
      environment.maxHeapBytes,
      environment.jitCompiler,
      environment.garbageCollectors,
      environment.memoryPoolIdentities,
      environment.threadAllocationMeter,
      environment.cpuModel,
      environment.cpuModelSource)

  private def evidencePayload(
      manifest: CampaignManifestV3,
      run: CampaignRun,
      environment: EnvironmentPolicy,
      arguments: JvmArgumentPolicy,
      manifestLength: Int,
      manifestDigest: String,
      runIndex: Int): EvidencePayload = {
    val scenarioEvidence = manifest.scenarios.zipWithIndex.map { case (scenario, scenarioIndex) =>
      val base = 10L + runIndex * 100L + scenarioIndex * 10L
      val timings = Vector(base, base + 3L)
      val allocations = Vector(base + 100L, base + 111L)
      ScenarioEvidence(
        scenario.id,
        scenario.expectedOutcome,
        scenario.validationQueryCheckpoints,
        scenario.validationBoundary,
        scenario.lastVerifierCheckpoint,
        timings,
        rightValue(statistics(timings)),
        allocations,
        rightValue(allocationStatistics(allocations)))
    }
    EvidencePayload(
      startedAtUtc = "2026-08-17T00:00:0" + runIndex + "Z",
      benchmarkDurationNs = 1000L + runIndex,
      profileId = manifest.profileId,
      implementationRevision = manifest.implementationRevision,
      verifierEntryPoint = manifest.verifierEntryPoint,
      resources = manifest.resources,
      warmupRounds = manifest.warmupRounds,
      sampleRounds = manifest.sampleRounds,
      campaignBinding = Some(CampaignBinding(run.id, manifestLength, manifestDigest)),
      environment = EnvironmentMetadata(
        environment.javaRuntimeName,
        environment.javaRuntimeVersion,
        environment.javaVmName,
        environment.javaVmVendor,
        environment.javaVmVersion,
        environment.javaVmInfo,
        environment.scalaVersion,
        environment.osName,
        environment.osVersion,
        environment.osArch,
        environment.availableProcessors,
        environment.maxHeapBytes,
        environment.jitCompiler,
        environment.garbageCollectors,
        environment.memoryPoolIdentities,
        environment.threadAllocationMeter,
        arguments.argumentCount,
        arguments.argumentsSha256,
        environment.cpuModel,
        environment.cpuModelSource),
      scenarios = scenarioEvidence,
      garbageCollectorDeltas = environment.garbageCollectors.zipWithIndex.map {
        case (name, index) => GarbageCollectorDelta(name, index.toLong, index.toLong + 1L)
      },
      memoryPoolPhaseEnvelopes = environment.memoryPoolIdentities.zipWithIndex.map {
        case (identity, index) =>
          val base = 100L + index.toLong * 100L
          MemoryPoolPhaseEnvelope(
            identity,
            MemoryUsageEvidence(base, base + 50L, -1L),
            MemoryUsageEvidence(base + 20L, base + 80L, base + 180L),
            MemoryUsageEvidence(base + 30L, base + 70L, base + 170L))
      },
      limitations = ExpectedCampaignLimitations)
  }

  private def uncheckedEnvelope(payload: EvidencePayload): String = {
    val payloadJson = renderPayload(payload)
    "{" +
      quote("schema") + ":" + quote(Schema) + "," +
      quote("digestAlgorithm") + ":" + quote(DigestAlgorithm) + "," +
      quote("digestDomain") + ":" + quote(DigestDomain) + "," +
      quote("canonicalization") + ":" + quote(Canonicalization) + "," +
      quote("evidenceDigest") + ":" + quote(evidenceDigest(payloadJson)) + "," +
      quote("payload") + ":" + payloadJson + "}\n"
  }

  private def resourceBytes(path: String): Array[Byte] = {
    val in = getClass.getResourceAsStream(path)
    require(in != null, "missing test resource")
    val out = new ByteArrayOutputStream()
    val buffer = new Array[Byte](8192)
    try {
      var read = in.read(buffer)
      while (read >= 0) {
        if (read > 0) out.write(buffer, 0, read)
        read = in.read(buffer)
      }
      out.toByteArray
    } finally {
      in.close()
      out.close()
    }
  }

  private def writeEvidence(directory: Path, fixture: Fixture): Vector[Path] =
    fixture.evidence.zipWithIndex.map { case (content, index) =>
      val path = directory.resolve("evidence-" + index + ".json")
      Files.write(path, content)
      path
    }

  private def archiveTemporaryFiles(directory: Path): Vector[Path] = {
    val stream = Files.newDirectoryStream(directory, ArchiveTemporaryFilePrefix + "*")
    try {
      val out = Vector.newBuilder[Path]
      val iterator = stream.iterator()
      while (iterator.hasNext) out += iterator.next()
      out.result()
    } finally stream.close()
  }

  private def deleteTree(directory: Path): Unit = {
    if (Files.exists(directory)) {
      val stream = Files.walk(directory)
      try {
        val paths = stream.iterator()
        val collected = Vector.newBuilder[Path]
        while (paths.hasNext) collected += paths.next()
        collected.result().sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
      } finally stream.close()
    }
  }

  private def bytes(value: String): Array[Byte] =
    value.getBytes(StandardCharsets.UTF_8)

  private def rightValue[A](value: Either[String, A]): A = value match {
    case Right(result) => result
    case Left(detail) => fail(detail)
  }
}
