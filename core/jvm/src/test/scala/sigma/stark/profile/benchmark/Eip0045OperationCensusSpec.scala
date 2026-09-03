/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.{MerkleVerifier, Poseidon2, Poseidon2Rng, ReadIop, VerifierOperationObserver}
import sigma.stark.profile.Risc0RawSealVerifier
import sigma.stark.profile.Risc0RawSealVerifier.Probe
import sigma.stark.profile.benchmark.Eip0045BenchmarkSupport.OperationOnlyVerifierRoute

private object Eip0045OperationCensusSpec {
  final case class Counts(
      topPairHashes: Int,
      queryPairHashes: Int,
      contentHashCalls: Int,
      contentHashPermutations: Int,
      rngCommits: Int,
      rngElementDraws: Int,
      rngPermutations: Int) {
    def totalPoseidonPermutations: Int =
      topPairHashes + queryPairHashes + contentHashPermutations + rngPermutations
  }

  val Zero: Counts = Counts(0, 0, 0, 0, 0, 0, 0)
}

class Eip0045OperationCensusSpec extends AnyFunSuite with Matchers {
  import Eip0045OperationCensusSpec._
  import VerifierOperationObserver._

  private final class CountingProbe extends Probe with VerifierOperationObserver {
    private var topPairHashes = 0
    private var queryPairHashes = 0
    private var contentHashCalls = 0
    private var contentHashPermutations = 0
    private var rngCommits = 0
    private var rngElementDraws = 0
    private var rngPermutations = 0

    override private[stark] def operationSinkOrNull: VerifierOperationObserver = this

    override def onOperation(operationId: Int): Unit = operationId match {
      case MerkleTopPairHash       => topPairHashes += 1
      case MerkleQueryPairHash     => queryPairHashes += 1
      case ContentHashCall         => contentHashCalls += 1
      case ContentHashPermutation  => contentHashPermutations += 1
      case RngCommit               => rngCommits += 1
      case RngElementDraw          => rngElementDraws += 1
      case RngPermutation          => rngPermutations += 1
      case other => fail("unknown verifier operation id: " + other)
    }

    def counts: Counts = Counts(
      topPairHashes,
      queryPairHashes,
      contentHashCalls,
      contentHashPermutations,
      rngCommits,
      rngElementDraws,
      rngPermutations)
  }

  private lazy val scenarios =
    Eip0045VerifierBenchmark.operationCensusScenariosForTest()

  private def scenario(id: String): Eip0045VerifierBenchmark.Scenario =
    scenarios.find(_.id == id).getOrElse(fail("missing benchmark scenario " + id))

  private def observedCounts(id: String): Counts = {
    val current = scenario(id)
    val probe = new CountingProbe
    current.runWithProbe(probe) shouldBe current.run()
    probe.counts
  }

  test("operation instrumentation preserves all six verifier outcomes") {
    scenarios.map(_.id) shouldBe Vector(
      "valid-proof",
      "early-transport-rejection",
      "early-canonical-cryptographic-rejection",
      "late-cryptographic-mutation",
      "late-claim-mismatch",
      "valid-independent-po2-16")

    scenarios.foreach { current =>
      val productionResult = current.run()
      val probe = new CountingProbe
      current.runWithProbe(probe) shouldBe productionResult
    }
  }

  test("operation-only benchmark closures preserve outcomes and exact counts") {
    scenarios.foreach { current =>
      current.runObservedOperations.resetOperationCounts()
      try {
        current.runObservedOperations() shouldBe current.run()
        current.runObservedOperations.operationCounts shouldBe
          current.expectedOperationCounts
      } finally current.runObservedOperations.resetOperationCounts()
    }
  }

  test("paired allocation gate rejects every single-counter mismatch") {
    val current = scenario("valid-proof")
    current.runObservedOperations.resetOperationCounts()
    try {
      current.runObservedOperations() shouldBe current.run()
      val expected = current.expectedOperationCounts
      val mutants = Vector(
        "topPairHashes" -> expected.copy(
          topPairHashes = expected.topPairHashes + 1),
        "queryPairHashes" -> expected.copy(
          queryPairHashes = expected.queryPairHashes + 1),
        "contentHashCalls" -> expected.copy(
          contentHashCalls = expected.contentHashCalls + 1),
        "contentHashPermutations" -> expected.copy(
          contentHashPermutations = expected.contentHashPermutations + 1),
        "rngCommits" -> expected.copy(
          rngCommits = expected.rngCommits + 1),
        "rngElementDraws" -> expected.copy(
          rngElementDraws = expected.rngElementDraws + 1),
        "rngPermutations" -> expected.copy(
          rngPermutations = expected.rngPermutations + 1))

      mutants.foreach { case (field, operationCounts) =>
        val mismatched = current.copy(expectedOperationCounts = operationCounts)
        val error = intercept[IllegalStateException] {
          Eip0045VerifierBenchmark.validateMeasuredRouteForTest(
            Vector(mismatched),
            OperationOnlyVerifierRoute,
            invocationCount = 1)
        }
        withClue(field + ": ") {
          error.getMessage should include("measured route observed operation counts")
        }
      }
    } finally current.runObservedOperations.resetOperationCounts()
  }

  test("dynamic census observes the complete stock-profile primitive vector") {
    val current = scenario("valid-proof")
    val productionResult = current.run()
    val probe = new CountingProbe

    current.runWithProbe(probe) shouldBe productionResult
    probe.counts shouldBe Counts(
      topPairHashes = 217,
      queryPairHashes = 4050,
      contentHashCalls = 353,
      contentHashPermutations = 1384,
      rngCommits = 12,
      rngElementDraws = 244,
      rngPermutations = 32)
    probe.counts.totalPoseidonPermutations shouldBe 5683
  }

  test("transport rejection performs no observed verifier primitive") {
    val current = scenario("early-transport-rejection")
    val probe = new CountingProbe

    current.runWithProbe(probe) shouldBe current.run()
    probe.counts shouldBe Zero
  }

  test("canonical byte-132 mutation stops at the exact early vector") {
    val current = scenario("early-canonical-cryptographic-rejection")
    val probe = new CountingProbe

    current.runWithProbe(probe) shouldBe current.run()
    probe.counts shouldBe Counts(
      topPairHashes = 31,
      queryPairHashes = 0,
      contentHashCalls = 1,
      contentHashPermutations = 3,
      rngCommits = 4,
      rngElementDraws = 0,
      rngPermutations = 4)
    probe.counts.totalPoseidonPermutations shouldBe 38
  }

  test("late paths and the independent receipt retain the full profile vector") {
    val primary = Counts(217, 4050, 353, 1384, 12, 244, 32)
    observedCounts("late-cryptographic-mutation") shouldBe primary
    observedCounts("late-claim-mismatch") shouldBe primary

    val independent = observedCounts("valid-independent-po2-16")
    independent shouldBe primary
    independent.totalPoseidonPermutations shouldBe 5683
  }

  test("content and transcript primitives expose isolated exact counts") {
    val contentProbe = new CountingProbe
    Poseidon2.unpaddedHash(Array.fill(17)(0), contentProbe).length shouldBe
      Poseidon2.CellsOut
    contentProbe.counts shouldBe Counts(0, 0, 1, 2, 0, 0, 0)

    val transcriptProbe = new CountingProbe
    val transcript = new ReadIop(Array.empty[Int])
    transcript.commit(Array.fill(Poseidon2.CellsOut)(0), transcriptProbe)
    var i = 0
    while (i < Poseidon2.CellsRate + 1) {
      transcript.randomElem(transcriptProbe)
      i += 1
    }
    transcriptProbe.counts shouldBe Counts(0, 0, 0, 0, 1, 17, 2)
  }

  test("Merkle top and query pair primitives expose isolated exact counts") {
    val topProbe = new CountingProbe
    val topIop = new ReadIop(Array.fill(2 * Poseidon2.CellsOut)(0))
    val tree = MerkleVerifier.create(topIop, 8, 1, 2, topProbe) match {
      case Right(value) => value
      case Left(detail) => fail(detail)
    }
    topProbe.counts shouldBe Counts(1, 0, 0, 0, 1, 0, 1)

    val queryProbe = new CountingProbe
    val queryWords = Array.fill(1 + 2 * Poseidon2.CellsOut)(0)
    tree.verify(new ReadIop(queryWords), 0, queryProbe).isLeft shouldBe true
    queryProbe.counts shouldBe Counts(0, 2, 1, 1, 0, 0, 0)
  }

  test("failed primitives emit no event and observer exceptions propagate") {
    val failedPrimitiveProbe = new CountingProbe
    val transcript = new ReadIop(Array.empty[Int])
    intercept[IllegalArgumentException] {
      transcript.commit(Array.empty[Int], failedPrimitiveProbe)
    }
    failedPrimitiveProbe.counts shouldBe Zero

    final class ObserverSentinel extends RuntimeException("operation observer sentinel")
    final class ThrowingProbe extends Probe with VerifierOperationObserver {
      override private[stark] def operationSinkOrNull: VerifierOperationObserver = this
      override def onOperation(operationId: Int): Unit = throw new ObserverSentinel
    }
    intercept[ObserverSentinel] {
      scenario("valid-proof").runWithProbe(new ThrowingProbe)
    }
  }

  test("production verifier state holds no operation observer or ThreadLocal") {
    val classes = Array[Class[_]](
      classOf[ReadIop],
      classOf[Poseidon2Rng],
      classOf[MerkleVerifier],
      classOf[Risc0RawSealVerifier],
      Class.forName("sigma.stark.FriVerifier$RoundInfo"))

    classes.foreach { clazz =>
      val forbidden = clazz.getDeclaredFields.filter { field =>
        classOf[VerifierOperationObserver].isAssignableFrom(field.getType) ||
          classOf[ThreadLocal[AnyRef]].isAssignableFrom(field.getType)
      }
      withClue(clazz.getName + " forbidden fields: ") {
        forbidden.map(_.getName) shouldBe empty
      }
    }
  }
}
