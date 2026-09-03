/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark.profile.benchmark

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.{FriVerifier, Poseidon2}
import sigma.stark.profile.RawSealV1Decoder

private[benchmark] object Eip0045StaticCensusModel {
  final case class ProfileShape(
      outerPo2: Int,
      queries: Int,
      inverseRate: Int,
      extensionDegree: Int,
      outputWords: Int,
      mixWords: Int,
      tapWords: Int,
      checkWords: Int,
      mainLeafWords: Vector[Int],
      friFold: Int,
      friFoldPo2: Int,
      friMinDegree: Int)

  final case class TreeCensus(
      rows: Int,
      leafWords: Int,
      topPairHashes: Int,
      queryPairHashes: Int,
      leafPermutations: Int)

  final case class ModeledCensus(
      mainTrees: Vector[TreeCensus],
      friTrees: Vector[TreeCensus],
      finalPolynomialWords: Int,
      contentHashes: Int,
      contentPermutations: Int,
      topPairHashes: Int,
      queryPairHashes: Int)

  sealed trait RngEvent
  case object Commit extends RngEvent
  final case class Draw(elements: Int) extends RngEvent
}

/** Partial static model of operation counts for the frozen EIP-0045 stock profile.
  *
  * This is a manual, non-instrumented model whose encoded dimensions and event
  * schedule can drift from the production verifier path. Passing this suite
  * establishes only internal consistency with the pinned expectations below.
  * It is diagnostic evidence only: it does not measure timing, allocation,
  * peak live memory, dispatch, or the full Sigma evaluation path, and cannot
  * freeze `dispatchJit` / `fixedJit` or close B5.
  */
class Eip0045StaticCensusSpec extends AnyFunSuite with Matchers {
  import Eip0045StaticCensusModel._

  private val profile = ProfileShape(
    outerPo2 = RawSealV1Decoder.ExpectedOuterPo2,
    queries = FriVerifier.Queries,
    inverseRate = FriVerifier.InvRate,
    extensionDegree = 4,
    outputWords = 32,
    mixWords = 20,
    tapWords = 643,
    checkWords = 16,
    mainLeafWords = Vector(12, 23, 128, 16),
    friFold = FriVerifier.FriFold,
    friFoldPo2 = FriVerifier.FriFoldPo2,
    friMinDegree = FriVerifier.FriMinDegree)

  private lazy val modeledCensus = deriveModeledCensus(profile)

  test("manual model derives the expected stock-profile Merkle pair counts") {
    profile.outerPo2 shouldBe 18
    profile.queries shouldBe 50
    modeledCensus.mainTrees.map(queryDepth) shouldBe Vector(15, 15, 15, 15)
    modeledCensus.friTrees.map(queryDepth) shouldBe Vector(11, 7, 3)
    modeledCensus.topPairHashes shouldBe 217
    modeledCensus.queryPairHashes shouldBe 4050
    modeledCensus.topPairHashes + modeledCensus.queryPairHashes shouldBe 4267
  }

  test("manual model derives the expected stock-profile content counts") {
    modeledCensus.finalPolynomialWords shouldBe 256
    modeledCensus.contentHashes shouldBe 353
    modeledCensus.contentPermutations shouldBe 1384

    val constructionInputs = Vector("RISC0_STARK:v1__", "RECURSION:rev1v1")
    constructionInputs.map(_.length) shouldBe Vector(16, 16)
    constructionInputs.map(value => hashPermutations(value.length)).sum shouldBe 2
  }

  test("manual transcript model derives the expected modeled Poseidon2 count") {
    val events = transcriptEvents(profile, modeledCensus.friTrees.length)
    events.count(_ == Commit) shouldBe 12
    events.collect { case Draw(elements) => elements }.sum shouldBe 244

    val modeledRngPermutations = countRngPermutations(events, Poseidon2.CellsRate)
    modeledRngPermutations shouldBe 32
    modeledCensus.topPairHashes + modeledCensus.queryPairHashes +
      modeledCensus.contentPermutations + modeledRngPermutations shouldBe 5683
  }

  private def deriveModeledCensus(shape: ProfileShape): ModeledCensus = {
    require(shape.outerPo2 >= 0 && shape.outerPo2 < 30)
    require(shape.friFold == 1 << shape.friFoldPo2)
    val degree = 1 << shape.outerPo2
    val domain = Math.multiplyExact(shape.inverseRate, degree)
    val mainTrees = shape.mainLeafWords.map(treeCensus(domain, _, shape.queries))

    val friTrees = Vector.newBuilder[TreeCensus]
    var friDegree = degree
    var friDomain = domain
    while (friDegree > shape.friMinDegree) {
      require(friDegree % shape.friFold == 0)
      require(friDomain % shape.friFold == 0)
      friTrees += treeCensus(
        friDomain / shape.friFold,
        shape.friFold * shape.extensionDegree,
        shape.queries)
      friDegree /= shape.friFold
      friDomain /= shape.friFold
    }
    val frozenFriTrees = friTrees.result()
    val finalPolynomialWords = Math.multiplyExact(shape.extensionDegree, friDegree)
    val protocolContentWords = Vector(
      shape.outputWords + 1,
      shape.extensionDegree * (shape.tapWords + shape.checkWords),
      finalPolynomialWords)
    val allTrees = mainTrees ++ frozenFriTrees
    val contentHashes = protocolContentWords.length + shape.queries * allTrees.length
    val contentPermutations = protocolContentWords.map(hashPermutations).sum +
      shape.queries * allTrees.map(_.leafPermutations).sum

    ModeledCensus(
      mainTrees = mainTrees,
      friTrees = frozenFriTrees,
      finalPolynomialWords = finalPolynomialWords,
      contentHashes = contentHashes,
      contentPermutations = contentPermutations,
      topPairHashes = allTrees.map(_.topPairHashes).sum,
      queryPairHashes = shape.queries * allTrees.map(_.queryPairHashes).sum)
  }

  private def treeCensus(rows: Int, leafWords: Int, queries: Int): TreeCensus = {
    val topSize = merkleTopSize(rows, queries)
    TreeCensus(
      rows = rows,
      leafWords = leafWords,
      topPairHashes = topSize - 1,
      queryPairHashes = log2Exact(rows) - log2Exact(topSize),
      leafPermutations = hashPermutations(leafWords))
  }

  private def merkleTopSize(rows: Int, queries: Int): Int = {
    require(queries > 0)
    val layers = log2Exact(rows)
    var topLayer = 0
    var layer = 1
    while (layer < layers && (1 << layer) <= queries) {
      topLayer = layer
      layer += 1
    }
    1 << topLayer
  }

  private def queryDepth(tree: TreeCensus): Int = tree.queryPairHashes

  private def hashPermutations(words: Int): Int = {
    require(words >= 0)
    if (words == 0) 1 else (words + Poseidon2.CellsRate - 1) / Poseidon2.CellsRate
  }

  private def log2Exact(value: Int): Int = {
    require(value > 0 && (value & (value - 1)) == 0, s"not a power of two: $value")
    Integer.numberOfTrailingZeros(value)
  }

  private def transcriptEvents(shape: ProfileShape, friRounds: Int): Vector[RngEvent] = {
    val prefix = Vector[RngEvent](
      Commit, // proof-system protocol digest
      Commit, // circuit protocol digest
      Commit, // output slice hash
      Commit, // code Merkle root
      Commit, // data Merkle root
      Draw(shape.mixWords),
      Commit, // accumulator Merkle root
      Draw(shape.extensionDegree), // constraint mixer
      Commit, // check Merkle root
      Draw(shape.extensionDegree), // evaluation point
      Commit, // coefficient-slice hash
      Draw(shape.extensionDegree)) // DEEP-ALI batch mixer
    val rounds = Vector.fill(friRounds)(
      Vector[RngEvent](Commit, Draw(shape.extensionDegree))).flatten
    prefix ++ rounds ++ Vector[RngEvent](
      Commit, // final-polynomial hash
      Draw(shape.queries * 4)) // randomBits consumes four elements per query
  }

  private def countRngPermutations(events: Vector[RngEvent], rate: Int): Int = {
    require(rate > 0)
    var poolUsed = 0
    var permutations = 0
    events.foreach {
      case Commit =>
        if (poolUsed != 0) {
          permutations += 1
          poolUsed = 0
        }
        permutations += 1
      case Draw(elements) =>
        require(elements >= 0)
        var remaining = elements
        while (remaining > 0) {
          if (poolUsed == rate) {
            permutations += 1
            poolUsed = 0
          }
          poolUsed += 1
          remaining -= 1
        }
    }
    permutations
  }
}
