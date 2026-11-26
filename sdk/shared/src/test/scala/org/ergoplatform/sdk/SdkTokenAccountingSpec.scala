package org.ergoplatform.sdk

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.ErgoBox
import org.ergoplatform.sdk.wallet.AssetUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.hash.Blake2b256
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.{ErgoTree, TrueLeaf}
import sigma.data.{Digest32Coll, SigmaConstants}

import scala.collection.mutable

class SdkTokenAccountingSpec extends AnyPropSpec with Matchers {

  private val rawTokenId = Blake2b256("sdk-token-accounting-test")
  private val tokenModId = bytesToId(rawTokenId)
  private val tokenId = Digest32Coll @@ Colls.fromArray(rawTokenId)
  private val txId = bytesToId(Blake2b256("sdk-token-accounting-tx"))
  private val trueTree = ErgoTree.fromProposition(TrueLeaf.toSigmaProp)

  property("UnsignedTransactionBuilder.collTokensToMap sums duplicate token IDs and checks overflow") {
    val dupTokens = Colls.fromItems(tokenId -> 4L, tokenId -> 6L)
    UnsignedTransactionBuilder.collTokensToMap(dupTokens) shouldBe Map(tokenModId -> 10L)

    val overflowTokens = Colls.fromItems(tokenId -> Long.MaxValue, tokenId -> 1L)
    an[ArithmeticException] should be thrownBy UnsignedTransactionBuilder.collTokensToMap(overflowTokens)
  }

  property("SdkIsos.isoErgoTokenSeqToLinkedMap sums duplicate ErgoTokens and checks overflow") {
    val dupTokens = IndexedSeq(new ErgoToken(rawTokenId, 4L), new ErgoToken(rawTokenId, 6L))
    SdkIsos.isoErgoTokenSeqToLinkedMap.to(dupTokens).toMap shouldBe Map(tokenModId -> 10L)

    val overflowTokens = IndexedSeq(new ErgoToken(rawTokenId, Long.MaxValue), new ErgoToken(rawTokenId, 1L))
    an[ArithmeticException] should be thrownBy SdkIsos.isoErgoTokenSeqToLinkedMap.to(overflowTokens)
  }

  property("OutBoxBuilder.tokens enforces cumulative MaxTokens and positive amounts") {
    val txB = new UnsignedTransactionBuilder(null)
    val maxTokens = SigmaConstants.MaxTokens.value

    val b1 = OutBoxBuilder(txB)
    val token = new ErgoToken(rawTokenId, 1L)
    an[IllegalArgumentException] should be thrownBy b1.tokens(new ErgoToken(rawTokenId, 0L))
    an[IllegalArgumentException] should be thrownBy b1.tokens(new ErgoToken(rawTokenId, -5L))

    val half = maxTokens / 2 + 1
    b1.tokens(Seq.fill(half)(token): _*)
    an[IllegalArgumentException] should be thrownBy b1.tokens(Seq.fill(maxTokens - half + 1)(token): _*)
  }

  property("UnsignedTransactionBuilder.buildUnsignedTx fails on ERG sum overflow") {
    val addr = org.ergoplatform.Pay2SAddress(trueTree)(org.ergoplatform.ErgoAddressEncoder(0.toByte))
    val bigIn1 = new ErgoBox(Long.MaxValue - 10L, trueTree, Colls.emptyColl, Map.empty, txId, 0: Short, 1)
    val bigIn2 = new ErgoBox(100L, trueTree, Colls.emptyColl, Map.empty, txId, 1: Short, 1)
    val out1 = new ErgoBoxCandidate(1000L, trueTree, 1)

    val resIn = UnsignedTransactionBuilder.buildUnsignedTx(
      IndexedSeq(bigIn1, bigIn2),
      IndexedSeq.empty,
      Seq(out1),
      currentHeight = 10,
      createFeeOutput = Some(1000L),
      changeAddress = addr,
      minChangeValue = 1000L,
      minerRewardDelay = 720
    )
    resIn.isFailure shouldBe true
    resIn.failed.get shouldBe an[ArithmeticException]

    val normalIn = new ErgoBox(Long.MaxValue, trueTree, Colls.emptyColl, Map.empty, txId, 2: Short, 1)
    val bigOut = new ErgoBoxCandidate(Long.MaxValue - 10L, trueTree, 1)
    val resFee = UnsignedTransactionBuilder.buildUnsignedTx(
      IndexedSeq(normalIn),
      IndexedSeq.empty,
      Seq(bigOut),
      currentHeight = 10,
      createFeeOutput = Some(100L),
      changeAddress = addr,
      minChangeValue = 1000L,
      minerRewardDelay = 720
    )
    resFee.isFailure shouldBe true
    resFee.failed.get shouldBe an[ArithmeticException]
  }

  property("AssetUtils.subtractAssetsMut rejects missing tokens, negative amounts, and underflow") {
    val m1 = mutable.Map(tokenModId -> 10L)
    AssetUtils.subtractAssetsMut(m1, Map(tokenModId -> 4L))
    m1.toMap shouldBe Map(tokenModId -> 6L)

    AssetUtils.subtractAssetsMut(m1, Map(tokenModId -> 6L))
    m1 shouldBe empty

    val missingId = bytesToId(Blake2b256("missing-token"))
    an[IllegalArgumentException] should be thrownBy AssetUtils.subtractAssetsMut(
      mutable.Map(tokenModId -> 5L), Map(missingId -> 1L))
    an[IllegalArgumentException] should be thrownBy AssetUtils.subtractAssetsMut(
      mutable.Map(tokenModId -> 5L), Map(tokenModId -> -1L))
    an[IllegalArgumentException] should be thrownBy AssetUtils.subtractAssetsMut(
      mutable.Map(tokenModId -> 5L), Map(tokenModId -> 6L))
  }
}
