package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.Token
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8}
import org.ergoplatform._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32, Sha256}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast._
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.syntax._
import sigma.data.{AvlTreeFlags, CAvlTree, Digest32Coll}
import sigma.Extensions.ArrayOps
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.util.NBitsUtils
import sigma.Coll
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}

import java.math.BigInteger
import java.nio.ByteBuffer
import scala.collection.compat.immutable.ArraySeq

class BitcoinRelayTxCheckSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {

  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val relayNftId = Array.fill(32)(0.toByte)
  private val relayToken = Digest32Coll @@ relayNftId.toColl
  private val canonicalRelayTokens = ArraySeq((relayToken, 1L): Token)
  private val relayBoxValue = 100000000L
  private val bitcoinHeaderHeight = 93500
  private val mainnetHeader566092Hex =
    "00000020a82ff9c62e69a6cbed277b7f2a9ac9da3c7133a59a6305000000000000000000f6cd5708a6ba38d8501502b5b4e5b93627e8dcc9bd13991894c6e04ade262aa99582815c505b2e17479a751b"
  private val mainnetHeader566093Hex =
    "00000020b45e33a345ad08ad2902cdd4101632fcbec009694b0c2500000000000000000016c99a795d8e0105d86f361341c7858d223fac261718bd608052822c5b4ae3cfd782815c505b2e17a56bb90b"
  // Bitcoin mainnet block 566094 (canonical hash 0000000000000000000c9ed898ad11d40b030b2e384879da8c70826b2e533b09).
  // Added to form the real 566092 -> 566093 -> 566094 chain required by the pinned
  // producer/consumer regression: transition 2 must decode the record transition 1 inserted.
  // Same difficulty period as 566092/566093 (no retarget boundary).
  private val mainnetHeader566094Hex =
    "00000020eccfa71bfa8a562fb464d312396f3fe99ac1ed3d403923000000000000000000ceb1f9869ce11ae60d3d912a0e0aaf8ef79f1f0b3b813bab63a435ea718908b13e8a815c505b2e17454d01eb"
  private val mainnetHeader566094CanonicalHash =
    "0000000000000000000c9ed898ad11d40b030b2e384879da8c70826b2e533b09"
  private val mainnetHeader562464Hex =
    "00000020ae55d7640b738e1c16091cc73666526e7fa12af66c0419000000000000000000f7825fe0714275fe54521f66e898cf743ed43dd93f185cb628df995823e4ee2d7d58605c886f2e176d085a4c"
  private val mainnetHeader564479Hex =
    "00000020b4fe0ef78ee4d02206011ba597b45ef84ef1029ad8650300000000000000000034fdbe970f5d00d2e37de72755077c7039976baa5417ddfd358013d8ea9cb8d374c5725c886f2e1795d4ee3a"
  private val mainnetHeader564480Hex =
    "000000200cd536b3eb1cd9c028e081f1455006276b293467c3e5170000000000000000007bc1b27489db01c85d38a4bc6d2280611e9804f506d83ad00d2a33ebd663992f76c7725c505b2e174fb90f55"
  private val forkHeaderHex =
    "01000000076379e2c0ec4a614ad1bf0ec716e6873f2c7abac604a08cc78e070000000000579a6bbcd07e9c3d622672ad20495d4485b5233395ab4081db7cab0fd2b577d2396cec4c2a8b091b031a7313"
  private val expectedHeader566093Target = BigInt("4440088742263677654396177039706714734771352055402463232")
  private val expectedHeader566093Work = BigInt("26078778141331078011536")
  private val bitcoinPowLimit = BigInt("26959535291011309493156476344723991336010898738574164086137773096960")
  private val targetTimespanSeconds = 1209600L
  // Seed cumulative work carried by the parent tip of the mainnet fixtures, shared so the pinned
  // cumulative-work test and the fixtures that produce it cannot drift apart.
  private val fixtureSeedWork = new BigInteger("100500500500")

  private lazy val btcRelayTree = compileV6(btcRelayScript)
  private lazy val btcTxCheckTree = compileV6(btcTxCheckScript)
  private lazy val failingFoldTxCheckTree = compileTxCheckMutant(
    "val elemHash = proofElem.slice(1,33)",
    "val elemHash = Coll[Byte](proofElem(33))")
  private lazy val failingCoinbaseFoldTxCheckTree = compileTxCheckMutant(
    "val elemHash = proofElem.slice(1,33)",
    "val elemHash = if (prevHash == doubleSha256(coinbaseBytes)) Coll[Byte](proofElem(33)) else proofElem.slice(1,33)")

  property("BtcRelay compiles") {
    btcRelayTree.version shouldBe V6SoftForkVersion
  }

  property("BtcTxCheck compiles") {
    btcTxCheckTree.version shouldBe V6SoftForkVersion
  }

  property("BtcRelay accepts a header added to best chain") {
    val fixture = bestChainAppendFixture()

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe true
  }

  property("BtcRelay accepts a header added to non-best chain with switch") {
    val fixture = forkAppendFixture(bestChainWorkWins = true)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe true
  }

  property("BtcRelay accepts a header added to non-best chain without switch") {
    val fixture = forkAppendFixture(bestChainWorkWins = false)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe true
  }

  property("BtcRelay accepts a retarget-boundary header with anchor proof") {
    val fixture = retargetAppendFixture()

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe true
  }

  property("BtcRelay rejects a successor guarded by a different proposition") {
    val fixture = bestChainAppendFixture(outputErgoTree = btcTxCheckTree)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects a relay NFT quantity other than one") {
    val fixture = bestChainAppendFixture(relayTokens = ArraySeq((relayToken, 2L): Token))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects a different relay NFT id") {
    val wrongRelayToken = Digest32Coll @@ Array.fill(32)(1.toByte).toColl
    val fixture = bestChainAppendFixture(relayTokens = ArraySeq((wrongRelayToken, 1L): Token))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed best-chain AVL key length") {
    val fixture = bestChainAppendFixture(outputBestChainKeyLength = 31)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed best-chain AVL value length") {
    val fixture = bestChainAppendFixture(outputBestChainValueLengthOpt = Some(88))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed best-chain AVL operation flags") {
    val fixture = bestChainAppendFixture(outputBestChainFlags = AvlTreeFlags.AllOperationsAllowed)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed all-headers AVL key length") {
    val fixture = bestChainAppendFixture(outputAllHeadersKeyLength = 31)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed all-headers AVL value length") {
    val fixture = bestChainAppendFixture(outputAllHeadersValueLengthOpt = Some(128))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed all-headers AVL operation flags") {
    val fixture = bestChainAppendFixture(outputAllHeadersFlags = AvlTreeFlags.AllOperationsAllowed)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed parent-chain AVL operation flags") {
    val fixture = bestChainAppendFixture(parentChainFlags = AvlTreeFlags.AllOperationsAllowed)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects changed best-chain AVL metadata on branch switch") {
    val fixture = forkAppendFixture(bestChainWorkWins = true, outputBestChainKeyLength = 31)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects header hash above compact target") {
    val invalidHeader = invalidPowHeaderBytes()
    relayPowHitIsBelowTarget(invalidHeader) shouldBe false
    val fixture = bestChainAppendFixture(newHeaderBytes = invalidHeader)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects compact target above Bitcoin pow limit") {
    val invalidHeader = headerWithNBits(fromHex(mainnetHeader566093Hex), 0x1d010000L)
    targetFromHeader(invalidHeader) > bitcoinPowLimit shouldBe true
    val fixture = bestChainAppendFixture(newHeaderBytes = invalidHeader)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects non-positive compact target") {
    val invalidHeader = headerWithNBits(fromHex(mainnetHeader566093Hex), 0x01003456L)
    targetFromHeader(invalidHeader) shouldBe BigInt(0)
    val fixture = bestChainAppendFixture(newHeaderBytes = invalidHeader, declaredBlockWork = Some(BigInteger.ONE))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects nBits change before retarget boundary") {
    val invalidHeader = headerWithNBits(fromHex(mainnetHeader566093Hex), 0x1d00ffffL)
    val fixture = bestChainAppendFixture(newHeaderBytes = invalidHeader)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects retarget-boundary header with wrong anchor height") {
    val fixture = retargetAppendFixture(anchorHeight = 562465)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects retarget-boundary header with wrong anchor proof") {
    val fixture = retargetAppendFixture()

    relayProves(fixture.input, fixture.output,
      fixture.contextVars.updated(9.toByte, ByteArrayConstant(Array.fill(32)(1.toByte)))) shouldBe false
  }

  property("BtcRelay rejects malformed header length") {
    val invalidHeader = fromHex(mainnetHeader566093Hex) :+ 0.toByte
    val fixture = bestChainAppendFixture(newHeaderBytes = invalidHeader)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects a tip height inconsistent with its authenticated parent record") {
    // The successor and proofs remain parent-derived; only input R6 differs.
    val fixture = bestChainAppendFixture(inputTipHeight = Some(566091))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects a direct-tip successor derived from inconsistent tip work") {
    val inconsistentTipWork = fixtureSeedWork.add(BigInteger.ONE)
    val blockWork = blockWorkFromHeader(fromHex(mainnetHeader566093Hex)).bigInteger
    // Reproduce the former two-source behavior: output R8 follows SELF.R8 while the inserted
    // all-headers record follows the authenticated parent's cumulative work.
    val nextTipWork = inconsistentTipWork.add(blockWork)
    val fixture = bestChainAppendFixture(
      inputTipWork = Some(inconsistentTipWork),
      outputTipWork = Some(nextTipWork))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects tip work inconsistent with its authenticated parent record") {
    // The successor and proofs remain parent-derived; only input R8 differs.
    val fixture = bestChainAppendFixture(inputTipWork = Some(fixtureSeedWork.add(BigInteger.ONE)))

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay binds a direct-tip parent chain digest to SELF best chain") {
    val fixture = bestChainAppendFixture(divergentParentChainDigest = true)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false

    // Removing only the digest equality recreates the parent contract's acceptance behavior
    // (apart from a neutral trailing && true), so this is both an isolated guard check and a
    // regression showing that the pre-fix contract accepted the divergent state.
    val digestGuard = "parentChainDigest == bestChainDigest.digest"
    val firstGuardIndex = btcRelayScript.indexOf(digestGuard)
    firstGuardIndex should not be -1
    firstGuardIndex shouldBe btcRelayScript.lastIndexOf(digestGuard)
    val mutantTree = compileV6(btcRelayScript.replace(digestGuard, "true"))
    val mutantFixture = bestChainAppendFixture(
      divergentParentChainDigest = true,
      relayErgoTree = mutantTree,
      outputErgoTree = mutantTree)

    relayProves(mutantFixture.input, mutantFixture.output, mutantFixture.contextVars, mutantTree) shouldBe true
  }

  property("BtcRelay ignores a supplied cumulative work var") {
    val fixture = bestChainAppendFixture()

    relayProves(fixture.input, fixture.output,
      fixture.contextVars.updated(7.toByte, ByteArrayConstant(BigInteger.ONE.toByteArray))) shouldBe true
  }

  property("cumulative work serialization uses minimal signed big-endian at the sign-byte boundary") {
    new BigInteger("127").toByteArray shouldBe Array(0x7f.toByte)
    new BigInteger("128").toByteArray shouldBe Array(0x00.toByte, 0x80.toByte)
  }

  property("V6 BigInt.toBytes and byteArrayToBigInt satisfy the sign-byte vectors in-interpreter") {
    // The same vectors the JVM-level tests pin, asserted by the V6 interpreter itself: the relay
    // record encoding depends on BigInt.toBytes agreeing with BigInteger.toByteArray, sign byte
    // included, and on byteArrayToBigInt inverting it.
    val tree = compileV6(bigIntBytesScript)
    val box = testBox(relayBoxValue, tree, creationHeight = 0)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(box.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))),
      IndexedSeq.empty,
      IndexedSeq(box.toCandidate))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 0,
      lastBlockUtxoRoot = sigma.data.AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(box),
      tx,
      self = box,
      activatedVersion = V6SoftForkVersion)

    proveAndVerify(tree, ctx, Map.empty) shouldBe true
  }

  property("cumulative work of the mainnet fixture is pinned with its canonical bytes") {
    val h2Work = fixtureSeedWork.add(blockWorkFromHeader(fromHex(mainnetHeader566093Hex)).bigInteger)
    h2Work shouldBe new BigInteger("26078778141431578512036")
    h2Work.signum shouldBe 1
    val suffix = h2Work.toByteArray
    suffix shouldBe fromHex("0585bbbfd457faf84aa4")
    new BigInteger(suffix) shouldBe h2Work
    suffix.length should (be >= 1 and be <= 32)
  }

  property("a canonical cumulative-work record written by one transition is decoded as the next transition's parent") {
    val h2Bytes = fromHex(mainnetHeader566093Hex)
    val h3Bytes = fromHex(mainnetHeader566094Hex)

    // The added header certifies itself inside the suite: canonical hash, parent linkage to
    // 566093, unchanged difficulty period, and valid proof of work.
    Base16.encode(doubleSha256(h3Bytes).reverse) shouldBe mainnetHeader566094CanonicalHash
    h3Bytes.slice(4, 36) shouldBe doubleSha256(h2Bytes)
    h3Bytes.slice(72, 76) shouldBe h2Bytes.slice(72, 76)
    relayPowHitIsBelowTarget(h3Bytes) shouldBe true

    // 2^87 is exactly 0x80 followed by ten zero bytes, so its top magnitude byte is 0x80 and
    // BigInteger.toByteArray is forced to prepend a 0x00 sign byte. h1's seed work is derived
    // backwards from it, so h2's cumulative work still comes out of the real work calculation.
    // Do not replace this with a rounder value whose top byte is below 0x80: the sign byte
    // would vanish and the record encoding this test exists to pin would stop being exercised.
    val h2CumWork = BigInteger.ONE.shiftLeft(87)
    h2CumWork.signum shouldBe 1
    val h1CumWork = h2CumWork.subtract(blockWorkFromHeader(h2Bytes).bigInteger)
    h1CumWork.signum shouldBe 1

    val h1 = HeaderFixture(hex = mainnetHeader566092Hex, height = 566092, cumulativeWork = h1CumWork)
    val h2 = HeaderFixture(h2Bytes, h1.height + 1, h1.cumulativeWork.add(blockWorkFromHeader(h2Bytes).bigInteger))
    h2.cumulativeWork shouldBe h2CumWork

    val h2CumWorkBytes = h2CumWork.toByteArray
    h2CumWorkBytes(0) shouldBe 0x00.toByte
    (h2CumWorkBytes(1) & 0x80) shouldBe 0x80

    // transition 1: append h2 onto tip h1, inserting h2's record into the all-headers tree
    val bestChain1 = MutableAvl(Seq(h1.id -> h1.headerAndHeight), AvlTreeFlags.InsertOnly)
    val h1Record = allHeadersRecord(h1, bestChain1.tree.digest.toArray, h1.cumulativeWork)
    val allHeaders1 = MutableAvl(Seq(h1.id -> h1Record), AvlTreeFlags.InsertOnly)
    val h1LookupProof = allHeaders1.lookupProof(h1.id)

    val input1 = relayBox(bestChain1.tree, allHeaders1.tree, h1.height, h1.id, h1.cumulativeWork)
    val bestChain1Before = bestChain1.tree
    val bestInsertProof1 = bestChain1.insertProof(h2.id, h2.headerAndHeight)
    val bestChain1After = bestChain1.tree
    val h2Record = allHeadersRecord(h2, bestChain1After.digest.toArray, h2.cumulativeWork)
    val allHeadersInsertProof1 = allHeaders1.insertProof(h2.id, h2Record)
    val allHeaders1After = allHeaders1.tree
    val output1 = relayCandidate(bestChain1After, allHeaders1After, h2.height, h2.id, h2.cumulativeWork)

    relayProves(input1, output1, relayContextVars(h2.bytes, bestInsertProof1, h1LookupProof, bestChain1Before,
      bestInsertProof1, allHeadersInsertProof1)) shouldBe true

    // the committed record carries the canonical encoding, sign byte included: transition 1 can only
    // succeed if the contract's inserted bytes match this independently constructed record (AVL digest equality).
    h2Record.drop(121) shouldBe h2CumWorkBytes
    h2Record.length shouldBe 121 + h2CumWorkBytes.length
    h2Record.slice(0, 80) shouldBe h2.bytes
    h2Record.slice(80, 88) shouldBe longToBytes(h2.height.toLong)
    h2Record.slice(88, 121) shouldBe bestChain1After.digest.toArray
    bestChain1After.digest.toArray.length shouldBe 33

    // transition 2: replay both records into the parent state and append h3 onto tip h2
    val h3 = HeaderFixture(h3Bytes, h2.height + 1, h2.cumulativeWork.add(blockWorkFromHeader(h3Bytes).bigInteger))
    val bestChain2 = MutableAvl(Seq(h1.id -> h1.headerAndHeight, h2.id -> h2.headerAndHeight), AvlTreeFlags.InsertOnly)
    val allHeaders2 = MutableAvl(Seq(h1.id -> h1Record, h2.id -> h2Record), AvlTreeFlags.InsertOnly)

    // authenticated continuity: transition 2 starts from the exact state transition 1 committed to
    allHeaders2.tree.digest.toArray shouldBe allHeaders1After.digest.toArray
    bestChain2.tree.digest.toArray shouldBe bestChain1After.digest.toArray

    val h2LookupProof = allHeaders2.lookupProof(h2.id)
    val input2 = relayBox(bestChain2.tree, allHeaders2.tree, h2.height, h2.id, h2.cumulativeWork)
    val bestChain2Before = bestChain2.tree
    val bestInsertProof2 = bestChain2.insertProof(h3.id, h3.headerAndHeight)
    val bestChain2After = bestChain2.tree
    val allHeadersInsertProof2 = allHeaders2.insertProof(h3.id,
      allHeadersRecord(h3, bestChain2After.digest.toArray, h3.cumulativeWork))
    val output2 = relayCandidate(bestChain2After, allHeaders2.tree, h3.height, h3.id, h3.cumulativeWork)

    relayProves(input2, output2, relayContextVars(h3.bytes, bestInsertProof2, h2LookupProof, bestChain2Before,
      bestInsertProof2, allHeadersInsertProof2)) shouldBe true
  }

  property("a switch transition derives cumulative work from the decoded parent record, not the tip register") {
    val h2Bytes = fromHex(mainnetHeader566093Hex)
    val h3Bytes = fromHex(mainnetHeader566094Hex)

    // Same sign-byte-crossing anchor as the chained producer/consumer test: 2^87 is 0x80 followed by
    // ten zero bytes, so BigInteger.toByteArray must prepend a 0x00 sign byte. h1's work is derived
    // backwards from it, so h2's cumulative work still comes out of the real work calculation.
    val h2CumWork = BigInteger.ONE.shiftLeft(87)
    val h2CumWorkBytes = h2CumWork.toByteArray
    h2CumWorkBytes(0) shouldBe 0x00.toByte
    (h2CumWorkBytes(1) & 0x80) shouldBe 0x80

    val h1CumWork = h2CumWork.subtract(blockWorkFromHeader(h2Bytes).bigInteger)
    h1CumWork.signum shouldBe 1
    val h3CumWork = h2CumWork.add(blockWorkFromHeader(h3Bytes).bigInteger)

    // The competing tip's cumulative work has to sit strictly inside (h2CumWork, h3CumWork): above
    // h2's so appending h2 to the branch does not switch, below h3's so appending h3 does. That is
    // what makes the tip register and the decoded parent work disagree at the switch. 10^22 is about
    // 38% of one block's work here (~2.6 * 10^22), so it lands well inside the window rather than at
    // an edge, and the assertions below fail loudly if either bound ever drifts past it.
    val competingTipWork = h2CumWork.add(new BigInteger("10000000000000000000000"))
    (h2CumWork.compareTo(competingTipWork) < 0) shouldBe true
    (competingTipWork.compareTo(h3CumWork) < 0) shouldBe true

    val h0 = HeaderFixture(hex = forkHeaderHex, height = 566092, cumulativeWork = competingTipWork)
    val h1 = HeaderFixture(hex = mainnetHeader566092Hex, height = 566092, cumulativeWork = h1CumWork)
    val h2 = HeaderFixture(h2Bytes, h1.height + 1, h1.cumulativeWork.add(blockWorkFromHeader(h2Bytes).bigInteger))
    h2.cumulativeWork shouldBe h2CumWork
    val h3 = HeaderFixture(h3Bytes, h2.height + 1, h2.cumulativeWork.add(blockWorkFromHeader(h3Bytes).bigInteger))
    h3.cumulativeWork shouldBe h3CumWork

    // transition 1: append h2 to the non-best branch. Its cumulative work loses to the competing
    // tip, so the tip registers stay on h0 and only the all-headers tree grows.
    val tipChain1 = MutableAvl(Seq(h0.id -> h0.headerAndHeight), AvlTreeFlags.InsertOnly)
    val branchChain1 = MutableAvl(Seq(h1.id -> h1.headerAndHeight), AvlTreeFlags.InsertOnly)
    val h1Record = allHeadersRecord(h1, branchChain1.tree.digest.toArray, h1.cumulativeWork)
    val h0Record = allHeadersRecord(h0, tipChain1.tree.digest.toArray, h0.cumulativeWork)
    val allHeaders1 = MutableAvl(Seq(h1.id -> h1Record, h0.id -> h0Record), AvlTreeFlags.InsertOnly)
    val h1LookupProof = allHeaders1.lookupProof(h1.id)

    val input1 = relayBox(tipChain1.tree, allHeaders1.tree, h0.height, h0.id, h0.cumulativeWork)
    val branchChain1Before = branchChain1.tree
    val branchInsertProof1 = branchChain1.insertProof(h2.id, h2.headerAndHeight)
    val branchChain1After = branchChain1.tree
    val h2Record = allHeadersRecord(h2, branchChain1After.digest.toArray, h2.cumulativeWork)
    val allHeadersInsertProof1 = allHeaders1.insertProof(h2.id, h2Record)
    val allHeaders1After = allHeaders1.tree
    val output1 = relayCandidate(tipChain1.tree, allHeaders1After, h0.height, h0.id, h0.cumulativeWork)

    // var 2 carries the branch insert proof as a placeholder: prevBlockId is h1, the tip is h0, so
    // the contract takes the else-arm of validTipUpdate and never evaluates it.
    relayProves(input1, output1, relayContextVars(h2.bytes, branchInsertProof1, h1LookupProof,
      branchChain1Before, branchInsertProof1, allHeadersInsertProof1)) shouldBe true

    h2Record.drop(121) shouldBe h2CumWorkBytes

    // transition 2's parent state is transition 1's post-state, replayed insert for insert
    val allHeaders2 = MutableAvl(
      Seq(h1.id -> h1Record, h0.id -> h0Record, h2.id -> h2Record), AvlTreeFlags.InsertOnly)
    val branchChain2 = MutableAvl(
      Seq(h1.id -> h1.headerAndHeight, h2.id -> h2.headerAndHeight), AvlTreeFlags.InsertOnly)
    val tipChain2 = MutableAvl(Seq(h0.id -> h0.headerAndHeight), AvlTreeFlags.InsertOnly)

    allHeaders2.tree.digest.toArray shouldBe allHeaders1After.digest.toArray
    branchChain2.tree.digest.toArray shouldBe branchChain1After.digest.toArray
    tipChain2.tree.digest.toArray shouldBe tipChain1.tree.digest.toArray

    // transition 2: append h3 to the branch. The tip register still says h0 / competingTipWork, so
    // the only source for the new cumulative work is the record transition 1 wrote for h2. The
    // inserted row height must likewise come from h2's authenticated record, not the unrelated tip.
    (h0.height + 1) should not be h3.height
    val h3BranchRow = h3.headerAndHeight
    val h2LookupProof = allHeaders2.lookupProof(h2.id)
    val input2 = relayBox(tipChain2.tree, allHeaders2.tree, h0.height, h0.id, h0.cumulativeWork)
    val branchChain2Before = branchChain2.tree
    val branchInsertProof2 = branchChain2.insertProof(h3.id, h3BranchRow)
    val branchChain2After = branchChain2.tree
    val allHeadersInsertProof2 = allHeaders2.insertProof(h3.id,
      allHeadersRecord(h3, branchChain2After.digest.toArray, h3.cumulativeWork))
    val allHeaders2After = allHeaders2.tree
    val contextVars2 = relayContextVars(h3.bytes, branchInsertProof2, h2LookupProof, branchChain2Before,
      branchInsertProof2, allHeadersInsertProof2)

    val output2 = relayCandidate(branchChain2After, allHeaders2After, h3.height, h3.id, h3.cumulativeWork)
    relayProves(input2, output2, contextVars2) shouldBe true
    val switchedTipRow = branchChain2.lookupValue(h3.id)
    switchedTipRow shouldBe h3.headerAndHeight
    byteArrayToLong(switchedTipRow.slice(80, 88)) shouldBe
      output2.additionalRegisters(R6).value.asInstanceOf[Int].toLong

    // Negative control: what a regression substituting the tip register for the decoded parent work
    // would produce. Everything else about the transition is identical, so the only check that can
    // reject it is R8 == parentCumWork + work. Input state, proofs and context vars are reused as-is:
    // relayProves builds a fresh prover and verifier per call and never touches the AVL provers, and
    // no further insert is replayed here.
    val tipDerivedWork = h0.cumulativeWork.add(blockWorkFromHeader(h3Bytes).bigInteger)
    (tipDerivedWork.compareTo(h3.cumulativeWork) != 0) shouldBe true
    val regressedOutput = relayCandidate(branchChain2After, allHeaders2After, h3.height, h3.id, tipDerivedWork)
    relayProves(input2, regressedOutput, contextVars2) shouldBe false
  }

  property("BtcRelay rejects switching to a lower-work branch") {
    val fixture = forkAppendFixture(bestChainWorkWins = false, forceSwitchOutput = true)

    relayProves(fixture.input, fixture.output, fixture.contextVars) shouldBe false
  }

  property("BtcRelay rejects wrong parent header proof") {
    val fixture = bestChainAppendFixture()
    val wrongProof = Array.fill(32)(1.toByte)

    relayProves(fixture.input, fixture.output, fixture.contextVars.updated(3.toByte, ByteArrayConstant(wrongProof))) shouldBe false
  }

  property("BtcTxCheck validates an odd-count Bitcoin tx Merkle proof with seven confirmations") {
    val fixture = txCheckFixture()

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe true
  }

  property("BtcTxCheck rejects wrong Merkle proof") {
    val fixture = txCheckFixture()
    val proof = fixture.merkleProof.map(_.toArray)
    proof(0)(1) = (proof(0)(1) ^ 1).toByte
    val wrongProof = CollectionConstant[SCollection[SByte.type]](proof.map(_.toColl).toColl, SCollection(SByte))

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars.updated(4.toByte, wrongProof)) shouldBe false
  }

  property("BtcTxCheck rejects Merkle proof with invalid direction flag") {
    val fixture = txCheckFixture()
    val proof = fixture.merkleProof.map(_.toArray)
    proof(0)(0) = 2.toByte
    val wrongProof = CollectionConstant[SCollection[SByte.type]](proof.map(_.toColl).toColl, SCollection(SByte))

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars.updated(4.toByte, wrongProof)) shouldBe false
  }

  property("BtcTxCheck rejects Merkle proof level with extra bytes") {
    val fixture = txCheckFixture()
    val proof = fixture.merkleProof.map(_.toArray)
    proof(0) = proof(0) :+ 0.toByte
    val wrongProof = CollectionConstant[SCollection[SByte.type]](proof.map(_.toColl).toColl, SCollection(SByte))

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars.updated(4.toByte, wrongProof)) shouldBe false
  }

  property("BtcTxCheck rejects a payment proof deeper than the authenticated coinbase proof") {
    val fixture = leafInternalAmbiguityFixture()

    fixture.merkleProof.length shouldBe fixture.coinbaseProof.length + 1
    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant("merkleProof.size == coinbaseProof.size", "true")
    val mutantFixture = leafInternalAmbiguityFixture(mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a wrong coinbase Merkle proof") {
    val fixture = txCheckFixture()
    val proof = fixture.coinbaseProof.map(_.toArray)
    proof(0)(1) = (proof(0)(1) ^ 1).toByte
    val wrongProof = proofConstant(proof.map(_.toColl))

    txCheckProves(
      fixture.input,
      fixture.relayDataInput,
      fixture.contextVars.updated(6.toByte, wrongProof)) shouldBe false

    val mutantTree = compileTxCheckMutant("computedCoinbaseRoot == merkleRootBytes", "true")
    val mutantFixture = txCheckFixture(txCheckErgoTree = mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars.updated(6.toByte, wrongProof),
      mutantTree) shouldBe true
  }

  property("BtcTxCheck requires the coinbase depth proof to identify leaf zero") {
    val base = txCheckFixture()
    val coinbaseId = doubleSha256(base.coinbaseBytes)
    val targetProof = Array((1.toByte +: coinbaseId).toColl)
    val wrongDirectionProof = Array((0.toByte +: coinbaseId).toColl)
    val fixture = txCheckFixtureForProofs(
      base.coinbaseBytes,
      targetProof,
      base.coinbaseBytes,
      wrongDirectionProof)

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant(
      "proofElem.size == 33 && proofElem(0) == 1",
      "proofElem.size == 33 && (proofElem(0) == 0 || proofElem(0) == 1)")
    val mutantFixture = txCheckFixtureForProofs(
      base.coinbaseBytes,
      targetProof,
      base.coinbaseBytes,
      wrongDirectionProof,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects malformed coinbase proof levels") {
    val fixture = txCheckFixture()
    val proof = fixture.coinbaseProof.map(_.toArray)
    proof(0) = proof(0) :+ 0.toByte
    val malformedProof = proofConstant(proof.map(_.toColl))

    txCheckProves(
      fixture.input,
      fixture.relayDataInput,
      fixture.contextVars.updated(6.toByte, malformedProof)) shouldBe false

    val mutantTree = compileTxCheckMutant(
      "proofElem.size == 33 && proofElem(0) == 1",
      "proofElem.size >= 33 && proofElem(0) == 1")
    val mutantFixture = txCheckFixture(txCheckErgoTree = mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars.updated(6.toByte, malformedProof),
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a 64-byte coinbase depth witness") {
    val coinbaseBytes = ambiguousCoinbase64ByteTx()
    val fixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty)

    coinbaseBytes.length shouldBe 64
    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant("coinbaseBytes.size != 64", "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a truncated coinbase depth witness") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes.take(41)
    val fixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty)

    coinbaseBytes.length shouldBe 41
    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant("coinbaseBytes.size >= 42", "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a depth witness with a nonzero coinbase prevout hash") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes.clone()
    coinbaseBytes(5) = 1.toByte
    val fixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty)

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant(
      "coinbaseBytes.slice(5, 37) == fromBase16(\"0000000000000000000000000000000000000000000000000000000000000000\")",
      "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a depth witness with a non-coinbase prevout index") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes.clone()
    coinbaseBytes(37) = 0xfe.toByte
    val fixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty)

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant(
      "coinbaseBytes.slice(37, 41) == fromBase16(\"ffffffff\")",
      "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects a depth witness with more than one coinbase input") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes.clone()
    coinbaseBytes(4) = 2.toByte
    val fixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty)

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant("coinbaseBytes(4) == 1", "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      Array.empty,
      coinbaseBytes,
      Array.empty,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck rejects Merkle paths deeper than the bounded verifier profile") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes
    val proof = selfDuplicatingProof(coinbaseBytes, depth = 33)
    val fixture = txCheckFixtureForProofs(coinbaseBytes, proof, coinbaseBytes, proof)

    proof.length shouldBe 33
    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false

    val mutantTree = compileTxCheckMutant("merkleProof.size <= maxMerkleDepth", "true")
    val mutantFixture = txCheckFixtureForProofs(
      coinbaseBytes,
      proof,
      coinbaseBytes,
      proof,
      mutantTree)
    txCheckProves(
      mutantFixture.input,
      mutantFixture.relayDataInput,
      mutantFixture.contextVars,
      mutantTree) shouldBe true
  }

  property("BtcTxCheck bounds proof work before evaluating over-limit paths") {
    val fixture = txCheckFixture(txCheckErgoTree = failingFoldTxCheckTree)
    val overLimitProof = selfDuplicatingProof(fixture.coinbaseBytes, depth = 33)
    val contextVars = fixture.contextVars
      .updated(4.toByte, proofConstant(overLimitProof))
      .updated(6.toByte, proofConstant(overLimitProof))

    val result = txCheckVerification(
      fixture.input,
      fixture.relayDataInput,
      contextVars,
      failingFoldTxCheckTree)

    result.isSuccess shouldBe true
    result.get._1 shouldBe false
  }

  property("BtcTxCheck rejects mismatched depth before evaluating the coinbase path") {
    val fixture = txCheckFixture(txCheckErgoTree = failingCoinbaseFoldTxCheckTree)
    val shorterCoinbaseProof = fixture.coinbaseProof.take(1)
    val contextVars = fixture.contextVars
      .updated(6.toByte, proofConstant(shorterCoinbaseProof))

    val result = txCheckVerification(
      fixture.input,
      fixture.relayDataInput,
      contextVars,
      failingCoinbaseFoldTxCheckTree)

    result.isSuccess shouldBe true
    result.get._1 shouldBe false
  }

  property("BtcTxCheck accepts Merkle paths at the bounded verifier profile limit") {
    val coinbaseBytes = txCheckFixture().coinbaseBytes
    val proof = selfDuplicatingProof(coinbaseBytes, depth = 32)
    val fixture = txCheckFixtureForProofs(coinbaseBytes, proof, coinbaseBytes, proof)

    proof.length shouldBe 32
    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe true
  }

  property("BtcTxCheck rejects a missing authenticated-depth witness") {
    val fixture = txCheckFixture()

    txCheckProves(
      fixture.input,
      fixture.relayDataInput,
      fixture.contextVars - 5.toByte) shouldBe false
    txCheckProves(
      fixture.input,
      fixture.relayDataInput,
      fixture.contextVars - 6.toByte) shouldBe false
  }

  property("BtcTxCheck rejects six conventional confirmations") {
    val fixture = txCheckFixture(tipHeight = bitcoinHeaderHeight + 5)

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false
  }

  property("BtcTxCheck demonstrates the confirmation impact of an understated R4 header height") {
    val correctHeight = txCheckFixture(
      tipHeight = bitcoinHeaderHeight + 5,
      headerHeight = bitcoinHeaderHeight)
    val understatedHeight = txCheckFixture(
      tipHeight = bitcoinHeaderHeight + 5,
      headerHeight = bitcoinHeaderHeight - 1)

    txCheckProves(correctHeight.input, correctHeight.relayDataInput, correctHeight.contextVars) shouldBe false
    // Consumer-side impact demonstration only: this fixture constructs R4 directly. The chained
    // relay switch test above is the regression that discriminates the corrected row producer.
    txCheckProves(understatedHeight.input, understatedHeight.relayDataInput,
      understatedHeight.contextVars) shouldBe true
  }

  property("BtcTxCheck rejects a relay NFT quantity other than one") {
    val fixture = txCheckFixture(relayTokens = ArraySeq((relayToken, 2L): Token))

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars) shouldBe false
  }

  property("BtcTxCheck rejects wrong transaction bytes") {
    val fixture = txCheckFixture()
    val txBytes = fixture.txBytes.clone()
    txBytes(0) = (txBytes(0) ^ 1).toByte

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars.updated(1.toByte, ByteArrayConstant(txBytes))) shouldBe false
  }

  property("BtcTxCheck rejects wrong header id") {
    val fixture = txCheckFixture()
    val headerId = fixture.headerId.clone()
    headerId(0) = (headerId(0) ^ 1).toByte

    txCheckProves(fixture.input, fixture.relayDataInput, fixture.contextVars.updated(2.toByte, ByteArrayConstant(headerId))) shouldBe false
  }

  property("BtcTxCheck rejects display-order header id instead of the relay key order") {
    val fixture = txCheckFixture()
    val displayOrderHeaderId = fixture.headerId.reverse

    txCheckProves(fixture.input, fixture.relayDataInput,
      fixture.contextVars.updated(2.toByte, ByteArrayConstant(displayOrderHeaderId))) shouldBe false
  }

  property("BtcTxCheck real fixture uses the relay header id byte order") {
    val fixture = txCheckFixture()
    val header = fromHex("01000000076379e2c0ec4a614ad1bf0ec716e6873f2c7abac604a08cc78e070000000000579a6bbcd07e9c3d622672ad20495d4485b5233395ab4081db7cab0fd2b577d2396cec4c2a8b091b031a7313")
    val displayOrderHeaderId = fromHex("000000000003b8e6533b3f238ee00ff8dd68c3a2377a213f7a72c3ef0fe0c54b")

    fixture.headerId.sameElements(doubleSha256(header)) shouldBe true
    fixture.headerId.reverse.sameElements(displayOrderHeaderId) shouldBe true
  }

  property("relay work calculation is pinned for the mainnet header fixture") {
    val header = fromHex(mainnetHeader566093Hex)

    targetFromHeader(header) shouldBe expectedHeader566093Target
    blockWorkFromHeader(header) shouldBe expectedHeader566093Work
  }

  property("relay header id byte order matches Bitcoin serialized parent links") {
    val parent = fromHex(mainnetHeader566092Hex)
    val child = fromHex(mainnetHeader566093Hex)
    val parentRelayId = doubleSha256(parent)

    child.slice(4, 36).sameElements(parentRelayId) shouldBe true
    child.slice(4, 36).sameElements(parentRelayId.reverse) shouldBe false
  }

  property("relay retarget calculation is pinned for the mainnet boundary fixture") {
    val anchor = fromHex(mainnetHeader562464Hex)
    val parent = fromHex(mainnetHeader564479Hex)
    val next = fromHex(mainnetHeader564480Hex)

    nBitsFromHeader(anchor) shouldBe 0x172e6f88L
    nBitsFromHeader(parent) shouldBe 0x172e6f88L
    nBitsFromHeader(next) shouldBe 0x172e5b50L
    timestampFromHeader(parent) - timestampFromHeader(anchor) shouldBe 1207543L
    expectedRetargetNBits(parent, anchor) shouldBe nBitsFromHeader(next)
  }

  private def compileV6(script: String): ErgoTree =
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion) {
      ErgoTree.fromProposition(ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
        compile(Map.empty, script).asBoolValue.toSigmaProp)
    }

  private def relayProves(
      relayInput: ErgoBox,
      relayOutput: ErgoBoxCandidate,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]],
      relayTree: ErgoTree = btcRelayTree): Boolean = {
    val contextExtension = ContextExtension(contextVars)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(relayInput.id, ProverResult(Array.emptyByteArray, contextExtension))),
      IndexedSeq.empty,
      IndexedSeq(relayOutput))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 0,
      lastBlockUtxoRoot = sigma.data.AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(relayInput),
      tx,
      self = relayInput,
      activatedVersion = V6SoftForkVersion)

    proveAndVerify(relayTree, ctx, contextVars)
  }

  private def txCheckProves(
      txCheckInput: ErgoBox,
      relayDataInput: ErgoBox,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]],
      txCheckTree: ErgoTree = btcTxCheckTree): Boolean = {
    val contextExtension = ContextExtension(contextVars)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(txCheckInput.id, ProverResult(Array.emptyByteArray, contextExtension))),
      IndexedSeq(DataInput(relayDataInput.id)),
      IndexedSeq(txCheckInput.toCandidate))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 0,
      lastBlockUtxoRoot = sigma.data.AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(relayDataInput),
      boxesToSpend = IndexedSeq(txCheckInput),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersion = V6SoftForkVersion)

    proveAndVerify(txCheckTree, ctx, contextVars)
  }

  private def txCheckVerification(
      txCheckInput: ErgoBox,
      relayDataInput: ErgoBox,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]],
      txCheckTree: ErgoTree): scala.util.Try[(Boolean, Long)] = {
    val contextExtension = ContextExtension(contextVars)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(txCheckInput.id, ProverResult(Array.emptyByteArray, contextExtension))),
      IndexedSeq(DataInput(relayDataInput.id)),
      IndexedSeq(txCheckInput.toCandidate))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 0,
      lastBlockUtxoRoot = sigma.data.AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(relayDataInput),
      boxesToSpend = IndexedSeq(txCheckInput),
      spendingTransaction = tx,
      selfIndex = 0,
      activatedVersion = V6SoftForkVersion)

    new ErgoLikeTestInterpreter().verify(
      txCheckTree,
      ctx,
      ProverResult(Array.emptyByteArray, contextExtension),
      fakeMessage)
  }

  private def compileTxCheckMutant(original: String, replacement: String): ErgoTree = {
    val firstIndex = btcTxCheckScript.indexOf(original)
    firstIndex should not be -1
    firstIndex shouldBe btcTxCheckScript.lastIndexOf(original)
    compileV6(btcTxCheckScript.replace(original, replacement))
  }

  private def proveAndVerify(
      tree: ErgoTree,
      ctx: ErgoLikeContext,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]]): Boolean = {
    val prover = contextVars.foldLeft(new ContextEnrichingTestProvingInterpreter()) {
      case (p, (id, value)) => p.withContextExtender(id, value)
    }
    val verifier = new ErgoLikeTestInterpreter
    prover.prove(tree, ctx, fakeMessage).toOption.exists { pr =>
      verifier.verify(tree, ctx.withExtension(pr.extension), pr, fakeMessage).get._1
    }
  }

  private case class RelayFixture(
      input: ErgoBox,
      output: ErgoBoxCandidate,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]])

  private case class TxCheckFixture(
      input: ErgoBox,
      relayDataInput: ErgoBox,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]],
      merkleProof: Array[Coll[Byte]],
      coinbaseBytes: Array[Byte],
      coinbaseProof: Array[Coll[Byte]],
      txBytes: Array[Byte],
      headerId: Array[Byte])

  private def bestChainAppendFixture(
      newHeaderBytes: Array[Byte] = fromHex(mainnetHeader566093Hex),
      declaredBlockWork: Option[BigInteger] = None,
      relayTokens: ArraySeq[Token] = canonicalRelayTokens,
      outputErgoTree: ErgoTree = btcRelayTree,
      outputBestChainFlags: AvlTreeFlags = AvlTreeFlags.InsertOnly,
      outputBestChainKeyLength: Int = 32,
      outputBestChainValueLengthOpt: Option[Int] = None,
      outputAllHeadersFlags: AvlTreeFlags = AvlTreeFlags.InsertOnly,
      outputAllHeadersKeyLength: Int = 32,
      outputAllHeadersValueLengthOpt: Option[Int] = None,
      parentChainFlags: AvlTreeFlags = AvlTreeFlags.InsertOnly,
      inputTipHeight: Option[Int] = None,
      inputTipWork: Option[BigInteger] = None,
      outputTipWork: Option[BigInteger] = None,
      divergentParentChainDigest: Boolean = false,
      relayErgoTree: ErgoTree = btcRelayTree): RelayFixture = {
    val h1 = HeaderFixture(
      hex = mainnetHeader566092Hex,
      height = 566092,
      cumulativeWork = fixtureSeedWork)
    val h2Work = declaredBlockWork.getOrElse(blockWorkFromHeader(newHeaderBytes).bigInteger)
    val h2 = HeaderFixture(
      bytes = newHeaderBytes,
      height = h1.height + 1,
      cumulativeWork = h1.cumulativeWork.add(h2Work))

    val bestChain = MutableAvl(Seq(h1.id -> h1.headerAndHeight), AvlTreeFlags.InsertOnly)
    // The divergent case keeps both AVL proofs valid while making the parent record's committed
    // chain digest disagree with SELF.R4, isolating the direct-tip coherence check.
    val parentChain = if (divergentParentChainDigest) {
      val unrelatedHeader = HeaderFixture(forkHeaderHex, h1.height, h1.cumulativeWork)
      MutableAvl(Seq(
        h1.id -> h1.headerAndHeight,
        unrelatedHeader.id -> unrelatedHeader.headerAndHeight), AvlTreeFlags.InsertOnly)
    } else {
      bestChain
    }
    val allHeaders = MutableAvl(Seq(
      h1.id -> allHeadersRecord(h1, parentChain.tree.digest.toArray, h1.cumulativeWork)), AvlTreeFlags.InsertOnly)
    val parentLookupProof = allHeaders.lookupProof(h1.id)

    val input = relayBox(bestChain.tree, allHeaders.tree, inputTipHeight.getOrElse(h1.height), h1.id,
      inputTipWork.getOrElse(h1.cumulativeWork),
      relayTokens = relayTokens,
      ergoTree = relayErgoTree)
    val parentChainBefore = parentChain.tree
    val bestInsertProof = bestChain.insertProof(h2.id, h2.headerAndHeight)
    val bestChainAfter = bestChain.tree
    val parentChainInsertProof = if (divergentParentChainDigest) {
      parentChain.insertProof(h2.id, h2.headerAndHeight)
    } else {
      bestInsertProof
    }
    val parentChainAfter = if (divergentParentChainDigest) parentChain.tree else bestChainAfter
    val allHeadersInsertProof = allHeaders.insertProof(h2.id,
      allHeadersRecord(h2, parentChainAfter.digest.toArray, h2.cumulativeWork))
    val outputBestChain = CAvlTree(bestChainAfter.treeData.copy(
      treeFlags = outputBestChainFlags,
      keyLength = outputBestChainKeyLength,
      valueLengthOpt = outputBestChainValueLengthOpt))
    val outputAllHeaders = CAvlTree(allHeaders.tree.treeData.copy(
      treeFlags = outputAllHeadersFlags,
      keyLength = outputAllHeadersKeyLength,
      valueLengthOpt = outputAllHeadersValueLengthOpt))
    val parentChainProvided = CAvlTree(parentChainBefore.treeData.copy(treeFlags = parentChainFlags))
    val output = relayCandidate(outputBestChain, outputAllHeaders, h2.height, h2.id,
      outputTipWork.getOrElse(h2.cumulativeWork),
      relayTokens = relayTokens, ergoTree = outputErgoTree)

    RelayFixture(input, output, relayContextVars(
      h2.bytes, bestInsertProof, parentLookupProof, parentChainProvided, parentChainInsertProof,
      allHeadersInsertProof))
  }

  private def forkAppendFixture(
      bestChainWorkWins: Boolean,
      forceSwitchOutput: Boolean = false,
      outputBestChainKeyLength: Int = 32): RelayFixture = {
    val h0 = HeaderFixture(
      hex = forkHeaderHex,
      height = 566092,
      cumulativeWork = if (bestChainWorkWins) fixtureSeedWork else new BigInteger("100500500500000000000000"))
    val h1 = HeaderFixture(
      hex = mainnetHeader566092Hex,
      height = 566092,
      cumulativeWork = new BigInteger("100500500600"))
    val h2Bytes = fromHex(mainnetHeader566093Hex)
    val h2 = HeaderFixture(h2Bytes, h1.height + 1, h1.cumulativeWork.add(blockWorkFromHeader(h2Bytes).bigInteger))

    val h0BestChain = MutableAvl(Seq(h0.id -> h0.headerAndHeight), AvlTreeFlags.InsertOnly)
    val h1BestChain = MutableAvl(Seq(h1.id -> h1.headerAndHeight), AvlTreeFlags.InsertOnly)
    val allHeaders = MutableAvl(Seq(
      h1.id -> allHeadersRecord(h1, h1BestChain.tree.digest.toArray, h1.cumulativeWork),
      h0.id -> allHeadersRecord(h0, h0BestChain.tree.digest.toArray, h0.cumulativeWork)), AvlTreeFlags.InsertOnly)
    val parentLookupProof = allHeaders.lookupProof(h1.id)

    val input = relayBox(h0BestChain.tree, allHeaders.tree, h0.height, h0.id, h0.cumulativeWork)
    val h1BestChainBefore = h1BestChain.tree
    val h1BestInsertProof = h1BestChain.insertProof(h2.id, h2.headerAndHeight)
    val h1BestChainAfter = h1BestChain.tree
    val allHeadersInsertProof = allHeaders.insertProof(h2.id, allHeadersRecord(h2, h1BestChainAfter.digest.toArray, h2.cumulativeWork))

    val selectedBestChain = if (bestChainWorkWins || forceSwitchOutput) h1BestChainAfter else h0BestChain.tree
    val outputBestChain = CAvlTree(selectedBestChain.treeData.copy(keyLength = outputBestChainKeyLength))
    val output =
      if (bestChainWorkWins || forceSwitchOutput) relayCandidate(outputBestChain, allHeaders.tree, h2.height, h2.id, h2.cumulativeWork)
      else relayCandidate(outputBestChain, allHeaders.tree, h0.height, h0.id, h0.cumulativeWork)

    RelayFixture(input, output, relayContextVars(h2.bytes, h1BestInsertProof, parentLookupProof, h1BestChainBefore,
      h1BestInsertProof, allHeadersInsertProof))
  }

  private def retargetAppendFixture(anchorHeight: Int = 562464): RelayFixture = {
    val anchor = HeaderFixture(
      hex = mainnetHeader562464Hex,
      height = anchorHeight,
      cumulativeWork = BigInteger.ZERO)
    val parent = HeaderFixture(
      hex = mainnetHeader564479Hex,
      height = 564479,
      cumulativeWork = fixtureSeedWork)
    val nextBytes = fromHex(mainnetHeader564480Hex)
    val next = HeaderFixture(
      bytes = nextBytes,
      height = parent.height + 1,
      cumulativeWork = parent.cumulativeWork.add(blockWorkFromHeader(nextBytes).bigInteger))

    val bestChain = MutableAvl(Seq(
      anchor.id -> anchor.headerAndHeight,
      parent.id -> parent.headerAndHeight), AvlTreeFlags.InsertOnly)
    val allHeaders = MutableAvl(Seq(parent.id -> allHeadersRecord(parent, bestChain.tree.digest.toArray, parent.cumulativeWork)),
      AvlTreeFlags.InsertOnly)
    val parentLookupProof = allHeaders.lookupProof(parent.id)

    val input = relayBox(bestChain.tree, allHeaders.tree, parent.height, parent.id, parent.cumulativeWork)
    val bestChainBefore = bestChain.tree
    val retargetAnchorProof = bestChain.lookupProof(anchor.id)
    val bestInsertProof = bestChain.insertProof(next.id, next.headerAndHeight)
    val bestChainAfter = bestChain.tree
    val allHeadersInsertProof = allHeaders.insertProof(next.id, allHeadersRecord(next, bestChainAfter.digest.toArray, next.cumulativeWork))
    val output = relayCandidate(bestChainAfter, allHeaders.tree, next.height, next.id, next.cumulativeWork)

    RelayFixture(input, output, relayContextVars(next.bytes, bestInsertProof, parentLookupProof, bestChainBefore, bestInsertProof,
      allHeadersInsertProof, retargetAnchorId = Some(anchor.id), retargetAnchorProof = Some(retargetAnchorProof)))
  }

  private def txCheckFixture(
      tipHeight: Int = bitcoinHeaderHeight + 6,
      headerHeight: Int = bitcoinHeaderHeight,
      relayTokens: ArraySeq[Token] = canonicalRelayTokens,
      txCheckErgoTree: ErgoTree = btcTxCheckTree): TxCheckFixture = {
    val header = fromHex("01000000076379e2c0ec4a614ad1bf0ec716e6873f2c7abac604a08cc78e070000000000579a6bbcd07e9c3d622672ad20495d4485b5233395ab4081db7cab0fd2b577d2396cec4c2a8b091b031a7313")
    // BtcRelay keys both authenticated trees by the raw double-SHA256 bytes. Reversal is
    // only for conventional display and must not cross this producer-consumer boundary.
    val headerId = doubleSha256(header)
    val headerAndHeight = header ++ longToBytes(headerHeight.toLong)
    val bestChain = MutableAvl(Seq(headerId -> headerAndHeight), AvlTreeFlags.InsertOnly)
    val headerProof = bestChain.lookupProof(headerId)

    val txBytes = fromHex("0100000001eba8353ac2e5503f15548975108013246457ed83d331db760f0595b8bd7c54cb000000008c4930460221008c64f29882d9a59cbb070d75b4cdca56c04b523b0af37a0ffecee24e31cb2814022100b183ab317ad217f4a6f4e610c6138e5c2d7681d40f46201f268a5a90c1c07afa0141040b362c040204c13f6e1ec78b60978bdd76d851d4a1612cd9e82ead5177694f8f37fa4e8c78579876bbaf8a561772f320d3125f36cd1f1c5e9eb3f8bc08b626d2ffffffff0280e9fd97000000001976a914f0630fd41ff0722cf29de4db609f06a4c17fad2d88ac002a7515000000001976a9141dea9e37227b8d7a6296849fc76e00e8f5a6674e88ac00000000")
    val txId = doubleSha256(txBytes)

    val tx1 = fromHex("a7c2b4a2cc940f9f541905048fe8352bd158dab18d15221fab7ee2187bd3cb5e")
    val tx2 = fromHex("1d74396699ae0effcd67fd5d031b780ff56c336bfc5d2d015d21db687d732764")
    val tx3Id = fromHex("d8c9d6a13a7fb8236833b1e93d298f4626deeb78b2f1814aa9a779961c08ce39")
    // This block has three transactions, so the first Merkle level pairs the
    // proven transaction with its own duplicate per Bitcoin's odd-count rule.
    // That edge case is covered here, without claiming full mutated-tree defense.
    val merkleProof = Array(
      (1.toByte +: tx3Id.reverse).toColl,
      (0.toByte +: doubleSha256(tx1.reverse ++ tx2.reverse)).toColl)
    val coinbaseBytes = fromHex(
      "01000000010000000000000000000000000000000000000000000000000000000000000000" +
      "ffffffff08042a8b091b025e3cffffffff0100f2052a01000000434104d77816ded32ccc56fa" +
      "d6f455676c07908da96a37a7b9d2fd510cd4ddd92f3104f3d6e7134bd159fed3741522265" +
      "a901d44ec2ab428231c0e4986c52a22f13577ac00000000")
    val coinbaseProof = Array(
      (1.toByte +: tx2.reverse).toColl,
      (1.toByte +: doubleSha256(tx3Id.reverse ++ tx3Id.reverse)).toColl)

    doubleSha256(coinbaseBytes).sameElements(tx1.reverse) shouldBe true
    coinbaseProof.length shouldBe merkleProof.length

    val relayDataInput = relayBox(bestChain.tree, bestChain.tree, tipHeight, headerId, BigInteger.ZERO,
      transactionId = ModifierId @@ Base16.encode(txId), relayTokens = relayTokens)
    val txCheckInput = testBox(
      relayBoxValue,
      txCheckErgoTree,
      creationHeight = 0,
      transactionId = ModifierId @@ Base16.encode(txId.reverse),
      boxIndex = 0)

    val contextVars: Map[Byte, EvaluatedValue[_ <: SType]] = Map(
      1.toByte -> ByteArrayConstant(txBytes),
      2.toByte -> ByteArrayConstant(headerId),
      3.toByte -> ByteArrayConstant(headerProof),
      4.toByte -> CollectionConstant[SCollection[SByte.type]](merkleProof.toColl, SCollection(SByte)),
      5.toByte -> ByteArrayConstant(coinbaseBytes),
      6.toByte -> CollectionConstant[SCollection[SByte.type]](coinbaseProof.toColl, SCollection(SByte)))

    TxCheckFixture(txCheckInput, relayDataInput, contextVars, merkleProof, coinbaseBytes,
      coinbaseProof, txBytes, headerId)
  }

  private def leafInternalAmbiguityFixture(
      txCheckErgoTree: ErgoTree = btcTxCheckTree): TxCheckFixture = {
    val base = txCheckFixture()
    val paymentTxId = doubleSha256(base.txBytes)
    val internalSibling = Array.fill(32)(0x42.toByte)
    val ambiguousLeaf = paymentTxId ++ internalSibling
    val ambiguousLeafId = doubleSha256(ambiguousLeaf)
    val coinbaseId = doubleSha256(base.coinbaseBytes)
    val paymentProof = Array(
      (1.toByte +: internalSibling).toColl,
      (0.toByte +: coinbaseId).toColl)
    val coinbaseProof = Array((1.toByte +: ambiguousLeafId).toColl)

    ambiguousLeaf.length shouldBe 64
    merkleRootFromProof(base.txBytes, paymentProof)
      .sameElements(merkleRootFromProof(base.coinbaseBytes, coinbaseProof)) shouldBe true

    txCheckFixtureForProofs(
      base.txBytes,
      paymentProof,
      base.coinbaseBytes,
      coinbaseProof,
      txCheckErgoTree)
  }

  private def txCheckFixtureForProofs(
      txBytes: Array[Byte],
      merkleProof: Array[Coll[Byte]],
      coinbaseBytes: Array[Byte],
      coinbaseProof: Array[Coll[Byte]],
      txCheckErgoTree: ErgoTree = btcTxCheckTree): TxCheckFixture = {
    val merkleRoot = merkleRootFromProof(txBytes, merkleProof)
    require(merkleRoot.sameElements(merkleRootFromProof(coinbaseBytes, coinbaseProof)))
    val header = fromHex(forkHeaderHex)
    Array.copy(merkleRoot, 0, header, 36, 32)
    val headerId = doubleSha256(header)
    val headerAndHeight = header ++ longToBytes(bitcoinHeaderHeight.toLong)
    val bestChain = MutableAvl(Seq(headerId -> headerAndHeight), AvlTreeFlags.InsertOnly)
    val headerProof = bestChain.lookupProof(headerId)
    val txId = doubleSha256(txBytes)
    val relayDataInput = relayBox(
      bestChain.tree,
      bestChain.tree,
      bitcoinHeaderHeight + 6,
      headerId,
      BigInteger.ZERO,
      transactionId = ModifierId @@ Base16.encode(txId))
    val txCheckInput = testBox(
      relayBoxValue,
      txCheckErgoTree,
      creationHeight = 0,
      transactionId = ModifierId @@ Base16.encode(txId.reverse),
      boxIndex = 0)
    val contextVars: Map[Byte, EvaluatedValue[_ <: SType]] = Map(
      1.toByte -> ByteArrayConstant(txBytes),
      2.toByte -> ByteArrayConstant(headerId),
      3.toByte -> ByteArrayConstant(headerProof),
      4.toByte -> proofConstant(merkleProof),
      5.toByte -> ByteArrayConstant(coinbaseBytes),
      6.toByte -> proofConstant(coinbaseProof))

    TxCheckFixture(txCheckInput, relayDataInput, contextVars, merkleProof, coinbaseBytes,
      coinbaseProof, txBytes, headerId)
  }

  private def merkleRootFromProof(txBytes: Array[Byte], proof: Array[Coll[Byte]]): Array[Byte] =
    proof.foldLeft(doubleSha256(txBytes)) { (prevHash, proofElemColl) =>
      val proofElem = proofElemColl.toArray
      require(proofElem.length >= 33)
      val elemHash = proofElem.slice(1, 33)
      if (proofElem(0) == 0) {
        doubleSha256(elemHash ++ prevHash)
      } else {
        doubleSha256(prevHash ++ elemHash)
      }
    }

  private def proofConstant(proof: Array[Coll[Byte]]): EvaluatedValue[_ <: SType] =
    CollectionConstant[SCollection[SByte.type]](proof.toColl, SCollection(SByte))

  private def selfDuplicatingProof(txBytes: Array[Byte], depth: Int): Array[Coll[Byte]] = {
    var current = doubleSha256(txBytes)
    Array.fill(depth) {
      val proofElem = (1.toByte +: current).toColl
      current = doubleSha256(current ++ current)
      proofElem
    }
  }

  private def ambiguousCoinbase64ByteTx(): Array[Byte] = {
    val tx = Array.fill(64)(0.toByte)
    tx(4) = 1.toByte
    java.util.Arrays.fill(tx, 37, 41, 0xff.toByte)
    tx(41) = 4.toByte
    tx(42) = 1.toByte
    tx(43) = 2.toByte
    tx(44) = 3.toByte
    tx(45) = 4.toByte
    java.util.Arrays.fill(tx, 46, 50, 0xff.toByte)
    tx(50) = 1.toByte
    tx(51) = 1.toByte
    tx(59) = 0.toByte
    tx
  }

  private def relayContextVars(
      newHeader: Array[Byte],
      bestInsertProof: Array[Byte],
      parentLookupProof: Array[Byte],
      parentChain: CAvlTree,
      parentChainInsertProof: Array[Byte],
      allHeadersInsertProof: Array[Byte],
      retargetAnchorId: Option[Array[Byte]] = None,
      retargetAnchorProof: Option[Array[Byte]] = None): Map[Byte, EvaluatedValue[_ <: SType]] = {
    val base = Map(
      1.toByte -> ByteArrayConstant(newHeader),
      2.toByte -> ByteArrayConstant(bestInsertProof),
      3.toByte -> ByteArrayConstant(parentLookupProof),
      4.toByte -> AvlTreeConstant(parentChain),
      5.toByte -> ByteArrayConstant(parentChainInsertProof),
      6.toByte -> ByteArrayConstant(allHeadersInsertProof))
    base ++ retargetAnchorId.map(8.toByte -> ByteArrayConstant(_)) ++
      retargetAnchorProof.map(9.toByte -> ByteArrayConstant(_))
  }

  private def relayBox(
      bestChain: CAvlTree,
      allHeaders: CAvlTree,
      tipHeight: Int,
      tipId: Array[Byte],
      tipWork: BigInteger,
      transactionId: ModifierId = ErgoBox.allZerosModifierId,
      relayTokens: ArraySeq[Token] = canonicalRelayTokens,
      ergoTree: ErgoTree = btcRelayTree): ErgoBox =
    testBox(
      relayBoxValue,
      ergoTree,
      creationHeight = 0,
      additionalTokens = relayTokens,
      additionalRegisters = Map(
        R4 -> AvlTreeConstant(bestChain),
        R5 -> AvlTreeConstant(allHeaders),
        R6 -> IntConstant(tipHeight),
        R7 -> ByteArrayConstant(tipId),
        R8 -> BigIntConstant(tipWork)),
      transactionId = transactionId,
      boxIndex = 0)

  private def relayCandidate(
      bestChain: CAvlTree,
      allHeaders: CAvlTree,
      tipHeight: Int,
      tipId: Array[Byte],
      tipWork: BigInteger,
      relayTokens: ArraySeq[Token] = canonicalRelayTokens,
      ergoTree: ErgoTree = btcRelayTree): ErgoBoxCandidate =
    relayBox(bestChain, allHeaders, tipHeight, tipId, tipWork,
      relayTokens = relayTokens, ergoTree = ergoTree).toCandidate

  private def allHeadersRecord(header: HeaderFixture, chainDigest: Array[Byte], cumulativeWork: BigInteger): Array[Byte] =
    header.headerAndHeight ++ chainDigest ++ cumulativeWork.toByteArray

  private case class HeaderFixture(bytes: Array[Byte], height: Int, cumulativeWork: BigInteger) {
    val id: Array[Byte] = doubleSha256(bytes)
    val headerAndHeight: Array[Byte] = bytes ++ longToBytes(height.toLong)
  }

  private object HeaderFixture {
    def apply(hex: String, height: Int, cumulativeWork: BigInteger): HeaderFixture =
      HeaderFixture(fromHex(hex), height, cumulativeWork)
  }

  private case class MutableAvl(entries: Seq[(Array[Byte], Array[Byte])], flags: AvlTreeFlags) {
    private val prover = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    entries.foreach { case (key, value) =>
      require(prover.performOneOperation(Insert(ADKey @@ key, ADValue @@ value)).isSuccess)
    }
    prover.generateProof()

    def tree: CAvlTree = CAvlTree(new sigma.data.AvlTreeData(prover.digest.toColl, flags, 32, None))

    def insertProof(key: Array[Byte], value: Array[Byte]): Array[Byte] = {
      require(prover.performOneOperation(Insert(ADKey @@ key, ADValue @@ value)).isSuccess)
      prover.generateProof()
    }

    def lookupProof(key: Array[Byte]): Array[Byte] = {
      require(prover.performOneOperation(Lookup(ADKey @@ key)).isSuccess)
      prover.generateProof()
    }

    def lookupValue(key: Array[Byte]): Array[Byte] = {
      val value = prover.performOneOperation(Lookup(ADKey @@ key)).get.get
      prover.generateProof()
      value
    }
  }

  private def blockWorkFromHeader(headerBytes: Array[Byte]): BigInt = {
    val target = targetFromHeader(headerBytes)

    (BigInt(1) << 256) / (target + 1)
  }

  private def targetFromHeader(headerBytes: Array[Byte]): BigInt = {
    NBitsUtils.decodeCompactBits(nBitsFromHeader(headerBytes))
  }

  private def nBitsFromHeader(headerBytes: Array[Byte]): Long =
    byteArrayToLong(Array.fill(4)(0.toByte) ++ headerBytes.slice(72, 76).reverse)

  private def timestampFromHeader(headerBytes: Array[Byte]): Long =
    byteArrayToLong(Array.fill(4)(0.toByte) ++ headerBytes.slice(68, 72).reverse)

  private def expectedRetargetNBits(parentHeader: Array[Byte], anchorHeader: Array[Byte]): Long = {
    val actualTimespan = timestampFromHeader(parentHeader) - timestampFromHeader(anchorHeader)
    val boundedTimespan = math.max(targetTimespanSeconds / 4, math.min(targetTimespanSeconds * 4, actualTimespan))
    val newTarget = (targetFromHeader(parentHeader) * boundedTimespan) / targetTimespanSeconds
    NBitsUtils.encodeCompactBits(newTarget.min(bitcoinPowLimit))
  }

  private def relayPowHitIsBelowTarget(headerBytes: Array[Byte]): Boolean = {
    val hitBytes = doubleSha256(headerBytes).reverse
    hitBytes.head >= 0 && BigInt(hitBytes) <= targetFromHeader(headerBytes)
  }

  private def invalidPowHeaderBytes(): Array[Byte] = {
    val bytes = fromHex(mainnetHeader566093Hex)
    bytes(79) = (bytes(79) ^ 1).toByte
    bytes
  }

  private def headerWithNBits(headerBytes: Array[Byte], nBits: Long): Array[Byte] = {
    val updated = headerBytes.clone()
    val nBitsBytes = ByteBuffer.allocate(4).putInt(nBits.toInt).array().reverse
    Array.copy(nBitsBytes, 0, updated, 72, 4)
    updated
  }

  private def byteArrayToLong(bytes: Array[Byte]): Long =
    ByteBuffer.wrap(bytes).getLong

  private def longToBytes(value: Long): Array[Byte] =
    ByteBuffer.allocate(8).putLong(value).array()

  private def doubleSha256(bytes: Array[Byte]): Array[Byte] =
    Sha256.hash(Sha256.hash(bytes))

  private def fromHex(hex: String): Array[Byte] =
    Base16.decode(hex).get

  private val bigIntBytesScript: String =
    """{
      |    // 127 is the largest magnitude that fits without a sign byte; 128 forces a 0x00 prefix.
      |    val small = bigInt("127")
      |    val boundary = bigInt("128")
      |    val fixtureWork = bigInt("26078778141431578512036")
      |
      |    sigmaProp(
      |      small.toBytes == fromBase16("7f") &&
      |      boundary.toBytes == fromBase16("0080") &&
      |      byteArrayToBigInt(boundary.toBytes) == boundary &&
      |      fixtureWork.toBytes == fromBase16("0585bbbfd457faf84aa4") &&
      |      byteArrayToBigInt(fixtureWork.toBytes) == fixtureWork
      |    )
      |}""".stripMargin

  private val btcRelayScript: String =
    """{
      |    // registers:
      |    // R4 - best headers-chain tree
      |    // R5 - all headers tree
      |    // R6 - tip height
      |    // R7 - tip block id
      |
      |    // R8 - tip cumulative work
      |    //
      |    // context vars:
      |    // #1 - new header bytes
      |    // #2 - best chain tree insert proof // if tip update
      |    // #3 - parent header lookup proof in all headers db
      |    // #4 - parent header's best chain digest
      |    // #5 - parent header's best chain insert proof
      |    // #6 - all headers insert proof
      |    // #8 - retarget-period anchor header id, only when new height is divisible by 2016
      |    // #9 - retarget-period anchor lookup proof, only when new height is divisible by 2016
      |
      |    // id -> header (80 bytes) + height (8 bytes)
      |    val bestChainDigest = SELF.R4[AvlTree].get
      |
      |    // id -> header (80 bytes) + height (8 bytes) + chain digest (33 bytes) + cumulative work
      |    // chain digest here is constructed in the same way as best header chain digest
      |    val allHeadersDigest = SELF.R5[AvlTree].get
      |
      |    val tipHeight = SELF.R6[Int].get
      |    val tipHash = SELF.R7[Coll[Byte]].get
      |    val tipWork = SELF.R8[BigInt].get
      |
      |    val selfOut = OUTPUTS(0)
      |    // Reference fixture relay NFT id. A deployed relay substitutes its real relay NFT and
      |    // initializes genesis under this proposition with exactly one unit of that token.
      |    val relayNftId = fromBase16("0000000000000000000000000000000000000000000000000000000000000000")
      |    val relayTokens = SELF.tokens
      |    val relayToken = if (relayTokens.size == 1) relayTokens(0) else (relayNftId, 0L)
      |    val relayIdentityOk = relayTokens.size == 1 &&
      |                          relayToken._1 == relayNftId &&
      |                          relayToken._2 == 1L
      |
      |    def reverse4(bytes: Coll[Byte]): Coll[Byte] = {
      |        Coll(bytes(3), bytes(2), bytes(1), bytes(0))
      |    }
      |
      |    def reverse32(bytes: Coll[Byte]): Coll[Byte] = {
      |        Coll(
      |          bytes(31), bytes(30), bytes(29), bytes(28),
      |          bytes(27), bytes(26), bytes(25), bytes(24),
      |          bytes(23), bytes(22), bytes(21), bytes(20),
      |          bytes(19), bytes(18), bytes(17), bytes(16),
      |          bytes(15), bytes(14), bytes(13), bytes(12),
      |          bytes(11), bytes(10), bytes(9), bytes(8),
      |          bytes(7), bytes(6), bytes(5), bytes(4),
      |          bytes(3), bytes(2), bytes(1), bytes(0)
      |        )
      |    }
      |
      |    def doubleSha256(bytes: Coll[Byte]) = sha256(sha256(bytes))
      |
      |    // Raw digest bytes match Bitcoin's serialized prevBlockId and are the relay AVL key.
      |    def headerId(headerBytes: Coll[Byte]) = doubleSha256(headerBytes)
      |
      |    val candidateHeaderBytes = getVar[Coll[Byte]](1).get
      |    val validHeaderSize = candidateHeaderBytes.size == 80
      |    val zero16 = Coll[Byte](
      |      0.toByte, 0.toByte, 0.toByte, 0.toByte,
      |      0.toByte, 0.toByte, 0.toByte, 0.toByte,
      |      0.toByte, 0.toByte, 0.toByte, 0.toByte,
      |      0.toByte, 0.toByte, 0.toByte, 0.toByte)
      |    val zero32 = zero16 ++ zero16
      |    val zero80 = zero32 ++ zero32 ++ zero16
      |    val headerBytes = (candidateHeaderBytes ++ zero80).slice(0, 80)
      |    val prevBlockId = headerBytes.slice(4, 36)
      |    // val merkleRootBytes = headerBytes.slice(36, 68)
      |    val nBitsBytes = reverse4(headerBytes.slice(72, 76))
      |
      |    // calculate target to validate PoW & calculate work
      |    val pad = Coll[Byte](0.toByte, 0.toByte, 0.toByte, 0.toByte)
      |    val nbits = byteArrayToLong(pad ++ nBitsBytes)
      |    val compactSizeOk = nBitsBytes(0) > 0.toByte && nBitsBytes(0) <= 32.toByte
      |    val compactSignOk = nBitsBytes(1) >= 0.toByte
      |    val target = if (compactSizeOk && compactSignOk) Global.decodeNbits(nbits) else bigInt("0")
      |    val bitcoinPowLimit = bigInt("26959535291011309493156476344723991336010898738574164086137773096960")
      |    val targetSanityOk = target > bigInt("0") && target <= bitcoinPowLimit
      |
      |    // block (header) id
      |    val id = headerId(headerBytes)
      |
      |    val validPow = targetSanityOk && {
      |        val hitBytes = reverse32(id)
      |        val hit = byteArrayToBigInt(hitBytes)
      |
      |        // <= according to https://bitcoin.stackexchange.com/a/105224
      |        hitBytes(0) >= 0.toByte && hit <= target
      |    }
      |
      |    val maxWorkNumerator = unsignedBigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")
      |    val maxSignedWork = unsignedBigInt("57896044618658097711785492504343953926634992332820282019728792003956564819967")
      |    val workUnsigned = if (targetSanityOk) {
      |        val targetUnsigned = target.toUnsigned
      |        ((maxWorkNumerator - targetUnsigned) / (targetUnsigned + unsignedBigInt("1"))) + unsignedBigInt("1")
      |    } else {
      |        unsignedBigInt("0")
      |    }
      |    val workFitsSigned = workUnsigned <= maxSignedWork
      |    val work = if (workFitsSigned) workUnsigned.toSigned else bigInt("0")
      |
      |    val parentProof = getVar[Coll[Byte]](3).get
      |    val parentData = allHeadersDigest.get(prevBlockId, parentProof).get
      |
      |    // Derive the new height from the authenticated parent record for every branch.
      |    val parentHeight = byteArrayToLong(parentData.slice(80, 88))
      |    val nextHeight = parentHeight + 1
      |    val parentChainDigest = parentData.slice(88, 121)
      |    val parentCumWork = byteArrayToBigInt(parentData.slice(121, parentData.size))
      |    val nextCumWork = parentCumWork + work
      |    val parentChainProvided = getVar[AvlTree](4).get
      |    val headerRow = (id, headerBytes ++ longToByteArray(nextHeight))
      |    // A direct-tip successor must advance R4, R6 and R8 from the same authenticated parent record;
      |    // otherwise valid proofs can advance R4 and the next all-headers record from different trees.
      |    val tipParentStateOk = prevBlockId != tipHash ||
      |                           (parentHeight == tipHeight.toLong &&
      |                            parentCumWork == tipWork &&
      |                            parentChainDigest == bestChainDigest.digest)
      |
      |    val validTipUpdate = if(prevBlockId == tipHash) {
      |
      |        val proof = getVar[Coll[Byte]](2).get
      |
      |        val nextTree: Option[AvlTree] = bestChainDigest.insert(Coll(headerRow), proof)
      |         // This will fail if the operation failed or the proof is incorrect due to calling .get on the Option
      |        val outputDigest: Coll[Byte] = nextTree.get.digest
      |
      |        val outBestChainTree = selfOut.R4[AvlTree].get
      |
      |        outBestChainTree.digest == outputDigest &&
      |        outBestChainTree.enabledOperations == bestChainDigest.enabledOperations &&
      |        outBestChainTree.keyLength == bestChainDigest.keyLength &&
      |        outBestChainTree.valueLengthOpt == bestChainDigest.valueLengthOpt &&
      |        selfOut.R6[Int].get == nextHeight &&
      |        selfOut.R7[Coll[Byte]].get == id &&
      |        selfOut.R8[BigInt].get == nextCumWork
      |    } else {
      |        true
      |    }
      |
      |    val allHeadersDbUpdate = {
      |        // parentData stores the full parent header so retarget checks can read timestamp and nBits.
      |        val difficultyTransitionOk = if (nextHeight % 2016L == 0L) {
      |            val anchorId = getVar[Coll[Byte]](8).get
      |            val anchorProof = getVar[Coll[Byte]](9).get
      |            val anchorData = parentChainProvided.get(anchorId, anchorProof).get
      |            val anchorHeader = anchorData.slice(0, 80)
      |            val anchorHeight = byteArrayToLong(anchorData.slice(80, 88))
      |            val anchorTime = byteArrayToLong(pad ++ reverse4(anchorHeader.slice(68, 72)))
      |            val parentTime = byteArrayToLong(pad ++ reverse4(parentData.slice(68, 72)))
      |            val rawTimespan = parentTime - anchorTime
      |            val boundedTimespan = if (rawTimespan < 302400L) {
      |                302400L
      |            } else {
      |                if (rawTimespan > 4838400L) 4838400L else rawTimespan
      |            }
      |            val boundedTimespanBigInt = byteArrayToBigInt(longToByteArray(boundedTimespan))
      |            val parentNbitsBytes = reverse4(parentData.slice(72, 76))
      |            val parentNbits = byteArrayToLong(pad ++ parentNbitsBytes)
      |            val parentTarget = Global.decodeNbits(parentNbits)
      |            val rawExpectedTarget = parentTarget * boundedTimespanBigInt / bigInt("1209600")
      |            val expectedTarget = if (rawExpectedTarget > bitcoinPowLimit) bitcoinPowLimit else rawExpectedTarget
      |
      |            anchorHeight == nextHeight - 2016L &&
      |            anchorId == headerId(anchorHeader) &&
      |            parentTarget > bigInt("0") &&
      |            parentTarget <= bitcoinPowLimit &&
      |            Global.encodeNbits(expectedTarget) == nbits
      |        } else {
      |            headerBytes.slice(72, 76) == parentData.slice(72, 76)
      |        }
      |
      |        val cumWorkBytes = nextCumWork.toBytes
      |
      |        val parentChainUpdateProof = getVar[Coll[Byte]](5).get
      |        val updDigest = parentChainProvided.insert(Coll(headerRow), parentChainUpdateProof).get.digest
      |
      |        val allHeadersInsertProof = getVar[Coll[Byte]](6).get
      |
      |        val keyVal = (id, (headerBytes ++ longToByteArray(nextHeight) ++ updDigest ++ cumWorkBytes))
      |        val allHeadersDbUpdated = allHeadersDigest.insert(Coll(keyVal), allHeadersInsertProof).get
      |        val newAllHeadersDigestProvided = selfOut.R5[AvlTree].get
      |
      |        val allHeadersUpdateOk = parentChainProvided.digest == parentChainDigest &&
      |                                    parentChainProvided.enabledOperations == bestChainDigest.enabledOperations &&
      |                                    parentChainProvided.keyLength == bestChainDigest.keyLength &&
      |                                    parentChainProvided.valueLengthOpt == bestChainDigest.valueLengthOpt &&
      |                                    allHeadersDbUpdated == newAllHeadersDigestProvided &&
      |                                    newAllHeadersDigestProvided.enabledOperations == allHeadersDigest.enabledOperations &&
      |                                    newAllHeadersDigestProvided.keyLength == allHeadersDigest.keyLength &&
      |                                    newAllHeadersDigestProvided.valueLengthOpt == allHeadersDigest.valueLengthOpt &&
      |                                    difficultyTransitionOk
      |
      |        if (nextCumWork > tipWork && prevBlockId != tipHash) {
      |            // switch to better chain
      |
      |            val outBestChainTree = selfOut.R4[AvlTree].get
      |
      |            val switchOk = outBestChainTree.digest == updDigest &&
      |                            outBestChainTree.enabledOperations == bestChainDigest.enabledOperations &&
      |                            outBestChainTree.keyLength == bestChainDigest.keyLength &&
      |                            outBestChainTree.valueLengthOpt == bestChainDigest.valueLengthOpt &&
      |                            selfOut.R6[Int].get == nextHeight &&
      |                            selfOut.R7[Coll[Byte]].get == id &&
      |                            selfOut.R8[BigInt].get == nextCumWork
      |
      |            allHeadersUpdateOk && switchOk
      |        } else {
      |            // add header along with metadata to all-headers tree
      |            val noSwitchOk = if (prevBlockId == tipHash) {
      |                true
      |            } else {
      |                val outBestChainTree = selfOut.R4[AvlTree].get
      |                outBestChainTree.digest == bestChainDigest.digest &&
      |                outBestChainTree.enabledOperations == bestChainDigest.enabledOperations &&
      |                outBestChainTree.keyLength == bestChainDigest.keyLength &&
      |                outBestChainTree.valueLengthOpt == bestChainDigest.valueLengthOpt &&
      |                selfOut.R6[Int].get == tipHeight &&
      |                selfOut.R7[Coll[Byte]].get == tipHash &&
      |                selfOut.R8[BigInt].get == tipWork
      |            }
      |
      |            allHeadersUpdateOk && noSwitchOk
      |        }
      |    }
      |
      |    // Preserve the exact token vector: extra successor tokens would invalidate
      |    // BtcTxCheck's singleton relay-identity check and make the relay unusable as a data input.
      |    val selfPreservation = selfOut.value >= SELF.value &&
      |                           selfOut.propositionBytes == SELF.propositionBytes &&
      |                           selfOut.tokens == SELF.tokens
      |
      |    sigmaProp(validHeaderSize && validPow && workFitsSigned && relayIdentityOk && tipParentStateOk &&
      |              selfPreservation && validTipUpdate && allHeadersDbUpdate)
      |}""".stripMargin

  private val btcTxCheckScript: String =
    """{
      |    // context vars:
      |    // #1 - tx bytes
      |    // #2 - header id in relay/internal hash byte order
      |    // #3 - headerProof
      |    // #4 - Merkle proof
      |    // #5 - stripped coinbase transaction bytes used as the authenticated depth witness
      |    // #6 - coinbase Merkle proof; every direction is left because coinbase is leaf zero
      |
      |    // registers:
      |    // no registers used
      |
      |    // Reference fixture relay NFT id. A deployed relay would substitute its real relay NFT.
      |    val relayNftId = fromBase16("0000000000000000000000000000000000000000000000000000000000000000")
      |    // Six descendants after the containing header means seven confirmations including that header.
      |    val minDescendants = 6
      |    // Bound proof work and stay inside Bitcoin's 32-bit transaction-count domain.
      |    val maxMerkleDepth = 32
      |
      |    val relayDataInput = CONTEXT.dataInputs(0)
      |
      |    val relayTokens = relayDataInput.tokens
      |    val relayToken = if (relayTokens.size == 1) relayTokens(0) else (relayNftId, 0L)
      |    // Relay authenticity is inductive: correctly initialized genesis plus BtcRelay's
      |    // proposition-and-token preservation keeps this singleton NFT under the relay script.
      |    val properRelay = relayTokens.size == 1 &&
      |                      relayToken._1 == relayNftId &&
      |                      relayToken._2 == 1L
      |
      |    def doubleSha256(bytes: Coll[Byte]) = sha256(sha256(bytes))
      |
      |    val txBytes = getVar[Coll[Byte]](1).get
      |    val txId = doubleSha256(txBytes)
      |
      |    // Transaction amount/recipient parsing is composed by the amount-binding parser example.
      |
      |    val headerId = getVar[Coll[Byte]](2).get
      |
      |    val headerProof = getVar[Coll[Byte]](3).get
      |
      |    val bestChain = relayDataInput.R4[AvlTree].get
      |
      |    val headerAndHeight = bestChain.get(headerId, headerProof).get
      |
      |    val height = byteArrayToLong(headerAndHeight.slice(80, 88))
      |
      |    val tipHeight = relayDataInput.R6[Int].get
      |
      |    val enoughConfs = (tipHeight - height) >= minDescendants
      |
      |    val merkleRootBytes = headerAndHeight.slice(36, 68)
      |
      |    val merkleProof = getVar[Coll[Coll[Byte]]](4).get
      |    val coinbaseBytes = getVar[Coll[Byte]](5).get
      |    val coinbaseProof = getVar[Coll[Coll[Byte]]](6).get
      |
      |    def computeLevel(prevHash: Coll[Byte], proofElem: Coll[Byte]) = {
      |        val elemHash = proofElem.slice(1,33)
      |        if(proofElem(0) == 0){
      |          doubleSha256(elemHash ++ prevHash)
      |        } else {
      |          doubleSha256(prevHash ++ elemHash)
      |        }
      |    }
      |
      |    // A second proof for the non-64-byte coinbase authenticates the actual leaf depth.
      |    // Equal depth rejects the shortened proof used to reinterpret a 64-byte leaf as an internal node,
      |    // without trusting an uncommitted transaction count.
      |    val proofDepthOk = merkleProof.size <= maxMerkleDepth &&
      |                       merkleProof.size == coinbaseProof.size
      |    // Keep every linear proof walk behind the constant-time depth envelope. BlockValue
      |    // evaluates materialized ValDefs strictly, so the nested guards make the cost order explicit.
      |    val properProof = if (proofDepthOk) {
      |      val coinbaseShapeOk = if (coinbaseBytes.size >= 42) {
      |        // A valid 64-byte coinbase is intentionally outside this verifier profile because
      |        // its bytes are indistinguishable from a Merkle internal node in a header-only proof.
      |        coinbaseBytes.size != 64 &&
      |        coinbaseBytes(4) == 1 &&
      |        coinbaseBytes.slice(5, 37) == fromBase16("0000000000000000000000000000000000000000000000000000000000000000") &&
      |        coinbaseBytes.slice(37, 41) == fromBase16("ffffffff")
      |      } else {
      |        false
      |      }
      |      val proofShapeOk = merkleProof.forall({ (proofElem: Coll[Byte]) =>
      |        proofElem.size == 33 && (proofElem(0) == 0 || proofElem(0) == 1)
      |      })
      |      val coinbaseProofShapeOk = coinbaseProof.forall({ (proofElem: Coll[Byte]) =>
      |        proofElem.size == 33 && proofElem(0) == 1
      |      })
      |      val proofShapesOk = coinbaseShapeOk && proofShapeOk && coinbaseProofShapeOk
      |
      |      if (proofShapesOk) {
      |        val computedMerkleRoot = merkleProof.fold(txId, computeLevel)
      |        val computedCoinbaseRoot = coinbaseProof.fold(doubleSha256(coinbaseBytes), computeLevel)
      |        computedMerkleRoot == merkleRootBytes &&
      |        computedCoinbaseRoot == merkleRootBytes
      |      } else {
      |        false
      |      }
      |    } else {
      |      false
      |    }
      |
      |    // This predicate authenticates relay-confirmed inclusion; spending policy is supplied by the composing contract.
      |
      |    sigmaProp(properRelay && enoughConfs && properProof)
      |}""".stripMargin
}
