package sigmastate.utxo.examples

import org.ergoplatform.{ErgoLikeTransaction, Input}
import scorex.crypto.hash.Sha256
import scorex.util.encode.Base16
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.AvlTreeData
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.Extensions.ArrayOps
import sigmastate.CompilerCrossVersionProps
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter.emptyEnv
import sigmastate.utils.Helpers._

import scala.collection.compat.immutable.ArraySeq

/** Example of parsing a Bitcoin transaction in ErgoScript with output amount binding.
  *
  * This is a complementary contribution alongside `BitcoinTxParsingExampleSpecification`
  * (PR #1177). Where the general parser proves recipient (txid + script hash on some
  * output), this variant additionally proves payment value: an output exists with both
  * the expected script hash AND a value satisfying a minimum-satoshi threshold (R6),
  * with same-output binding (script and amount must be on the same output).
  *
  * The contract is bounded to bridge-shaped transactions:
  *   - non-witness serialization only
  *   - single-byte CompactSize encoding (counts and lengths < 0xfd)
  *   - 1 or 2 inputs, 1-4 outputs
  *
  * SELF box registers:
  *   R4: expected Bitcoin txid in natural (internal) byte order (Coll[Byte], 32 bytes)
  *   R5: expected SHA-256 hash of an output scriptPubKey (Coll[Byte], 32 bytes)
  *   R6: minimum output value in satoshis (Long)
  *
  * Context extension:
  *   var(1): Bitcoin transaction bytes (non-witness serialization)
  *
  * This primitive is designed to compose with Bitcoin SPV verification: when paired
  * with a relay that proves header authenticity and a Merkle proof check that proves
  * inclusion, the three primitives together enable trustless verification of Bitcoin
  * payments from within Ergo contracts.
  */
class BitcoinTxParsingAmountBindingSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {

  override val printVersions: Boolean = false
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  val contractScript: String =
    """{
      |  val txBytes = getVar[Coll[Byte]](1).get
      |  val expectedTxid = SELF.R4[Coll[Byte]].get
      |  val expectedScriptHash = SELF.R5[Coll[Byte]].get
      |  val minSatoshis = SELF.R6[Long].get
      |
      |  val maxBtcSatoshis = 2100000000000000L
      |  val minSatoshisOk = minSatoshis >= 0L && minSatoshis <= maxBtcSatoshis
      |
      |  val txidOk = sha256(sha256(txBytes)) == expectedTxid
      |
      |  def readByte(pos: Int): Int = {
      |    val signed = txBytes(pos).toInt
      |    if (signed < 0) signed + 256 else signed
      |  }
      |
      |  def inputEnd(start: Int): Int = {
      |    val scriptLen = readByte(start + 36)
      |    start + 37 + scriptLen + 4
      |  }
      |
      |  def outputEnd(start: Int): Int = {
      |    val scriptLen = readByte(start + 8)
      |    start + 9 + scriptLen
      |  }
      |
      |  def amountFitsBitcoinSupply(start: Int): Boolean = {
      |    val b6 = readByte(start + 6)
      |    val b7 = readByte(start + 7)
      |    b7 == 0 && b6 <= 7
      |  }
      |
      |  def readAmount(start: Int): Long = {
      |    val b0 = readByte(start).toLong
      |    val b1 = readByte(start + 1).toLong
      |    val b2 = readByte(start + 2).toLong
      |    val b3 = readByte(start + 3).toLong
      |    val b4 = readByte(start + 4).toLong
      |    val b5 = readByte(start + 5).toLong
      |    val b6 = readByte(start + 6).toLong
      |    b0 +
      |      b1 * 256L +
      |      b2 * 65536L +
      |      b3 * 16777216L +
      |      b4 * 4294967296L +
      |      b5 * 1099511627776L +
      |      b6 * 281474976710656L
      |  }
      |
      |  def outputMatches(start: Int): Boolean = {
      |    val scriptLen = readByte(start + 8)
      |    if (scriptLen > 0 && amountFitsBitcoinSupply(start)) {
      |      val amount = readAmount(start)
      |      val amountOk = amount <= maxBtcSatoshis && amount >= minSatoshis
      |      val scriptStart = start + 9
      |      val scriptBytes = txBytes.slice(scriptStart, scriptStart + scriptLen)
      |      amountOk && sha256(scriptBytes) == expectedScriptHash
      |    } else {
      |      false
      |    }
      |  }
      |
      |  val sizeOk = txBytes.size >= 61
      |  val inputCount = readByte(4)
      |  val inputCountOk = inputCount == 1 || inputCount == 2
      |
      |  val input1ScriptLenOk = readByte(5 + 36) < 0xfd
      |  val afterInput1 = inputEnd(5)
      |
      |  val input2ScriptLenOk = if (inputCount == 2) {
      |    readByte(afterInput1 + 36) < 0xfd
      |  } else {
      |    true
      |  }
      |  val afterInput2 = if (inputCount == 2) inputEnd(afterInput1) else afterInput1
      |
      |  val inputsEnd = afterInput2
      |
      |  val outputCount = readByte(inputsEnd)
      |  val outputCountOk = outputCount >= 1 && outputCount <= 4
      |
      |  val output1Start = inputsEnd + 1
      |  val output1ScriptLenOk = readByte(output1Start + 8) < 0xfd
      |  val match1 = outputMatches(output1Start)
      |  val afterOutput1 = outputEnd(output1Start)
      |
      |  val output2ScriptLenOk = if (outputCount >= 2) {
      |    readByte(afterOutput1 + 8) < 0xfd
      |  } else {
      |    true
      |  }
      |  val match2 = if (outputCount >= 2) outputMatches(afterOutput1) else false
      |  val afterOutput2 = if (outputCount >= 2) outputEnd(afterOutput1) else afterOutput1
      |
      |  val output3ScriptLenOk = if (outputCount >= 3) {
      |    readByte(afterOutput2 + 8) < 0xfd
      |  } else {
      |    true
      |  }
      |  val match3 = if (outputCount >= 3) outputMatches(afterOutput2) else false
      |  val afterOutput3 = if (outputCount >= 3) outputEnd(afterOutput2) else afterOutput2
      |
      |  val output4ScriptLenOk = if (outputCount == 4) {
      |    readByte(afterOutput3 + 8) < 0xfd
      |  } else {
      |    true
      |  }
      |  val match4 = if (outputCount == 4) outputMatches(afterOutput3) else false
      |  val afterOutput4 = if (outputCount == 4) outputEnd(afterOutput3) else afterOutput3
      |
      |  val outputsEnd = afterOutput4
      |
      |  val locktimeOk = outputsEnd == txBytes.size - 4
      |  val anyOutputMatches = match1 || match2 || match3 || match4
      |  val allVarintsOk = input1ScriptLenOk && input2ScriptLenOk &&
      |                     output1ScriptLenOk && output2ScriptLenOk &&
      |                     output3ScriptLenOk && output4ScriptLenOk
      |
      |  sigmaProp(
      |    sizeOk &&
      |    txidOk &&
      |    minSatoshisOk &&
      |    inputCountOk &&
      |    outputCountOk &&
      |    allVarintsOk &&
      |    locktimeOk &&
      |    anyOutputMatches
      |  )
      |}""".stripMargin

  // === Fixtures ===

  /** Rosen Bridge transaction (mainnet, segwit). Stripped of witness data so the
    * bytes are exactly what the txid commits to. 1 input, 3 outputs (OP_RETURN,
    * P2WPKH, P2WPKH). Used as the foundation for byte-surgery test cases.
    *
    * txid (display order):  bcb5cb37f1307cc2240a14c7add48b57a479c55ab2123e8bcc02bdf15b3e1ce5
    * txid (natural order):  e51c3e5bf1bd02cc8b3e12b25ac579a4578bd4adc7140a24c27c30f137cbb5bc
    */
  val rosenBridgeTxHex: String =
    "0200000001f69c854ccbfa5ed35e4c39f5d85fc44763ef9bdeefa045d831f6d3" +
    "4212a89fb10000000000ffffffff030000000000000000356a33000000000000" +
    "0021c400000000000000a82102fd52469f33344592dff455bb2e934d86e1d3f2" +
    "587c2eeb635441f5f4cfdc9ef8e09304000000000016001483cf4566652b5385" +
    "b1f94494034f78f177792265f00900000000000016001437f45872b029e6bf13" +
    "9bf43be4b44548127a82e300000000"
  val rosenBridgeTx: Array[Byte] = Base16.decode(rosenBridgeTxHex).getOrThrow

  /** Output 1's scriptPubKey (P2WPKH, 22 bytes). Offset 118..140 in stripped tx. */
  val output1Script: Array[Byte] = rosenBridgeTx.slice(118, 140)

  /** Output 2's scriptPubKey (P2WPKH, 22 bytes). Offset 149..171 in stripped tx. */
  val output2Script: Array[Byte] = rosenBridgeTx.slice(149, 171)

  // === Helpers ===

  /** Double-SHA256 of the transaction bytes = Bitcoin txid in internal (natural)
    * byte order. This is what register R4 must contain.
    */
  def btcTxId(tx: Array[Byte]): Array[Byte] = Sha256.hash(Sha256.hash(tx))

  /** Constructs a synthetic legacy Bitcoin transaction (1 input, N outputs)
    * with the given output (value, script) pairs. Used for adversarial tests
    * that need fine-grained control over output shape.
    */
  def syntheticTx(outputs: Seq[(Long, Array[Byte])]): Array[Byte] = {
    def le32(n: Int): Array[Byte] = Array(n, n >> 8, n >> 16, n >> 24).map(_.toByte)
    def le64(n: Long): Array[Byte] = (0 until 8).map(i => (n >> (8 * i)).toByte).toArray
    val version = le32(1)
    val inputCount = Array(1.toByte)
    val prevTxId = Array.fill(32)(0.toByte)
    val vout = le32(0)
    val scriptSigLen = Array(0.toByte)
    val sequence = le32(-1)
    val input = prevTxId ++ vout ++ scriptSigLen ++ sequence
    val outputCount = Array(outputs.size.toByte)
    val outs = outputs.toArray.flatMap { case (value, script) =>
      le64(value) ++ Array(script.length.toByte) ++ script
    }
    val lockTime = le32(0)
    version ++ inputCount ++ input ++ outputCount ++ outs ++ lockTime
  }

  /** Creates a box guarded by the amount-binding contract with the given expected
    * txid (R4), expected output script hash (R5), and minimum-satoshi threshold (R6),
    * then attempts to spend it by providing `txBytes` in context variable 1.
    */
  private def spend(
      txId: Array[Byte],
      scriptHash: Array[Byte],
      minSatoshis: Long,
      txBytes: Array[Byte],
      expectSuccess: Boolean): Unit = {
    val prop = compile(emptyEnv, contractScript).asBoolValue.toSigmaProp
    val tree = mkTestErgoTree(prop)

    val boxToSpend = testBox(10, tree, creationHeight = 5,
      additionalRegisters = Map(
        org.ergoplatform.ErgoBox.R4 -> ByteArrayConstant(txId.toColl),
        org.ergoplatform.ErgoBox.R5 -> ByteArrayConstant(scriptHash.toColl),
        org.ergoplatform.ErgoBox.R6 -> LongConstant(minSatoshis)))
    val newBox = testBox(10, TrueTree, creationHeight = 0)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(boxToSpend.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))),
      ArraySeq.empty,
      IndexedSeq(newBox))
    val ctx = ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx, self = boxToSpend, ergoTreeVersionInTests)

    val prover = new ContextEnrichingTestProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] =
        Map(1.toByte -> ByteArrayConstant(txBytes.toColl))
    }
    val res = prover.prove(emptyEnv, tree, ctx, fakeMessage)
    if (expectSuccess) {
      val pr = res.getOrThrow
      val verifier = new ErgoLikeTestInterpreter
      verifier.verify(emptyEnv, tree, ctx.withExtension(pr.extension), pr.proof, fakeMessage)
        .getOrThrow._1 shouldBe true
    } else {
      val ex = res.failed.get
      ex.getMessage should include("Script reduced to false")
    }
  }

  /** Same as `spend` but returns the verification cost (in JIT cost units)
    * for a successful spend. Used by benchmark properties to measure how
    * close the contract runs to the cost budget for worst-case input shapes.
    */
  private def spendAndMeasureCost(
      txId: Array[Byte],
      scriptHash: Array[Byte],
      minSatoshis: Long,
      txBytes: Array[Byte]): Long = {
    val prop = compile(emptyEnv, contractScript).asBoolValue.toSigmaProp
    val tree = mkTestErgoTree(prop)

    val boxToSpend = testBox(10, tree, creationHeight = 5,
      additionalRegisters = Map(
        org.ergoplatform.ErgoBox.R4 -> ByteArrayConstant(txId.toColl),
        org.ergoplatform.ErgoBox.R5 -> ByteArrayConstant(scriptHash.toColl),
        org.ergoplatform.ErgoBox.R6 -> LongConstant(minSatoshis)))
    val newBox = testBox(10, TrueTree, creationHeight = 0)
    val tx = new ErgoLikeTransaction(
      IndexedSeq(Input(boxToSpend.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))),
      ArraySeq.empty,
      IndexedSeq(newBox))
    val ctx = ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(boxToSpend),
      spendingTransaction = tx, self = boxToSpend, ergoTreeVersionInTests)

    val prover = new ContextEnrichingTestProvingInterpreter() {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] =
        Map(1.toByte -> ByteArrayConstant(txBytes.toColl))
    }
    val pr = prover.prove(emptyEnv, tree, ctx, fakeMessage).getOrThrow
    val verifier = new ErgoLikeTestInterpreter
    val result = verifier.verify(emptyEnv, tree, ctx.withExtension(pr.extension), pr.proof, fakeMessage).getOrThrow
    result._1 shouldBe true
    result._2
  }

  // === Properties ===

  property("amount-binding contract compiles cleanly") {
    val prop = compile(emptyEnv, contractScript).asBoolValue.toSigmaProp
    assert(prop != null)
  }

  property("real transaction - spend with output 1 script and exact amount") {
    // Output 1 of the Rosen Bridge tx pays 300,000 satoshis to the P2WPKH at offset 118.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 300000L,
      txBytes = rosenBridgeTx,
      expectSuccess = true)
  }

  property("real transaction - accepts amount exceeding R6 threshold") {
    // Output 1 value 300,000 sats; threshold 299,999. Value >= R6 holds.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 299999L,
      txBytes = rosenBridgeTx,
      expectSuccess = true)
  }

  property("real transaction - rejects when amount is below R6 threshold") {
    // Output 1 value 300,000 sats; threshold 300,001. Value < R6 -> reject.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 300001L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects underpayment at non-trivial threshold") {
    // Output 2 of the tx pays 2,544 sats; threshold 100,000. Output 2's script
    // matches R5 here, but its value is far below R6.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output2Script),
      minSatoshis = 100000L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects wrong txid") {
    // R4 must equal sha256(sha256(txBytes)). Reversing the txid makes the
    // authenticity check fail, defending against transaction substitution.
    spend(
      txId = btcTxId(rosenBridgeTx).reverse,
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 300000L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects wrong expected script hash") {
    // R5 must match the SHA-256 of an output's scriptPubKey. A hash of unrelated
    // bytes fails the recipient check.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(Array[Byte](1, 2, 3)),
      minSatoshis = 300000L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects script hash of a non-output fragment") {
    // Even if R5 equals the hash of some bytes that appear in the transaction
    // but are not an output scriptPubKey, the contract must reject. Here we
    // hash a fragment from inside the OP_RETURN output's data area.
    val nonOutputFragment = rosenBridgeTx.slice(50, 80)
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(nonOutputFragment),
      minSatoshis = 300000L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects negative minSatoshis") {
    // R6 must satisfy 0 <= minSatoshis <= 21,000,000 BTC. A negative threshold
    // is a malformed commitment and the contract must reject.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = -1L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("real transaction - rejects minSatoshis above Bitcoin supply") {
    // 21,000,000 BTC = 2,100,000,000,000,000 satoshis. A threshold of
    // 2,100,000,000,000,001 sats is non-physical for Bitcoin.
    spend(
      txId = btcTxId(rosenBridgeTx),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 2100000000000001L,
      txBytes = rosenBridgeTx,
      expectSuccess = false)
  }

  property("modified transaction - rejects output value exceeding Bitcoin supply") {
    // Output 1's value field is at offset 109..117 (8 bytes, little-endian).
    // Setting byte 7 to 0xFF makes the value > 21M BTC, which the supply-bound
    // byte-pattern check (b7 == 0 && b6 <= 7) must reject. We also recompute
    // the txid so the modified bytes are authenticated; the supply check is
    // what catches them, not the txid check.
    val tampered = rosenBridgeTx.clone()
    tampered(116) = 0xFF.toByte  // byte 7 of Output 1's value field
    spend(
      txId = btcTxId(tampered),
      scriptHash = Sha256.hash(output1Script),
      minSatoshis = 300000L,
      txBytes = tampered,
      expectSuccess = false)
  }

  property("synthetic transaction - rejects when script and amount on different outputs") {
    // Same-output binding regression test. Output 1 has the matching script
    // but underpays (100 sats < 100,000 threshold). Output 2 has high value
    // (500,000 sats > threshold) but a different script. A buggy contract
    // that allowed (script-match-anywhere) AND (amount-match-anywhere) would
    // incorrectly accept; the correct contract requires both on the same
    // output, so it rejects.
    val matchingScript = Array.tabulate(22)(i => (i * 5 + 1).toByte)
    val differentScript = Array.tabulate(22)(i => (i * 7 + 13).toByte)
    val tx = syntheticTx(Seq(
      100L -> matchingScript,
      500000L -> differentScript))
    spend(
      txId = btcTxId(tx),
      scriptHash = Sha256.hash(matchingScript),
      minSatoshis = 100000L,
      txBytes = tx,
      expectSuccess = false)
  }

  property("benchmark - minimal spend cost (1 output)") {
    // Best-case shape: 1 input, 1 output. The match happens on output 1
    // so only one outputMatches evaluation runs.
    val script = Array.tabulate(22)(i => (i + 1).toByte)
    val tx = syntheticTx(Seq(500000L -> script))
    val cost = spendAndMeasureCost(
      txId = btcTxId(tx),
      scriptHash = Sha256.hash(script),
      minSatoshis = 100000L,
      txBytes = tx)
    info(s"Minimal-shape verification cost: $cost units")
    cost should be > 0L
  }

  property("benchmark - worst-case spend cost (4 outputs, match on last)") {
    // Worst-case shape: 1 input, 4 outputs. The matching output is output 4,
    // so all four outputMatches evaluations run, each performing a sha256
    // on the script bytes. Demonstrates the cost ceiling for the bounded
    // transaction shape.
    val matchingScript = Array.tabulate(22)(i => (i + 1).toByte)
    val script2 = Array.tabulate(22)(i => (i * 3).toByte)
    val script3 = Array.tabulate(22)(i => (i * 5).toByte)
    val script4 = Array.tabulate(22)(i => (i * 7).toByte)
    val tx = syntheticTx(Seq(
      1000L -> script2,
      2000L -> script3,
      3000L -> script4,
      500000L -> matchingScript))
    val cost = spendAndMeasureCost(
      txId = btcTxId(tx),
      scriptHash = Sha256.hash(matchingScript),
      minSatoshis = 100000L,
      txBytes = tx)
    info(s"Worst-case verification cost (4 outputs, match on last): $cost units")
    cost should be > 0L
  }
}
