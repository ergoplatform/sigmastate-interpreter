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

/** Example of parsing a Bitcoin transaction in ErgoScript
  * (see https://github.com/ergoplatform/sigmastate-interpreter/issues/1114).
  *
  * The contract below guards a box which can be spent only by providing (in a context
  * extension variable) the bytes of a Bitcoin transaction such that:
  *  - the transaction has the id stored in `SELF.R4` (txid = double-SHA256 of the
  *    transaction bytes, in internal byte order, i.e. not reversed as in explorers), and
  *  - the transaction has an output whose scriptPubKey bytes hash (SHA256) equals
  *    the value stored in `SELF.R5`.
  *
  * The script fully parses the legacy Bitcoin transaction format:
  * {{{
  *   version(4) | nInputs(varint) | inputs | nOutputs(varint) | outputs | lockTime(4)
  *   input  = prevTxId(32) | vout(4) | scriptSigLen(varint) | scriptSig | sequence(4)
  *   output = value(8) | scriptLen(varint) | scriptPubKey
  * }}}
  * Since ErgoScript has no unbounded loops, inputs and outputs are traversed with
  * `fold` over a fixed range, so the contract supports transactions with up to
  * `MaxInputs`/`MaxOutputs` inputs/outputs (the counts are checked explicitly).
  * Var-length integers are supported in the 1-byte and 3-byte (0xfd) encodings, which
  * covers any realistic transaction (5/9-byte varints would require >65535 inputs,
  * outputs or script bytes).
  *
  * NOTE: for segwit transactions the txid is defined as double-SHA256 of the transaction
  * serialized WITHOUT witness data, so such transactions should be passed to the contract
  * in the legacy (witness-stripped) serialization, which is exactly the serialization
  * the txid commits to.
  */
class BitcoinTxParsingExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** The first ever Bitcoin transaction (block 170, Satoshi -> Hal Finney):
    * legacy format, 1 input, 2 P2PK outputs,
    * txid f4184fc596403b9d638783cf57adfe4c75c605f6356fbc91338530e9831e9e16. */
  val block170TxHex: String =
    "0100000001c997a5e56e104102fa209c6a852dd90660a20b2d9c352423edce25857fcd370400000000" +
    "4847304402204e45e16932b8af514961a1d3a1a25fdf3f4f7732e9d624c6c61548ab5fb8cd41022018" +
    "1522ec8eca07de4860a4acdd12909d831cc56cbbac4622082221a8768d1d0901ffffffff0200ca9a3b" +
    "00000000434104ae1a62fe09c5f51b13905f07f06b99a2f7159b2225f374cd378d71302fa28414e7aa" +
    "b37397f554a7df5f142c21c1b7303b8a0626f1baded5c72a704f7e6cd84cac00286bee000000004341" +
    "0411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9" +
    "744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac00000000"

  val block170Tx: Array[Byte] = Base16.decode(block170TxHex).getOrThrow

  /** scriptPubKey of the first output (pays 10 BTC to Hal Finney's public key). */
  val block170Output0Script: Array[Byte] = block170Tx.slice(128, 128 + 67)

  /** scriptPubKey of the second output (the 40 BTC change back to Satoshi). */
  val block170Output1Script: Array[Byte] = block170Tx.slice(204, 204 + 67)

  /** Bitcoin's variable-length integer (1-byte and 0xfd 3-byte encodings). */
  private def varInt(n: Int): Array[Byte] =
    if (n < 0xfd) Array(n.toByte)
    else Array(0xfd.toByte, (n & 0xff).toByte, ((n >> 8) & 0xff).toByte)

  /** Serializes a synthetic legacy Bitcoin transaction with the given number of inputs
    * (with deterministic dummy outpoints and signatures) and the given output
    * scriptPubKeys, for testing the contract on multi-input/output transactions. */
  def syntheticTx(nInputs: Int, outputs: Seq[(Long, Array[Byte])], scriptSigSize: Int = 71): Array[Byte] = {
    def le32(n: Int): Array[Byte] = Array(n, n >> 8, n >> 16, n >> 24).map(_.toByte)
    def le64(n: Long): Array[Byte] = (0 until 8).map(i => (n >> (8 * i)).toByte).toArray
    val version = le32(1)
    val inputs = (0 until nInputs).toArray.flatMap { i =>
      val prevTxId = Array.fill(32)((i + 1).toByte)
      val vout = le32(i)
      val scriptSig = Array.tabulate(scriptSigSize)(j => (i + j).toByte)
      val sequence = le32(-1)
      prevTxId ++ vout ++ varInt(scriptSig.length) ++ scriptSig ++ sequence
    }
    val outs = outputs.toArray.flatMap { case (value, script) =>
      le64(value) ++ varInt(script.length) ++ script
    }
    val lockTime = le32(0)
    version ++ varInt(nInputs) ++ inputs ++ varInt(outputs.length) ++ outs ++ lockTime
  }

  /** Double-SHA256 of the transaction bytes = Bitcoin txid in internal byte order. */
  def btcTxId(tx: Array[Byte]): Array[Byte] = Sha256.hash(Sha256.hash(tx))

  val contractScript: String =
    """{
      |  val tx = getVar[Coll[Byte]](1).get
      |  val expectedTxId = SELF.R4[Coll[Byte]].get
      |  val expectedScriptHash = SELF.R5[Coll[Byte]].get
      |
      |  // txid = double-SHA256 of the (witness-stripped) transaction bytes;
      |  // this also authenticates all the bytes parsed below
      |  val idOk = sha256(sha256(tx)) == expectedTxId
      |
      |  // unsigned value of a byte
      |  val ub = { (b: Byte) =>
      |    val v = b.toInt
      |    if (v < 0) v + 256 else v
      |  }
      |
      |  // reads Bitcoin's variable-length integer at the given offset,
      |  // returns (value, encoding size); supports 1-byte and 0xfd (3-byte) encodings
      |  val readVarInt = { (off: Int) =>
      |    val first = ub(tx(off))
      |    if (first < 253) (first, 1)
      |    else (ub(tx(off + 1)) + 256 * ub(tx(off + 2)), 3)
      |  }
      |
      |  val maxInputs = Coll(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      |  val maxOutputs = Coll(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      |
      |  // skip version (4 bytes), read the input count
      |  val nInputsRes = readVarInt(4)
      |  val nInputs = nInputsRes._1
      |  val inputsStart = 4 + nInputsRes._2
      |  val inputCountOk = nInputs <= 10
      |
      |  // Only scan inputs when the count is within the supported bound; otherwise the
      |  // transaction is malformed/too large and the contract must reject.
      |  val outCountOff = if (inputCountOk) {
      |    maxInputs.fold(inputsStart, { (off: Int, i: Int) =>
      |      if (i < nInputs) {
      |        val sLen = readVarInt(off + 36)
      |        off + 36 + sLen._2 + sLen._1 + 4
      |      } else off
      |    })
      |  } else inputsStart
      |
      |  // read the output count and scan the outputs: each is value(8) + scriptPubKey(varint-prefixed)
      |  val nOutputsRes = if (inputCountOk) readVarInt(outCountOff) else (0, 0)
      |  val nOutputs = nOutputsRes._1
      |  val outputCountOk = inputCountOk && nOutputs <= 10
      |
      |  val outputsStart = outCountOff + nOutputsRes._2
      |  val scanRes = if (outputCountOk) {
      |    maxOutputs.fold((outputsStart, false), { (acc: (Int, Boolean), i: Int) =>
      |      if (i < nOutputs) {
      |        val off = acc._1
      |        val sLen = readVarInt(off + 8)
      |        val scriptStart = off + 8 + sLen._2
      |        val scriptEnd = scriptStart + sLen._1
      |        val script = tx.slice(scriptStart, scriptEnd)
      |        (scriptEnd, acc._2 || sha256(script) == expectedScriptHash)
      |      } else acc
      |    })
      |  } else (outputsStart, false)
      |
      |  // the whole transaction must be consumed, up to the final lockTime (4 bytes)
      |  val parsedAll = if (outputCountOk) scanRes._1 + 4 == tx.size else false
      |
      |  sigmaProp(idOk && inputCountOk && outputCountOk && parsedAll && scanRes._2)
      |}""".stripMargin

  /** Creates a box guarded by the contract with the given expected txid (R4) and
    * expected output script hash (R5), then tries to spend it providing `txBytes`
    * in the context variable. */
  private def spend(
      txId: Array[Byte],
      scriptHash: Array[Byte],
      txBytes: Array[Byte],
      expectSuccess: Boolean): Unit = {
    val prop = compile(emptyEnv, contractScript).asBoolValue.toSigmaProp
    val tree = mkTestErgoTree(prop)

    val boxToSpend = testBox(10, tree, creationHeight = 5,
      additionalRegisters = Map(
        org.ergoplatform.ErgoBox.R4 -> ByteArrayConstant(txId.toColl),
        org.ergoplatform.ErgoBox.R5 -> ByteArrayConstant(scriptHash.toColl)))
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

  property("real transaction - spend with the first output's script") {
    spend(btcTxId(block170Tx), Sha256.hash(block170Output0Script), block170Tx, expectSuccess = true)
  }

  property("real transaction - spend with the second output's script") {
    spend(btcTxId(block170Tx), Sha256.hash(block170Output1Script), block170Tx, expectSuccess = true)
  }

  property("real transaction - wrong expected script hash") {
    spend(btcTxId(block170Tx), Sha256.hash(Array[Byte](1, 2, 3)), block170Tx, expectSuccess = false)
  }

  property("real transaction - script hash of a non-output fragment") {
    // hash of the input's scriptSig: present in the transaction, but not as an output script
    val scriptSig = block170Tx.slice(42, 42 + 72)
    spend(btcTxId(block170Tx), Sha256.hash(scriptSig), block170Tx, expectSuccess = false)
  }

  property("real transaction - wrong txid") {
    spend(btcTxId(block170Tx).reverse, Sha256.hash(block170Output0Script), block170Tx,
      expectSuccess = false)
  }

  property("real transaction - tampered bytes") {
    val tampered = block170Tx.clone()
    tampered(120) = (tampered(120) ^ 1).toByte
    spend(btcTxId(block170Tx), Sha256.hash(block170Output0Script), tampered, expectSuccess = false)
  }

  property("synthetic transaction - multiple inputs and outputs") {
    val marker = Array.tabulate(25)(_.toByte)
    val tx = syntheticTx(3, Seq(
      50000L -> Array.tabulate(22)(i => (i * 7).toByte),
      75000L -> marker,
      1000L -> Array.tabulate(34)(i => (i * 3).toByte)))
    spend(btcTxId(tx), Sha256.hash(marker), tx, expectSuccess = true)
  }

  property("synthetic transaction - 0xfd varint for a long script") {
    val longScript = Array.tabulate(300)(i => (i * 5).toByte) // scriptLen uses the 3-byte varint
    val tx = syntheticTx(2, Seq(1000L -> longScript), scriptSigSize = 260)
    spend(btcTxId(tx), Sha256.hash(longScript), tx, expectSuccess = true)
  }

  property("synthetic transaction - too many inputs is rejected") {
    val marker = Array.tabulate(25)(_.toByte)
    val tx = syntheticTx(11, Seq(1000L -> marker))
    spend(btcTxId(tx), Sha256.hash(marker), tx, expectSuccess = false)
  }

  property("synthetic transaction - max supported inputs and outputs") {
    val marker = Array.tabulate(25)(_.toByte)
    val outs = (1 to 9).map(i => (i * 1000L) -> Array.tabulate(22)(j => (i * j).toByte)) :+ (10000L -> marker)
    val tx = syntheticTx(10, outs)
    spend(btcTxId(tx), Sha256.hash(marker), tx, expectSuccess = true)
  }
}
