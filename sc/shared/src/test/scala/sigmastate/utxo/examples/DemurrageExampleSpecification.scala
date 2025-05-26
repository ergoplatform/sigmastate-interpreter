package sigmastate.utxo.examples

import sigmastate.interpreter.Interpreter._
import org.ergoplatform._
import sigma.data.AvlTreeData
import sigma.ast.ShortConstant
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigma.interpreter.ContextExtension
import sigma.ast.syntax._

class DemurrageExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = true
  implicit lazy val IR = new TestingIRContext

  /**
    * Demurrage currency example.
    *
    * The idea is that miners enforce users to combine a guarding script of a user ("regular_script") with a condition
    * that anyone (presumably, a miner) can spend no more "demurrage_cost" amount of tokens from an output of the user
    * after "demurrage_period" blocks since output creation. If the user is relocating the money from the output before
    * that height, the miner can charge according to output lifetime.
    *
    * We assume that it is enforced by a consensus protocol to store height when an input got into a block in the
    * register R3 (if the transaction is not included into the blockchain yet, then R3 contains the current height of
    * the blockchain).
    *
    * (regular_script) ∨
    * (height > (self.R3 + demurrage_period)
    *   ∧ has_output(value >= self.value − demurrage_cost, script = self.script, R3 + 50 <= height)
    * )
    */
  property("Evaluation - Demurrage Example") {
    val demurragePeriod = 100
    val demurrageCoeff = 2

    val outIdxVarId = Byte.MaxValue

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter()

    //backer's prover with his private key
    val userProver = new ContextEnrichingTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val minerProver = new ContextEnrichingTestProvingInterpreter().withContextExtender(outIdxVarId, ShortConstant(0))

    val regScript = userProver.dlogSecrets.head.publicImage

    val env = Map(
      ScriptNameProp -> "Demurrage",
      "demurragePeriod" -> demurragePeriod,
      "demurrageCoeff" -> demurrageCoeff,
      "regScript" -> regScript
    )

    val propTree = mkTestErgoTree(compile(env,
      """{
        | val outIdx = getVar[Short](127).get
        | val out = OUTPUTS(outIdx)
        |
        | val c1 = allOf(Coll(
        |   HEIGHT >= SELF.R3[(Int, Coll[Byte])].get._1 + demurragePeriod,
        |   SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Int, Coll[Byte])].get._1) <= 0
        | ))
        |
        | val c2 = allOf(Coll(
        |   HEIGHT >= SELF.R3[(Int, Coll[Byte])].get._1 + demurragePeriod,
        |   out.R3[(Int, Coll[Byte])].get._1 == HEIGHT,
        |   out.value >= SELF.value - demurrageCoeff * SELF.bytes.size * (HEIGHT - SELF.R3[(Int, Coll[Byte])].get._1),
        |   out.propositionBytes == SELF.propositionBytes
        | ))
        |
        | anyOf(Coll(regScript, c1, c2))
        | }
      """.stripMargin).asSigmaProp)

    val inHeight = 0
    val outValue = 100
    val approxSize = createBox(outValue, propTree, inHeight).bytes.length + 2
    val inValue: Int = (outValue + demurrageCoeff * demurragePeriod * approxSize).toInt

    val ce = ContextExtension(Map(outIdxVarId -> ShortConstant(0)))

    //case 1: demurrage time hasn't come yet
    val currentHeight1 = inHeight + demurragePeriod - 1

    val tx1 = createTransaction(createBox(outValue, propTree, currentHeight1))
    val selfBox = createBox(inValue, propTree, inHeight)
    val ctx1 = ErgoLikeContextTesting(
      currentHeight1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx1,
      self = selfBox, activatedVersionInTests,
      extension = ce)

    //user can spend all the money
    val uProof1 = userProver.prove(propTree, ctx1, fakeMessage).get.proof
    verifier.verify(emptyEnv, propTree, ctx1, uProof1, fakeMessage).get._1 shouldBe true

    //miner can't spend any money
    minerProver.prove(propTree, ctx1, fakeMessage).isSuccess shouldBe false
    verifier.verify(propTree, ctx1, NoProof, fakeMessage).get._1 shouldBe false

    //case 2: demurrage time has come
    val currentHeight2 = inHeight + demurragePeriod
    val tx2 = createTransaction(createBox(outValue, propTree, currentHeight2))
    val ctx2 = ErgoLikeContextTesting(
      currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx2,
      self = selfBox, activatedVersionInTests,
      extension = ce)

    //user can spend all the money
    val uProof2 = userProver.prove(propTree, ctx2, fakeMessage).get.proof
    verifier.verify(env, propTree, ctx2, uProof2, fakeMessage).get._1 shouldBe true

    //miner can spend "demurrageCoeff * demurragePeriod" tokens
    val b = createBox(outValue, propTree, currentHeight2)
    val tx3 = createTransaction(b)
    val ctx3 = ErgoLikeContextTesting(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(b, selfBox),
      spendingTransaction = tx3,
      self = selfBox, activatedVersionInTests)

    assert(ctx3.spendingTransaction.outputs.head.propositionBytes sameElements ctx3.boxesToSpend(ctx3.selfIndex).propositionBytes)

    val mProverRes1 = minerProver.prove(propTree, ctx3, fakeMessage).get
    val _ctx3: ErgoLikeContext = ctx3.withExtension(mProverRes1.extension)
    verifier.verify(propTree, _ctx3, mProverRes1, fakeMessage: Array[Byte]).get._1 shouldBe true

    //miner can't spend more
    val b2 = createBox(outValue - 1, propTree, currentHeight2)
    val tx4 = createTransaction(b2)
    val ctx4 = ErgoLikeContextTesting(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(b2, selfBox),
      spendingTransaction = tx4,
      self = selfBox, activatedVersionInTests,
      extension = ce)

    minerProver.prove(propTree, ctx4, fakeMessage).isSuccess shouldBe false
    verifier.verify(propTree, ctx4, NoProof, fakeMessage).get._1 shouldBe false

    //miner can spend less
    val tx5 = createTransaction(createBox(outValue + 1, propTree, currentHeight2))

    val ctx5 = ErgoLikeContextTesting(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx5,
      self = selfBox, activatedVersionInTests,
      extension = ce)

    val mProof2 = minerProver.prove(propTree, ctx5, fakeMessage).get
    verifier.verify(propTree, ctx5, mProof2, fakeMessage).get._1 shouldBe true

    //miner can destroy a box if it contains less than the storage fee
    val iv = inValue - outValue
    val b3 = createBox(iv, FalseTree, currentHeight2)
    val tx6 = createTransaction(b3)
    val selfBox6 = createBox(iv, propTree, inHeight)
    val ctx6 = ErgoLikeContextTesting(
      currentHeight = currentHeight2,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(b3, selfBox6),
      spendingTransaction = tx6,
      self = selfBox6, activatedVersionInTests,
      extension = ce)

    val mProof3 = minerProver.prove(propTree, ctx6, fakeMessage).get
    verifier.verify(propTree, ctx6, mProof3, fakeMessage).get._1 shouldBe true

  }
}
