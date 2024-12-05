package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import sigma.data.AvlTreeData
import sigmastate.CompilerCrossVersionProps
import sigma.ast.{ErgoTree, IntConstant, LongConstant}
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigma.ast.syntax._
import sigmastate.interpreter.Interpreter


class ColdWalletAdvContractExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
  property("Evaluation - ColdWallet Advanced Contract Example") {

    val alice = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val carol = new ContextEnrichingTestProvingInterpreter // private key controlling hot-wallet funds
    val carolPubKey = carol.dlogSecrets.head.publicImage

    val blocksIn24h = 500
    val percent1Key = 1
    val percent2Key = 10
    val minSpend = 100

    val env = Map(
      "user1" -> alicePubKey,
      "user2" -> bobPubKey,
      "user3" -> carolPubKey,
      "blocksIn24h" -> IntConstant(blocksIn24h),
      "percent1Key" -> IntConstant(percent1Key),
      "percent2Key" -> IntConstant(percent2Key),
      "minSpend" -> IntConstant(minSpend)
    )

    // assume that person topping up this address creates the box correctly... i.e., sets the correct values of R4 and R5
    // To Do: example that allows spending multiple Boxes with the same script
    //        idea is as follows:
    //           1. make global avbl1Key as sum of all avbl1Key's, where each avbl1Key is computed via its own start
    //           2. make global avbl2Key as sum of all avbl2Key's, where each avbl2Key is computed via its own start
    //           3. make start of output box as max of all starts of the inputs
    //           4. output must contain at least one box of same type with min bal based on avbl1Key/avbl2Key

    val scriptProp = compile(env,
      """{
        |  val depth = HEIGHT - SELF.creationInfo._1 // number of confirmations
        |  val start = min(depth, SELF.R4[Int].get) // height at which period started
        |
        |  val notExpired = HEIGHT - start <= blocksIn24h
        |  val newStart:Int = if (notExpired) start else HEIGHT
        |  val selfValue = SELF.value      // this declarations necessary to make GraphBuilding equivalent to calcTree
        |  val selfR6 = SELF.R6[Long].get  // see see TestsBase.compile
        |  // available for one user to spend in this period
        |  val avbl1Key = if (notExpired)
        |                  SELF.R5[Long].get
        |                else
        |                  max(selfValue * percent1Key / 100, minSpend)
        |
        |  // available for two users to spend in this period
        |  val avbl2Key = if (notExpired)
        |                  selfR6
        |                else
        |                  max(selfValue * percent2Key / 100, minSpend)
        |
        |  val out = OUTPUTS(0) // change output
        |
        |  // to do: double check if negatives values of avbl1Key create any problem
        |  val isValid1Key = INPUTS.size == 1 && out.propositionBytes == SELF.propositionBytes &&
        |    out.R4[Int].get >= newStart && out.value >= selfValue - avbl1Key &&
        |    out.value - out.R5[Long].get == selfValue - avbl1Key && out.R6[Long].get == selfR6
        |
        |  val isValid2Key = INPUTS.size == 1 && out.propositionBytes == SELF.propositionBytes &&
        |    out.R4[Int].get >= newStart && out.value >= selfValue - avbl2Key &&
        |    out.value - out.R6[Long].get == selfValue - avbl2Key && out.R5[Long].get == SELF.R5[Long].get
        |
        |  allOf(Coll(user1, user2, user3)) || (
        |    (anyOf(Coll(user1, user2, user3)) && (selfValue <= avbl1Key || isValid1Key)) ||
        |    (atLeast(2, Coll(user1, user2, user3)) && (selfValue <= avbl2Key || isValid2Key))
        |  )
        |}""".stripMargin).asSigmaProp

    val scriptTree = mkTestErgoTree(scriptProp)
    val address = Pay2SHAddress(scriptProp)

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below
    val depositAmount = 100000L
    val depositHeight = 100
    val avbl1Key = depositAmount * percent1Key/100
    val avbl2Key = depositAmount * percent2Key/100

    val depositOutput = testBox(depositAmount, address.script, depositHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // can keep value in R4 initially
        R5 -> LongConstant(avbl1Key), // keeping it below min will make UTXO unspendable
        R6 -> LongConstant(avbl2Key) // keeping it below min will make UTXO unspendable
      )
    )

    val firstWithdrawHeight = depositHeight + 1 //

    val spendEnv = Interpreter.emptyEnv

    // One of Alice, Bob  or Carol withdraws
    val firstWithdrawAmount1Key = depositAmount * percent1Key / 100 // less than or equal to percent
    val firstChangeAmount1Key = depositAmount - firstWithdrawAmount1Key

    val firstChangeOutput1Key = testBox(firstChangeAmount1Key, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(avbl1Key - firstWithdrawAmount1Key), // new avbl1Key (= 0)
        R6 -> LongConstant(avbl2Key) // new avbl2Key (= old avbl2Key)
      )
    )
    val firstWithdrawOutput1Key = testBox(firstWithdrawAmount1Key, ErgoTree.fromSigmaBoolean(carolPubKey), firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx1Key = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput1Key, firstWithdrawOutput1Key))

    val firstWithdrawContext1Key = ErgoLikeContextTesting(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx1Key,
      self = depositOutput,
      activatedVersionInTests
    )

    val verifier = new ErgoLikeTestInterpreter

    val proofAliceWithdraw = alice.prove(spendEnv, scriptTree, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, scriptTree, firstWithdrawContext1Key, proofAliceWithdraw, fakeMessage).get._1 shouldBe true

    val proofBobWithdraw = bob.prove(env, scriptTree, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, scriptTree, firstWithdrawContext1Key, proofBobWithdraw, fakeMessage).get._1 shouldBe true

    val proofCarolWithdraw = carol.prove(env, scriptTree, firstWithdrawContext1Key, fakeMessage).get.proof
    verifier.verify(env, scriptTree, firstWithdrawContext1Key, proofCarolWithdraw, fakeMessage).get._1 shouldBe true

    // any two of Alice, Bob  or Carol withdraws
    val firstWithdrawAmount2Key = depositAmount * percent2Key / 100 // less than or equal to percent
    val firstChangeAmount2Key = depositAmount - firstWithdrawAmount2Key

    val firstChangeOutput2Key = testBox(firstChangeAmount2Key, address.script, firstWithdrawHeight, Nil,
      Map(
        R4 -> IntConstant(depositHeight), // newStart (= old start)
        R5 -> LongConstant(avbl1Key), // new avbl1Key (= 0)
        R6 -> LongConstant(avbl2Key - firstWithdrawAmount2Key) // new avbl2Key (= old avbl2Key)
      )
    )
    val firstWithdrawOutput2Key = testBox(firstWithdrawAmount2Key, ErgoTree.fromSigmaBoolean(carolPubKey), firstWithdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val firstWithdrawTx2Key = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(firstChangeOutput2Key, firstWithdrawOutput2Key))

    val firstWithdrawContext2Key = ErgoLikeContextTesting(
      currentHeight = firstWithdrawHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = firstWithdrawTx2Key,
      self = depositOutput,
      activatedVersionInTests
    )

    val proofAliceBobWithdraw = alice.withSecrets(bob.dlogSecrets).prove(spendEnv, scriptTree, firstWithdrawContext2Key, fakeMessage).get.proof
    verifier.verify(env, scriptTree, firstWithdrawContext2Key, proofAliceBobWithdraw, fakeMessage).get._1 shouldBe true

  }

}
