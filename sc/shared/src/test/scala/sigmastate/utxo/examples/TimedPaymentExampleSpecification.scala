package sigmastate.utxo.examples

import org.ergoplatform._
import sigma.ast.{ErgoTree, IntConstant}
import sigma.data.AvlTreeData
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigma.ast.syntax._
import sigma.exceptions.InterpreterException

class TimedPaymentExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  import ErgoAddressEncoder._

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)

  property("Evaluation - Timed payment Tx Example") {

    val alice = new ContextEnrichingTestProvingInterpreter // customer at coffee shop
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val bob = new ContextEnrichingTestProvingInterpreter // owner of coffee shop (or payment address of coffee shop)
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val env = Map(
      "alice" -> alicePubKey
    )

    val script = mkTestErgoTree(compile(env,
      """{ alice && HEIGHT <= getVar[Int](1).get }""".stripMargin
    ).asSigmaProp)

    val address = Pay2SHAddress(script)
    // The above is a "timed address".
    // Payments sent from this wallet are must be confirmed within a certain height (given in the first output's R4)

    val depositAmount = 10
    val depositHeight = 50

    // someone creates a transaction that outputs a box depositing money into the wallet.
    // In the example, we don't create the transaction; we just create a box below


    val depositOutput = testBox(depositAmount, address.script, depositHeight)

    // Now Alice wants to give Bob (coffee shop owner) some amount from the wallet in a "timed" way.

    val withdrawAmount = 10
    val withdrawHeight = 100
    val confDeadline = 110

    val timedWithdrawOutput = testBox(withdrawAmount, ErgoTree.fromSigmaBoolean(bobPubKey), withdrawHeight)

    //normally this transaction would be invalid, but we're not checking it in this test
    val withdrawTx = createTransaction(IndexedSeq(timedWithdrawOutput))

    val withdrawContext = ErgoLikeContextTesting(
      currentHeight = 109,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput,
      activatedVersionInTests
    )

    val proofWithdraw = alice.withContextExtender(
      1, IntConstant(confDeadline)
    ).prove(env, script, withdrawContext, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter

    verifier.verify(env, script, withdrawContext, proofWithdraw, fakeMessage).get._1 shouldBe true

    val withdrawContextBad = ErgoLikeContextTesting(
      currentHeight = 111,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(depositOutput),
      spendingTransaction = withdrawTx,
      self = depositOutput,
      activatedVersionInTests
    )
    an [InterpreterException] should be thrownBy (alice.withContextExtender(
      1, IntConstant(confDeadline - 20)
    ).prove(env, script, withdrawContext, fakeMessage).get.proof)

    an [InterpreterException] should be thrownBy (alice.withContextExtender(
      1, IntConstant(confDeadline)
    ).prove(env, script, withdrawContextBad, fakeMessage).get.proof)

    // below gives error. Need to check if it is designed behavior or a bug
    // verifier.verify(env, script, withdrawContextBad, proofWithdraw, fakeMessage).get._1 shouldBe false
  }
}
