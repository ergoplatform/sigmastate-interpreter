package sigmastate

import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigma.ast.syntax._
import org.scalatest.TryValues._
import sigma.data.AvlTreeData
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}

import scala.util.Success

class FailingToProveSpec extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  /**
    * Both properties should work fine.
    * Currently the problem in first case is that even if we evaluate `withdrawCondition1 == true`
    * we are still try to evaluate `withdrawCondition2` which leads to something like IndexOutOfBoundsError.
    * Cause second condition has 3 outputs in body, while we are have only two in tx.
    */
  property("successfully evaluate proof 1") {
    val interpreter = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter()

    val env = Map.empty[String, Any]
    val tree = mkTestErgoTree(compile(env,
      s"""
         | {
         |  val withdrawCondition1 =
         |        OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  val withdrawCondition2 =
         |        OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue.toSigmaProp)

    val selfBox = testBox(200L, tree, 0)
    val o1 = testBox(101L, TrueTree, 5001)
    val o2 = testBox(99L, TrueTree, 5001)
    val tx =  createTransaction(IndexedSeq(o1, o2))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 5001,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self = selfBox,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      activatedVersion = activatedVersionInTests)
    val proof = interpreter.prove(emptyEnv + (ScriptNameProp -> "prove"), tree, ctx, fakeMessage).success.value.proof
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), tree, ctx, proof, fakeMessage) shouldBe Success((true, 17L))
  }

  property("successfully evaluate proof 2") {
    val interpreter = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter()

    val env = Map.empty[String, Any]
    val tree = mkTestErgoTree(compile(env,
      s"""
         | {
         |
         |  val withdrawCondition1 =
         |              OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  val withdrawCondition2 =
         |              OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue.toSigmaProp)

    val selfBox = testBox(200L, tree, 0)
    val o1 = testBox(102L, TrueTree, 5001)
    val o2 = testBox(98L, TrueTree, 5001)
    val o3 = testBox(100L, TrueTree, 5001)
    val tx =  createTransaction(IndexedSeq(o1, o2, o3))
    val ctx = ErgoLikeContextTesting(
      currentHeight = 5001,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self = selfBox,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      activatedVersion = activatedVersionInTests)
    val proof = interpreter.prove(emptyEnv + (ScriptNameProp -> "prove"), tree, ctx, fakeMessage).success.value.proof
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), tree, ctx, proof, fakeMessage) shouldBe Success((true, 27L))
  }

}
