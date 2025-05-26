
package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6, R7}
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigma.data.{AvlTreeData, ProveDlog}
import sigma.ast.{ByteArrayConstant, ByteConstant, ErgoTree, IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter._
import sigma.ast.syntax._

class RPSGameExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  implicit lazy val IR = new TestingIRContext
  /** RPS game:

     Alice creates a RPS game of "playAmount" ergs by creating a Half-game UTXO called the "halfGameOutput" output below.
     Another player (Bob) then sends a transaction spending Alice's UTXO and creating two outputs called "fullGameOutputs" ("Full game" UTXOs).
     After Alice opens her commitment (see below), the fullGameOutputs can be spent by the winner.
     The transactions encode the following protocol.

     protocol:
       Step 1: Alice commits to secret number a in {0, 1, 2} as follows:
                  Generate random s and compute h = Hash(s||a)
                  h is the commitment to a
               Alice also selects the "play amount", the amount each player must spend to participate.
               She generates a halfGameOutput encoding h and some spending condition given below by halfGameScript
       Step 2: Bob chooses random number b from {0, 1, 2} (public) and creates a new tx spending Alice's UTXO along with
               some others and creating two outputs that has the spending conditions given by fullGameScript.
               (one of the conditions being that the amount of that output is >= twice the play amount.)
       Step 3: Alice reveals (s, a) to open her commitment and winner is decided as per RPS rules.
               If Alice fails to open her commitment before some deadline then Bob automatically wins.

    For simplicity, we will use following bytes to designate choices
        0x00 = 0 = rock
        0x01 = 1 = paper
        0x02 = 2 = scissors

    */
  property("Evaluation - RpsGame Example") {

    // Alice is first player, who initiates the game
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey:ProveDlog = alice.dlogSecrets.head.publicImage

    val a:Byte = (scala.util.Random.nextInt.abs % 3).toByte
    val s:Array[Byte] = Random.randomBytes(31) // random padding
    val h = Blake2b256(s :+ a) // Alice's commitment

    val fullGameEnv = Map(
      ScriptNameProp -> "fullGameScriptEnv",
      "alice" -> alicePubKey,
      "k" -> h
    )

    val fullGameScript = mkTestErgoTree(compile(fullGameEnv,
      """{
        |  val s = getVar[Coll[Byte]](0).get  // Alice's secret byte string s
        |  val a = getVar[Byte](1).get        // Alice's secret choice a (represented as a byte)
        |  val b = SELF.R4[Byte].get          // Bob's public choice b (represented as a byte)
        |  val bob = SELF.R5[SigmaProp].get
        |  val bobDeadline = SELF.R6[Int].get // after this height, Bob gets to spend unconditionally
        |  val drawPubKey = SELF.R7[SigmaProp].get
        |
        |  (bob && HEIGHT > bobDeadline) || {
        |    val valid_a = (a == 0 || a == 1 || a == 2) && blake2b256(s ++ Coll(a)) == k
        |    valid_a && {
        |      val a_minus_b = a - b
        |      if (a_minus_b == 0) drawPubKey else {
        |        if ((a_minus_b) == 1 || (a_minus_b) == -2) alice else bob
        |      }
        |    }
        |  }
        |}""".stripMargin
    ).asSigmaProp)

    val halfGameEnv = Map(
      ScriptNameProp -> "halfGameScript",
      "alice" -> alicePubKey,
      "fullGameScriptHash" -> Blake2b256(fullGameScript.bytes)
    )

    // Note that below script allows Alice to spend the half-game output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-game output
    // before some minimum height.
    val halfGameScript = mkTestErgoTree(compile(halfGameEnv,
      """{
        |  OUTPUTS.forall{(out:Box) =>
        |    val b             = out.R4[Byte].get
        |    val bobDeadline   = out.R6[Int].get
        |    val validBobInput = b == 0 || b == 1 || b == 2
        |    // Bob needs to ensure that out.R5 contains bobPubKey
        |    bobDeadline >= HEIGHT+30 &&
        |    out.value >= SELF.value &&
        |    validBobInput &&
        |    blake2b256(out.propositionBytes) == fullGameScriptHash
        |  } && OUTPUTS.size == 2 &&
        |  OUTPUTS(0).R7[SigmaProp].get == alice // Alice does not care for Bob's draw case
        |  // Bob needs to ensure that OUTPUTS(1).R7 contains his public key
        |}
      """.stripMargin).asBoolValue.toSigmaProp)

    /////////////////////////////////////////////////////////
    //// Alice starts creating a Half-Game
    /////////////////////////////////////////////////////////

    // she creates a transaction that outputs a box with halfGame script.
    // In the example, we don't create the transaction; we just create a box below

    val halfGameCreationHeight = 70
    val playAmount = 10      // LongConstant(10)

    val halfGameOutput = testBox(playAmount, halfGameScript, halfGameCreationHeight)

    /////////////////////////////////////////////////////////
    //// above halfGameOutput is a Half-Game "box" created by Alice.
    /////////////////////////////////////////////////////////

    //a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    /////////////////////////////////////////////////////////
    //// Bob will spend halfGameOutput and generate a full game
    /////////////////////////////////////////////////////////

    // a variable indicating height at which the tx spending halfGameTx is created
    val fullGameCreationHeight = halfGameCreationHeight + 10

    val bob = new ContextEnrichingTestProvingInterpreter
    val bobPubKey:ProveDlog = bob.dlogSecrets.head.publicImage
    val bobDeadline = 120 // height after which it become's Bob's money
    val b:Byte = (scala.util.Random.nextInt.abs % 3).toByte

    val fullGameOutput0 = testBox(playAmount, fullGameScript, fullGameCreationHeight, Nil,
      Map(
        R4 -> ByteConstant(b),
        R5 -> SigmaPropConstant(bobPubKey),
        R6 -> IntConstant(bobDeadline),
        R7 -> SigmaPropConstant(alicePubKey)
      )
    )

    val fullGameOutput1 = testBox(playAmount, fullGameScript, fullGameCreationHeight, Nil,
      Map(
        R4 -> ByteConstant(b),
        R5 -> SigmaPropConstant(bobPubKey),
        R6 -> IntConstant(bobDeadline),
        R7 -> SigmaPropConstant(bobPubKey)
      )
    )

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val fullGameTx = createTransaction(IndexedSeq(fullGameOutput0, fullGameOutput1))

    val fullGameContext = ErgoLikeContextTesting(
      currentHeight = fullGameCreationHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(halfGameOutput),
      spendingTransaction = fullGameTx,
      self = halfGameOutput, // what is the use of self?
      activatedVersionInTests
    )

    // bob (2nd player) is generating a proof and it is passing verification
    val proofFullGame = bob.prove(halfGameEnv, halfGameScript, fullGameContext, fakeMessage).get.proof

    verifier.verify(halfGameEnv, halfGameScript, fullGameContext, proofFullGame, fakeMessage).get._1 shouldBe true

    /////////////////////////////////////////////////////////
    //// fullGameOutput represents the Full-Game "box" created by Bob.
    /////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////
    // Possibility 1: Alice reveals secret
    /////////////////////////////////////////////////////////

    val gameOverHeight = fullGameCreationHeight + 10
    // assume paying to Carol
    val carol = new ContextEnrichingTestProvingInterpreter
    val carolPubKey:ProveDlog = carol.dlogSecrets.head.publicImage

    // note that playAmount below is not checked. It could be anything.
    val gameOverOutput = testBox(playAmount, ErgoTree.fromSigmaBoolean(carolPubKey), gameOverHeight)

    // normally this transaction would be invalid, but we're not checking it in this test
    val gameOverTx = createTransaction(gameOverOutput)

    val aliceProver = alice.withContextExtender(0, ByteArrayConstant(s)).withContextExtender(1, ByteConstant(a))
    val bobProver = bob.withContextExtender(0, ByteArrayConstant(s)).withContextExtender(1, ByteConstant(a))

    val winContext0 = ErgoLikeContextTesting(
      currentHeight = gameOverHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput0, fullGameOutput1),
      spendingTransaction = gameOverTx,
      self = fullGameOutput0, activatedVersionInTests
    )

    val winContext1 = ErgoLikeContextTesting(
      currentHeight = gameOverHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput0, fullGameOutput1),
      spendingTransaction = gameOverTx,
      self = fullGameOutput1, activatedVersionInTests
    )

    a - b match {
      case 0 => // draw
        printDebug("Draw")
        /////////////////////////////////////////////////////////
        // Possibility 1.1: draw
        /////////////////////////////////////////////////////////

        val drawContextAlice = ErgoLikeContextTesting(
          currentHeight = gameOverHeight,
          lastBlockUtxoRoot = AvlTreeData.dummy,
          minerPubkey = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend = IndexedSeq(fullGameOutput0),
          spendingTransaction = gameOverTx,
          self = fullGameOutput0, activatedVersionInTests
        )

        val proofAliceDraw = aliceProver.prove(fullGameEnv, fullGameScript, drawContextAlice, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, drawContextAlice, proofAliceDraw, fakeMessage).get._1 shouldBe true

        val drawContextBob = ErgoLikeContextTesting(
          currentHeight = gameOverHeight,
          lastBlockUtxoRoot = AvlTreeData.dummy,
          minerPubkey = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend = IndexedSeq(fullGameOutput1),
          spendingTransaction = gameOverTx,
          self = fullGameOutput1, activatedVersionInTests
        )

        val proofBobDraw = bobProver.prove(fullGameEnv, fullGameScript, drawContextBob, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, drawContextBob, proofBobDraw, fakeMessage).get._1 shouldBe true

      case 1 | -2 => // alice wins
        printDebug("Alice won")
        /////////////////////////////////////////////////////////
        // Possibility 1.2: Alice wins
        /////////////////////////////////////////////////////////

        val proofAliceWin0 = aliceProver.prove(fullGameEnv, fullGameScript, winContext0, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, winContext0, proofAliceWin0, fakeMessage).get._1 shouldBe true

        val proofAliceWin1 = aliceProver.prove(fullGameEnv, fullGameScript, winContext1, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, winContext1, proofAliceWin1, fakeMessage).get._1 shouldBe true
      case _ => // bob wins
        printDebug("Bob won")
        /////////////////////////////////////////////////////////
        // Possibility 1.3: Bob wins
        /////////////////////////////////////////////////////////
        val proofBobWin0 = bobProver.prove(fullGameEnv, fullGameScript, winContext0, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, winContext0, proofBobWin0, fakeMessage).get._1 shouldBe true

        val proofBobWin1 = bobProver.prove(fullGameEnv, fullGameScript, winContext1, fakeMessage).get
        verifier.verify(fullGameEnv, fullGameScript, winContext1, proofBobWin1, fakeMessage).get._1 shouldBe true
    }

    /////////////////////////////////////////////////////////
    // Possibility 2: (Bob wins and Alice does not reveal secret) OR (Alice wins and does not spend before bobDeadline)
    // Bob can spend after bobDeadline.
    /////////////////////////////////////////////////////////

    val defaultWinHeight = bobDeadline + 1

    // assume Bob is paying to Carol
    // note that playAmount*2 below is not checked. It could be anything.
    val defaultWinOutput = testBox(playAmount*2, ErgoTree.fromSigmaBoolean(carolPubKey), defaultWinHeight)

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val defaultWinTx = createTransaction(defaultWinOutput)

    val defaultWinContext0 = ErgoLikeContextTesting(
      currentHeight = defaultWinHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput0, fullGameOutput1),
      spendingTransaction = defaultWinTx,
      self = fullGameOutput0, // what is the use of self?
      activatedVersionInTests
    )
    val defaultWinContext1 = ErgoLikeContextTesting(
      currentHeight = defaultWinHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput0, fullGameOutput1),
      spendingTransaction = defaultWinTx,
      self = fullGameOutput1, // what is the use of self?
      activatedVersionInTests
    )

    val sDummy = Array[Byte]()  // empty value for s; commitment cannot be opened but still Bob will be able to spend
    val aDummy:Byte = 0
    // below we need to specify a and s (even though they are not needed)
    val proofDefaultWin0 = bobProver.prove(fullGameEnv, fullGameScript, defaultWinContext0, fakeMessage).get
    verifier.verify(fullGameEnv, fullGameScript, defaultWinContext0, proofDefaultWin0, fakeMessage).get._1 shouldBe true

    val proofDefaultWin1 = bobProver.prove(fullGameEnv, fullGameScript, defaultWinContext1, fakeMessage).get
    verifier.verify(fullGameEnv, fullGameScript, defaultWinContext1, proofDefaultWin1, fakeMessage).get._1 shouldBe true
  }

}
