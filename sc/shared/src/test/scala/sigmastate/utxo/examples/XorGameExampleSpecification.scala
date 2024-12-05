
package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6}
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigma.Colls
import sigma.data.{AvlTreeData, ProveDlog}
import sigma.ast.{ByteArrayConstant, ByteConstant, IntConstant, SigmaPropConstant}
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter._
import sigma.ast.syntax._

class XorGameExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** XOR game:

     Alice creates a XOR game of "playAmount" ergs by creating a Half-game UTXO called the "halfGameOutput" output below.
     Another player (Bob) then sends a transaction spending Alice's UTXO and creating another output called "fullGameOutput" (a "Full game" UTXO).
     After Alice opens her commitment (see below), the fullGameOutput can be spent by the winner.
     The transactions encode the following protocol.

     protocol:
       Step 1: Alice commits to secret bit a as follows:
                  Generate random s and compute h = Hash(s||a)
                  h is the commitment to a
               Alice also selects the "play amount", the amount each player must spend to participate.
               She generates a halfGameOutput encoding h and some spending condition given below by halfGameScript
       Step 2: Bob chooses random bit b (public) and creates a new tx spending Alice's UTXO along with
               some others and creating one output that has the spending conditions given by fullGameScript.
               (one of the conditions being that the amount of that output is >= twice the play amount.)
       Step 3: Alice reveals (s, a) to open her commitment and wins if a == b. Otherwise Bob wins.
               If Alice fails to open her commitment before some deadline then Bob automatically wins.

    For simplicity, we will use following bytes to designate bits
        0x00 = false
        0x01 = true

    */
  property("Evaluation - XorGame Example") {

    // Alice is first player, who initiates the game
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey = alice.dlogSecrets.head.publicImage

    val a:Byte = if (scala.util.Random.nextBoolean) 0x01 else 0x00 // Alice's random choice
    val s:Array[Byte] = Random.randomBytes(31) // random padding
    val h = Blake2b256(s :+ a) // Alice's commitment

    val fullGameEnv = Map(
      "alice" -> alicePubKey,
      "h" -> Colls.fromArray(h)
    )

    val fullGameScript = mkTestErgoTree(compile(fullGameEnv,
      """{
        |  val s           = getVar[Coll[Byte]](0).get  // Alice's secret byte string s
        |  val a           = getVar[Byte](1).get        // Alice's secret bit a (represented as a byte)
        |  val b           = SELF.R4[Byte].get          // Bob's public bit b (represented as a byte)
        |  val bob         = SELF.R5[SigmaProp].get     // Bob's public key
        |  val bobDeadline = SELF.R6[Int].get           // after this height, Bob gets to spend unconditionally
        |
        |  (bob && HEIGHT > bobDeadline) || {
        |    blake2b256(s ++ Coll(a)) == h && {         // h is Alice's original commitment from the halfGameScript
        |      alice && a == b || bob && a != b
        |    }
        |  }
        |}""".stripMargin
    ).asSigmaProp)

    val halfGameEnv = Map(
      "alice" -> alicePubKey,
      "fullGameScriptHash" -> Colls.fromArray(Blake2b256(fullGameScript.bytes))
    )

    // Note that below script allows Alice to spend the half-game output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-game output
    // before some minimum height.
    val halfGameScript = mkTestErgoTree(compile(halfGameEnv,
      """{
        |  alice || {
        |    val out           = OUTPUTS(0)
        |    val b             = out.R4[Byte].get
        |    val bobDeadline   = out.R6[Int].get
        |    val validBobInput = b == 0 || b == 1
        |    // Bob needs to ensure that out.R5 contains his public key
        |    OUTPUTS.size == 1 &&
        |    bobDeadline >= HEIGHT+30 &&
        |    out.value >= SELF.value * 2 &&
        |    validBobInput &&
        |    blake2b256(out.propositionBytes) == fullGameScriptHash
        |  }
        |}
      """.stripMargin).asSigmaProp)

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
    // Possibility 1: Alice aborts her Half-Game (before the Full-Game is created)
    /////////////////////////////////////////////////////////

    // Alice pays to Carol. Game ends here
    val carol = new ContextEnrichingTestProvingInterpreter
    val carolPubKey: ProveDlog = carol.dlogSecrets.head.publicImage
    val carolPubKeyTree = mkTestErgoTree(carolPubKey)

    val abortHalfGameHeight = halfGameCreationHeight + 10 // can be anything

    // NOTES:
    // We need to supply the additional parameters for R4, R5 and R6, even though they are not needed in the proof of this
    // branch (i.e., when Alice is spending this output)
    // Ideally, we should just have given:
    //
    //   val abortHalfGameOutput = ErgoBox(playAmount, carolPubKey, abortHalfGameHeight) // gives error

    val abortHalfGameOutput = testBox(playAmount, carolPubKeyTree, abortHalfGameHeight, Nil,
      Map(
        R4 -> ByteConstant(0), // dummy data. Has to be given, even though not needed as per halfGameScript
        R5 -> SigmaPropConstant((new ContextEnrichingTestProvingInterpreter).dlogSecrets.head.publicImage), // dummy statement
        R6 -> IntConstant(0) // dummy data. Has to be given, even though not needed as per halfGameScript
      )
    )

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val abortHalfGameTx = createTransaction(abortHalfGameOutput)

    val abortHalfGameContext = ErgoLikeContextTesting(
      currentHeight = abortHalfGameHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(halfGameOutput),
      spendingTransaction = abortHalfGameTx,
      self = halfGameOutput, // what is the use of self?
      activatedVersionInTests
    )

    val proofAbortHalfGame = alice.prove(halfGameEnv, halfGameScript, abortHalfGameContext, fakeMessage).get.proof

    verifier.verify(halfGameEnv, halfGameScript, abortHalfGameContext, proofAbortHalfGame, fakeMessage).get._1 shouldBe true

    /////////////////////////////////////////////////////////
    //// Possibility 2: Bob will spend halfGameOutput and generate a full game
    /////////////////////////////////////////////////////////

    //a variable indicating height at which the tx spending halfGameTx is created
    val fullGameCreationHeight = halfGameCreationHeight + 10

    val bob = new ContextEnrichingTestProvingInterpreter
    val bobPubKey:ProveDlog = bob.dlogSecrets.head.publicImage
    val bobDeadline = 120 // height after which it become's Bob's money
    val b:Byte = if (scala.util.Random.nextBoolean) 0x01 else 0x00

    val fullGameOutput = testBox(playAmount*2, fullGameScript, fullGameCreationHeight, Nil,
      Map(
        R4 -> ByteConstant(b),
        R5 -> SigmaPropConstant(bobPubKey),
        R6 -> IntConstant(bobDeadline)
      )
    )

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val fullGameTx = createTransaction(fullGameOutput)

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


    val winner = {
      if (a != b) {
        /////////////////////////////////////////////////////////
        // Possibility 2.1: Bob wins and Alice reveals secret
        // Bob can spend anytime. But we show how he will spend before bobDeadline
        /////////////////////////////////////////////////////////
        printDebug("Bob won")
        bob
      } else {
        /////////////////////////////////////////////////////////
        // Possibility 2.2: Alice wins (and she may or may not reveal secret).
        // Alice must spend before bobDeadline height
        /////////////////////////////////////////////////////////
        printDebug("Alice won")
        alice
      }
    }.withContextExtender(
      0, ByteArrayConstant(s)
    ).withContextExtender(
      1, ByteConstant(a)
    )

    val gameOverHeight = fullGameCreationHeight + 10

    // assume winner is paying to Carol
    // note that playAmount*2 below is not checked. It could be anything.
    val gameOverOutput = testBox(playAmount*2, carolPubKeyTree, gameOverHeight)

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val gameOverTx = createTransaction(gameOverOutput)

    val gameOverContext = ErgoLikeContextTesting(
      currentHeight = gameOverHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput),
      spendingTransaction = gameOverTx,
      self = fullGameOutput, // what is the use of self?
      activatedVersionInTests
    )

    val proofGameOver = winner.prove(fullGameEnv, fullGameScript, gameOverContext, fakeMessage).get

    verifier.verify(fullGameEnv, fullGameScript, gameOverContext, proofGameOver, fakeMessage).get._1 shouldBe true

    /////////////////////////////////////////////////////////
    // Possibility 2.3: (Bob wins and Alice does not reveal secret) OR (Alice wins and does not spend before bobDeadline)
    // Bob can spend after bobDeadline.
    /////////////////////////////////////////////////////////

    val defaultWinHeight = bobDeadline + 1

    // assume Bob is paying to Carol
    // note that playAmount*2 below is not checked. It could be anything.
    val defaultWinOutput = testBox(playAmount*2, carolPubKeyTree, defaultWinHeight)

    //normally this transaction would invalid (why?), but we're not checking it in this test
    val defaultWinTx = createTransaction(defaultWinOutput)

    val defaultWinContext = ErgoLikeContextTesting(
      currentHeight = defaultWinHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fullGameOutput),
      spendingTransaction = defaultWinTx,
      self = fullGameOutput, // what is the use of self?
      activatedVersionInTests
    )

    val sDummy = Array[Byte]()  // empty value for s; commitment cannot be opened but still Bob will be able to spend
    val aDummy:Byte = 0
    // below we need to specify a and s (even though they are not needed)
    val proofDefaultWin = bob.withContextExtender(
      0, ByteArrayConstant(sDummy)
    ).withContextExtender(
      1, ByteConstant(aDummy)
    ).prove(fullGameEnv, fullGameScript, defaultWinContext, fakeMessage).get

    verifier.verify(fullGameEnv, fullGameScript, defaultWinContext, proofDefaultWin, fakeMessage).get._1 shouldBe true

  }

}
