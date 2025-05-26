package sigmastate.utxo.examples

import java.math.BigInteger
import org.ergoplatform.ErgoBox.{R4, R5}
import scorex.crypto.hash.Blake2b256
import sigma.data.{AvlTreeData, ProveDHTuple, ProveDlog}
import sigma.util.Extensions.EcpOps
import sigmastate.CompilerCrossVersionProps
import sigma.ast.{ErgoTree, GroupElementConstant}
import sigma.ast.syntax.GroupElementConstant
import sigma.crypto.CryptoConstants
import sigmastate.crypto.DiffieHellmanTupleProverInput
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter._
import sigma.ast.syntax._
import sigma.eval.SigmaDsl

class MixExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  property("Evaluation - Mix Example") {
    import CryptoConstants.dlogGroup

    val g = dlogGroup.generator

    // Alice is first player, who initiates the mix
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey: ProveDlog = alice.dlogSecrets.head.publicImage

    val x: BigInteger = alice.dlogSecrets.head.w // x is Alice's private key

    val gX = alicePubKey.value // g_x is Alice's public key (g_x = g^x)
    // Alternative 1:
    //      val g_x = alicePubKey.value
    // Alternative 2:
    //    Generate x ourselves (e.g., val x = BigInt(randomBytes).bigInteger)
    //    To generate g_x from a BigInteger x, and to generate alicePubKey as ProveDlog type from g_x, use the following:
    //      val g_x:EcPointType = dlogGroup.exponentiate(g, x)
    //      val alicePubKey:ProveDlog = ProveDlog(g_x)

    val fullMixEnv = Map(
      ScriptNameProp -> "fullMixEnv",
      "g" -> g.toGroupElement,
      "gX" -> gX.toGroupElement
    )

    ProveDlog(gX) shouldBe alicePubKey

    // y is Bob's secret key and h = g^y is kind of like his "public key"
    // The Diffie-Hellman solution is g_xy = g_y^x = g_x^y = g^xy.
    val fullMixScript = mkTestErgoTree(compile(fullMixEnv,
      """{
        |  val c1 = SELF.R4[GroupElement].get
        |  val c2 = SELF.R5[GroupElement].get
        |  proveDlog(c2) ||            // either c2 is g^y
        |  proveDHTuple(g, c1, gX, c2) // or c2 is u^y = g^xy
        |}""".stripMargin
    ).asSigmaProp)

    val halfMixEnv = Map(
      ScriptNameProp -> "halfMixEnv",
      "g" -> g.toGroupElement,
      "gX" -> gX.toGroupElement,
      "fullMixScriptHash" -> Blake2b256(fullMixScript.bytes)
    )

    // Note that below script allows Alice to spend the half-mix output anytime before Bob spends it.
    // We could also consider a more restricted version of the game where Alice is unable to spend the half-mix output
    // before some minimum height.

    // The proveDHTuple instruction takes parameters (g, h, u, v) where g, h are generators (discrete log bases)
    // with u = g^x and v = h^x. Note that y = log_g(h), where y is Bob's secret.

    val halfMixScript = mkTestErgoTree(compile(halfMixEnv,
      """{
        |  val c1 = OUTPUTS(0).R4[GroupElement].get
        |  val c2 = OUTPUTS(0).R5[GroupElement].get
        |
        |  OUTPUTS.size == 2 &&
        |  OUTPUTS(0).value == SELF.value &&
        |  OUTPUTS(1).value == SELF.value &&
        |  blake2b256(OUTPUTS(0).propositionBytes) == fullMixScriptHash &&
        |  blake2b256(OUTPUTS(1).propositionBytes) == fullMixScriptHash &&
        |  OUTPUTS(1).R4[GroupElement].get == c2 &&
        |  OUTPUTS(1).R5[GroupElement].get == c1 && {
        |    proveDHTuple(g, gX, c1, c2) ||
        |    proveDHTuple(g, gX, c2, c1)
        |  }
        |}""".stripMargin
    ).asSigmaProp)


    /////////////////////////////////////////////////////////
    //// Alice starts creating a Half-Mix box
    /////////////////////////////////////////////////////////

    // she creates a transaction that outputs a box with halfGame script.
    // In the example, we don't create the transaction; we just create a box below

    val halfMixCreationHeight = 70
    val mixAmount = 10

    val halfMixOutput = testBox(mixAmount, halfMixScript, halfMixCreationHeight)
    // above halMixOutput is a Half-Mix box created by Alice.

    // a blockchain node verifying a block containing a spending transaction
    val verifier = new ErgoLikeTestInterpreter

    /////////////////////////////////////////////////////////
    //// Bob picks Alice's Half-Mix box and creates a Full-Mix box
    ///////////////////////////////////////////////////////////

    // If Alice wants to abort the mix, she can take Bob's role and spend her Half-Mix output

    val bob = new ContextEnrichingTestProvingInterpreter
    val bobPubKey: ProveDlog = bob.dlogSecrets.head.publicImage

    val y: BigInteger = bob.dlogSecrets.head.w // y is Bob's private key

    val gY = GroupElementConstant(bobPubKey.value) // g^y
    val gY_alt = GroupElementConstant(dlogGroup.exponentiate(g, y))

    gY shouldBe gY_alt

    // To Do: Extract below g_x from halfMixOutput
    val gXY = GroupElementConstant(dlogGroup.exponentiate(gX, y))
    val gXY_alt = GroupElementConstant(dlogGroup.exponentiate(gY, x))

    gXY shouldBe gXY_alt

    val randomBit = scala.util.Random.nextBoolean
    // randomBit is interpreted as follows
    //     0 is false
    //     1 is true

    val (c0, c1) = if (randomBit) (gXY, gY) else (gY, gXY)

    val fullMixCreationHeight = 80

    // if randomBit is 0 (i.e., false) below box is spendable by Alice, else by Bob
    val fullMixOutput0 = testBox(mixAmount, fullMixScript, fullMixCreationHeight, Nil,
      Map(
        R4 -> c0,
        R5 -> c1
      )
    )

    // if randomBit is 1 (i.e., true) below box is spendable by Alice, else by Bob
    val fullMixOutput1 = testBox(mixAmount, fullMixScript, fullMixCreationHeight, Nil,
      Map(
        R4 -> c1,
        R5 -> c0
      )
    )

    // normally this transaction would be invalid, but we're not checking it in this test
    val fullMixTx = createTransaction(IndexedSeq(fullMixOutput0, fullMixOutput1))

    val fullMixContext = ErgoLikeContextTesting(
      currentHeight = fullMixCreationHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(halfMixOutput),
      spendingTransaction = fullMixTx,
      self = halfMixOutput, activatedVersionInTests
    )

    // bob (2nd player) is generating a proof and it is passing verification
    // To Do: Extract below g_x from halfMixOutput
    val dhtBob = DiffieHellmanTupleProverInput(y, ProveDHTuple(g, gX, gY, gXY))

    val proofFullMix = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtBob)
    ).prove(halfMixEnv, halfMixScript, fullMixContext, fakeMessage).get.proof

    verifier.verify(halfMixEnv, halfMixScript, fullMixContext, proofFullMix, fakeMessage).get._1 shouldBe true

    //////////////////////////////////////////////
    //// Setup for spending the above created outputs (fullMixOutput0, fullMixOutput1)
    //////////////////////////////////////////////

    // some 3rd person that will be paid
    val carol = new ContextEnrichingTestProvingInterpreter
    val carolPubKey: ProveDlog = carol.dlogSecrets.head.publicImage

    val spendHeight = 90
    val carolOutput = testBox(mixAmount, ErgoTree.fromSigmaBoolean(carolPubKey), spendHeight)

    // normally this transaction would be invalid, but we're not checking it in this test
    val spendingTx = createTransaction(carolOutput)

    //////////////////////////////////////////////
    //// Alice spending her output
    //////////////////////////////////////////////

    val fullMixOutput0_R4 = fullMixOutput0.additionalRegisters(R4).v
    val fullMixOutput0_R5 = fullMixOutput0.additionalRegisters(R5).v

    fullMixOutput0_R4 shouldBe c0
    fullMixOutput0_R5 shouldBe c1

    val r4X = SigmaDsl.GroupElement(dlogGroup.exponentiate(fullMixOutput0_R4.asInstanceOf[GroupElementConstant], x)) // R4^x

    // if R4^x == R5 then this fullMixOutput0 is Alice's output else its Bob's output.
    val (aliceAnonBox, bobAnonBox) = if (r4X == fullMixOutput0_R5.asInstanceOf[GroupElementConstant].value) {
      printDebug("First output is Alice's")
      (fullMixOutput0, fullMixOutput1)
    } else {
      printDebug("First output is Bob's")
      SigmaDsl.GroupElement(dlogGroup.exponentiate(fullMixOutput0_R5.asInstanceOf[GroupElementConstant], x)) shouldBe fullMixOutput0_R4.asInstanceOf[GroupElementConstant].value
      (fullMixOutput1, fullMixOutput0)
    }

    val aliceSpendContext = ErgoLikeContextTesting(
      currentHeight = spendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(aliceAnonBox),
      spendingTransaction = spendingTx,
      self = aliceAnonBox, activatedVersionInTests
    )

    // To Do: Extract below g_y, g_xy from fullMixOutputs registers
    val dhtAlice = DiffieHellmanTupleProverInput(x, ProveDHTuple(g, gY, gX, gXY))

    val proofAliceSpend = (new ContextEnrichingTestProvingInterpreter).withDHSecrets(
      Seq(dhtAlice)
    ).prove(fullMixEnv, fullMixScript, aliceSpendContext, fakeMessage).get.proof

    verifier.verify(fullMixEnv, fullMixScript, aliceSpendContext, proofAliceSpend, fakeMessage).get._1 shouldBe true

    //////////////////////////////////////////////
    //// Bob spending his output
    //////////////////////////////////////////////

    val bobSpendContext = ErgoLikeContextTesting(
      currentHeight = spendHeight,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(bobAnonBox),
      spendingTransaction = spendingTx,
      self = bobAnonBox, activatedVersionInTests
    )

    val proofBobSpend = bob.prove(fullMixEnv, fullMixScript, bobSpendContext, fakeMessage).get.proof

    verifier.verify(fullMixEnv, fullMixScript, bobSpendContext, proofBobSpend, fakeMessage).get._1 shouldBe true

  }

}