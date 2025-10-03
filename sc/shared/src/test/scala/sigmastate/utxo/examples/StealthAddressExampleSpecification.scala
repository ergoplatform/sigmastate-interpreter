package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.{R4, R5, R6, R7}
import org.ergoplatform._
import sigma.data.{AvlTreeData, ProveDHTuple}
import sigmastate._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigma.ast.GroupElementConstant
import sigma.crypto.CryptoConstants
import sigmastate.crypto.DiffieHellmanTupleProverInput


class StealthAddressExampleSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  // ScalaHub's Stealth Address Contract from ErgoForum
  // https://www.ergoforum.org/t/stealth-address-contract/255/2
  // Exact implementation using proveDHTuple
  private val stealthAddressScript = """
    |{
    |  val gr = SELF.R4[GroupElement].get
    |  val gy = SELF.R5[GroupElement].get
    |  val ur = SELF.R6[GroupElement].get
    |  val uy = SELF.R7[GroupElement].get
    |  proveDHTuple(gr, gy, ur, uy)
    |}
    |""".stripMargin

  private lazy val stealthAddressProp = compile(Map.empty, stealthAddressScript).toSigmaProp
  private lazy val stealthAddressTree = mkTestErgoTree(stealthAddressProp)

  property("basic stealth address contract") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    import CryptoConstants.dlogGroup
    
    // Alice's private key (x) and public key (u = g^x)
    val alicePrivateKey = new java.math.BigInteger("555555555")
    val alicePublicKey = dlogGroup.exponentiate(dlogGroup.generator, alicePrivateKey)
    
    // Bob's random secrets (r and y)
    val r = new java.math.BigInteger("123456789")
    val y = new java.math.BigInteger("987654321")
    
    // Compute the required values for proveDHTuple
    val gr = dlogGroup.exponentiate(dlogGroup.generator, r)  // g^r
    val gy = dlogGroup.exponentiate(dlogGroup.generator, y)  // g^y
    val ur = dlogGroup.exponentiate(alicePublicKey, r)       // u^r = (g^x)^r = g^(x*r)
    val uy = dlogGroup.exponentiate(alicePublicKey, y)       // u^y = (g^x)^y = g^(x*y)
    
    val inputBox = testBox(
      1000000L, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(gr),
        R5 -> GroupElementConstant(gy),
        R6 -> GroupElementConstant(ur),
        R7 -> GroupElementConstant(uy)
      )
    )
    val outputBox = testBox(1000000L, stealthAddressTree, 100000)
    
    val tx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(inputBox.id)),
      IndexedSeq(outputBox)
    )

    val ctx = ErgoLikeContextTesting(
      currentHeight = 100000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      tx,
      self = inputBox,
      activatedVersionInTests
    )

    val dhtSecret = DiffieHellmanTupleProverInput(alicePrivateKey, ProveDHTuple(gr, gy, ur, uy))
    val proverWithSecret = prover.withDHSecrets(Seq(dhtSecret))
    
    val pr = proverWithSecret.prove(stealthAddressTree, ctx, fakeMessage).get
    verifier.verify(stealthAddressTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }
}