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

  // Helper method to create a valid stealth address box
  private def createValidStealthBox(
    alicePrivateKey: java.math.BigInteger,
    r: java.math.BigInteger,
    y: java.math.BigInteger,
    value: Long = 1000000L
  ) = {
    import CryptoConstants.dlogGroup
    
    val alicePublicKey = dlogGroup.exponentiate(dlogGroup.generator, alicePrivateKey)
    
    val gr = dlogGroup.exponentiate(dlogGroup.generator, r)  // g^r
    val gy = dlogGroup.exponentiate(dlogGroup.generator, y)  // g^y
    val ur = dlogGroup.exponentiate(alicePublicKey, r)       // u^r = (g^x)^r = g^(x*r)
    val uy = dlogGroup.exponentiate(alicePublicKey, y)       // u^y = (g^x)^y = g^(x*y)
    
    testBox(
      value, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(gr),
        R5 -> GroupElementConstant(gy),
        R6 -> GroupElementConstant(ur),
        R7 -> GroupElementConstant(uy)
      )
    )
  }

  // Helper method to create a prover with the correct DH secrets
  private def createProverWithSecrets(
    prover: ContextEnrichingTestProvingInterpreter,
    alicePrivateKey: java.math.BigInteger,
    gr: sigma.crypto.EcPointType, gy: sigma.crypto.EcPointType, ur: sigma.crypto.EcPointType, uy: sigma.crypto.EcPointType
  ) = {
    val dhtSecret = DiffieHellmanTupleProverInput(alicePrivateKey, ProveDHTuple(gr, gy, ur, uy))
    prover.withDHSecrets(Seq(dhtSecret))
  }

  property("basic stealth address contract") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Alice's private key (x)
    val alicePrivateKey = new java.math.BigInteger("555555555")
    
    // Bob's random secrets (r and y)
    val r = new java.math.BigInteger("123456789")
    val y = new java.math.BigInteger("987654321")
    
    val inputBox = createValidStealthBox(alicePrivateKey, r, y)
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

    import CryptoConstants.dlogGroup
    val alicePublicKey = dlogGroup.exponentiate(dlogGroup.generator, alicePrivateKey)
    val gr = dlogGroup.exponentiate(dlogGroup.generator, r)
    val gy = dlogGroup.exponentiate(dlogGroup.generator, y)
    val ur = dlogGroup.exponentiate(alicePublicKey, r)
    val uy = dlogGroup.exponentiate(alicePublicKey, y)
    
    val proverWithSecret = createProverWithSecrets(prover, alicePrivateKey, gr, gy, ur, uy)
    
    val pr = proverWithSecret.prove(stealthAddressTree, ctx, fakeMessage).get
    verifier.verify(stealthAddressTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("stealth address fails with wrong private key") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    // Alice's private key (x)
    val alicePrivateKey = new java.math.BigInteger("555555555")
    
    // Bob's random secrets (r and y)
    val r = new java.math.BigInteger("123456789")
    val y = new java.math.BigInteger("987654321")
    
    val inputBox = createValidStealthBox(alicePrivateKey, r, y)
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

    // Try to spend with wrong private key (Eve instead of Alice)
    val evePrivateKey = new java.math.BigInteger("999999999")
    
    import CryptoConstants.dlogGroup
    val alicePublicKey = dlogGroup.exponentiate(dlogGroup.generator, alicePrivateKey)
    val gr = dlogGroup.exponentiate(dlogGroup.generator, r)
    val gy = dlogGroup.exponentiate(dlogGroup.generator, y)
    val ur = dlogGroup.exponentiate(alicePublicKey, r)
    val uy = dlogGroup.exponentiate(alicePublicKey, y)
    
    val proverWithWrongSecret = createProverWithSecrets(prover, evePrivateKey, gr, gy, ur, uy)
    
    // The proving might succeed (generating an invalid proof), but verification should fail
    val proofResult = proverWithWrongSecret.prove(stealthAddressTree, ctx, fakeMessage)
    
    // If proof generation succeeds, verification should fail
    if (proofResult.isSuccess) {
      verifier.verify(stealthAddressTree, ctx, proofResult.get, fakeMessage).get._1 shouldBe false
    } else {
      // Proof generation failed (which is also acceptable)
      proofResult.isSuccess shouldBe false
    }
  }

  property("stealth address fails with invalid Diffie-Hellman tuple") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    import CryptoConstants.dlogGroup
    
    // Alice's private key (x)
    val alicePrivateKey = new java.math.BigInteger("555555555")
    
    // Create values that don't form a valid Diffie-Hellman tuple
    // We'll use completely random unrelated values
    val gr = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("111111111"))
    val gy = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("222222222"))
    val ur = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("333333333"))  // Not related to gr
    val uy = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("444444444"))  // Not related to gy
    
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

    // Try to prove with Alice's key, but since the tuple is invalid, verification should fail
    val proverWithSecret = createProverWithSecrets(prover, alicePrivateKey, gr, gy, ur, uy)
    
    val proofResult = proverWithSecret.prove(stealthAddressTree, ctx, fakeMessage)
    
    // If proof generation succeeds, verification should fail
    if (proofResult.isSuccess) {
      verifier.verify(stealthAddressTree, ctx, proofResult.get, fakeMessage).get._1 shouldBe false
    } else {
      // Proof generation failed (which is also acceptable)
      proofResult.isSuccess shouldBe false
    }
  }

  property("stealth address fails with missing registers") {
    val prover = new ContextEnrichingTestProvingInterpreter

    // Create input box WITHOUT the required registers
    val inputBox = testBox(1000000L, stealthAddressTree, 100000) // No additional registers
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

    // Should fail because required registers (R4-R7) are missing
    prover.prove(stealthAddressTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("stealth address fails with partial registers") {
    val prover = new ContextEnrichingTestProvingInterpreter

    import CryptoConstants.dlogGroup
    
    // Create input box with only some registers
    val gr = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("111111111"))
    val gy = dlogGroup.exponentiate(dlogGroup.generator, new java.math.BigInteger("222222222"))
    
    val inputBox = testBox(
      1000000L, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(gr),
        R5 -> GroupElementConstant(gy)
        // Missing R6 and R7
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

    // Should fail because registers R6 and R7 are missing
    prover.prove(stealthAddressTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("stealth address fails with wrong register types") {
    val prover = new ContextEnrichingTestProvingInterpreter

    // Create input box with wrong types in registers
    val inputBox = testBox(
      1000000L, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> sigma.ast.IntConstant(123),  // Wrong type - should be GroupElement
        R5 -> sigma.ast.IntConstant(456),  // Wrong type - should be GroupElement
        R6 -> sigma.ast.IntConstant(789),  // Wrong type - should be GroupElement
        R7 -> sigma.ast.IntConstant(999)   // Wrong type - should be GroupElement
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

    // Should fail due to type mismatch when accessing registers
    prover.prove(stealthAddressTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("stealth address fails with swapped register values") {
    val prover = new ContextEnrichingTestProvingInterpreter

    import CryptoConstants.dlogGroup
    
    // Alice's private key (x)
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
    
    // Create input box with swapped register values
    val inputBox = testBox(
      1000000L, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(gy),  // Swapped: should be gr
        R5 -> GroupElementConstant(gr),  // Swapped: should be gy
        R6 -> GroupElementConstant(uy),  // Swapped: should be ur
        R7 -> GroupElementConstant(ur)   // Swapped: should be uy
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

    val proverWithSecret = createProverWithSecrets(prover, alicePrivateKey, gr, gy, ur, uy)
    
    // Should fail because register values are swapped
    proverWithSecret.prove(stealthAddressTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("stealth address fails with tampered ephemeral keys") {
    val prover = new ContextEnrichingTestProvingInterpreter

    import CryptoConstants.dlogGroup
    
    // Alice's private key (x)
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
    
    // Create input box with tampered ephemeral keys (different r and y)
    val tamperedR = new java.math.BigInteger("999999999")
    val tamperedY = new java.math.BigInteger("888888888")
    val tamperedGr = dlogGroup.exponentiate(dlogGroup.generator, tamperedR)
    val tamperedGy = dlogGroup.exponentiate(dlogGroup.generator, tamperedY)
    
    val inputBox = testBox(
      1000000L, 
      stealthAddressTree, 
      100000,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(tamperedGr),  // Tampered: different gr
        R5 -> GroupElementConstant(tamperedGy),  // Tampered: different gy
        R6 -> GroupElementConstant(ur),          // Original ur (doesn't match tampered gr)
        R7 -> GroupElementConstant(uy)           // Original uy (doesn't match tampered gy)
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

    val proverWithSecret = createProverWithSecrets(prover, alicePrivateKey, gr, gy, ur, uy)
    
    // Should fail because ephemeral keys are tampered
    proverWithSecret.prove(stealthAddressTree, ctx, fakeMessage).isSuccess shouldBe false
  }
}