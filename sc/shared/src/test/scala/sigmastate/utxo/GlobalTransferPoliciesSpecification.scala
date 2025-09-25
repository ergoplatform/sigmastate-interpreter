package sigmastate.utxo

import org.ergoplatform._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.ast._
import sigma.data.{AvlTreeData, AvlTreeFlags, Digest32Coll}
import sigma.Colls
import sigma.eval.SigmaDsl
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.CompilerCrossVersionProps
import sigma.Extensions.ArrayOps

class GlobalTransferPoliciesSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  override val printVersions: Boolean = false
  implicit lazy val IR: TestingIRContext = new TestingIRContext {
  }

  val env = Map.empty[String, Any]

  property("script hash verification from context var and data input R4 with singleton token check") {
    // Test that the script compiles correctly
    val compiledScript = compile(env, 
      """{
        |  // Get script from context variable #1
        |  val scriptBytes = getVar[Coll[Byte]](1).get
        |  
        |  // Compute script hash
        |  val computedHash = blake2b256(scriptBytes)
        |  
        |  // Get expected hash from R4 of first data input
        |  val expectedHash = CONTEXT.dataInputs(0).R4[Coll[Byte]].get
        |  
        |  // Check that data input has exactly one token (singleton)
        |  val hasSingletonToken = CONTEXT.dataInputs(0).tokens.size == 1
        |  
        |  // Check that token #0 has specific ID (example: token ID from context variable #2)
        |  val expectedTokenId = getVar[Coll[Byte]](2).get
        |  val correctTokenId = CONTEXT.dataInputs(0).tokens(0)._1 == expectedTokenId
        |  
        |  // Verify all conditions
        |  sigmaProp(computedHash == expectedHash && hasSingletonToken && correctTokenId)
        |}""".stripMargin)
    
    // If we get here without exceptions, the script compiles correctly
    compiledScript should not be null
  }

  property("script hash verification with AVL+ tree whitelist for output #0 with token transfer") {
    // Test that the script compiles correctly
    val compiledScript = compile(env, 
      """{
        |  // Get script from context variable #1
        |  val scriptBytes = getVar[Coll[Byte]](1).get
        |  
        |  // Compute script hash
        |  val computedHash = blake2b256(scriptBytes)
        |  
        |  // Get expected hash from R4 of first data input
        |  val expectedHash = CONTEXT.dataInputs(0).R4[Coll[Byte]].get
        |  
        |  // Check that data input has exactly one token (singleton)
        |  val hasSingletonToken = CONTEXT.dataInputs(0).tokens.size == 1
        |  
        |  // Check that token #0 has specific ID (example: token ID from context variable #2)
        |  val expectedTokenId = getVar[Coll[Byte]](2).get
        |  val correctTokenId = CONTEXT.dataInputs(0).tokens(0)._1 == expectedTokenId
        |  
        |  // Get output #0 script hash
        |  val output0ScriptHash = blake2b256(OUTPUTS(0).propositionBytes)
        |  
        |  // Get AVL tree whitelist from R5 of data input
        |  val whitelistTree = CONTEXT.dataInputs(0).R5[AvlTree].get
        |  
        |  // Get proof for output #0 script hash from context variable #3
        |  val proof = getVar[Coll[Byte]](3).get
        |  
        |  // Check if output #0 script hash is in whitelist
        |  val isWhitelisted = whitelistTree.get(output0ScriptHash, proof).isDefined
        |  
        |  // Verify token transfer: ensure output #0 first token id matches self's first token id and amounts are the same
        |  val selfFirstToken = SELF.tokens(0)
        |  val output0FirstToken = OUTPUTS(0).tokens(0)
        |  val tokensTransferredCorrectly = selfFirstToken._1 == output0FirstToken._1 && selfFirstToken._2 == output0FirstToken._2
        |  
        |  // Verify all conditions
        |  sigmaProp(computedHash == expectedHash && 
        |            hasSingletonToken && 
        |            correctTokenId &&
        |            isWhitelisted &&
        |            tokensTransferredCorrectly)
        |}""".stripMargin)
    
    // If we get here without exceptions, the script compiles correctly
    compiledScript should not be null
    
    // Test that the script compiles to a SigmaProp
    compiledScript.tpe.toString should include ("SigmaProp")
    
    // Create different ErgoTrees to get different script hashes
    val prover = new ContextEnrichingTestProvingInterpreter()
    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    
    // Create different ErgoTrees to get different script hashes
    val whitelistedTree = ErgoTree.fromSigmaBoolean(pubkey1)
    val nonWhitelistedTree = ErgoTree.fromSigmaBoolean(pubkey2)
    
    // Create the script to be verified
    val scriptToVerify = "some_script_code".getBytes
    val scriptHash = Blake2b256(scriptToVerify)
    
    // Create a token ID for the data input
    val tokenId = Digest32Coll @@ Colls.fromArray(Blake2b256("singleton_token"))
    
    // Create AVL tree whitelist with script hashes
    val whitelistedScriptHash = Blake2b256(whitelistedTree.bytes)
    val nonWhitelistedScriptHash = Blake2b256(nonWhitelistedTree.bytes)
    
    // Create AVL prover and add whitelisted script hash
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    avlProver.performOneOperation(Insert(ADKey @@@ whitelistedScriptHash, ADValue @@ "allowed".getBytes))
    avlProver.generateProof() // Generate initial proof
    
    val treeData = SigmaDsl.avlTree(new AvlTreeData(avlProver.digest.toArray.toColl, AvlTreeFlags.ReadOnly, 32, None))
    
    // Create data input with script hash and AVL tree whitelist
    val dataInput = testBox(0, TrueTree, 0, Seq((tokenId, 1L)), 
      Map(
        ErgoBox.R4 -> ByteArrayConstant(scriptHash),
        ErgoBox.R5 -> AvlTreeConstant(treeData)
      )
    )
    
    // Create self box with tokens to be transferred
    val selfTokenId = Digest32Coll @@ Colls.fromArray(Blake2b256("transfer_token"))
    val selfTokens = Seq((selfTokenId, 100L)) // 100 tokens to transfer
    
    // Output boxes with tokens transferred from self box
    val whitelistedOutput = testBox(10, whitelistedTree, 0, selfTokens)  // Whitelisted script with tokens
    val nonWhitelistedOutput = testBox(10, nonWhitelistedTree, 0, selfTokens)  // Non-whitelisted script with tokens
    
    // Test case 1: Output #0 is whitelisted (should succeed)
    val spendingTx1 = createTransaction(IndexedSeq(dataInput), IndexedSeq(whitelistedOutput, nonWhitelistedOutput))
    val selfBox = testBox(20, ErgoTree.fromProposition(ergoTreeHeaderInTests, compiledScript.asInstanceOf[Value[SSigmaProp.type]]), 0, selfTokens)
    
    // Generate proof for whitelisted script hash
    avlProver.performOneOperation(Lookup(ADKey @@@ whitelistedScriptHash))
    val whitelistProof = avlProver.generateProof()
    
    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(dataInput),
      boxesToSpend = IndexedSeq(selfBox),
      spendingTx1,
      selfIndex = 0,
      activatedVersionInTests
    )
    
    val testProver = prover
      .withContextExtender(1, ByteArrayConstant(scriptToVerify))
      .withContextExtender(2, ByteArrayConstant(tokenId.toArray))
      .withContextExtender(3, ByteArrayConstant(whitelistProof))
    
    val pr1 = testProver.prove(selfBox.ergoTree, ctx1, fakeMessage)
    pr1.isSuccess shouldBe true  // Should succeed because output #0 is whitelisted
    
    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(selfBox.ergoTree, ctx1, pr1.get, fakeMessage).get._1 shouldBe true
    
    // Test case 2: Output #0 is NOT whitelisted (should fail)
    val spendingTx2 = createTransaction(IndexedSeq(dataInput), IndexedSeq(nonWhitelistedOutput, whitelistedOutput))
    
    // Generate proof for non-whitelisted script hash (should fail lookup)
    avlProver.performOneOperation(Lookup(ADKey @@@ nonWhitelistedScriptHash))
    val nonWhitelistProof = avlProver.generateProof()
    
    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(dataInput),
      boxesToSpend = IndexedSeq(selfBox),
      spendingTx2,
      selfIndex = 0,
      activatedVersionInTests
    )
    
    val testProver2 = prover
      .withContextExtender(1, ByteArrayConstant(scriptToVerify))
      .withContextExtender(2, ByteArrayConstant(tokenId.toArray))
      .withContextExtender(3, ByteArrayConstant(nonWhitelistProof))
    
    val pr2 = testProver2.prove(selfBox.ergoTree, ctx2, fakeMessage)
    pr2.isSuccess shouldBe false  // Should fail because output #0 is not whitelisted
  }

}