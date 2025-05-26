package sigmastate.utxo.examples

import scorex.utils.Longs
import org.ergoplatform.ErgoBox.RegisterId
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigmastate._
import sigmastate.eval._
import sigma.ast.syntax._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import org.ergoplatform._
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import sigma.Extensions.ArrayOps
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.Context
import sigma.ast.syntax._
import sigma.crypto.{BigIntegers, CryptoConstants, CryptoFacade}
import sigma.data.{AvlTreeData, AvlTreeFlags}
import sigmastate.utils.Helpers._

class OracleExamplesSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps { suite =>
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)
  private val reg2 = ErgoBox.nonMandatoryRegisters(1)
  private val reg3 = ErgoBox.nonMandatoryRegisters(2)
  private val reg4 = ErgoBox.nonMandatoryRegisters(3)

  /**
    * An oracle example.
    *
    * A trusted weather station is publishing temperature data on blockchain.
    * Alice and Bob are making a contract based on the data:
    * they have locked coins in such way that if output from the station shows that
    * temperature announced by the oracle is > 15 degrees, money are going to Alice, otherwise to Bob.
    *
    * We consider that for validating transaction only limited number of last headers and the spending transaction
    * should be enough, in addition to outputs being spent by the transaction. Thus there is no need for knowledge
    * of an arbitrary output. To show the coin of the weather service in the spending transaction, outputs from Alice
    * and Bob are referencing to the coin by using Merkle proofs against UTXO set root hash of the latest known block.
    *
    * A tricky moment is how Alice and Bob can be sure that a coin is indeed created by the service, having just the
    * coin (and also service's public key `x = g^w`, where service's secret w is not known.
    *
    *
    * For that, we consider that the service creates a coin with registers of following semantics (R0 & R1 are standard):
    *
    * R1 - coin amount
    * R2 - protecting script, which is the pubkey of the service, `x =  g^w`
    * R3 - temperature data, number
    * R4 - `a = g^r`, where r is secret random nonce
    * R5 - z = r + ew mod q
    * R6 - timestamp
    *
    * Then Alice and Bob are requiring from the coin that the following equation holds:
    * (`g^z = a * x^e`, where e = hash(R3 ++ R6)
    *
    * Thus Alice, for example, is created a coin with the following statement (we skip timeouts for simplicity):
    * "the coin is spendable by presenting a proof of Alice's private key knowledge if against UTXO set root hash for
    * the last known block there is a coin along with a Merkle proof, for which following requirements hold:
    * R2 = dlog(x) /\ g^(R5) = R4 * x^(hash(R3 ++ R6)) /\ (R3) > 15 . Similarly, the coin is spendable by a proof of
    * knowledge of the Bob's private key if all the same conditions are met but (R3) <= 15.".
    *
    * The Bob can create a box with the same guarding conditions. However, if Alice's box is already in the state, then
    * Bob can stick to it by using the trick from the "along with a brother" test.
    *
    *
    */
  // TODO v6.0: re-implement this example using UBigInt type
  // Note, the value `z` computed in the test doesn't fit into BigInt type.
  // This makes the oracleBox.bytes fail deserialization and thus, such box cannot be
  // accepted by the blockchain (see assertExceptionThrown in the test).
  // This test is `ignored` after fitsIn256Bits check is added to SBigInt serialization.
  ignore("oracle example") {
    val oracle = new ContextEnrichingTestProvingInterpreter
    val aliceTemplate = new ContextEnrichingTestProvingInterpreter
    val bob = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubImage = oraclePrivKey.publicImage
    val oraclePubKey = oraclePubImage

    val alicePubKey = aliceTemplate.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val group = CryptoConstants.dlogGroup

    val temperature: Long = 18

    //create 128 bits random number
    val r = BigInt(BigIntegers.createRandomBigInteger(128, CryptoFacade.createSecureRandom()))
    val a = group.exponentiate(group.generator, r.bigInteger)

    val ts = System.currentTimeMillis()

    // we need to fit the resulted BigInt in group order
    val reducedHashSize = 31
    val e = BigInt(Blake2b256.hash(Longs.toByteArray(temperature) ++ Longs.toByteArray(ts)).take(reducedHashSize))

    val z = (r + e.bigInteger.multiply(oraclePrivKey.w)).mod(group.order).bigInteger

    val oracleBox = testBox(
      value = 1L,
      ergoTree = ErgoTree.fromSigmaBoolean(oraclePubKey),
      creationHeight = 0,
      additionalRegisters = Map(
        reg1 -> LongConstant(temperature),
        reg2 -> GroupElementConstant(a),
        reg3 -> BigIntConstant(z),
        reg4 -> LongConstant(ts)),
      boxIndex = 1
    )

    assertExceptionThrown(
      oracleBox.bytes,
      exceptionLike[IllegalArgumentException]("doesn't fit into 256 bits")
    )

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    avlProver.performOneOperation(Insert(ADKey @@@ oracleBox.id, ADValue @@ oracleBox.bytes))
    avlProver.generateProof()

    val lastBlockUtxoDigest = avlProver.digest
    val treeData = new AvlTreeData(lastBlockUtxoDigest.toColl, AvlTreeFlags.ReadOnly, 32, None)

    def extract[T <: SType](Rn: RegisterId)(implicit tT: T) =
      ExtractRegisterAs[T](GetVarBox(22: Byte).get, Rn)(tT).get

    def withinTimeframe(sinceHeight: Int,
                        timeoutHeight: Int,
                        fallback: SigmaPropValue)(script: SigmaPropValue) =
      SigmaOr(SigmaAnd(GE(Height, IntConstant(sinceHeight)), LT(Height, IntConstant(timeoutHeight)), script),
        SigmaAnd(GE(Height, IntConstant(timeoutHeight)), fallback))

    val contractLogic = SigmaOr(SigmaAnd(GT(extract[SLong.type](reg1), LongConstant(15)), alicePubKey),
      SigmaAnd(LE(extract[SLong.type](reg1), LongConstant(15)), bobPubKey))

    val oracleProp = SigmaAnd(
      OptionIsDefined(IR.builder.mkMethodCall(
        LastBlockUtxoRootHash, SAvlTreeMethods.getMethod,
        IndexedSeq(ExtractId(GetVarBox(22: Byte).get), GetVarByteArray(23: Byte).get)).asOption[SByteArray]),
      EQ(extract[SByteArray](ErgoBox.ScriptRegId), ByteArrayConstant(ErgoTree.fromSigmaBoolean(oraclePubKey).bytes)),
      EQ(Exponentiate(GroupGenerator, extract[SBigInt.type](reg3)),
        MultiplyGroup(extract[SGroupElement.type](reg2),
          Exponentiate(GroupElementConstant(oraclePubImage.value),
            ByteArrayToBigInt(
              Slice(
                CalcBlake2b256(
                  Append(LongToByteArray(extract[SLong.type](reg1)), LongToByteArray(extract[SLong.type](reg4)))),
                IntConstant(0), IntConstant(reducedHashSize)
              )
            )
          )
        )
      ),
      contractLogic)

    avlProver.performOneOperation(Lookup(ADKey @@@ oracleBox.id))
    val proof = avlProver.generateProof()

    val newBox1 = testBox(20, ErgoTree.fromSigmaBoolean(alicePubKey), 0, boxIndex = 2)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = createTransaction(newBoxes)

    val sinceHeight = 40
    val timeout = 60

    val propAlice = mkTestErgoTree(
      withinTimeframe(sinceHeight, timeout, alicePubKey)(oracleProp))

    val sAlice = testBox(10, propAlice, 0, Seq(), Map(), boxIndex = 3)

    //"along with a brother" script
    val propAlong = AND(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ByteArrayConstant(sAlice.id)))
    val propBob = mkTestErgoTree(
      withinTimeframe(sinceHeight, timeout, bobPubKey)(propAlong))
    val sBob = testBox(10, propBob, 0, Seq(), Map(), boxIndex = 4)

   val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = treeData,
      ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(sAlice, sBob),
      spendingTransaction,
      self = sAlice, activatedVersionInTests)

    val alice = aliceTemplate
      .withContextExtender(22: Byte, BoxConstant(oracleBox))
      .withContextExtender(23: Byte, ByteArrayConstant(proof))
    val prA = alice.prove(emptyEnv + (ScriptNameProp -> "alice_prove"),
      propAlice, ctx, fakeMessage).getOrThrow

    val prB = bob.prove(emptyEnv + (ScriptNameProp -> "bob_prove"),
      propBob, ctx, fakeMessage).getOrThrow

    val ctxv = ctx.withExtension(prA.extension)
    verifier.verify(emptyEnv + (ScriptNameProp -> "alice_verify"),
      propAlice, ctxv, prA, fakeMessage).getOrThrow._1 shouldBe true

    verifier.verify(emptyEnv + (ScriptNameProp -> "bob_verify"),
      propBob, ctx, prB, fakeMessage).getOrThrow._1 shouldBe true

    //todo: check timing conditions - write tests for height  < 40 and >= 60
  }

  /**
    * In previous example, Alice and Bob can use the same box with temperature written into multiple times (possibly,
    * in one block). Costs for a prover are high though.
    *
    * In the example below we consider an alternative approach with one-time oracle box. An oracle creates a box with
    * temperature written by request, and its only spendable by a transaction which is using Allce's and Bob's boxes.
    * Protection is similar to "along with a brother" example.
    *
    * As oracle is creating the box with the data on request, it can also participate in a spending transaction.
    * Heavyweight authentication from the previous example is not needed then.
    */
  property("lightweight oracle example") {
    val oracle = new ContextEnrichingTestProvingInterpreter
    val alice = new ContextEnrichingTestProvingInterpreter
    val bob = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val oraclePrivKey = oracle.dlogSecrets.head
    val oraclePubKey = oraclePrivKey.publicImage

    val alicePubKey = alice.dlogSecrets.head.publicImage
    val bobPubKey = bob.dlogSecrets.head.publicImage

    val temperature: Long = 18

    val oracleBox = testBox(
      value = 1L,
      ergoTree = mkTestErgoTree(oraclePubKey),
      creationHeight = 0,
      additionalRegisters = Map(reg1 -> LongConstant(temperature))
    )

    val contractLogic = SigmaOr(
      SigmaAnd(GT(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), reg1).get, LongConstant(15)), alicePubKey),
      SigmaAnd(LE(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), reg1).get, LongConstant(15)), bobPubKey)
    )

    val prop = SigmaOr(
      EQ(SizeOf(Inputs), IntConstant(3)),
      EQ(
        ExtractScriptBytes(ByIndex(Inputs, 0)),
        ByteArrayConstant(mkTestErgoTree(oraclePubKey).bytes)),
      contractLogic
    )

    val sOracle = oracleBox
    val sAlice = testBox(10, ErgoTree.fromProposition(prop), 0, Seq(), Map())
    val sBob = testBox(10, ErgoTree.fromProposition(prop), 0, Seq(), Map())

    val newBox1 = testBox(20, mkTestErgoTree(alicePubKey), 0)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(sOracle, sAlice, sBob),
      spendingTransaction,
      self = sOracle, activatedVersionInTests)

    val prA = alice.prove(emptyEnv + (ScriptNameProp -> "alice_prove"),
      mkTestErgoTree(prop), ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"),
      mkTestErgoTree(prop), ctx, prA, fakeMessage).get._1 shouldBe true
  }

  case class OracleContract[Spec <: ContractSpec]
      ( temperature: Long,
      oracle: Spec#ProvingParty, alice: Spec#ProvingParty, bob: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    def pkOracle = oracle.pubKey
    def pkA = alice.pubKey
    def pkB = bob.pubKey
    def inRegId = reg1.asIndex

    lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "pkOracle" -> pkOracle, "inRegId" -> inRegId)

    lazy val prop = proposition("buyer", { ctx: Context =>
      import ctx._
      val okInputs = INPUTS.length == 3
      val okInput0 = INPUTS(0).propositionBytes == pkOracle.propBytes
      val inReg = INPUTS(0).R4[Long].get
      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
      okInputs && okInput0 && okContractLogic
    },
    """{
     |      val okInputs = INPUTS.size == 3
     |      val okInput0 = INPUTS(0).propositionBytes == pkOracle.propBytes
     |      val inReg = INPUTS(0).R4[Long].get
     |      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
     |      okInputs && okInput0 && okContractLogic
     |}
    """.stripMargin)

    lazy val oracleSignature = proposition(
      "oracleSignature", _ => pkOracle, "pkOracle",
      scriptVersion = Some(0) // this version is required for `INPUTS(0).propositionBytes == pkOracle.propBytes`
    )

    lazy val aliceSignature  = proposition(
      "aliceSignature", _ => pkA, "pkA")
  }

  property("lightweight oracle example (ErgoDsl)") {
    val spec = TestContractSpec(suite)(new TestingIRContext)
    val oracle = spec.ProvingParty("Alice")
    val alice = spec.ProvingParty("Alice")
    val bob = spec.ProvingParty("Bob")

    val temperature: Long = 18
    val contract = OracleContract[spec.type](temperature, oracle, alice, bob)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = candidateBlock(0).newTransaction()
    val sOracle = mockTx
        .outBox(value = 1L, contract.oracleSignature)
        .withRegs(reg1 -> temperature)

    val sAlice = mockTx.outBox(10, contract.prop)
    val sBob   = mockTx.outBox(10, contract.prop)

    val tx = candidateBlock(50).newTransaction().spending(sOracle, sAlice, sBob)
    tx.outBox(20, contract.aliceSignature)
    val in = tx.inputs(1)
    val res = in.runDsl()
    res shouldBe alice.pubKey

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
