package sigmastate.utxo.examples

import org.ergoplatform._
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.Colls
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigmastate._
import sigma.ast.syntax._
import sigma.data.{AvlTreeData, AvlTreeFlags}
import sigma.eval.Extensions.SigmaBooleanOps
import sigma.eval.SigmaDsl
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.serialization.ValueSerializer


class FsmExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  private implicit lazy val IR: TestingIRContext = new TestingIRContext
  /**
    * Similarly to the MAST-like example (in the MASTExampleSpecification class), we can do more complex contracts,
    * e.g. ones with cycles. For example, we can do a contract described as a finite state machine.
    *
    * Let's consider that a input in a finite state machine is a script associated with a transition (so the transition
    * could happen only if the script is satisfied by a spending proof in a spending transaction), or a hash of the
    * script. Then we assume that a machine is described via a table of transitions, for example, as below:
    *
    * state1 | script_hash1 | state2
    * state2 | script_hash3 | state3
    * state2 | script_hash2 | state1
    * state3 | script_hash4 | -
    *
    * where state3 is a final state, which denotes end of a contract execution (and by executing a script with the hash
    * script_hash4 it is possible to create any box as a result).
    *
    * We put the table description into an authenticated data structure (such as the Merkle tree, we're using
    * authenticated AVL+ trees though). We require root digest of the tree to be preserved. We require FSM framework
    * script to be preserved as well. So to trigger a transition from one state a box remembers to another one,
    * a spending transaction should create a box with preserved root digest, and some (valid) new state, show
    * a proof for a corresponding transition description in the tree, and show a proof for transition script (e.g. a
    * signature). Thus the FSM is being revealed on the run, and the states not visited yet are not visible (on chain).
    */
  property("simple FSM example") {

    val prover = new ContextEnrichingTestProvingInterpreter

    val script1 = prover.dlogSecrets.head.publicImage.toSigmaPropValue
    val script2 = prover.dhSecrets.head.publicImage.toSigmaPropValue
    val script3 = SigmaAnd(script1, script2)
    val script4 = prover.dlogSecrets.tail.head.publicImage.toSigmaPropValue //a script to leave FSM

    val script1Hash = hash.Blake2b256(ValueSerializer.serialize(script1))
    val script2Hash = hash.Blake2b256(ValueSerializer.serialize(script2))
    val script3Hash = hash.Blake2b256(ValueSerializer.serialize(script3))
    val script4Hash = hash.Blake2b256(ValueSerializer.serialize(script4))

    val state1Id = 1: Byte
    val state2Id = 2: Byte
    val state3Id = 3: Byte

    //artificial state id to mark the fact of leaving the FSM
    val leaveFsmStateId = 0: Byte

    val transition12 = Array(state1Id, state2Id)
    val transition21 = Array(state2Id, state1Id)
    val transition23 = Array(state2Id, state3Id)
    val transition30 = Array(state3Id, leaveFsmStateId)

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 34, valueLengthOpt = Some(0))
    avlProver.performOneOperation(Insert(ADKey @@ (transition12 ++ script1Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition21 ++ script2Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition23 ++ script3Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition30 ++ script4Hash), ADValue @@ Array.emptyByteArray))
    avlProver.generateProof()

    val digest = Colls.fromArray(avlProver.digest)
    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 34, Some(0)))

    val fsmDescRegister = ErgoBox.nonMandatoryRegisters.head
    val currentStateRegister = ErgoBox.nonMandatoryRegisters(1)

    val scriptVarId = 2: Byte
    val transitionProofId = 3: Byte

    val isMember = OptionIsDefined(
      IR.builder.mkMethodCall(
        OptionGet(ExtractRegisterAs[SAvlTree.type](Self, fsmDescRegister)),
        SAvlTreeMethods.getMethod,
        IndexedSeq(Append(
          ConcreteCollection.fromItems[SByte.type](
            OptionGet(ExtractRegisterAs[SByte.type](Self, currentStateRegister)),
            OptionGetOrElse(ExtractRegisterAs[SByte.type](ByIndex(Outputs, IntConstant.Zero),
                                          currentStateRegister),ByteConstant(-1))),
          CalcBlake2b256(GetVarByteArray(scriptVarId).get)
        ),
        GetVarByteArray(transitionProofId).get)
      ).asOption[SByteArray]
    )

    val scriptPreservation = EQ(ExtractScriptBytes(ByIndex(Outputs, IntConstant.Zero)), ExtractScriptBytes(Self))

    val treePreservation = EQ(
      ExtractRegisterAs[SAvlTree.type](ByIndex(Outputs, IntConstant.Zero),
        fsmDescRegister).getOrElse(AvlTreeConstant(SigmaDsl.avlTree(AvlTreeData.dummy))),
      ExtractRegisterAs[SAvlTree.type](Self, fsmDescRegister).get)

    val preservation = AND(scriptPreservation, treePreservation)

    val finalStateCheck = EQ(ExtractRegisterAs[SByte.type](Self, currentStateRegister).get, ByteConstant(state3Id))

    /*
    val finalScriptCorrect = OptionIsDefined(
      TreeLookup(
        OptionGet(ExtractRegisterAs[SAvlTree.type](Self, fsmDescRegister)),
        Append(
          ConcreteCollection[SByte.type](
            OptionGet(ExtractRegisterAs[SByte.type](Self, currentStateRegister)),
            ByteConstant(leaveFsmStateId)),
          CalcBlake2b256(TaggedByteArray(scriptVarId))
        ),
        TaggedByteArray(transitionProofId))
    )*/

    val finalScriptCorrect = TrueLeaf


    val fsmScript = mkTestErgoTree(SigmaOr(
      SigmaAnd(isMember, DeserializeContext(scriptVarId, SSigmaProp), preservation), //going through FSM
      SigmaAnd(finalStateCheck, finalScriptCorrect, DeserializeContext(scriptVarId, SSigmaProp)) //leaving FSM
    ))


    //creating a box in an initial state

    val fsmBox1 = testBox(100, fsmScript, 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state1Id)))

    //successful transition from state1 to state2
    val fsmBox2 = testBox(100, fsmScript, 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state2Id)))

    avlProver.performOneOperation(Lookup(ADKey @@ (transition12 ++ script1Hash)))
    val transition12Proof = avlProver.generateProof()

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox1),
      createTransaction(fsmBox2),
      self = fsmBox1, activatedVersionInTests)

    val spendingProof = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(emptyEnv + (ScriptNameProp -> "prove"), fsmScript, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(fsmScript, ctx, spendingProof, fakeMessage).get._1 shouldBe true

    //successful transition back from state2 to state1

    avlProver.performOneOperation(Lookup(ADKey @@ (transition21 ++ script2Hash)))
    val transition21Proof = avlProver.generateProof()

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 51,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(fsmBox1),
      self = fsmBox2, activatedVersionInTests)

    val spendingProof2 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script2)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition21Proof))
      .prove(fsmScript, ctx2, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(fsmScript, ctx2, spendingProof2, fakeMessage).get._1 shouldBe true


    //Box for state3

    val fsmBox3 = testBox(100, fsmScript, 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state3Id)))

    //transition from state1 to state3 is impossible

    val transition13 = Array(state1Id, state3Id)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition13 ++ script1Hash)))
    val transition13Proof = avlProver.generateProof()

    val ctx3 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox1),
      createTransaction(fsmBox3),
      self = fsmBox1, activatedVersionInTests)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition13Proof))
      .prove(fsmScript, ctx3, fakeMessage)
      .isSuccess shouldBe false

    //prover tries to transit to state3 from state2 by presenting proof for state1 -> state2
    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(fsmScript, ctx3, fakeMessage)
      .isSuccess shouldBe false

    //successful transition from state2 to state3
    avlProver.performOneOperation(Lookup(ADKey @@ (transition23 ++ script3Hash)))
    val transition23Proof = avlProver.generateProof()

    val ctx23 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(fsmBox3),
      self = fsmBox2, activatedVersionInTests)

    val spendingProof23 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script3)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition23Proof))
      .prove(fsmScript, ctx23, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(fsmScript, ctx23, spendingProof23, fakeMessage).get._1 shouldBe true

    //clearing FSM out of the box in the final state

    val freeBox = testBox(100, TrueTree, 0)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition30 ++ script4Hash)))
    val transition30Proof = avlProver.generateProof()

    val ctx30 = ErgoLikeContextTesting(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox3),
      createTransaction(freeBox),
      self = fsmBox3, activatedVersionInTests)

    val spendingProof30 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(fsmScript, ctx30, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(fsmScript, ctx30, spendingProof30, fakeMessage).get._1 shouldBe true

    //it is impossible to leave FSM at state2
    val ctx20 = ErgoLikeContextTesting(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(freeBox),
      self = fsmBox2, activatedVersionInTests)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(fsmScript, ctx20, fakeMessage)
      .isSuccess shouldBe false
  }

  property("simple FSM example (ErgoScript)") {

    val prover = new ContextEnrichingTestProvingInterpreter

    val script1 = prover.dlogSecrets.head.publicImage.toSigmaPropValue
    val script2 = prover.dhSecrets.head.publicImage.toSigmaPropValue
    val script3 = SigmaAnd(script1, script2)
    val script4 = prover.dlogSecrets.tail.head.publicImage.toSigmaPropValue //a script to leave FSM

    val script1Hash = hash.Blake2b256(ValueSerializer.serialize(script1))
    val script2Hash = hash.Blake2b256(ValueSerializer.serialize(script2))
    val script3Hash = hash.Blake2b256(ValueSerializer.serialize(script3))
    val script4Hash = hash.Blake2b256(ValueSerializer.serialize(script4))

    val state1Id = 1: Byte
    val state2Id = 2: Byte
    val state3Id = 3: Byte

    //artificial state id to mark the fact of leaving the FSM
    val leaveFsmStateId = 0: Byte

    val transition12 = Array(state1Id, state2Id)
    val transition21 = Array(state2Id, state1Id)
    val transition23 = Array(state2Id, state3Id)
    val transition30 = Array(state3Id, leaveFsmStateId)

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 34, valueLengthOpt = Some(0))
    avlProver.performOneOperation(Insert(ADKey @@ (transition12 ++ script1Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition21 ++ script2Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition23 ++ script3Hash), ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@ (transition30 ++ script4Hash), ADValue @@ Array.emptyByteArray))
    avlProver.generateProof()

    val digest = Colls.fromArray(avlProver.digest)
    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 34, Some(0)))

    val fsmDescRegister = ErgoBox.nonMandatoryRegisters.head //R4
    val currentStateRegister = ErgoBox.nonMandatoryRegisters(1) //R5

    val scriptVarId = 2: Byte
    val transitionProofId = 3: Byte

    val isMember = OptionIsDefined(
      IR.builder.mkMethodCall(
        OptionGet(ExtractRegisterAs[SAvlTree.type](Self, fsmDescRegister)),
        SAvlTreeMethods.getMethod,
        IndexedSeq(Append(
          ConcreteCollection.fromItems[SByte.type](
            OptionGet(ExtractRegisterAs[SByte.type](Self, currentStateRegister)),
            OptionGetOrElse(ExtractRegisterAs[SByte.type](ByIndex(Outputs, IntConstant.Zero),
              currentStateRegister),ByteConstant(-1))),
          CalcBlake2b256(GetVarByteArray(scriptVarId).get)
        ),
          GetVarByteArray(transitionProofId).get)
      ).asOption[SByteArray]
    )

    val es = s"""{
      | val fsmDescRegister = SELF.R4[AvlTree].get
      | val currentStateRegister = SELF.R5[Byte].get
      | val threeByte = 0x03
      | val elseByte = -0x01.toByte
      | val scriptVar = getVar[Coll[Byte]]($scriptVarId).get
      | val scriptHash = blake2b256(scriptVar)
      | val transitionProof = getVar[Coll[Byte]]($transitionProofId).get
      | val hash = Coll(currentStateRegister, OUTPUTS(0).R5[Byte].getOrElse(elseByte)).append(scriptHash)
      |
      | val scriptPreservation = SELF.propositionBytes == OUTPUTS(0).propositionBytes
      | val treePreservation = OUTPUTS(0).R4[AvlTree].get == SELF.R4[AvlTree].get
      | val preservation = scriptPreservation && treePreservation
      |
      | val member = fsmDescRegister.get(hash, transitionProof)
      | val isMember = member.isDefined
      | val script = true
      | val finalStateCheck = currentStateRegister == threeByte
      | val finalScriptCorrect = true
      |
      | (isMember && preservation && script) || (finalStateCheck && finalScriptCorrect && script)
      |}""".stripMargin


    val esProp = (env: Map[String, _])
      => mkTestErgoTree(compile(env, es)(IR).asBoolValue.toSigmaProp)

    val testBoxEnv = emptyEnv + (ScriptNameProp -> "test")
    //creating a box in an initial state

    val fsmBox1 = testBox(100, esProp(testBoxEnv), 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state1Id)))

    //successful transition from state1 to state2
    val fsmBox2 = testBox(100, esProp(testBoxEnv), 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state2Id)))

    avlProver.performOneOperation(Lookup(ADKey @@ (transition12 ++ script1Hash)))
    val transition12Proof = avlProver.generateProof()

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox1),
      createTransaction(fsmBox2),
      self = fsmBox1, activatedVersionInTests)

    val proofEnv = emptyEnv + (ScriptNameProp -> "prove")
    val spendingProof = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(proofEnv, esProp(proofEnv), ctx, fakeMessage).get

    val verifyEnv = emptyEnv + (ScriptNameProp -> "verify")
    (new ErgoLikeTestInterpreter)
      .verify(verifyEnv, esProp(verifyEnv), ctx, spendingProof, fakeMessage).get._1 shouldBe true

    //successful transition back from state2 to state1

    avlProver.performOneOperation(Lookup(ADKey @@ (transition21 ++ script2Hash)))
    val transition21Proof = avlProver.generateProof()

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 51,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(fsmBox1),
      self = fsmBox2, activatedVersionInTests)

    val spendingProof2 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script2)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition21Proof))
      .prove(esProp(proofEnv), ctx2, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(esProp(proofEnv), ctx2, spendingProof2, fakeMessage).get._1 shouldBe true

    //Box for state3

    val fsmBox3 = testBox(100, esProp(proofEnv), 0, Seq(), Map(fsmDescRegister -> AvlTreeConstant(treeData),
      currentStateRegister -> ByteConstant(state3Id)))

    //transition from state1 to state3 is impossible

    val transition13 = Array(state1Id, state3Id)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition13 ++ script1Hash)))
    val transition13Proof = avlProver.generateProof()

    val ctx3 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox1),
      createTransaction(fsmBox3),
      self = fsmBox1, activatedVersionInTests)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition13Proof))
      .prove(esProp(proofEnv), ctx3, fakeMessage)
      .isSuccess shouldBe false

    //prover tries to transit to state3 from state2 by presenting proof for state1 -> state2
    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script1)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition12Proof))
      .prove(esProp(proofEnv), ctx3, fakeMessage)
      .isSuccess shouldBe false

    //successful transition from state2 to state3
    avlProver.performOneOperation(Lookup(ADKey @@ (transition23 ++ script3Hash)))
    val transition23Proof = avlProver.generateProof()

    val ctx23 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(fsmBox3),
      self = fsmBox2, activatedVersionInTests)

    val spendingProof23 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script3)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition23Proof))
      .prove(esProp(proofEnv), ctx23, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(esProp(proofEnv), ctx23, spendingProof23, fakeMessage).get._1 shouldBe true

    //clearing FSM out of the box in the final state

    val freeBox = testBox(100, TrueTree, 0)

    avlProver.performOneOperation(Lookup(ADKey @@ (transition30 ++ script4Hash)))
    val transition30Proof = avlProver.generateProof()

    val ctx30 = ErgoLikeContextTesting(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox3),
      createTransaction(freeBox),
      self = fsmBox3, activatedVersionInTests)

    val spendingProof30 = prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(esProp(proofEnv), ctx30, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(esProp(verifyEnv), ctx30, spendingProof30, fakeMessage).get._1 shouldBe true

    //it is impossible to leave FSM at state2
    val ctx20 = ErgoLikeContextTesting(
      currentHeight = 52,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fsmBox2),
      createTransaction(freeBox),
      self = fsmBox2, activatedVersionInTests)

    //honest prover fails
    prover
      .withContextExtender(scriptVarId, ByteArrayConstant(ValueSerializer.serialize(script4)))
      .withContextExtender(transitionProofId, ByteArrayConstant(transition30Proof))
      .prove(esProp(proofEnv), ctx20, fakeMessage)
      .isSuccess shouldBe false
  }
}
