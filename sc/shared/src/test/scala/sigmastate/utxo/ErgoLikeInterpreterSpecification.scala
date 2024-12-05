package sigmastate.utxo

import scorex.utils.Bytes
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import org.scalatest.TryValues._
import scorex.crypto.hash.Blake2b256
import sigma.{Colls, VersionContext}
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigmastate._
import sigma.ast.syntax._
import sigma.data.{AvlTreeData, CBox, ProveDHTuple, ProveDlog, TrivialProp}
import sigma.util.Extensions.EcpOps
import sigma.validation.ValidationException
import sigmastate.eval._
import sigmastate.interpreter.Interpreter._
import sigmastate.helpers._
import sigmastate.helpers.TestingHelpers._
import sigma.interpreter.ContextExtension.VarBinding
import sigma.eval.Extensions.SigmaBooleanOps
import sigma.interpreter.{ContextExtension, CostedProverResult}
import sigma.serialization.{SerializationSpecification, ValueSerializer}
import sigmastate.utils.Helpers._

class ErgoLikeInterpreterSpecification extends CompilerTestingCommons
  with SerializationSpecification
  with CompilerCrossVersionProps {

  implicit lazy val IR: TestingIRContext = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  def sum[T <: SNumericType](input: Value[SCollection[T]], varId: Int)(implicit tT: T) =
    Fold(input,
      Constant(tT.upcast(0.toByte), tT),
      FuncValue(Array((varId, STuple(tT, tT))),
        Plus(
          SelectField(ValUse(varId, STuple(tT, tT)), 1).asNumValue,
          SelectField(ValUse(varId, STuple(tT, tT)), 2).asNumValue))
    )

  property("scripts EQ/NEQ") {
    val prover1 = new ContextEnrichingTestProvingInterpreter
    val prover2 = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val h1 = SigmaPropConstant(prover1.dlogSecrets.head.publicImage)
    val h2 = SigmaPropConstant(prover2.dlogSecrets.head.publicImage)

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

    val e = compile(Map(
      "h1" -> Colls.fromArray(ErgoTree.withSegregation(ergoTreeHeaderInTests, h1).bytes),
      "h2" -> Colls.fromArray(ErgoTree.withSegregation(ergoTreeHeaderInTests, h2).bytes)),
      "h1 == h1")
    val exp = TrueLeaf
    e shouldBe exp

    val res = verifier.fullReduction(mkTestErgoTree(exp), ctx).value
    res shouldBe TrivialProp.TrueProp
    val ergoTree = mkTestErgoTree(
      EQ(
        ByteArrayConstant(ErgoTree.withSegregation(ergoTreeHeaderInTests, h1).bytes),
        ByteArrayConstant(ErgoTree.withSegregation(ergoTreeHeaderInTests, h2).bytes)
      ).toSigmaProp
    )
    val res2 = verifier.fullReduction(ergoTree, ctx).value
    res2 shouldBe TrivialProp.FalseProp
  }

  property("DH tuple") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val fakeProver = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = SigmaPropConstant(ProveDHTuple(ci.g, ci.h, ci.u, ci.v))
    val propTree = mkTestErgoTree(prop)

    val wrongProp = SigmaPropConstant(ProveDHTuple(ci.g, ci.h, ci.u, ci.u))

    val env = Map("g" -> ci.g.toGroupElement, "h" -> ci.h.toGroupElement, "u" -> ci.u.toGroupElement, "v" -> ci.v.toGroupElement, "s" -> secret.publicImage)
    val compiledProp1 = compile(env, "s").asSigmaProp
    val compiledProp2 = compile(env, "proveDHTuple(g, h, u, v)").asSigmaProp
    compiledProp1 shouldBe prop
    compiledProp2 shouldBe prop


    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)

    val pr = prover.prove(propTree, ctx, fakeMessage).get
    verifier.verify(propTree, ctx, pr, fakeMessage).get._1 shouldBe true

    fakeProver.prove(propTree, ctx, fakeMessage).isSuccess shouldBe false
    prover.prove(mkTestErgoTree(wrongProp), ctx, fakeMessage).isSuccess shouldBe false
  }

  property("DH tuple - simulation") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""").asSigmaProp
    val compiledPropTree = mkTestErgoTree(compiledProp)

    val prop = SigmaOr(pubkeyA, pubdhB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)

    val prA = proverA.prove(compiledPropTree, ctx, fakeMessage).get
    verifier.verify(compiledPropTree, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""").asSigmaProp
    val compiledPropTree = mkTestErgoTree(compiledProp)

    val prop = SigmaAnd(pubkeyA, pubdhA)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf, activatedVersionInTests)

    val prA = proverA.prove(compiledPropTree, ctx, fakeMessage).get

    verifier.verify(compiledPropTree, ctx, prA, fakeMessage).get._1 shouldBe true

    proverB.prove(compiledPropTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  // related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/428
  // checks that cost of the folded function doesn't depend on data
  property("mixing scenario w. timeout") {
    val height = 50
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets.head.publicImage

    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyB2 = proverB.dlogSecrets.head.publicImage

    val newBox1 = new ErgoBoxCandidate(10, mkTestErgoTree(pubkeyB2), height)
    val newBox2 = new ErgoBoxCandidate(10, mkTestErgoTree(pubkeyA2), height)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytesWithNoRef): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = createTransaction(newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int): ErgoTree = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> Colls.fromArray(properHash))
      val compiledProp = compile(env,
        """{
          |  val notTimePassed = HEIGHT <= timeout
          |  val outBytes = OUTPUTS.map({(box: Box) => box.bytesWithoutRef})
          |  val outSumBytes = outBytes.fold(Coll[Byte](), {(arr1: Coll[Byte], arr2: Coll[Byte]) => arr1 ++ arr2})
          |  val timePassed = HEIGHT > timeout
          |  notTimePassed && blake2b256(outSumBytes) == properHash || timePassed && sender
           }""".stripMargin).asSigmaProp

      mkTestErgoTree(compiledProp)
    }

    val ctx = ErgoLikeContextTesting(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf, activatedVersionInTests)

    //before timeout
    val prA = proverA.prove(
      emptyEnv + (ScriptNameProp -> "before_timeout_prove"),
      mixingRequestProp(pubkeyA, 100), ctx, fakeMessage).get
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "before_timeout_verify1"),
      mixingRequestProp(pubkeyA, 100), ctx, prA, fakeMessage).get._1 shouldBe true
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "before_timeout_verify2"),
      mixingRequestProp(pubkeyB, 100), ctx, prA, fakeMessage).get._1 shouldBe true

    //after timeout
    val prA2 = proverA.prove(
        emptyEnv + (ScriptNameProp -> "after_timeout_prove"),
        mixingRequestProp(pubkeyA, 40), ctx, fakeMessage).get
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "after_timeout_verify1"),
      mixingRequestProp(pubkeyA, 40), ctx, prA2, fakeMessage).get._1 shouldBe true
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "after_timeout_verify2"),
      mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).map(_._1).getOrElse(false) shouldBe false
  }

  property("map + sum") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  val outValues = OUTPUTS.map({ (box: Box) => box.value })
        |  pubkey && outValues.fold(0L, { (x: Long, y: Long) => x + y }) > 20
         }""".stripMargin).asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExp = SigmaAnd(
      Seq(
        SigmaPropConstant(pubkey),
        BoolToSigmaProp(
          GT(
            sum[SLong.type](MapCollection(Outputs,
              FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))), 1),
            LongConstant(20))
        )
      )
    )
    prop shouldBe propExp

    val newBox1 = testBox(11, pubkeyTree, 0)
    val newBox2 = testBox(10, pubkeyTree, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf, activatedVersionInTests)

    val pr = prover.prove(propTree, ctx, fakeMessage).get
    verifier.verify(propTree, ctx, pr, fakeMessage).getOrThrow
  }

  property("byindex") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS(0).value > 10""").asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExp = SigmaAnd(pubkey, BoolToSigmaProp(GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10))))
    prop shouldBe propExp

    val newBox1 = testBox(11, pubkeyTree, 0)
    val newBox2 = testBox(10, pubkeyTree, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf, activatedVersionInTests)

    val pr = prover.prove(propTree, ctx, fakeMessage).get
    verifier.verify(propTree, ctx, pr, fakeMessage).getOrThrow


    val fProp1 = SigmaAnd(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(11)))
    prover.prove(mkTestErgoTree(fProp1), ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = SigmaAnd(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), LongConstant(11)))
    prover.prove(mkTestErgoTree(fProp2), ctx, fakeMessage).isSuccess shouldBe false
  }

  property("P2SH") {
    val scriptId = 21.toByte

    val prover0 = new ContextEnrichingTestProvingInterpreter()

    val script = SigmaPropConstant(prover0.dlogSecrets.head.publicImage)
    val scriptBytes = ValueSerializer.serialize(script)
    val scriptHash = Blake2b256(scriptBytes)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(CalcBlake2b256(GetVarByteArray(scriptId).get), scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SSigmaProp)
    val prop = SigmaAnd(hashEquals, scriptIsCorrect)
    val propTree = mkTestErgoTree(prop)

    val recipientProposition = SigmaPropConstant(new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage)
    val selfBox = testBox(20, TrueTree, 0, Seq(), Map())
    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      createTransaction(testBox(1, mkTestErgoTree(recipientProposition), 0)),
      self = selfBox, activatedVersionInTests)

    val proof = prover.prove(propTree, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(propTree, ctx, proof, fakeMessage).get._1 shouldBe true
  }

  property("Prove keys from registers") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    val regPubkey1 = ErgoBox.nonMandatoryRegisters.head
    val regPubkey2 = ErgoBox.nonMandatoryRegisters.tail.head

    val prop = compile(Map(),
      """{
        |  val pubkey1 = SELF.R4[GroupElement].get
        |  val pubkey2 = SELF.R5[GroupElement].get
        |  proveDlog(pubkey1) && proveDlog(pubkey2)
        |}""".stripMargin).asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = SigmaAnd(
      CreateProveDlog(ExtractRegisterAs[SGroupElement.type](Self, regPubkey1).get),
      CreateProveDlog(ExtractRegisterAs[SGroupElement.type](Self, regPubkey2).get))
    prop shouldBe propExpected

    val newBox1 = testBox(10, mkTestErgoTree(pubkey3), 0)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = createTransaction(newBoxes)

    val s1 = testBox(20, TrueTree, 0, Seq(),
      Map(regPubkey1 -> GroupElementConstant(pubkey1.value),
        regPubkey2 -> GroupElementConstant(pubkey2.value)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s1),
      spendingTransaction,
      self = s1, activatedVersionInTests)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), propTree, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), propTree, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true


    //make sure that wrong case couldn't be proved
    val s2 = testBox(20, TrueTree, 0, Seq(),
      Map(regPubkey1 -> GroupElementConstant(pubkey1.value)))
    val wrongCtx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s2),
      spendingTransaction,
      self = s2, activatedVersionInTests)

    prover.prove(propTree, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(propTree, wrongCtx, pr, fakeMessage).isFailure shouldBe true
  }

  //p2sh with 160-bit hash function (which is about just cutting first 160 bits from 256-bit hash)
  property("P2SH - 160 bits") {
    val scriptId = 21.toByte

    val bitsCount = 160
    val bytesCount = bitsCount / 8

    val prover0 = new ContextEnrichingTestProvingInterpreter()

    val customScript = prover0.dlogSecrets.head.publicImage.toSigmaPropValue
    val scriptBytes = ValueSerializer.serialize(customScript)
    val scriptHash = Blake2b256(scriptBytes).take(bytesCount)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(
      Slice(CalcBlake2b256(GetVarByteArray(scriptId).get), IntConstant(0), IntConstant(bytesCount)),
      scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SSigmaProp)
    val prop = SigmaAnd(hashEquals.toSigmaProp, scriptIsCorrect)
    val propTree = mkTestErgoTree(prop)

    val recipientPubkey = new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    val selfBox = testBox(20, TrueTree, 0, Seq(), Map())
    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      createTransaction(testBox(1, mkTestErgoTree(recipientPubkey), 0)),
      self = selfBox, activatedVersionInTests)

    val proof = prover.prove(propTree, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(propTree, ctx, proof, fakeMessage).get._1 shouldBe true
  }


  /**
    * An example script where a box could be spent only along with a box with given id
    * (and no more boxes could be provided as an input of a spending transaction).
    */
  property("Along with a brother (using Box constant in environment)") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val tb = testBox(value = 10, ergoTree = mkTestErgoTree(pubkey1), creationHeight = 0)
    val brother = if(VersionContext.current.isJitActivated) {
      CBox(tb)
    } else {
      tb
    }

    val tbWithWrongId = testBox(value = 10,
      ergoTree = mkTestErgoTree(pubkey1),
      creationHeight = 0,
      boxIndex = 120: Short)

    val newBox = testBox(20, mkTestErgoTree(pubkey2), 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = createTransaction(newBoxes)

    val env = Map("brother" -> brother)
    val prop = compile(env,
      """{
        |  val okInputs = INPUTS.size == 2
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = BinAnd(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ExtractId(BoxConstant(tb)))).toSigmaProp
    prop shouldBe propExpected

    // try a version of the script that matches the white paper
    val altEnv = Map("friend" -> brother)
    val altProp = compile(altEnv, """INPUTS.size == 2 && INPUTS(0).id == friend.id""").asBoolValue.toSigmaProp
    altProp shouldBe prop

    val s = testBox(10, propTree, 0, Seq(), Map())

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(tb, s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr = prover.prove(emptyEnv, propTree, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv, propTree, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true

    val wrongCtx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(tbWithWrongId, s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    prover.prove(propTree, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(propTree, wrongCtx, pr, fakeMessage).getOrThrow._1 shouldBe false

    val prop2 = compile(env,
      """{
        |  val okInputs = INPUTS.size == 3
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue.toSigmaProp
    val prop2Tree = mkTestErgoTree(prop2)

    prover.prove(emptyEnv, prop2Tree, ctx, fakeMessage).isFailure shouldBe true
    verifier
      .verify(emptyEnv, prop2Tree, ctx, pr, fakeMessage)
      .getOrThrow._1 shouldBe false
  }

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and possibly others, too).
    */
  property("Along with a friend and maybe others (!!! Box constant in environment)") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val friend = testBox(10, mkTestErgoTree(pubkey1), 0)
    val friendWithWrongId = testBox(10, mkTestErgoTree(pubkey1), 0, boxIndex = 120: Short)

    val newBox = testBox(20, mkTestErgoTree(pubkey2), 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = createTransaction(newBoxes)

    val friendVar = if(VersionContext.current.isJitActivated){
      CBox(friend)
    } else {
      friend
    }
    val env = Map("friend" -> friendVar)
    val prop = compile(env,
      """{
        |
        | def isFriend(inputBox: Box) = inputBox.id == friend.id
        | INPUTS.exists (isFriend)
         }""".stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = Exists(Inputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractId(ValUse(1, SBox)),
          ExtractId(BoxConstant(friend))
        )
      )
    ).toSigmaProp

    prop shouldBe propExpected

    // same script written differently
    val altProp = compile(env, "INPUTS.exists ({ (inputBox: Box) => inputBox.id == friend.id })").asBoolValue.toSigmaProp
    altProp shouldBe prop

    val s = testBox(10, propTree, 0, Seq(), Map())

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(friend, s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr1 = prover.prove(propTree, ctx1, fakeMessage).success.value
    verifier.verify(propTree, ctx1, pr1, fakeMessage).success.value._1 shouldBe true

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s, friend),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr2 = prover.prove(propTree, ctx2, fakeMessage).success.value
    verifier.verify(propTree, ctx2, pr2, fakeMessage).success.value._1 shouldBe true

    val pr3 = prover.prove(propTree, ctx2, fakeMessage).success.value
    verifier.verify(propTree, ctx2, pr3, fakeMessage).success.value._1 shouldBe true

    val wrongCtx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(friendWithWrongId, s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    prover.prove(propTree, wrongCtx1, fakeMessage).isFailure shouldBe true
    verifier.verify(propTree, wrongCtx1, pr1, fakeMessage).success.value._1 shouldBe false
  }

  property("If") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val preimageHello = "hello world".getBytes("UTF-8")
    val preimageWrong = "wrong".getBytes("UTF-8")

    val helloHash = Blake2b256.hash(preimageHello)

    val env = Map("helloHash" -> Colls.fromArray(helloHash))
    val prop = compile(env,
      """{
        |  val cond = INPUTS(0).value > 10
        |  val preimage = if (cond)
        |    INPUTS(2).R4[Coll[Byte]].get
        |  else
        |    INPUTS(1).R4[Coll[Byte]].get
        |  helloHash == blake2b256(preimage)
         }""".stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = EQ(ByteArrayConstant(helloHash),
      CalcBlake2b256(
        If(GT(ExtractAmount(ByIndex(Inputs, 0)), LongConstant(10)),
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 2), reg1).get,
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 1), reg1).get))).toSigmaProp
    prop shouldBe propExpected

    val input0 = testBox(10, pubkeyTree, 0, Seq(), Map())
    val input1 = testBox(1, pubkeyTree, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageHello)))
    val input2 = testBox(1, pubkeyTree, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageWrong)))
    val input3 = testBox(10, propTree, 0, Seq(), Map())

    val output = testBox(22, pubkeyTree, 0, Seq(), Map())

    val spendingTransaction = createTransaction(output)

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1, input2, input3),
      spendingTransaction,
      self = input3, activatedVersionInTests)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), propTree, ctx1, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), propTree, ctx1, pr, fakeMessage).get._1 shouldBe true

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(
        copyBox(input0)(value = 20), // to go through `then` branch of `if` in the script
        input1, input2, input3),
      spendingTransaction,
      self = input3, activatedVersionInTests)
    prover.prove(propTree, ctx2, fakeMessage).isFailure shouldBe true
  }

  property("DeserializeRegister value type mismatch") {
    val prover = new ContextEnrichingTestProvingInterpreter

    val sigmaProp = SigmaPropConstant(prover.dlogSecrets.head.publicImage)
    // put SigmaProp into the register
    val regValue = ByteArrayConstant(ValueSerializer.serialize(sigmaProp))
    val box = testBox(20, TrueTree, 0, Seq(), Map(R4 -> regValue))

    // expect SBoolean in the register
    val prop = DeserializeRegister(R4, SBoolean).toSigmaProp

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(box),
      createTransaction(IndexedSeq(testBox(10, TrueTree, 0))),
      self = box, activatedVersionInTests)

    an[RuntimeException] should be thrownBy
      prover.prove(emptyEnv + (ScriptNameProp -> "prove"), mkTestErgoTree(prop), ctx, fakeMessage).getOrThrow
  }

  property("DeserializeContext value(script) type mismatch") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { scriptProp =>
      val verifier = new ErgoLikeTestInterpreter
      val scriptId = 21.toByte
      val prover0 = new ContextEnrichingTestProvingInterpreter()
      // serialize boolean expression
      val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(ValueSerializer.serialize(scriptProp)))
      val prop = OR(
        LogicalNot(DeserializeContext(scriptId, scriptProp.tpe)),
        DeserializeContext(scriptId, scriptProp.tpe)
      ).toSigmaProp
      val propTree = mkTestErgoTree(prop)

      val box = testBox(20, TrueTree, 0, Seq(), Map())
      val ctx = ErgoLikeContextTesting(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(box),
        createTransaction(IndexedSeq(testBox(10, TrueTree, 0))),
        self = box, activatedVersionInTests)

      val pr = prover.prove(propTree, ctx, fakeMessage).get
      // make sure verifier will fail on deserializing context with mismatched type
      // try to deserialize it as an expression with integer type
      val prop1 = EQ(DeserializeContext(scriptId, SInt), IntConstant(1)).toSigmaProp
      val prop1Tree = mkTestErgoTree(prop1)

      an[ValidationException] should be thrownBy
        verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop1Tree, ctx, pr, fakeMessage).get
      // make sure prover fails as well on deserializing context with mismatched type
      an[ValidationException] should be thrownBy prover.prove(prop1Tree, ctx, fakeMessage).get
    }
  }

  property("DeserializeContext can return expression of non-Boolean/SigmaProp type") {
    def prove(ergoTree: ErgoTree, script: VarBinding): CostedProverResult = {
      val boxToSpend = testBox(10, ergoTree, creationHeight = 5)
      val ctx = ErgoLikeContextTesting.dummy(boxToSpend, activatedVersionInTests)
          .withExtension(
            ContextExtension(Seq(script).toMap)) // provide script bytes in context variable

      val prover = new ErgoLikeTestProvingInterpreter()
      prover.prove(ergoTree, ctx, fakeMessage).getOrThrow
    }

    val script = "{ 1 + 2 }"
    val scriptProp = compile(Map.empty, script)  // of Int type
    val scriptBytes = ValueSerializer.serialize(scriptProp)
    val tree = mkTestErgoTree(EQ(DeserializeContext(1, SInt), IntConstant(3)).toSigmaProp)
    prove(tree, script = 1.toByte -> ByteArrayConstant(scriptBytes))
  }

  property("non-const ProveDHT") {
    import sigma.crypto.CryptoConstants.dlogGroup
    compile(Map("gA" -> dlogGroup.generator.toGroupElement),
      "proveDHTuple(gA, OUTPUTS(0).R4[GroupElement].get, gA, gA)"
    ).asInstanceOf[BlockValue].result shouldBe a [CreateProveDHTuple]
  }

  property("non-const ProveDlog") {
    compile(Map(), "proveDlog(OUTPUTS(0).R4[GroupElement].get)" ) shouldBe a [CreateProveDlog]
  }
}
