package sigmastate.utxo

import sigma.ast._
import sigmastate._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigma.ast.syntax._
import org.ergoplatform._
import sigma.ast.SCollection._
import sigma.data.AvlTreeData
import SCollectionMethods.{FlatMapMethod, IndexOfMethod, IndicesMethod, PatchMethod, UpdateManyMethod, UpdatedMethod}
import sigma.ast
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigma.serialization.OpCodes._
import sigma.ast.MethodCall
import sigma.eval.Extensions.SigmaBooleanOps
import sigmastate.utils.Helpers._

class CollectionOperationsSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private def context(boxesToSpend: IndexedSeq[ErgoBox] = IndexedSeq(),
                      outputs: IndexedSeq[ErgoBox]): ErgoLikeContext =
    {
      val (selfBox, toSpend) = if (boxesToSpend.isEmpty) (fakeSelf, IndexedSeq(fakeSelf)) else (boxesToSpend(0), boxesToSpend)
      ErgoLikeContextTesting(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = toSpend,
        spendingTransaction = createTransaction(outputs),
        self = selfBox, activatedVersionInTests)
    }

  private def assertProof(code: String,
                          expectedComp: SigmaPropValue,
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    val propTree = mkTestErgoTree(prop)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), propTree, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), propTree, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
  }

  private def assertProof(code: String,
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long]) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, None, outputBoxValues, boxesToSpendValues)
    val propTree = mkTestErgoTree(prop)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), propTree, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), propTree, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
  }

  private def assertProverFail(code: String,
                               expectedComp: SigmaPropValue,
                               outputBoxValues: IndexedSeq[Long],
                               boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, _, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    val propTree = mkTestErgoTree(prop)
    prover.prove(propTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  private def buildEnv(code: String,
                       expectedComp: Value[SType],
                       outputBoxValues: IndexedSeq[Long],
                       boxesToSpendValues: IndexedSeq[Long]):
  (ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaPropValue, ErgoLikeContext) =
    buildEnv(code, Some(expectedComp), outputBoxValues, boxesToSpendValues)

  private def buildEnv(code: String,
                       expectedComp: Option[Value[SType]],
                       outputBoxValues: IndexedSeq[Long],
                       boxesToSpendValues: IndexedSeq[Long]):
  (ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaPropValue, ErgoLikeContext) = {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(), code).asBoolValue.toSigmaProp

    expectedComp.foreach(prop shouldBe _)
    val ctx = context(boxesToSpendValues.map(testBox(_, pubkeyTree, 0)),
      outputBoxValues.map(testBox(_, pubkeyTree, 0)))
    (prover, verifier, prop, ctx)
  }

  property("exists") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.toSigmaPropValue
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(), "OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })").asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val expProp = Exists(Outputs,
      FuncValue(Vector((1, SBox)),
        GT(Plus(ExtractAmount(ValUse(1, SBox)), LongConstant(5)), LongConstant(10)))
    ).toSigmaProp
    prop shouldBe expProp

    val newBox1 = testBox(16, pubkeyTree, 0)
    val newBox2 = testBox(15, pubkeyTree, 0)

    val spendingTransaction = createTransaction(Array(newBox1, newBox2))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = Array(fakeSelf),
      spendingTransaction,
      self = fakeSelf, activatedVersionInTests)

    {
      val pr = prover.prove(propTree, ctx, fakeMessage).get
      verifier.verify(propTree, ctx, pr, fakeMessage).get._1 shouldBe true
    }

    // negative case for `exists`
    {
      val newBox1 = testBox(1, pubkeyTree, 0)
      val newBox2 = testBox(5, pubkeyTree, 0)
      val tx2 = createTransaction(Array(newBox1, newBox2))
      val ctx2 = ErgoLikeContextTesting(
        currentHeight = ctx.preHeader.height,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = Array(fakeSelf),
        spendingTransaction = tx2,
        self = fakeSelf, activatedVersionInTests)

      prover.prove(propTree, ctx2, fakeMessage).isFailure shouldBe true
    }
  }

  property("forall") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      ).toSigmaProp
    prop shouldBe propExpected

    val newBox1 = testBox(10, pubkeyTree, 0)
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
    verifier.verify(propTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("forall - fail") {
    val prover = new ContextEnrichingTestProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      ).toSigmaProp
    prop shouldBe propExpected

    val newBox1 = testBox(10, pubkeyTree, 0)
    val newBox2 = testBox(11, pubkeyTree, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf, activatedVersionInTests)

    prover.prove(propTree, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.toSigmaPropValue
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].get == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ast.ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).get,
          Plus(ast.ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1)))
      )
    ).toSigmaProp
    prop shouldBe propExpected

    val newBox1 = testBox(10, pubkeyTree, 0, Seq(), Map(reg1 -> LongConstant(3)))
    val newBox2 = testBox(10, pubkeyTree, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = testBox(20, TrueTree, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), propTree, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), propTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].getOrElse(0L) == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue.toSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ast.ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).getOrElse(LongConstant(0)),
          Plus(ast.ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1))
        )
      )
    ).toSigmaProp

    prop shouldBe propExpected

    val newBox1 = testBox(10, pubkeyTree, 0)
    val newBox2 = testBox(10, pubkeyTree, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = testBox(20, TrueTree, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr = prover.prove(propTree, ctx, fakeMessage).get
    verifier.verify(propTree, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage
    val pubkeyTree = mkTestErgoTree(pubkey)

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asSigmaProp
    val propTree = mkTestErgoTree(prop)

    val propExpected = SigmaAnd(pubkey, BoolToSigmaProp(EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1)))))
    prop shouldBe propExpected

    val newBox1 = testBox(11, pubkeyTree, 0)
    val newBox2 = testBox(10, pubkeyTree, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = testBox(21, pubkeyTree, 0)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s, activatedVersionInTests)

    val pr = prover.prove(propTree, ctx, fakeMessage).get
    verifier.verify(propTree, ctx, pr, fakeMessage)


    val fProp = SigmaAnd(pubkey, EQ(SizeOf(Outputs), SizeOf(Inputs)))
    prover.prove(mkTestErgoTree(fProp), ctx, fakeMessage).isSuccess shouldBe false
  }

  property("slice") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS.slice(1, OUTPUTS.size).forall({ (box: Box) => box.value == 10 })"
    val expectedPropTree = ForAll(
      Slice(Outputs, IntConstant(1), SizeOf(Outputs)),
      FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
    ).toSigmaProp
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("slice - fail") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    // starting index out of bounds
    val code = "OUTPUTS.slice(3, OUTPUTS.size).size == 1"
    val expectedPropTree = EQ(
      SizeOf(Slice(Outputs, IntConstant(3), SizeOf(Outputs))),
      IntConstant(1))
    assertProverFail(code, expectedPropTree, outputBoxValues)
  }

  property("append") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "(OUTPUTS ++ OUTPUTS).size == 4"
    val expectedPropTree = EQ(SizeOf(Append(Outputs, Outputs)), IntConstant(4))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(0).value == 10"
    val expectedPropTree = EQ(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with evaluation") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(OUTPUTS.size - 1).value == 10"
    val expectedPropTree = EQ(
      ExtractAmount(
        ByIndex(Outputs,
          ArithOp(SizeOf(Outputs), IntConstant(1), MinusCode))),
      LongConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with default value") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map { (box: Box) => box.value }
        |.getOrElse(3, 0L)== 0""".stripMargin
    val expectedPropTree = EQ(
      ByIndex(
        MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
        IntConstant(3),
        Some(LongConstant(0))
      ),
      LongConstant(0)
    )

    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with evaluated default value") {
    val outputBoxValues = IndexedSeq(20L, 0L)
    val code = "OUTPUTS.getOrElse(3, OUTPUTS(0)).value == 20"
    val expectedPropTree = EQ(
      ExtractAmount(
        ByIndex(Outputs,
          IntConstant(3),
          Some(ByIndex(Outputs, IntConstant(0))))),
      LongConstant(20))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("map fold") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map({ (box: Box) => box.value })
        |.fold(true, { (acc: Boolean, val: Long) => acc && (val < 0) }) == false""".stripMargin
    val expectedPropTree = LogicalNot(
      Fold(
        MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
        TrueLeaf,
        FuncValue(Vector((1, STuple(SBoolean, SLong))),
          BinAnd(
            SelectField(ValUse(1, STuple(SBoolean, SLong)), 1).asBoolValue,
            LT(SelectField(ValUse(1, STuple(SBoolean, SLong)), 2), LongConstant(0)))))
    )
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("map") {
    assertProof("OUTPUTS.map({ (out: Box) => out.value })(0) == 1L",
      EQ(
        ByIndex(
          MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
          IntConstant(0)
        ),
        LongConstant(1)
      ),
      IndexedSeq(1L, 1L))
  }

  property("forall for custom collection") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val indexCollection = Coll(0, 1, 2, 3, 4, 5)
        |  def elementRule(index: Int) = {
        |    val boundaryIndex = if (index == 0) 5 else (index - 1)
        |    boundaryIndex >= 0 && boundaryIndex <= 5
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val expectedPropTree = ForAll(
      ConcreteCollection(Array(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(Array((1, SInt)),
        BlockValue(
          Array(ValDef(3, If(EQ(ValUse(1, SInt), IntConstant(0)), IntConstant(5), Minus(ValUse(1, SInt), IntConstant(1))))),
          BinAnd(GE(ValUse(3, SInt), IntConstant(0)), LE(ValUse(3, SInt), IntConstant(5)))
        )
      )
    )
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("ByIndex for non-evaluated index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val string = Coll(1, 1, 0, 0, 0, 1)
        |  val indexCollection = Coll(0, 1, 2, 3, 4, 5)
        |  val elementRule = {(index: Int) =>
        |    val boundedIndex = if (index <= 0) 5 else (index - 1)
        |    val element = string(boundedIndex)
        |    element == 0 || element == 1
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val expectedPropTree = ForAll(
      ConcreteCollection(Array(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(
        Array((1, SInt)),
        BlockValue(
          Array(
            ValDef(3,
              ByIndex(
                ConcreteCollection(Array(IntConstant(1), IntConstant(1), IntConstant(0), IntConstant(0), IntConstant(0), IntConstant(1)), SInt),
                If(
                  LE(ValUse(1, SInt), IntConstant(0)),
                  IntConstant(5),
                  Minus(ValUse(1, SInt), IntConstant(1))
                ),
                None))),
          BinOr(EQ(ValUse(3, SInt), IntConstant(0)), EQ(ValUse(3, SInt), IntConstant(1)))
        )
      )
    )

    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("flatMap") {
    assertProof(
      s"OUTPUTS.flatMap({ (out: Box) => out.propositionBytes })(0) == $ergoTreeHeaderInTests.toByte",
      EQ(
        ByIndex(
          MethodCall(Outputs,
            FlatMapMethod.withConcreteTypes(Map(tIV -> SBox, tOV -> SByte)),
            Vector(FuncValue(1, SBox,
              ExtractScriptBytes(ValUse(1, SBox))
            )),
            Map()
          ).asCollection[SByte.type],
          IntConstant(0)
        ),
        ByteConstant(ergoTreeHeaderInTests)
      ),
      IndexedSeq(1L, 1L))
  }

  property("indexOf") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).indexOf(1L, 0) == 0",
      EQ(
        MethodCall(MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
          IndexOfMethod.withConcreteTypes(Map(tIV -> SLong)),
          Vector(LongConstant(1), IntConstant(0)),
          Map()),
        IntConstant(0)
      ),
      IndexedSeq(1L, 1L))
  }

  property("indices") {
    assertProof("OUTPUTS.indices == Coll(0, 1)",
      EQ(MethodCall(Outputs, IndicesMethod.withConcreteTypes(Map(tIV -> SBox)), Vector(), Map()), ConcreteCollection.fromItems(IntConstant(0), IntConstant(1))),
      IndexedSeq(1L, 1L))
  }

  property("zip") {
    assertProof("OUTPUTS.zip(INPUTS).size == 2",
      EQ(
        SizeOf(MethodCall(Outputs,
          SCollectionMethods.ZipMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SBox)),
          Vector(Inputs),
          Map()).asCollection[STuple]),
        IntConstant(2)),
      IndexedSeq(1L, 2L), IndexedSeq(3L, 4L))
  }

  property("zip (nested)") {
    assertProof(
      """OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS)
        | .map({ (t: (((Box, Box), Box), Box)) =>
        | t._1._2.value + t._2.value
        | }).fold(0L, { (a: Long, v: Long) => a + v }) == 10""".stripMargin,
      IndexedSeq(1L, 2L), IndexedSeq(3L, 4L))
  }

  property("patch") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).patch(0, Coll(3L), 1)(0) == 3L",
      EQ(
        ByIndex(
          MethodCall(
            MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
            PatchMethod.withConcreteTypes(Map(tIV -> SLong)),
            Vector(IntConstant(0), ConcreteCollection.fromItems(LongConstant(3)), IntConstant(1)),
            Map()).asCollection[SType],
          IntConstant(0)
        ),
        LongConstant(3)),
      IndexedSeq(1L, 2L))
  }

  property("updated") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).updated(0, 3L)(0) == 3L",
      EQ(
        ByIndex(
          MethodCall(
            MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
            UpdatedMethod.withConcreteTypes(Map(tIV -> SLong)),
            Vector(IntConstant(0), LongConstant(3)),
            Map()).asCollection[SType],
          IntConstant(0)
        ),
        LongConstant(3)),
      IndexedSeq(1L, 2L))
  }

  property("updateMany") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).updateMany(Coll(0), Coll(3L))(0) == 3L",
      EQ(
        ByIndex(
          MethodCall(
            MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
            UpdateManyMethod.withConcreteTypes(Map(tIV -> SLong)),
            Vector(ConcreteCollection.fromItems(IntConstant(0)), ConcreteCollection.fromItems(LongConstant(3))),
            Map()).asCollection[SType],
          IntConstant(0)
        ),
        LongConstant(3)),
      IndexedSeq(1L, 2L))
  }
}
