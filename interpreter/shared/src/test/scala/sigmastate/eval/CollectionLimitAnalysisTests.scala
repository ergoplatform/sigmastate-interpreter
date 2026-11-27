package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.SCollection.SByteArray
import sigma.ast.{Append, BlockValue, BoolToSigmaProp, ByteArrayConstant, ByIndex,
  ConcreteCollection, EQ, ErgoTree, FuncValue, If, IntConstant, JitCost, MethodCall,
  SByte, SCollection, SCollectionMethods, SInt, SPair, SSigmaProp, STuple, SType,
  SelectField, SizeOf, Slice, ValDef, ValUse, Value}
import sigma.data.CSigmaProp
import sigma.eval.EvalSettings
import sigma.util.MaxArrayLength
import sigma.{Coll, ContractsTestkit, VersionContext}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class CollectionLimitAnalysisTests extends AnyFunSuite with ContractsTestkit with Matchers {
  private val settings = EvalSettings(
    isMeasureOperationTime = false,
    isMeasureScriptTime = false,
    costTracingEnabled = true)

  private def appendDefs = (1 to 16).map { id =>
    val previous: Value[SByteArray] =
      if (id == 1) ByteArrayConstant(Array(1.toByte))
      else ValUse(id - 1, SByteArray)
    ValDef(id, Append(previous, previous))
  }

  private def flatMapMethod(inputType: SType) =
    SCollectionMethods.FlatMapMethod.withConcreteTypes(
      Map(SCollection.tIV -> inputType, SCollection.tOV -> SByte))

  private def makeErgoTree(proposition: Value[SSigmaProp.type]) = {
    val ergoTree = new ErgoTree(
      ErgoTree.DefaultHeader, ErgoTree.EmptyConstants,
      Right(BlockValue(appendDefs, proposition).asInstanceOf[Value[SSigmaProp.type]]))
    ergoTree.bytes.length should be < org.ergoplatform.ErgoBox.MaxBoxSize
    ergoTree
  }

  private def evaluate(ergoTree: ErgoTree) = {
    val context = newContext(
      height = 0,
      self = newAliceBox(1L),
      activatedScriptVersion = VersionContext.V6SoftForkVersion,
      currErgoTreeVersion = ergoTree.version)
    val accumulator = new CostAccumulator(initialCost = JitCost(0), costLimit = None)
    val (result, _) = VersionContext.withVersions(
      VersionContext.V6SoftForkVersion, ergoTree.version) {
      CErgoTreeEvaluator.eval(
        context, accumulator, ergoTree.constants,
        ergoTree.toProposition(replaceConstants = false), settings)
    }
    (result, accumulator)
  }

  private def rootCause(error: Throwable) =
    Iterator.iterate[Throwable](error)(_.getCause).takeWhile(_ != null).toSeq.last

  private def evaluationError(ergoTree: ErgoTree) =
    rootCause(the[Exception] thrownBy evaluate(ergoTree))

  test("ErgoTree flatMap rejects oversized output before evaluating the next mapper call") {
    val inner = ValUse(16, SByteArray)
    val invalidResult = ByIndex(
      ConcreteCollection(Array(ByteArrayConstant(Array(1.toByte))), SByteArray),
      IntConstant(1))
    val mapper = FuncValue(Vector((17, SInt)),
      If(EQ(ValUse(17, SInt), IntConstant(0)), invalidResult, inner))
    val input = ConcreteCollection(
      Array(IntConstant(1), IntConstant(1), IntConstant(0)), SInt)
    val flatMap = MethodCall.typed[Value[SCollection[SByte.type]]](
      input, flatMapMethod(SInt), IndexedSeq(mapper), Map.empty)
    val ergoTree = makeErgoTree(
      BoolToSigmaProp(EQ(SizeOf(flatMap), IntConstant(131072))))

    evaluationError(ergoTree).getMessage should include(MaxArrayLength.toString)
  }

  test("ErgoTree flatMap rejects before materializing a 64 MiB result") {
    val inner = ValUse(16, SByteArray)
    val indicesMethod = SCollectionMethods.IndicesMethod.withConcreteTypes(
      Map(SCollection.tIV -> SByte))
    val input = MethodCall.typed[Value[SCollection[SInt.type]]](
      Slice(inner, IntConstant(0), IntConstant(1024)),
      indicesMethod, Vector.empty, Map.empty)
    val mapper = FuncValue(Vector((17, SInt)), inner)
    val flatMap = MethodCall.typed[Value[SCollection[SByte.type]]](
      input, flatMapMethod(SInt), IndexedSeq(mapper), Map.empty)
    val ergoTree = makeErgoTree(BoolToSigmaProp(EQ(SizeOf(flatMap), IntConstant(0))))

    val start = System.nanoTime()
    val error = rootCause(the[Exception] thrownBy evaluate(ergoTree))
    val elapsedMillis = (System.nanoTime() - start) / 1000000

    val fullResultSize = 1024 * 65536
    println(s"flatMap 64 MiB reproduction: $elapsedMillis ms, error: ${error.getMessage}")
    error.getMessage should include(MaxArrayLength.toString)
    withClue(s"flatMap materialized the complete result: ${error.getMessage}") {
      error.getMessage.contains(fullResultSize.toString) shouldBe false
    }
  }

  test("ErgoTree flatMap accepts output exactly at MaxArrayLength") {
    val inner = Slice(ValUse(16, SByteArray), IntConstant(0), IntConstant(MaxArrayLength / 2))
    val input = ConcreteCollection(Array(IntConstant(1), IntConstant(1)), SInt)
    val mapper = FuncValue(Vector((17, SInt)), inner)
    val flatMap = MethodCall.typed[Value[SCollection[SByte.type]]](
      input, flatMapMethod(SInt), IndexedSeq(mapper), Map.empty)
    val ergoTree = makeErgoTree(
      BoolToSigmaProp(EQ(SizeOf(flatMap), IntConstant(MaxArrayLength))))

    val (result, _) = evaluate(ergoTree)
    result shouldBe CSigmaProp(true)
  }

  test("ErgoTree pair flatMap rejects oversized output before evaluating the next mapper call") {
    val inner = ValUse(16, SByteArray)
    val invalidResult = ByIndex(
      ConcreteCollection(Array(ByteArrayConstant(Array(1.toByte))), SByteArray),
      IntConstant(1))
    val pairType = SPair(SInt, SInt)
    val pairElement = SelectField.typed[Value[SInt.type]](ValUse(17, pairType), 1.toByte)
    val mapper = FuncValue(Vector((17, pairType)),
      If(EQ(pairElement, IntConstant(0)), invalidResult, inner))
    val input = ConcreteCollection(
      Array(IntConstant(1), IntConstant(1), IntConstant(0)), SInt)
    val zipMethod = SCollectionMethods.ZipMethod.withConcreteTypes(
      Map(SCollection.tIV -> SInt, SCollection.tOV -> SInt))
    val zipped = MethodCall.typed[Value[SCollection[STuple]]](
      input, zipMethod, Vector(input), Map.empty)
    val flatMap = MethodCall.typed[Value[SCollection[SByte.type]]](
      zipped, flatMapMethod(pairType), IndexedSeq(mapper), Map.empty)
    val ergoTree = makeErgoTree(
      BoolToSigmaProp(EQ(SizeOf(flatMap), IntConstant(131072))))

    evaluationError(ergoTree).getMessage should include(MaxArrayLength.toString)
  }

  test("slice charges for the actual number of copied items when endpoint subtraction overflows") {
    val expression = Slice(
      ByteArrayConstant(Array.fill[Byte](MaxArrayLength)(1)),
      IntConstant(Int.MinValue),
      IntConstant(Int.MaxValue))
    val accumulator = new CostAccumulator(initialCost = JitCost(0), costLimit = None)
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      DefaultProfiler,
      settings)

    val result = VersionContext.withVersions(
      VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      expression.evalTo[Coll[Byte]](Map.empty)(evaluator)
    }

    result.length shouldBe MaxArrayLength
    accumulator.totalCost.value shouldBe 15 + Slice.costKind.cost(MaxArrayLength).value
  }
}
