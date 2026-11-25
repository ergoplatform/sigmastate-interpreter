package sigmastate.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.ast.SCollection.SByteArray
import sigma.ast.{BoolToSigmaProp, ByteArrayConstant, CollectionConstant, ConcreteCollection,
  DecodePoint, ErgoTree, Global, IntArrayConstant, JitCost, MethodCall, SCollection,
  SGlobalMethods, SGroupElement, SInt, SeqCostItem, SType, SubstConstants, TrueLeaf, Value}
import sigma.crypto.CryptoConstants
import sigma.eval.EvalSettings
import sigma.eval.SigmaDsl
import sigma.serialization.{SerializerException, SigmaByteWriter}
import sigma.{Coll, ContractsTestkit, GroupElementRType, VersionContext}
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultProfiler
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class SubstConstantsCostTests extends AnyFunSuite with ContractsTestkit with Matchers {

  /** Template tree with one segregated `Coll[Byte]` constant. */
  private val template = ErgoTree(
    ErgoTree.setConstantSegregation(ErgoTree.ZeroHeader),
    Vector(ByteArrayConstant(Array.emptyByteArray)),
    BoolToSigmaProp(TrueLeaf))

  /** Template tree with one segregated `Coll[Coll[Byte]]` constant (nested collection). */
  private val nestedTemplate = ErgoTree(
    ErgoTree.setConstantSegregation(ErgoTree.ZeroHeader),
    Vector(CollectionConstant[SByteArray](
      Colls.fromArray(Array[Coll[Byte]](Colls.fromArray(Array.emptyByteArray))),
      SByteArray)),
    BoolToSigmaProp(TrueLeaf))

  private def substNode(template: ErgoTree, newValBytes: Array[Byte]): SubstConstants[SByteArray] =
    SubstConstants(
      ByteArrayConstant(template.bytes),
      IntArrayConstant(Array(0)),
      ConcreteCollection(
        Array[Value[SByteArray]](ByteArrayConstant(newValBytes)),
        SByteArray))

  /** Same as `substNode` but injects a single `Coll[Coll[Byte]]` constant with the given inner arrays. */
  private def substNodeNested(newValInners: Seq[Array[Byte]]): SubstConstants[SCollection[SByteArray]] =
    SubstConstants(
      ByteArrayConstant(nestedTemplate.bytes),
      IntArrayConstant(Array(0)),
      ConcreteCollection(
        Array[Value[SCollection[SByteArray]]](
          CollectionConstant[SByteArray](
            Colls.fromArray(newValInners.map(bytes => Colls.fromArray(bytes): Coll[Byte]).toArray),
            SByteArray)),
        SCollection(SByteArray)))

  private def newEvaluator(): (CostAccumulator, CErgoTreeEvaluator) = {
    val evalSettings = EvalSettings(
      isMeasureOperationTime = false,
      isMeasureScriptTime = false,
      costTracingEnabled = true)
    val accumulator = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(evalSettings.scriptCostLimitInEvaluator)))
    val evaluator = new CErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator,
      DefaultProfiler,
      evalSettings)
    (accumulator, evaluator)
  }

  private def evalValue[T <: SType](node: Value[T]): (Any, CostAccumulator, CErgoTreeEvaluator) = {
    val (accumulator, evaluator) = newEvaluator()
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      val res = node.evalTo[T#WrappedType](Map.empty)(evaluator)
      (res, accumulator, evaluator)
    }
  }

  private def eval[T <: SType](node: SubstConstants[T]): (Coll[Byte], CostAccumulator, CErgoTreeEvaluator) = {
    val (res, accumulator, evaluator) = evalValue(node)
    (res.asInstanceOf[Coll[Byte]], accumulator, evaluator)
  }

  private def evalWithException[T <: SType](node: SubstConstants[T]): Throwable = {
    val (_, ev) = newEvaluator()
    the [Exception] thrownBy {
      VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
        node.evalTo[Coll[Byte]](Map.empty)(ev)
      }
    }
  }

  test("charged cost grows with serialized output size") {
    val smallBytes = Array.fill[Byte](100)(1)
    val bigBytes = Array.fill[Byte](60000)(1)

    val (resSmall, accSmall, evSmall) = eval(substNode(template, smallBytes))
    val (resBig, accBig, evBig) = eval(substNode(template, bigBytes))

    resBig.length should be > resSmall.length + 50000
    accBig.totalCost.value should be > accSmall.totalCost.value

    // the per-byte serialization cost reported via writer callbacks
    def writtenBytes(ev: CErgoTreeEvaluator): Int =
      ev.getCostTrace().collect {
        case item: SeqCostItem if item.costKind == SigmaByteWriter.PutChunkCost.costKind => item.nItems
      }.sum

    writtenBytes(evBig) should be > writtenBytes(evSmall) + 50000
  }

  test("output just below MaxArrayLength still succeeds") {
    val inner = Array.fill[Byte](60000)(1)

    val (res, _, _) = eval(substNodeNested(Seq(inner)))

    res.length should be > 60000
  }

  test("output above MaxArrayLength is rejected at Colls.fromArray") {
    val inners = Seq(Array.fill[Byte](49994)(1), Array.fill[Byte](49995)(1))

    val ex = evalWithException(substNodeNested(inners))

    ex shouldBe a [IllegalArgumentException]
    ex.getMessage should fullyMatch regex
      "requirement failed: Cannot create collection with size \\d+ greater than 100000"
  }

  test("nested collections are rejected mid-serialization by the writer limit") {
    val inners = Seq(Array.fill[Byte](60000)(1), Array.fill[Byte](60000)(1))

    val ex = evalWithException(substNodeNested(inners))

    ex shouldBe a [SerializerException]
    ex.getMessage should fullyMatch regex
      "Writer output limit of 100000 bytes is exceeded"
  }

  private val point = SigmaDsl.GroupElement(CryptoConstants.dlogGroup.generator)

  /** Template with one Coll[GroupElement] constant of `n` points. */
  private def groupTemplate(n: Int): ErgoTree = ErgoTree(
    ErgoTree.setConstantSegregation(ErgoTree.ZeroHeader),
    Vector(CollectionConstant[SGroupElement.type](
      Colls.fromArray(Array.fill(n)(point)),
      SGroupElement)),
    BoolToSigmaProp(TrueLeaf))

  /** Substitution with no positions: pure parse + re-serialize of all constants. */
  private def substNoOp(t: ErgoTree): SubstConstants[SInt.type] =
    SubstConstants(
      ByteArrayConstant(t.bytes),
      IntArrayConstant(Array.emptyIntArray),
      ConcreteCollection(Array.empty[Value[SInt.type]], SInt))

  test("substConstants byte charge covers worst-case GroupElement decoding (SC-03)") {
    val nPoints = 1900
    val bigTemplate = groupTemplate(nPoints)

    eval(substNoOp(bigTemplate)) // warm-up
    val (resBig, _, evBig) = eval(substNoOp(bigTemplate))

    resBig.toArray shouldEqual bigTemplate.bytes

    val parseItems = evBig.getCostTrace().collect {
      case item: SeqCostItem if item.costKind == SGlobalMethods.DeserializeCostKind => item
    }
    parseItems should have length 1
    parseItems.head.nItems shouldBe bigTemplate.bytes.length
    parseItems.head.cost.value should be >= nPoints * DecodePoint.costKind.cost.value
  }

  test("Global.deserializeTo uses worst-case deserialization cost") {
    val encoded = point.getEncoded
    val deserialize = MethodCall.typed[Value[SGroupElement.type]](
      Global,
      SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(SType.tT -> SGroupElement)),
      Vector(ByteArrayConstant(encoded.toArray)),
      Map(SType.tT -> SGroupElement))

    val (res, _, ev) = evalValue(deserialize)

    res shouldBe point
    val parseItems = ev.getCostTrace().collect {
      case item: SeqCostItem if item.opDesc == SGlobalMethods.deserializeToMethod.opDesc => item
    }
    parseItems should have length 1
    parseItems.head.costKind shouldBe SGlobalMethods.DeserializeCostKind
    parseItems.head.nItems shouldBe encoded.length
  }

  test("default writer limit is 128 KiB") {
    val w = sigma.serialization.SigmaSerializer.startWriter()
    w.maxLength shouldBe 128 * 1024
    w.putBytes(Array.fill[Byte](w.maxLength)(1))

    val ex = the[SerializerException] thrownBy w.put(1)

    ex.getMessage shouldBe "Writer output limit of 131072 bytes is exceeded"
  }

  test("putShortBytes checks the limit after writing two bytes") {
    val w = sigma.serialization.SigmaSerializer.startWriter()
    w.putBytes(Array.fill[Byte](w.maxLength - 2)(1))
    w.putShortBytes(1)
    w.length() shouldBe w.maxLength

    val ex = the[SerializerException] thrownBy w.putShortBytes(1)

    ex.getMessage shouldBe "Writer output limit of 131072 bytes is exceeded"
    w.length() shouldBe w.maxLength + 2
  }
}
