package sigmastate

import org.ergoplatform.ErgoBox
import org.ergoplatform.validation.ValidationRules._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.TrivialProp
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.exceptions.{CostLimitException, InterpreterException, OpcodeUnavailableException, StarkOpcodeErgoTreeVersionException}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization._
import sigma.validation.{ChangedRule, ValidationException}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter.emptyEnv

class LegacyVerifyStarkVersionSpecification extends AnyPropSpec with Matchers {
  private val verifier = new ErgoLikeTestInterpreter
  private val proof = ProverResult.empty
  private val message = Array.empty[Byte]

  private val emptyChunks = ConcreteCollection[SByteArray](Vector.empty, SByteArray)
    .asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val verifyStark = VerifyStark(
    emptyChunks,
    ByteArrayConstant(Array.empty[Byte]),
    ByteArrayConstant(Array.empty[Byte]),
    ByteArrayConstant(Array.empty[Byte]))

  private def tree(version: Int, proposition: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(version.toByte),
      proposition)

  private def context(
      ergoTree: ErgoTree,
      activatedVersion: Byte = 3,
      extension: ContextExtension = ContextExtension.empty) = {
    val self = createBox(1000000L, ergoTree)
    ErgoLikeContextTesting.dummy(self, activatedVersion)
      .withExtension(extension)
  }

  private def assertLegacyVersionFailure(
      version: Int,
      body: => Any): StarkOpcodeErgoTreeVersionException = {
    val failure = the[StarkOpcodeErgoTreeVersionException] thrownBy body
    failure.actualVersion shouldBe version
    failure.requiredVersion shouldBe 4
    failure.getMessage shouldBe
      ("VerifyStark requires ErgoTree version 4 or higher; got " + version)
    failure
  }

  property("ordinary v0-v3 fullReduction outcomes and costs remain golden") {
    (0 to 3).foreach { version =>
      val ordinaryTree = tree(version, GT(Height, IntConstant(-1)).toSigmaProp)
      val result = verifier.fullReduction(ordinaryTree, context(ordinaryTree))
      result.value shouldBe TrivialProp.TrueProp
      result.cost shouldBe 6L

      val constantTree = tree(version, TrueSigmaProp)
      val constantResult = verifier.fullReduction(constantTree, context(constantTree))
      constantResult.value shouldBe TrivialProp.TrueProp
      constantResult.cost shouldBe 5L
    }
  }

  property("direct VerifyStark is rejected by fullReduction for every legacy tree version") {
    (0 to 3).foreach { version =>
      val directTree = tree(version, verifyStark.toSigmaProp)
      assertLegacyVersionFailure(version,
        verifier.fullReduction(directTree, context(directTree)))
    }
  }

  property("v4 passes the structural gate and reaches the fail-closed capability boundary") {
    val v4Tree = tree(VersionContext.StarkVerificationVersion, verifyStark.toSigmaProp)
    val failure = the[OpcodeUnavailableException] thrownBy
      verifier.fullReduction(
        v4Tree,
        context(v4Tree, activatedVersion = VersionContext.StarkVerificationVersion))
    failure.opCode shouldBe 0xb9
  }

  property("outer ErgoTree version validation retains precedence") {
    val directTree = tree(3, verifyStark.toSigmaProp)
    val result = verifier.verify(
      emptyEnv,
      directTree,
      context(directTree, activatedVersion = 2),
      proof,
      message)
    val failure = result.failed.get
    failure shouldBe a[InterpreterException]
    failure should not be a[StarkOpcodeErgoTreeVersionException]
    failure.getMessage shouldBe "ErgoTree version 3 is higher than activated 2"
  }

  property("structural scan covers dead branches, lambda bodies, and register defaults") {
    val deadBranch = If(TrueLeaf, TrueLeaf, verifyStark).toSigmaProp
    val emptyBooleans = ConcreteCollection[SBoolean.type](Vector.empty, SBoolean)
    val lambdaBody = Exists(
      emptyBooleans,
      FuncValue(Vector((1, SBoolean)), verifyStark)).toSigmaProp
    val registerDefault = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(verifyStark)).toSigmaProp

    Seq(deadBranch, lambdaBody, registerDefault).foreach { proposition =>
      val legacyTree = tree(3, proposition)
      assertLegacyVersionFailure(3,
        verifier.fullReduction(legacyTree, context(legacyTree)))
    }
  }

  property("successfully inserted context subtree is scanned without recursive substitution") {
    val insertedBytes = ValueSerializer.serialize(verifyStark)
    val outerTree = tree(3, DeserializeContext(1, SBoolean).toSigmaProp)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(insertedBytes)))

    assertLegacyVersionFailure(3,
      verifier.fullReduction(outerTree, context(outerTree, extension = extension)))
  }

  property("v4 context materialization reaches capability handling instead of the legacy gate") {
    val insertedBytes = ValueSerializer.serialize(verifyStark)
    val outerTree = tree(
      VersionContext.StarkVerificationVersion,
      DeserializeContext(1, SBoolean).toSigmaProp)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(insertedBytes)))

    val failure = the[OpcodeUnavailableException] thrownBy
      verifier.fullReduction(
        outerTree,
        context(
          outerTree,
          activatedVersion = VersionContext.StarkVerificationVersion,
          extension = extension))
    failure.opCode shouldBe 0xb9
  }

  property("nested deserialization in an inserted subtree retains historical non-recursive behavior") {
    val nestedBytes = ValueSerializer.serialize(DeserializeContext(2, SBoolean))
    val starkBytes = ValueSerializer.serialize(verifyStark)
    val outerTree = tree(3, DeserializeContext(1, SBoolean).toSigmaProp)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(nestedBytes),
      2.toByte -> ByteArrayConstant(starkBytes)))
    val failure = the[RuntimeException] thrownBy
      verifier.fullReduction(outerTree, context(outerTree, extension = extension))
    failure should not be a[StarkOpcodeErgoTreeVersionException]
    failure.getMessage shouldBe
      "Should be overriden in class sigma.ast.DeserializeContext: DeserializeContext(2,SBoolean)"
  }

  private val FutureBoolCode: OpCode =
    OpCode @@ (TypeCodes.LastConstantCode + 56).toByte
  private case object FutureBool extends NotReadyValueBoolean with ValueCompanion {
    override def companion = this
    override val opCode: OpCode = FutureBoolCode
    override val opType = SFunc(SContext, SBoolean)
    override val costKind = FixedCost(JitCost(1))
    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = false
  }
  private val FutureBoolSerializer = CaseObjectSerialization(FutureBool, FutureBool)

  private def futureBoolBytes(): Array[Byte] = {
    ValueSerializer.addSerializer(FutureBoolCode, FutureBoolSerializer)
    try ValueSerializer.serialize(FutureBool)
    finally ValueSerializer.removeSerializer(FutureBoolCode)
  }

  property("seen flag survives a later ordinary deserialization soft-fork terminal") {
    val starkBytes = ValueSerializer.serialize(verifyStark)
    val unsupportedBytes = futureBoolBytes()
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(starkBytes),
      2.toByte -> ByteArrayConstant(unsupportedBytes)))
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))
    val outerTree = tree(3, BinAnd(
      DeserializeContext(1, SBoolean),
      DeserializeContext(2, SBoolean)).toSigmaProp)
    val ctx = context(outerTree, extension = extension)
      .withValidationSettings(validationSettings)

    assertLegacyVersionFailure(3,
      verifier.fullReduction(outerTree, ctx))
  }

  property("ordinary deserialization soft-fork outcome and cost remain golden") {
    val unsupportedBytes = futureBoolBytes()
    val extension = ContextExtension(Map(
      2.toByte -> ByteArrayConstant(unsupportedBytes)))
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))
    val outerTree = tree(3, DeserializeContext(2, SBoolean).toSigmaProp)
    val result = verifier.fullReduction(
      outerTree,
      context(outerTree, extension = extension)
        .withValidationSettings(validationSettings))

    result.value shouldBe TrivialProp.TrueProp
    result.cost shouldBe 14L
  }

  property("top-level v4 unparsed soft fork is a terminal with the historical result") {
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))
    val validationError = ValidationException(
      "future top-level opcode",
      CheckValidOpCode,
      Seq(FutureBoolCode))
    val unparsedTree = new ErgoTree(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      ErgoTree.EmptyConstants,
      Left(UnparsedErgoTree(Array(FutureBoolCode.toByte), validationError)))
    val ctx = context(
      unparsedTree,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withValidationSettings(validationSettings)

    val historical = verifier.fullReduction(unparsedTree, ctx)
    historical.value shouldBe TrivialProp.TrueProp
    verifier.preflightFullReduction(unparsedTree, ctx) shouldBe Left(historical)
  }

  property("v0-v3 parse-before-cost and v4 charge-before-parse remain distinct") {
    val unsupportedBytes = futureBoolBytes()
    val extension = ContextExtension(Map(
      2.toByte -> ByteArrayConstant(unsupportedBytes)))
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))

    val legacyTree = tree(3, DeserializeContext(2, SBoolean).toSigmaProp)
    val legacyOuterCost = legacyTree.bytes.length.toLong * verifier.CostPerTreeByte
    val legacyExpected = verifier.fullReduction(
      legacyTree,
      context(legacyTree, extension = extension)
        .withValidationSettings(validationSettings))
    val legacyResult = verifier.fullReduction(
      legacyTree,
      context(legacyTree, extension = extension)
        .withValidationSettings(validationSettings)
        // The evaluator's JIT-to-block conversion rounds the reported cost
        // down, so one block-cost unit of headroom admits the same result.
        .withCostLimit(legacyExpected.cost + 1L))
    legacyResult.value shouldBe TrivialProp.TrueProp
    legacyResult.cost shouldBe legacyExpected.cost
    legacyResult.cost shouldBe legacyOuterCost

    val v4Tree = tree(4, DeserializeContext(2, SBoolean).toSigmaProp)
    val v4OuterCost = v4Tree.bytes.length.toLong * verifier.CostPerTreeByte
    val failure = the[CostLimitException] thrownBy verifier.fullReduction(
      v4Tree,
      context(
        v4Tree,
        activatedVersion = VersionContext.StarkVerificationVersion,
        extension = extension)
        .withValidationSettings(validationSettings)
        .withCostLimit(v4OuterCost))
    failure.estimatedCost shouldBe
      (v4OuterCost + unsupportedBytes.length.toLong * verifier.CostPerByteDeserialized)

    val Left(v4Terminal) = verifier.preflightFullReduction(
      v4Tree,
      context(
        v4Tree,
        activatedVersion = VersionContext.StarkVerificationVersion,
        extension = extension)
        .withValidationSettings(validationSettings))
    v4Terminal.value shouldBe TrivialProp.TrueProp
    v4Terminal.cost shouldBe
      (v4OuterCost + unsupportedBytes.length.toLong * verifier.CostPerByteDeserialized)
  }

  property("v4 cyclic materialization creates fresh charged occurrences until the normal cost limit") {
    val cycleNode = DeserializeContext(1, SBoolean)
    val cycleBytes = ValueSerializer.serialize(cycleNode)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(cycleBytes)))
    val v4Tree = tree(4, cycleNode.toSigmaProp)
    val outerCost = v4Tree.bytes.length.toLong * verifier.CostPerTreeByte
    val occurrenceCost = cycleBytes.length.toLong * verifier.CostPerByteDeserialized
    val admittedOccurrences = 64L
    val limit = outerCost + admittedOccurrences * occurrenceCost

    val failure = the[CostLimitException] thrownBy verifier.fullReduction(
      v4Tree,
      context(
        v4Tree,
        activatedVersion = VersionContext.StarkVerificationVersion,
        extension = extension)
        .withCostLimit(limit))
    failure.estimatedCost shouldBe outerCost + (admittedOccurrences + 1L) * occurrenceCost
  }

  property("deterministic deserialization parse, type, and cost failures retain precedence") {
    val unsupportedBytes = futureBoolBytes()
    val parseExtension = ContextExtension(Map(
      2.toByte -> ByteArrayConstant(unsupportedBytes)))
    val parseTree = tree(3, BinAnd(
      verifyStark,
      DeserializeContext(2, SBoolean)).toSigmaProp)

    a[ValidationException] shouldBe thrownBy(
      verifier.fullReduction(parseTree, context(parseTree, extension = parseExtension)))

    val wrongTypeBytes = ValueSerializer.serialize(IntConstant(1))
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(wrongTypeBytes)))
    val typedTree = tree(3, BinAnd(
      verifyStark,
      DeserializeContext(1, SBoolean)).toSigmaProp)

    a[ValidationException] shouldBe thrownBy(
      verifier.fullReduction(typedTree, context(typedTree, extension = extension)))

    a[CostLimitException] shouldBe thrownBy(
      verifier.fullReduction(typedTree, context(typedTree).withCostLimit(0L)))
  }
}
