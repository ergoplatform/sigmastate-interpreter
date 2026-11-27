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
import sigma.exceptions.{InterpreterException, OpcodeUnavailableException, StarkOpcodeErgoTreeVersionException}
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

  private def v4Bytes(v: Value[SType]): Array[Byte] =
    VersionContext.withVersions(4, 4) {
      ValueSerializer.serialize(v)
    }

  private def tree(version: Int, proposition: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(version.toByte),
      proposition)

  private def unsegregatedTree(version: Int, proposition: SigmaPropValue): ErgoTree =
    ErgoTree.withoutSegregation(
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

  private def syntheticContext(
      version: Int,
      activatedVersion: Byte) =
    context(tree(version, TrueSigmaProp), activatedVersion)

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

  property("direct VerifyStark is rejected by fromProposition and withoutSegregation for every legacy tree version") {
    (0 to 3).foreach { version =>
      val ve1 = the[ValidationException] thrownBy tree(version, verifyStark.toSigmaProp)
      ve1.rule shouldBe CheckValidOpCode
      ve1.args shouldBe Seq(OpCodes.VerifyStarkCode)

      val ve2 = the[ValidationException] thrownBy unsegregatedTree(version, verifyStark.toSigmaProp)
      ve2.rule shouldBe CheckValidOpCode
      ve2.args shouldBe Seq(OpCodes.VerifyStarkCode)
    }
  }

  property("v0-v3 UnparsedErgoTree with size bit carrying 0xB9 is rejected at spend time even when rule 1002 enables 0xB9") {
    val propBytes = v4Bytes(verifyStark.toSigmaProp)
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(OpCodes.VerifyStarkCode)))

    (0 to 3).foreach { version =>
      val header = (ErgoTree.SizeFlag | version.toByte).toByte
      val w = SigmaSerializer.startWriter()
      w.put(header)
      w.putUInt(propBytes.length.toLong)
      w.putBytes(propBytes)
      val encoded = w.toBytes

      val decoded = VersionContext.withVersions(4, version.toByte) {
        ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(encoded)
      }
      decoded.isRightParsed shouldBe false
      val ctx = context(decoded, activatedVersion = 4)
        .withValidationSettings(validationSettings)

      assertLegacyVersionFailure(version,
        verifier.fullReduction(decoded, ctx))
    }
  }

  property("v0-v3 DeserializeContext and DeserializeRegister containing 0xB9 are rejected even when rule 1002 enables 0xB9") {
    val starkBytes = v4Bytes(verifyStark)
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(OpCodes.VerifyStarkCode)))

    (0 to 3).foreach { version =>
      val outerTree = tree(version, DeserializeContext(1, SBoolean).toSigmaProp)
      val extension = ContextExtension(Map(1.toByte -> ByteArrayConstant(starkBytes)))
      val ctx = context(outerTree, activatedVersion = 4, extension = extension)
        .withValidationSettings(validationSettings)

      assertLegacyVersionFailure(version, verifier.fullReduction(outerTree, ctx))

      val regTree = tree(version, DeserializeRegister(ErgoBox.R4, SBoolean, None).toSigmaProp)
      val regBox = createBox(1000000L, regTree, additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(starkBytes)))
      val regCtx = ErgoLikeContextTesting.dummy(regBox, activatedVersion = 4)
        .withValidationSettings(validationSettings)

      assertLegacyVersionFailure(version, verifier.fullReduction(regTree, regCtx))
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

  property("outer ErgoTree version validation retains precedence over v4 VerifyStark trees") {
    val v4Tree = tree(VersionContext.StarkVerificationVersion, verifyStark.toSigmaProp)
    val result = verifier.verify(
      emptyEnv,
      v4Tree,
      syntheticContext(3, activatedVersion = 3),
      proof,
      message)
    val failure = result.failed.get
    failure shouldBe a[InterpreterException]
    failure should not be a[StarkOpcodeErgoTreeVersionException]
    failure.getMessage shouldBe "ErgoTree version 4 is higher than activated 3"
  }

  property("serializer rejects dead branches, lambda bodies, and register defaults containing VerifyStark on v0-v3") {
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
      val ve = the[ValidationException] thrownBy unsegregatedTree(3, proposition)
      ve.rule shouldBe CheckValidOpCode
      ve.args shouldBe Seq(OpCodes.VerifyStarkCode)
    }
  }

  property("v4 retains single-pass deserialization (no unbounded nested depth blowup)") {
    val nestedBytes = v4Bytes(DeserializeContext(2, SBoolean))
    val starkBytes = v4Bytes(verifyStark)
    val outerTree = tree(
      VersionContext.StarkVerificationVersion,
      DeserializeContext(1, SBoolean).toSigmaProp)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(nestedBytes),
      2.toByte -> ByteArrayConstant(starkBytes)))

    // Single-pass substitution stops after outer DeserializeContext(1);
    // evaluating the unexpanded nested DeserializeContext(2) fails cleanly
    // without recursive expansion or stack overflow.
    val failure = the[RuntimeException] thrownBy
      verifier.fullReduction(
        outerTree,
        context(outerTree, activatedVersion = 4, extension = extension))
    failure should not be a[StackOverflowError]
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

  property("wire-deserialized top-level v4 unparsed soft fork is a terminal with the historical result") {
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))
    val futurePropBytes = {
      ValueSerializer.addSerializer(FutureBoolCode, FutureBoolSerializer)
      try VersionContext.withVersions(4, 4) { ValueSerializer.serialize(FutureBool.toSigmaProp) }
      finally ValueSerializer.removeSerializer(FutureBoolCode)
    }
    val header = (ErgoTree.SizeFlag | VersionContext.StarkVerificationVersion).toByte
    val w = SigmaSerializer.startWriter()
    w.put(header)
    w.putUInt(futurePropBytes.length.toLong)
    w.putBytes(futurePropBytes)
    val wireBytes = w.toBytes

    // Real ErgoTreeSerializer produces DefaultHeader (v0) wrapping UnparsedErgoTree(wireBytes, ve).
    val unparsedTree = VersionContext.withVersions(4, 4) {
      ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(wireBytes)
    }
    unparsedTree.isRightParsed shouldBe false
    unparsedTree.version shouldBe 0

    val ctx = context(
      unparsedTree,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withValidationSettings(validationSettings)

    val historical = verifier.fullReduction(unparsedTree, ctx)
    historical.value shouldBe TrivialProp.TrueProp
    verifier.preflightFullReduction(unparsedTree, ctx) shouldBe Left(historical)
  }
}
