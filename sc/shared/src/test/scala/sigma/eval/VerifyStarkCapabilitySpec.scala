package sigma.eval

import org.ergoplatform.ErgoBox
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax.SigmaPropValue
import sigma.data.TrivialProp
import sigma.exceptions.{
  CostLimitException,
  OpcodeUnavailableException,
  StarkProfileQuarantinedException,
  StarkUnsupportedFutureGenerationException
}
import sigma.interpreter.ContextExtension
import sigma.serialization.ValueSerializer
import sigma.stark.ErgoStarkStatement
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.interpreter.Interpreter._

class VerifyStarkCapabilitySpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

  private val ChainDomainId = Array.tabulate[Byte](DigestBytes)(_.toByte)
  private val TransitionId = Array.tabulate[Byte](DigestBytes)(i => (0xa0 + i).toByte)
  private val interpreter = new ErgoLikeTestInterpreter

  private final class RecordingRuntime(
      sourceProfileId: Array[Byte],
      minBytes: Int = 3,
      maxBytes: Int = 3,
      maxPayload: Int = 3,
      var result: Boolean = true,
      var thrown: Throwable = null) extends StarkProfileRuntime {
    var calls: Int = 0
    var observedStatement: Array[Byte] = null
    var observedProgram: Array[Byte] = null
    var observedChunks: Array[Array[Byte]] = null

    override private[sigma] def profileId: Array[Byte] = sourceProfileId
    override private[sigma] def minProofBytes: Int = minBytes
    override private[sigma] def maxProofBytes: Int = maxBytes
    override private[sigma] def maxApplicationPayloadBytes: Int = maxPayload
    override private[sigma] def verify(
        statement: Array[Byte],
        programId: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = {
      calls += 1
      observedStatement = statement.clone()
      observedProgram = programId.clone()
      observedChunks = proofChunks.map(_.clone())
      if (thrown != null) throw thrown
      result
    }
  }

  private def digest(first: Int): Array[Byte] = {
    val out = new Array[Byte](DigestBytes)
    out(0) = first.toByte
    out
  }

  private def right[A](value: Either[ConstructionFailure, A]): A = value match {
    case Right(v) => v
    case Left(f)  => fail("unexpected capability failure: " + f)
  }

  private def activeSnapshot(
      runtime: RecordingRuntime,
      dispatchJit: Int = 100,
      fixedJit: Int = 200,
      purpose: ValidationPurpose = HistoricalBlockValidation,
      starkGen: Int = 1,
      maxSupportedGen: Int = 1): Snapshot = {
    val entry = right(active(runtime, fixedJit))
    right(snapshot(
      ChainDomainId,
      starkGeneration = starkGen,
      maxSupportedStarkGeneration = maxSupportedGen,
      transitionId = TransitionId,
      purpose,
      dispatchJit,
      Vector(entry)))
  }

  private val inaccessibleChunks =
    OptionGet(GetVar(99.toByte, SCollection(SByteArray)))
  private val inaccessibleBytes =
    OptionGet(GetVar(98.toByte, SByteArray))

  private def chunks(values: Array[Byte]*): Value[SCollection[SCollection[SByte.type]]] =
    ConcreteCollection[SByteArray](
      values.map(ByteArrayConstant(_)).toIndexedSeq,
      SByteArray).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

  private def callNode(
      profile: Value[SByteArray],
      program: Value[SByteArray] = inaccessibleBytes,
      payload: Value[SByteArray] = inaccessibleBytes,
      proof: Value[SCollection[SCollection[SByte.type]]] = inaccessibleChunks): VerifyStark =
    VerifyStark(proof, payload, program, profile)

  private def executableNode(id: Array[Byte]): VerifyStark =
    callNode(
      ByteArrayConstant(id),
      ByteArrayConstant(digest(9)),
      ByteArrayConstant(Array[Byte](7, 8)),
      chunks(Array[Byte](4, 5, 6)))

  private def v4Tree(proposition: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      proposition)

  private def runV4(
      proposition: SigmaPropValue,
      capability: StarkVerificationCapability,
      extension: ContextExtension = ContextExtension.empty,
      registers: Map[ErgoBox.NonMandatoryRegisterId, EvaluatedValue[_ <: SType]] = Map.empty,
      costLimit: Long = 1000000L): ReductionResult = {
    val tree = v4Tree(proposition)
    val box = createBox(1000000L, tree, additionalRegisters = registers)
    val ctx = ErgoLikeContextTesting.dummy(
      box,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(extension)
      .withCostLimit(costLimit)
      .withStarkVerificationCapability(capability)
    interpreter.fullReduction(tree, ctx)
  }

  test("ordinary v4 script without VerifyStark runs even with Unavailable capability (wallet compatibility)") {
    val res = runV4(GT(Height, IntConstant(-1)).toSigmaProp, Unavailable)
    res.value shouldBe TrivialProp.TrueProp
  }

  test("Unavailable capability rejects v4 script containing VerifyStark even in a dead branch") {
    val dead = If(TrueLeaf, TrueLeaf, executableNode(digest(1))).toSigmaProp
    val ex = the[OpcodeUnavailableException] thrownBy runV4(dead, Unavailable)
    ex.opCode shouldBe 0xb9
  }

  test("active profile constructs exact ErgoStatementV1 and invokes runtime after full precharge") {
    val pid = digest(1)
    val progId = digest(9)
    val payload = Array[Byte](7, 8)
    val runtime = new RecordingRuntime(pid, minBytes = 3, maxBytes = 3, maxPayload = 16, result = true)
    val snap = activeSnapshot(runtime, dispatchJit = 100, fixedJit = 200)
    val tree = v4Tree(executableNode(pid).toSigmaProp)
    val box = createBox(1000000L, tree)
    val ctx = ErgoLikeContextTesting.dummy(box, activatedVersion = 4)
      .withStarkVerificationCapability(snap)

    val res = interpreter.fullReduction(tree, ctx)
    res.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1
    runtime.observedProgram shouldBe progId
    runtime.observedChunks.map(_.toVector) shouldBe Array(Vector[Byte](4, 5, 6))

    val expectedContractId = Blake2b256.hash(tree.bytes)
    val Right(expectedStmt) = ErgoStarkStatement.build(ChainDomainId, pid, progId, expectedContractId, payload)
    runtime.observedStatement shouldBe expectedStmt
  }

  test("Absent profile returns false after dispatchJit without evaluating heavy children") {
    val runtime = new RecordingRuntime(digest(1))
    val snap = activeSnapshot(runtime, dispatchJit = 100, fixedJit = 50000)
    // Heavy children read absent GetVar(98/99); evaluating them would throw.
    val absentCall = callNode(profile = ByteArrayConstant(digest(2)))
    val res = runV4(absentCall.toSigmaProp, snap)
    res.value shouldBe TrivialProp.FalseProp
    runtime.calls shouldBe 0
  }

  test("Quarantined profile fails deterministically even under negation without evaluating heavy children") {
    val qid = digest(2)
    val qEntry = right(quarantined(qid))
    val snap = right(snapshot(ChainDomainId, 1, TransitionId, HistoricalBlockValidation, 100, Vector(qEntry)))

    val negated = LogicalNot(callNode(profile = ByteArrayConstant(qid))).toSigmaProp
    val ex = the[StarkProfileQuarantinedException] thrownBy runV4(negated, snap)
    ex.profileIdHex should startWith("02")
  }

  test("Active profile charges full fixedJit BEFORE evaluating remaining children") {
    val pid = digest(1)
    val runtime = new RecordingRuntime(pid)
    val snap = activeSnapshot(runtime, dispatchJit = 100, fixedJit = 100000)
    // Cost limit allows dispatchJit (10 block units) but not fixedJit (10,000 block units).
    // Heavy children read absent GetVar(98/99): CostLimitException must trigger BEFORE them.
    val call = callNode(profile = ByteArrayConstant(pid))
    an[CostLimitException] should be thrownBy runV4(call.toSigmaProp, snap, costLimit = 500L)
    runtime.calls shouldBe 0
  }

  test("stale node HistoricalBlockValidation falls back on unknown or dynamic profileId without running user logic") {
    val knownId = digest(1)
    val unknownId = digest(2)
    val runtime = new RecordingRuntime(knownId)
    // G = 2 > Gmax = 1 (stale binary holding Tmax = {knownId})
    val staleHistorical = activeSnapshot(
      runtime,
      purpose = HistoricalBlockValidation,
      starkGen = 2,
      maxSupportedGen = 1)

    // Unknown static profileId => whole-input soft-fork fallback (TrueProp) before user logic.
    val unknownCall = LogicalNot(callNode(profile = ByteArrayConstant(unknownId))).toSigmaProp
    val fallbackRes = runV4(unknownCall, staleHistorical)
    fallbackRes.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 0

    // Dynamic profileId => whole-input soft-fork fallback (TrueProp).
    val dynCall = callNode(profile = OptionGet(GetVar(1.toByte, SByteArray))).toSigmaProp
    val dynRes = runV4(dynCall, staleHistorical)
    dynRes.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 0

    // Known static profileId in Tmax => executes normally under Tmax!
    runtime.result = false
    val knownRes = runV4(executableNode(knownId).toSigmaProp, staleHistorical)
    knownRes.value shouldBe TrivialProp.FalseProp
    runtime.calls shouldBe 1
  }

  test("stale node rejects future generation on AdmissionValidation, CandidateConstruction, and ProverReduction") {
    val knownId = digest(1)
    val runtime = new RecordingRuntime(knownId)
    Seq(AdmissionValidation, CandidateConstruction, ProverReduction).foreach { purpose =>
      val staleSnap = activeSnapshot(
        runtime,
        purpose = purpose,
        starkGen = 3,
        maxSupportedGen = 2)
      val ex = the[StarkUnsupportedFutureGenerationException] thrownBy
        runV4(executableNode(knownId).toSigmaProp, staleSnap)
      ex.starkGeneration shouldBe 3
      ex.maxSupportedGeneration shouldBe 2
      runtime.calls shouldBe 0
    }
  }

  test("ProverReduction purpose reduces v4 STARK scripts cleanly at supported generation") {
    val pid = digest(1)
    val runtime = new RecordingRuntime(pid, result = true)
    val proverSnap = activeSnapshot(runtime, purpose = ProverReduction, starkGen = 1, maxSupportedGen = 1)
    val res = runV4(executableNode(pid).toSigmaProp, proverSnap)
    res.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1
  }

  test("v4 wrong-typed DeserializeRegister keeps consistent rejection semantics (never silently selects default)") {
    val prop = DeserializeRegister(ErgoBox.R4, SBoolean, Some(TrueLeaf)).toSigmaProp
    val v3Tree = ErgoTree.fromProposition(ErgoTree.defaultHeaderWithVersion(3.toByte), prop)
    val v3Box = createBox(1000000L, v3Tree, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(42)))
    val v3Failure = intercept[RuntimeException] {
      interpreter.fullReduction(v3Tree, ErgoLikeContextTesting.dummy(v3Box, activatedVersion = 3))
    }

    val tree = v4Tree(prop)
    val box = createBox(1000000L, tree, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(42)))
    val v4Failure = intercept[RuntimeException] {
      interpreter.fullReduction(tree, ErgoLikeContextTesting.dummy(box, activatedVersion = 4))
    }

    // Matches v0-v3 ErgoLikeInterpreter behavior identically: wrong-typed register never selects default.
    v4Failure.getClass shouldBe v3Failure.getClass
    v4Failure.getMessage shouldBe v3Failure.getMessage
  }

  test("preflightFullReduction exposes plan and enforces single-use continuation") {
    val pid = digest(1)
    val runtime = new RecordingRuntime(pid, result = true)
    val snap = activeSnapshot(runtime)
    val tree = v4Tree(executableNode(pid).toSigmaProp)
    val ctx = ErgoLikeContextTesting.dummy(createBox(1000000L, tree), activatedVersion = 4)
      .withStarkVerificationCapability(snap)

    val Right(preflight) = interpreter.preflightFullReduction(tree, ctx)
    preflight.plan.occurrences.length shouldBe 1
    preflight.plan.occurrences.head.profileId match {
      case id: StaticStarkProfileId => id.bytes shouldBe pid
      case other                    => fail(s"expected StaticStarkProfileId, got $other")
    }

    val res = interpreter.continueFullReduction(preflight)
    res.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1

    an[IllegalStateException] should be thrownBy interpreter.continueFullReduction(preflight)
  }

  test("v4 DeserializeContext scans substituted VerifyStark and enforces capability") {
    val pid = digest(1)
    val insertedBytes = VersionContext.withVersions(4, 4) {
      ValueSerializer.serialize(executableNode(pid))
    }
    val prop = DeserializeContext(1, SBoolean).toSigmaProp
    val ext = ContextExtension(Map(1.toByte -> ByteArrayConstant(insertedBytes)))

    // Unavailable fails closed.
    an[OpcodeUnavailableException] should be thrownBy runV4(prop, Unavailable, extension = ext)

    // Active snapshot verifies cleanly.
    val runtime = new RecordingRuntime(pid, result = true)
    val res = runV4(prop, activeSnapshot(runtime), extension = ext)
    res.value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1
  }

  test("Interpreter defense-in-depth rejects synthetic in-memory v0-v3 AST containing VerifyStark in dead branch, lambda, or register default") {
    val call = executableNode(digest(1))
    val deadBranch = If(TrueLeaf, TrueLeaf, call).toSigmaProp
    val emptyBooleans = ConcreteCollection[SBoolean.type](Vector.empty, SBoolean)
    val lambdaBody = Exists(emptyBooleans, FuncValue(Vector((1, SBoolean)), call)).toSigmaProp
    val registerDefault = DeserializeRegister(ErgoBox.R4, SBoolean, Some(call)).toSigmaProp
    val dummyBox = createBox(1000000L, ErgoTree.fromProposition(ErgoTree.defaultHeaderWithVersion(3.toByte), TrueLeaf.toSigmaProp))

    Seq(call.toSigmaProp, deadBranch, lambdaBody, registerDefault).foreach { prop =>
      val header = ErgoTree.setRequiredBits(ErgoTree.defaultHeaderWithVersion(3.toByte))
      val syntheticLegacy = ErgoTree(
        header,
        ErgoTree.EmptyConstants,
        Right(prop),
        Array[Byte](header, 0x01, 0x7f),
        Some(false),
        Some(false))
      val failure = the[sigma.exceptions.StarkOpcodeErgoTreeVersionException] thrownBy
        interpreter.fullReduction(syntheticLegacy, ErgoLikeContextTesting.dummy(dummyBox, activatedVersion = 3))
      failure.actualVersion shouldBe 3
      failure.requiredVersion shouldBe 4
    }
  }

  test("v4 DeserializeRegister preflight scans both non-shadowed and shadowed defaults exactly once") {
    val pid = digest(1)
    val defaultCall = executableNode(pid)
    val prop = DeserializeRegister(ErgoBox.R4, SBoolean, Some(defaultCall)).toSigmaProp
    val tree = v4Tree(prop)

    // Case 1: R4 absent -> non-shadowed default is returned and must appear ONCE in plan.occurrences.
    val boxWithoutR4 = createBox(1000000L, tree)
    val runtime1 = new RecordingRuntime(pid, result = true)
    val ctxWithoutR4 = ErgoLikeContextTesting.dummy(boxWithoutR4, activatedVersion = 4)
      .withStarkVerificationCapability(activeSnapshot(runtime1))
    val Right(preflight1) = interpreter.preflightFullReduction(tree, ctxWithoutR4)
    preflight1.plan.occurrences.length shouldBe 1
    interpreter.continueFullReduction(preflight1).value shouldBe TrivialProp.TrueProp
    runtime1.calls shouldBe 1

    // Case 2: R4 present with TrueLeaf -> defaultCall is shadowed, yet STILL scanned once in plan.occurrences
    // so Unavailable / future-generation policy cannot be bypassed via shadowed defaults.
    val trueBytes = VersionContext.withVersions(4, 4) { ValueSerializer.serialize(TrueLeaf) }
    val boxWithR4 = createBox(1000000L, tree, additionalRegisters = Map(ErgoBox.R4 -> ByteArrayConstant(trueBytes)))
    val ctxWithR4 = ErgoLikeContextTesting.dummy(boxWithR4, activatedVersion = 4)
      .withStarkVerificationCapability(Unavailable)
    val Right(preflight2) = interpreter.preflightFullReduction(tree, ctxWithR4)
    preflight2.plan.occurrences.length shouldBe 1
    an[OpcodeUnavailableException] should be thrownBy interpreter.continueFullReduction(preflight2)
  }

  test("runtime exception inside StarkProfileRuntime.verify fails closed") {
    val pid = digest(1)
    val runtime = new RecordingRuntime(
      pid,
      thrown = new sigma.exceptions.StarkProfileRuntimeException("simulated verifier failure"))
    val snap = activeSnapshot(runtime)
    val ex = the[sigma.exceptions.StarkProfileRuntimeException] thrownBy
      runV4(executableNode(pid).toSigmaProp, snap)
    ex.getMessage shouldBe "simulated verifier failure"
    runtime.calls shouldBe 1
  }
}
