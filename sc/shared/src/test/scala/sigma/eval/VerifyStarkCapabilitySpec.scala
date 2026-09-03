package sigma.eval

import org.ergoplatform.ErgoBox
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax.TrueSigmaProp
import sigma.data.TrivialProp
import sigma.exceptions.{CostLimitException, InterpreterException, OpcodeUnavailableException, StarkOpcodeErgoTreeVersionException, StarkProfileQuarantinedException}
import sigma.interpreter.ContextExtension
import sigma.serialization.ValueSerializer
import sigma.stark.profile.{ProfileBlake2b256, StarkPreVerifierObserver}
import sigma.validation.{ValidationException, ValidationRules}
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.Interpreter._
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}
import sigma.ast.syntax.SigmaPropValue

class VerifyStarkCapabilitySpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

  private val ChainDomainId = Array.tabulate[Byte](ProfileIdBytes)(_.toByte)
  private val interpreter = new ErgoLikeTestInterpreter

  private val ProfileIdEvaluated = "profile-id-evaluated"
  private val DispatchCharged = "dispatch-charged"
  private val ProfileIdValidated = "profile-id-validated"
  private val ProfileIdMaterialized = "profile-id-materialized"
  private val ByteCompared = "byte-compared"
  private val EntryCompared = "entry-compared"
  private val LookupCompleted = "lookup-completed"
  private val ActiveLifecycleSelected = "active-lifecycle-selected"
  private val FixedCharged = "fixed-charged"
  private val ProgramIdEvaluated = "program-id-evaluated"
  private val ProgramIdValidated = "program-id-validated"
  private val ApplicationPayloadEvaluated = "application-payload-evaluated"
  private val ApplicationPayloadValidated = "application-payload-validated"
  private val ProofChunksEvaluated = "proof-chunks-evaluated"
  private val ProofChunkCountValidated = "proof-chunk-count-validated"
  private val ProofChunkValidated = "proof-chunk-validated"

  private val EqualLookupEvents =
    Vector.fill(ProfileIdBytes)(ByteCompared) :+ EntryCompared
  private val ThroughActiveSelection =
    Vector(
      ProfileIdEvaluated,
      DispatchCharged,
      ProfileIdValidated,
      ProfileIdMaterialized) ++
      EqualLookupEvents ++
      Vector(LookupCompleted, ActiveLifecycleSelected)
  private val ThroughFixedCharge = ThroughActiveSelection :+ FixedCharged
  private val ThroughProgramValidation =
    ThroughFixedCharge ++ Vector(ProgramIdEvaluated, ProgramIdValidated)
  private val ThroughPayloadValidation =
    ThroughProgramValidation ++
      Vector(ApplicationPayloadEvaluated, ApplicationPayloadValidated)
  private val ThroughProofCountValidation =
    ThroughPayloadValidation ++
      Vector(ProofChunksEvaluated, ProofChunkCountValidated)

  private class RecordingRouteObserver extends VerifyStarkEvaluationObserver {
    private val recorded = scala.collection.mutable.ArrayBuffer.empty[String]
    def events: Vector[String] = recorded.toVector

    protected def observe(event: String): Unit = recorded += event

    override def onProfileIdEvaluated(): Unit = observe(ProfileIdEvaluated)
    override def onDispatchCharged(): Unit = observe(DispatchCharged)
    override def onProfileIdValidated(): Unit = observe(ProfileIdValidated)
    override def onProfileIdMaterialized(): Unit = observe(ProfileIdMaterialized)
    override def onByteComparison(): Unit = observe(ByteCompared)
    override def onEntryComparison(): Unit = observe(EntryCompared)
    override def onLookupCompleted(): Unit = observe(LookupCompleted)
    override def onActiveLifecycleSelected(): Unit = observe(ActiveLifecycleSelected)
    override def onFixedCharged(): Unit = observe(FixedCharged)
    override def onProgramIdEvaluated(): Unit = observe(ProgramIdEvaluated)
    override def onProgramIdValidated(): Unit = observe(ProgramIdValidated)
    override def onApplicationPayloadEvaluated(): Unit =
      observe(ApplicationPayloadEvaluated)
    override def onApplicationPayloadValidated(): Unit =
      observe(ApplicationPayloadValidated)
    override def onProofChunksEvaluated(): Unit = observe(ProofChunksEvaluated)
    override def onProofChunkCountValidated(): Unit =
      observe(ProofChunkCountValidated)
    override def onProofChunkValidated(): Unit = observe(ProofChunkValidated)
    override def onProofChunkMaterialized(): Unit = observe("proof-chunk-materialized")
    override def onProgramIdMaterialized(): Unit = observe("program-id-materialized")
    override def onApplicationPayloadMaterialized(): Unit =
      observe("application-payload-materialized")
    override def onSelfPropositionBytesMaterialized(): Unit =
      observe("self-proposition-bytes-materialized")
    override def onContractIdBuilt(): Unit = observe("contract-id-built")
    override def onStatementBuilt(): Unit = observe("statement-built")
    override def onJournalDigestBuilt(): Unit = observe("journal-digest-built")
    override def onTaggedStructDigestBuilt(): Unit =
      observe("tagged-struct-digest-built")
    override def onOkClaimBuilt(): Unit = observe("ok-claim-built")
    override def onRawVerifierEntered(): Unit = observe("raw-verifier-entered")
  }

  private final class RouteObserverSentinel extends RuntimeException

  private final class ThrowingRouteObserver(
      target: String,
      sentinel: RouteObserverSentinel) extends RecordingRouteObserver {
    override protected def observe(event: String): Unit =
      if (event == target) throw sentinel else super.observe(event)
  }

  private final class RecordingRuntime(
      sourceProfileId: Array[Byte],
      var result: Boolean = true,
      var thrown: Throwable = null) extends StarkProfileRuntime {
    var calls: Int = 0
    var observedChain: Array[Byte] = null
    var observedProgram: Array[Byte] = null
    var observedContract: Array[Byte] = null
    var observedPayload: Array[Byte] = null
    var observedChunks: Array[Array[Byte]] = null

    override private[sigma] def profileId: Array[Byte] = sourceProfileId
    override private[sigma] def exactProofBytes: Int = 3
    override private[sigma] def maxApplicationPayloadBytes: Int = 3
    override private[sigma] def canonicalProofChunkLengths: Array[Int] = Array(1, 2)
    override private[sigma] def verify(
        chainDomainId: Array[Byte],
        programId: Array[Byte],
        contractId: Array[Byte],
        applicationPayload: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = {
      calls += 1
      observedChain = chainDomainId.clone()
      observedProgram = programId.clone()
      observedContract = contractId.clone()
      observedPayload = applicationPayload.clone()
      observedChunks = proofChunks.map(_.clone())
      if (thrown != null) throw thrown
      result
    }
  }

  private def profileId(first: Int): Array[Byte] = {
    val result = new Array[Byte](ProfileIdBytes)
    result(0) = first.toByte
    result
  }

  private def right[A](value: Either[ConstructionFailure, A]): A = value match {
    case Right(result) => result
    case Left(failure) => fail("unexpected capability rejection: " + failure)
  }

  private def activeSnapshot(
      runtime: RecordingRuntime,
      dispatchJit: Int = 100,
      fixedJit: Int = 200): Snapshot = {
    val entry = right(active(runtime, fixedJit))
    right(snapshot(
      ChainDomainId,
      protocolGeneration = 7,
      HistoricalBlockValidation,
      dispatchJit,
      Vector(entry)))
  }

  private def evaluator(
      capability: StarkVerificationCapability,
      costLimit: JitCost = JitCost.fromBlockCost(1000000)): CErgoTreeEvaluator = {
    val settings = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(JitCost(0), Some(costLimit))
    val tree = ErgoTree.fromProposition(TrueSigmaProp)
    val box = createBox(1000000L, tree)
    val sigmaContext = ErgoLikeContextTesting.dummy(
      box,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withErgoTreeVersion(VersionContext.StarkVerificationVersion)
      .toSigmaContext()
    new CErgoTreeEvaluator(
      sigmaContext,
      ErgoTree.EmptyConstants,
      accumulator,
      CErgoTreeEvaluator.DefaultProfiler,
      settings,
      capability)
  }

  private def evalDirect(
      evaluator: CErgoTreeEvaluator,
      node: Value[_ <: SType]): Any =
    VersionContext.withVersions(
      VersionContext.StarkVerificationVersion,
      VersionContext.StarkVerificationVersion) {
      evaluator.eval(Map.empty, node)
    }

  private def evalObserved(
      evaluator: CErgoTreeEvaluator,
      node: VerifyStark,
      observer: StarkPreVerifierObserver): Any =
    VersionContext.withVersions(
      VersionContext.StarkVerificationVersion,
      VersionContext.StarkVerificationVersion) {
      node.evalObserved(Map.empty, observer)(evaluator)
    }

  private def chunks(values: Array[Byte]*): Value[SCollection[SCollection[SByte.type]]] =
    ConcreteCollection[SByteArray](
      values.map(ByteArrayConstant(_)).toIndexedSeq,
      SByteArray).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

  private val inaccessibleChunks =
    ConstantPlaceholder(0, SCollection(SByteArray))
      .asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
  private val inaccessibleBytes = ConstantPlaceholder(0, SByteArray)

  private def node(
      profile: Value[SByteArray],
      program: Value[SByteArray] = inaccessibleBytes,
      payload: Value[SByteArray] = inaccessibleBytes,
      proof: Value[SCollection[SCollection[SByte.type]]] = inaccessibleChunks): VerifyStark =
    VerifyStark(proof, payload, program, profile)

  private def executableNode(id: Array[Byte]): VerifyStark =
    node(
      ByteArrayConstant(id),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array[Byte](7, 8)),
      chunks(Array[Byte](4), Array[Byte](5, 6)))

  private def v4Tree(proposition: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      proposition)

  private def fullReduction(
      proposition: SigmaPropValue,
      capability: StarkVerificationCapability,
      extension: ContextExtension = ContextExtension.empty) = {
    val tree = v4Tree(proposition)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(extension)
      .withStarkVerificationCapability(capability)
    interpreter.fullReduction(tree, context)
  }

  private def successfulPreflight(
      tree: ErgoTree,
      context: org.ergoplatform.ErgoLikeContext): interpreter.StarkPreflightResult =
    interpreter.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }

  private def staticProfileBytes(
      occurrence: StarkPreflightOccurrence): Array[Byte] = occurrence.profileId match {
    case static: StaticStarkProfileId => static.bytes
    case other => fail("expected static profile id, got " + other)
  }

  private final class CountingInterpreter extends ErgoLikeTestInterpreter {
    var v4DeserializeCalls: Int = 0

    override protected def deserializeMeasuredV4(
        context: CTX,
        updateContext: CTX => Unit,
        scriptBytes: Array[Byte]): Value[SType] = {
      v4DeserializeCalls += 1
      super.deserializeMeasuredV4(context, updateContext, scriptBytes)
    }
  }

  test("unavailable capability fails before every child and native charge") {
    val e = evaluator(Unavailable)
    val before = e.getAccumulatedCost
    val failure = the[OpcodeUnavailableException] thrownBy
      evalDirect(e, node(inaccessibleBytes))
    failure.opCode shouldBe 0xb9
    e.getAccumulatedCost shouldBe before
  }

  test("whole-input unavailable preflight rejects VerifyStark in a dead v4 branch") {
    val deadCall = node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(2)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array.empty[Byte]))
    val proposition = If(TrueLeaf, TrueLeaf, deadCall).toSigmaProp
    val tree = v4Tree(proposition)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withStarkVerificationCapability(Unavailable)
    val Right(preflight) = interpreter.preflightFullReduction(tree, context)
    preflight.plan.occurrences should have size 1

    val failure = the[OpcodeUnavailableException] thrownBy
      interpreter.continueFullReduction(preflight)
    failure.opCode shouldBe 0xb9
  }

  test("public preflight enforces the ordinary outer language-version gate") {
    val id = profileId(3)
    val runtime = new RecordingRuntime(id)
    val tree = v4Tree(executableNode(id).toSigmaProp)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = (VersionContext.StarkVerificationVersion - 1).toByte)
      .withStarkVerificationCapability(activeSnapshot(runtime))

    val failure = the[InterpreterException] thrownBy
      interpreter.preflightFullReduction(tree, context)
    failure.getMessage should include ("higher than activated")
    runtime.calls shouldBe 0
  }

  test("public preflight returns an ordinary terminal for a future-version stale node") {
    val futureVersion = (VersionContext.MaxSupportedScriptVersion + 1).toByte
    val tree = ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(futureVersion),
      TrueSigmaProp)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = futureVersion)

    interpreter.preflightFullReduction(tree, context) shouldBe
      Left(ReductionResult(TrivialProp.TrueProp, context.initCost))
  }

  test("legacy structural gate scans a deep no-deserialize tree iteratively") {
    var body: Value[SBoolean.type] = executableNode(profileId(1))
    var depth = 0
    while (depth < 25000) {
      body = BinAnd(TrueLeaf, body)
      depth += 1
    }
    val proposition = body.toSigmaProp
    // The parser normally supplies this cached flag. Supplying it explicitly
    // isolates the structural opcode scan from the separate hasDeserialize scan.
    val tree = ErgoTree(
      ErgoTree.defaultHeaderWithVersion(3.toByte),
      ErgoTree.EmptyConstants,
      Right(proposition),
      Array.empty[Byte],
      Some(false),
      Some(false))
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = 3)

    val failure = the[StarkOpcodeErgoTreeVersionException] thrownBy
      interpreter.fullReduction(tree, context)
    failure.actualVersion shouldBe 3
    failure.requiredVersion shouldBe VersionContext.StarkVerificationVersion
  }

  test("v4 preflight recursively materializes nested context bytes before availability") {
    val nestedBytes = ValueSerializer.serialize(DeserializeContext(2, SBoolean))
    val serializableCall = node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(2)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array.empty[Byte]))
    val starkBytes = ValueSerializer.serialize(serializableCall)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(nestedBytes),
      2.toByte -> ByteArrayConstant(starkBytes)))

    val failure = the[OpcodeUnavailableException] thrownBy fullReduction(
      DeserializeContext(1, SBoolean).toSigmaProp,
      Unavailable,
      extension)
    failure.opCode shouldBe 0xb9
  }

  test("v4 materialization permits serializer-owned transitions to compact Boolean encodings") {
    val selectedBytes = ValueSerializer.serialize(TrueLeaf)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(selectedBytes)))
    val dynamicBoolean = DeserializeContext(1, SBoolean)
    val booleanCollection = ConcreteCollection[SBoolean.type](
      List(dynamicBoolean, TrueLeaf), SBoolean)

    fullReduction(
      AND(booleanCollection).toSigmaProp,
      Unavailable,
      extension).value shouldBe TrivialProp.TrueProp
    fullReduction(
      EQ(dynamicBoolean, TrueLeaf).toSigmaProp,
      Unavailable,
      extension).value shouldBe TrivialProp.TrueProp
  }

  test("active v4 closure verifies a nested call while an active dead branch remains lazy") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)
    val call = node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array[Byte](7, 8)),
      chunks(Array[Byte](4), Array[Byte](5, 6)))
    val nestedBytes = ValueSerializer.serialize(DeserializeContext(2, SBoolean))
    val callBytes = ValueSerializer.serialize(call)
    val extension = ContextExtension(Map(
      1.toByte -> ByteArrayConstant(nestedBytes),
      2.toByte -> ByteArrayConstant(callBytes)))

    fullReduction(
      DeserializeContext(1, SBoolean).toSigmaProp,
      capability,
      extension).value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1

    fullReduction(
      If(TrueLeaf, TrueLeaf, call).toSigmaProp,
      capability).value shouldBe TrivialProp.TrueProp
    runtime.calls shouldBe 1
  }

  test("preflight plan uses materialized DFS order across dead and inserted branches") {
    val firstId = profileId(11)
    val insertedId = profileId(12)
    val lastId = profileId(13)
    val insertedBytes = ValueSerializer.serialize(executableNode(insertedId))
    val proposition = BinAnd(
      executableNode(firstId),
      If(
        FalseLeaf,
        DeserializeContext(1, SBoolean),
        executableNode(lastId))).toSigmaProp
    val tree = v4Tree(proposition)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        1.toByte -> ByteArrayConstant(insertedBytes))))
      .withStarkVerificationCapability(Unavailable)

    val preflight = successfulPreflight(tree, context)
    preflight.plan.occurrences.map(o => staticProfileBytes(o).toVector) shouldBe
      Vector(firstId.toVector, insertedId.toVector, lastId.toVector)

    val exposed = staticProfileBytes(preflight.plan.occurrences.head)
    exposed(0) = 99.toByte
    staticProfileBytes(preflight.plan.occurrences.head) should contain theSameElementsInOrderAs firstId
  }

  test("unordered type-substitution metadata cannot affect canonical occurrence order") {
    val firstId = profileId(14)
    val secondId = profileId(15)
    val keyA = STypeVar("PlanOrderA")
    val keyB = STypeVar("PlanOrderB")

    def planFor(entries: Seq[(STypeVar, SType)]): Vector[Vector[Byte]] = {
      val typeSubst = entries.foldLeft(
        scala.collection.immutable.HashMap.empty[STypeVar, SType])(_ + _)
      val generator = MethodCall(
        Global,
        SGlobalMethods.groupGeneratorMethod,
        IndexedSeq.empty,
        typeSubst)
      val proposition = If(
        EQ(generator, generator),
        executableNode(firstId),
        executableNode(secondId)).toSigmaProp
      // Keep the deliberately extra type metadata intact instead of passing
      // through serialization, which omits substitutions not owned by method.
      val tree = ErgoTree(
        ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
        ErgoTree.EmptyConstants,
        proposition)
      val context = ErgoLikeContextTesting.dummy(
        createBox(1000000L, tree),
        activatedVersion = VersionContext.StarkVerificationVersion)
      successfulPreflight(tree, context).plan.occurrences
        .map(o => staticProfileBytes(o).toVector).toVector
    }

    planFor(Vector(keyA -> SInt, keyB -> SLong)) shouldBe
      planFor(Vector(keyB -> SLong, keyA -> SInt))
    planFor(Vector(keyA -> SInt, keyB -> SLong)) shouldBe
      Vector(firstId.toVector, secondId.toVector)
  }

  test("preflight classifies resolved placeholders, malformed constants, and dynamic IDs without eval") {
    val exactId = profileId(21)
    val expectedExactId = exactId.clone()
    val exactTree = v4Tree(executableNode(exactId).toSigmaProp)
    val templateCall = exactTree.toProposition(replaceConstants = false) match {
      case BoolToSigmaProp(call: VerifyStark) => call
      case other => fail("unexpected segregated proposition: " + other)
    }
    templateCall.profileId shouldBe a[ConstantPlaceholder[_]]
    val exactContext = ErgoLikeContextTesting.dummy(
      createBox(1000000L, exactTree),
      activatedVersion = VersionContext.StarkVerificationVersion)
    val exactPreflight = successfulPreflight(exactTree, exactContext)
    staticProfileBytes(exactPreflight.plan.occurrences.head) should
      contain theSameElementsInOrderAs exactId
    exactId(0) = 99.toByte
    val firstRead = staticProfileBytes(exactPreflight.plan.occurrences.head)
    firstRead(1) = 98.toByte
    staticProfileBytes(exactPreflight.plan.occurrences.head) should
      contain theSameElementsInOrderAs expectedExactId

    val malformed = executableNode(Array.fill[Byte](VerifyStark.DigestBytes - 1)(1))
    val dynamic = node(
      CalcBlake2b256(ByteArrayConstant(Array[Byte](1))),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array.empty[Byte]))
    val shapesTree = v4Tree(BinAnd(malformed, dynamic).toSigmaProp)
    val shapesContext = ErgoLikeContextTesting.dummy(
      createBox(1000000L, shapesTree),
      activatedVersion = VersionContext.StarkVerificationVersion)
    successfulPreflight(shapesTree, shapesContext).plan.occurrences.map(_.profileId) shouldBe
      Vector(MalformedStarkProfileId, DynamicStarkProfileId)

    val wrongTypeProfile = IntConstant(7).asInstanceOf[Value[SByteArray]]
    val wrongTypeCall = node(
      wrongTypeProfile,
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array.empty[Byte]))
    val wrongTypeTree = ErgoTree(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      ErgoTree.EmptyConstants,
      wrongTypeCall.toSigmaProp)
    val wrongTypeContext = ErgoLikeContextTesting.dummy(
      createBox(1000000L, wrongTypeTree),
      activatedVersion = VersionContext.StarkVerificationVersion)
    successfulPreflight(wrongTypeTree, wrongTypeContext)
      .plan.occurrences.head.profileId shouldBe MalformedStarkProfileId

    val materializedId = profileId(22)
    val profileBytes = ValueSerializer.serialize(ByteArrayConstant(materializedId))
    val materializedProfileCall = node(
      DeserializeContext(8, SByteArray),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array.empty[Byte]))
    val materializedProfileTree = v4Tree(materializedProfileCall.toSigmaProp)
    val materializedProfileContext = ErgoLikeContextTesting.dummy(
      createBox(1000000L, materializedProfileTree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        8.toByte -> ByteArrayConstant(profileBytes))))
    val materializedProfilePlan = successfulPreflight(
      materializedProfileTree,
      materializedProfileContext).plan
    staticProfileBytes(materializedProfilePlan.occurrences.head) should
      contain theSameElementsInOrderAs materializedId
  }

  test("v4 register and context materialization share the complete recursive closure") {
    val selectedId = profileId(31)
    val nestedContextBytes = ValueSerializer.serialize(DeserializeContext(2, SBoolean))
    val callBytes = ValueSerializer.serialize(executableNode(selectedId))
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      None).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(nestedContextBytes)))
    val context = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        2.toByte -> ByteArrayConstant(callBytes))))
      .withStarkVerificationCapability(Unavailable)
    val counting = new CountingInterpreter

    val preflight = counting.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }
    counting.v4DeserializeCalls shouldBe 2
    preflight.preflightBlockCost shouldBe
      (context.initCost +
       tree.bytes.length.toLong * counting.CostPerTreeByte +
       nestedContextBytes.length.toLong * counting.CostPerByteDeserialized +
       callBytes.length.toLong * counting.CostPerByteDeserialized)
    staticProfileBytes(preflight.plan.occurrences.head) should
      contain theSameElementsInOrderAs selectedId
    a[OpcodeUnavailableException] shouldBe thrownBy(
      counting.continueFullReduction(preflight))
    counting.v4DeserializeCalls shouldBe 2
  }

  test("v4 preflight covers lambda bodies and selected register defaults without evaluating them") {
    val selectedId = profileId(35)
    val runtime = new RecordingRuntime(selectedId)
    val capability = activeSnapshot(runtime)
    val call = executableNode(selectedId)
    val emptyBooleans = ConcreteCollection[SBoolean.type](Vector.empty, SBoolean)
    val propositions = Vector(
      Exists(
        emptyBooleans,
        FuncValue(Vector((1, SBoolean)), call)).toSigmaProp,
      DeserializeRegister(
        ErgoBox.R4,
        SBoolean,
        Some(call)).toSigmaProp)

    propositions.foreach { proposition =>
      val tree = v4Tree(proposition)
      val context = ErgoLikeContextTesting.dummy(
        createBox(1000000L, tree),
        activatedVersion = VersionContext.StarkVerificationVersion)
        .withStarkVerificationCapability(capability)
      val preflight = successfulPreflight(tree, context)
      preflight.plan.occurrences should have size 1
      staticProfileBytes(preflight.plan.occurrences.head) should
        contain theSameElementsInOrderAs selectedId
      runtime.calls shouldBe 0
    }
  }

  test("v4 preflight retains a register default when present bytes select another subtree") {
    val defaultId = profileId(36)
    val selectedId = profileId(37)
    val selectedBytes = ValueSerializer.serialize(executableNode(selectedId))
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(executableNode(defaultId))).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(selectedBytes)))
    val context = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withStarkVerificationCapability(Unavailable)

    val preflight = successfulPreflight(tree, context)
    preflight.plan.occurrences.map(o => staticProfileBytes(o).toVector) shouldBe
      Vector(defaultId.toVector, selectedId.toVector)
    a[OpcodeUnavailableException] shouldBe thrownBy(
      interpreter.continueFullReduction(preflight))
  }

  test("v4 preflight recursively materializes a shadowed register default") {
    val defaultId = profileId(38)
    val selectedBytes = ValueSerializer.serialize(TrueLeaf)
    val nestedDefaultBytes = ValueSerializer.serialize(executableNode(defaultId))
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(DeserializeContext(2, SBoolean))).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(selectedBytes)))
    val context = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        2.toByte -> ByteArrayConstant(nestedDefaultBytes))))
      .withStarkVerificationCapability(Unavailable)
    val counting = new CountingInterpreter

    val preflight = counting.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }
    counting.v4DeserializeCalls shouldBe 2
    preflight.plan.occurrences.map(o => staticProfileBytes(o).toVector) shouldBe
      Vector(defaultId.toVector)
    a[OpcodeUnavailableException] shouldBe thrownBy(
      counting.continueFullReduction(preflight))
    counting.v4DeserializeCalls shouldBe 2
  }

  test("v4 wrong-typed register selects its default without attempting byte parsing") {
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(TrueLeaf)).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> IntConstant(1)))
    val context = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withStarkVerificationCapability(Unavailable)
    val counting = new CountingInterpreter

    val preflight = counting.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }
    counting.v4DeserializeCalls shouldBe 0
    counting.continueFullReduction(preflight).value shouldBe TrivialProp.TrueProp
    counting.v4DeserializeCalls shouldBe 0
  }

  test("selected register parsing fails before a malformed shadowed default") {
    val wrongSelectedBytes = ValueSerializer.serialize(ByteConstant(1))
    val wrongDefaultBytes = ValueSerializer.serialize(ByteConstant(2))
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(DeserializeContext(2, SBoolean))).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(wrongSelectedBytes)))
    val context = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        2.toByte -> ByteArrayConstant(wrongDefaultBytes))))
    val counting = new CountingInterpreter

    val failure = the[RuntimeException] thrownBy
      counting.preflightFullReduction(tree, context)
    failure.getMessage should include ("Failed deserialization")
    counting.v4DeserializeCalls shouldBe 1
  }

  test("selected register cost is committed before a shadowed default exceeds the limit") {
    val selectedBytes = ValueSerializer.serialize(TrueLeaf)
    val defaultBytes = ValueSerializer.serialize(TrueLeaf)
    val proposition = DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(DeserializeContext(2, SBoolean))).toSigmaProp
    val tree = v4Tree(proposition)
    val self = createBox(
      1000000L,
      tree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(selectedBytes)))
    val baseContext = ErgoLikeContextTesting.dummy(
      self,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        2.toByte -> ByteArrayConstant(defaultBytes))))
    val counting = new CountingInterpreter
    val attemptedCost =
      baseContext.initCost +
      tree.bytes.length.toLong * counting.CostPerTreeByte +
      selectedBytes.length.toLong * counting.CostPerByteDeserialized +
      defaultBytes.length.toLong * counting.CostPerByteDeserialized
    val context = baseContext.withCostLimit(attemptedCost - 1L)

    val failure = the[CostLimitException] thrownBy
      counting.preflightFullReduction(tree, context)
    failure.estimatedCost shouldBe attemptedCost
    counting.v4DeserializeCalls shouldBe 2
  }

  test("Value-prefix suffixes are opaque but fully charged for context and register sources") {
    val hiddenCallBytes = ValueSerializer.serialize(executableNode(profileId(39)))
    val sourceBytes = ValueSerializer.serialize(TrueLeaf) ++ hiddenCallBytes

    val contextTree = v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)
    val contextSource = ErgoLikeContextTesting.dummy(
      createBox(1000000L, contextTree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        1.toByte -> ByteArrayConstant(sourceBytes))))
      .withStarkVerificationCapability(Unavailable)
    val contextInterpreter = new CountingInterpreter
    val contextPreflight = contextInterpreter.preflightFullReduction(
      contextTree,
      contextSource) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected context terminal: " + terminal)
    }
    contextPreflight.plan.occurrences shouldBe empty
    contextPreflight.preflightBlockCost shouldBe
      (contextSource.initCost +
       contextTree.bytes.length.toLong * contextInterpreter.CostPerTreeByte +
       sourceBytes.length.toLong * contextInterpreter.CostPerByteDeserialized)
    contextInterpreter.continueFullReduction(contextPreflight).value shouldBe
      TrivialProp.TrueProp

    val registerTree = v4Tree(DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      None).toSigmaProp)
    val registerSelf = createBox(
      1000000L,
      registerTree,
      additionalRegisters = Map(
        ErgoBox.R4 -> ByteArrayConstant(sourceBytes)))
    val registerSource = ErgoLikeContextTesting.dummy(
      registerSelf,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withStarkVerificationCapability(Unavailable)
    val registerInterpreter = new CountingInterpreter
    val registerPreflight = registerInterpreter.preflightFullReduction(
      registerTree,
      registerSource) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected register terminal: " + terminal)
    }
    registerPreflight.plan.occurrences shouldBe empty
    registerPreflight.preflightBlockCost shouldBe
      (registerSource.initCost +
       registerTree.bytes.length.toLong * registerInterpreter.CostPerTreeByte +
       sourceBytes.length.toLong * registerInterpreter.CostPerByteDeserialized)
    registerInterpreter.continueFullReduction(registerPreflight).value shouldBe
      TrivialProp.TrueProp
  }

  test("a VerifyStark Value prefix is visible while its byte suffix remains opaque") {
    val selectedId = profileId(40)
    val sourceBytes =
      ValueSerializer.serialize(executableNode(selectedId)) ++
      Array[Byte](0x01, 0x02, 0x03)
    val tree = v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        1.toByte -> ByteArrayConstant(sourceBytes))))
      .withStarkVerificationCapability(Unavailable)
    val counting = new CountingInterpreter

    val preflight = counting.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }
    preflight.plan.occurrences.map(o => staticProfileBytes(o).toVector) shouldBe
      Vector(selectedId.toVector)
    preflight.preflightBlockCost shouldBe
      (context.initCost +
       tree.bytes.length.toLong * counting.CostPerTreeByte +
       sourceBytes.length.toLong * counting.CostPerByteDeserialized)
    a[OpcodeUnavailableException] shouldBe thrownBy(
      counting.continueFullReduction(preflight))
  }

  test("a wrong-typed Value prefix never recovers from a valid suffix") {
    val sourceBytes =
      ValueSerializer.serialize(ByteConstant(1)) ++
      ValueSerializer.serialize(TrueLeaf)
    val tree = v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        1.toByte -> ByteArrayConstant(sourceBytes))))
    val counting = new CountingInterpreter

    a[ValidationException] shouldBe thrownBy(
      counting.preflightFullReduction(tree, context))
    counting.v4DeserializeCalls shouldBe 1
  }

  test("opaque preflight continuation is single-use and does not reparse or double-verify") {
    val selectedId = profileId(41)
    val runtime = new RecordingRuntime(selectedId)
    val capability = activeSnapshot(runtime)
    val callBytes = ValueSerializer.serialize(executableNode(selectedId))
    val tree = v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)
    val context = ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(ContextExtension(Map(
        1.toByte -> ByteArrayConstant(callBytes))))
      .withStarkVerificationCapability(capability)
    val counting = new CountingInterpreter

    val preflight = counting.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected soft-fork terminal: " + terminal)
    }
    val expectedPreflightCost =
      context.initCost +
      tree.bytes.length.toLong * counting.CostPerTreeByte +
      callBytes.length.toLong * counting.CostPerByteDeserialized
    preflight.preflightBlockCost shouldBe expectedPreflightCost

    val repeatedInterpreter = new CountingInterpreter
    val repeatedPreflight = repeatedInterpreter.preflightFullReduction(tree, context) match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected repeated soft-fork terminal: " + terminal)
    }
    repeatedPreflight.preflightBlockCost shouldBe preflight.preflightBlockCost
    repeatedInterpreter.v4DeserializeCalls shouldBe 1

    counting.v4DeserializeCalls shouldBe 1
    runtime.calls shouldBe 0

    counting.continueFullReduction(preflight).value shouldBe TrivialProp.TrueProp
    counting.v4DeserializeCalls shouldBe 1
    runtime.calls shouldBe 1

    val secondUse = the[IllegalStateException] thrownBy
      counting.continueFullReduction(preflight)
    secondUse.getMessage shouldBe "STARK preflight result has already been consumed"
    counting.v4DeserializeCalls shouldBe 1
    runtime.calls shouldBe 1
  }

  test("integrated observer freezes lifecycle and charge prefixes") {
    val unavailableObserver = new RecordingRouteObserver
    an[OpcodeUnavailableException] shouldBe thrownBy(
      evalObserved(
        evaluator(Unavailable),
        node(inaccessibleBytes),
        unavailableObserver))
    unavailableObserver.events shouldBe empty

    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)

    val dispatchObserver = new RecordingRouteObserver
    a[CostLimitException] shouldBe thrownBy(
      evalObserved(
        evaluator(capability, costLimit = JitCost(50)),
        node(ByteArrayConstant(profileId(1))),
        dispatchObserver))
    dispatchObserver.events shouldBe Vector(ProfileIdEvaluated)

    val malformedObserver = new RecordingRouteObserver
    evalObserved(
      evaluator(capability),
      node(ByteArrayConstant(Array[Byte](1))),
      malformedObserver) shouldBe false
    malformedObserver.events shouldBe Vector(ProfileIdEvaluated, DispatchCharged)

    val absentObserver = new RecordingRouteObserver
    evalObserved(
      evaluator(capability),
      node(ByteArrayConstant(profileId(2))),
      absentObserver) shouldBe false
    absentObserver.events shouldBe Vector(
      ProfileIdEvaluated,
      DispatchCharged,
      ProfileIdValidated,
      ProfileIdMaterialized,
      ByteCompared,
      EntryCompared,
      LookupCompleted)

    val quarantinedEntry = right(quarantined(profileId(3)))
    val quarantinedCapability = right(snapshot(
      ChainDomainId,
      protocolGeneration = 8,
      AdmissionValidation,
      dispatchJit = 100,
      Vector(quarantinedEntry)))
    val quarantinedObserver = new RecordingRouteObserver
    a[StarkProfileQuarantinedException] shouldBe thrownBy(
      evalObserved(
        evaluator(quarantinedCapability),
        node(ByteArrayConstant(profileId(3))),
        quarantinedObserver))
    quarantinedObserver.events shouldBe
      (Vector(
        ProfileIdEvaluated,
        DispatchCharged,
        ProfileIdValidated,
        ProfileIdMaterialized) ++
        EqualLookupEvents :+ LookupCompleted)

    val fixedObserver = new RecordingRouteObserver
    a[CostLimitException] shouldBe thrownBy(
      evalObserved(
        evaluator(capability, costLimit = JitCost(150)),
        node(ByteArrayConstant(profileId(1))),
        fixedObserver))
    fixedObserver.events shouldBe ThroughActiveSelection

    val childObserver = new RecordingRouteObserver
    an[IndexOutOfBoundsException] shouldBe thrownBy(
      evalObserved(
        evaluator(capability),
        node(ByteArrayConstant(profileId(1))),
        childObserver))
    childObserver.events shouldBe ThroughFixedCharge
    runtime.calls shouldBe 0
  }

  test("integrated observer freezes every active child and proof shape guard") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)
    val exactProfile = ByteArrayConstant(profileId(1))
    val exactProgram = ByteArrayConstant(profileId(9))
    val exactPayload = ByteArrayConstant(Array[Byte](7, 8))

    val cases = Vector(
      "short-program" -> (
        node(exactProfile, ByteArrayConstant(Array[Byte](1))) ->
          (ThroughFixedCharge :+ ProgramIdEvaluated)),
      "oversized-payload" -> (
        node(
          exactProfile,
          exactProgram,
          ByteArrayConstant(Array[Byte](1, 2, 3, 4))) ->
          (ThroughProgramValidation :+ ApplicationPayloadEvaluated)),
      "wrong-chunk-count" -> (
        node(
          exactProfile,
          exactProgram,
          exactPayload,
          chunks(Array[Byte](4))) ->
          (ThroughPayloadValidation :+ ProofChunksEvaluated)),
      "wrong-first-chunk" -> (
        node(
          exactProfile,
          exactProgram,
          exactPayload,
          chunks(Array.empty[Byte], Array[Byte](5, 6))) ->
          ThroughProofCountValidation),
      "wrong-second-chunk" -> (
        node(
          exactProfile,
          exactProgram,
          exactPayload,
          chunks(Array[Byte](4), Array[Byte](5))) ->
          (ThroughProofCountValidation :+ ProofChunkValidated)))

    cases.foreach { case (label, (value, expectedEvents)) =>
      withClue(label + ": ") {
        val observer = new RecordingRouteObserver
        evalObserved(evaluator(capability), value, observer) shouldBe false
        observer.events shouldBe expectedEvents
      }
    }
    runtime.calls shouldBe 0
  }

  test("integrated observer exceptions preserve object identity") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)
    val targets = Vector(
      ProfileIdEvaluated,
      DispatchCharged,
      ProfileIdValidated,
      ProfileIdMaterialized,
      ByteCompared,
      EntryCompared,
      LookupCompleted,
      ActiveLifecycleSelected,
      FixedCharged,
      ProgramIdEvaluated,
      ProgramIdValidated,
      ApplicationPayloadEvaluated,
      ApplicationPayloadValidated,
      ProofChunksEvaluated,
      ProofChunkCountValidated,
      ProofChunkValidated)

    targets.foreach { target =>
      withClue(target + ": ") {
        val sentinel = new RouteObserverSentinel
        val observed = intercept[RouteObserverSentinel] {
          evalObserved(
            evaluator(capability),
            executableNode(profileId(1)),
            new ThrowingRouteObserver(target, sentinel))
        }
        observed should be theSameInstanceAs sentinel
      }
    }
  }

  test("malformed and absent profile IDs pay dispatch without touching heavy children") {
    val activeRuntime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(activeRuntime)

    val malformedEvaluator = evaluator(capability)
    evalDirect(malformedEvaluator, node(ByteArrayConstant(Array[Byte](1)))) shouldBe false
    malformedEvaluator.getAccumulatedCost.value shouldBe 105

    val absentEvaluator = evaluator(capability)
    evalDirect(absentEvaluator, node(ByteArrayConstant(profileId(2)))) shouldBe false
    absentEvaluator.getAccumulatedCost.value shouldBe 105
    activeRuntime.calls shouldBe 0
  }

  test("quarantine is non-Boolean, charged, and cannot be bypassed by negation or soft fork") {
    val quarantinedEntry = right(quarantined(profileId(3)))
    val capability = right(snapshot(
      ChainDomainId,
      8,
      AdmissionValidation,
      dispatchJit = 100,
      Vector(quarantinedEntry)))
    val call = node(ByteArrayConstant(profileId(3)))

    val direct = evaluator(capability)
    val failure = the[StarkProfileQuarantinedException] thrownBy evalDirect(direct, call)
    failure.profileIdHex should startWith("03")
    direct.getAccumulatedCost.value shouldBe 105

    the[StarkProfileQuarantinedException] thrownBy
      evalDirect(evaluator(capability), LogicalNot(call))

    implicit val validationSettings = ValidationRules.coreSettings
    the[StarkProfileQuarantinedException] thrownBy
      ValidationRules.trySoftForkable(false) {
        evalDirect(evaluator(capability), call)
        true
      }
  }

  test("active profile pays full fixed charge before evaluating programId") {
    val runtime = new RecordingRuntime(profileId(1))
    val e = evaluator(activeSnapshot(runtime))
    an[IndexOutOfBoundsException] shouldBe thrownBy(
      evalDirect(e, node(ByteArrayConstant(profileId(1)))))
    e.getAccumulatedCost.value shouldBe 305
    runtime.calls shouldBe 0
  }

  test("active child gates preserve order and never invoke the runtime early") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)

    val badProgram = evaluator(capability)
    evalDirect(badProgram, node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(Array[Byte](1)))) shouldBe false
    badProgram.getAccumulatedCost.value shouldBe 310

    val oversizedPayload = evaluator(capability)
    evalDirect(oversizedPayload, node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array[Byte](1, 2, 3, 4)))) shouldBe false

    val wrongPartition = evaluator(capability)
    evalDirect(wrongPartition, node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array[Byte](1)),
      chunks(Array[Byte](1, 2, 3)))) shouldBe false
    runtime.calls shouldBe 0
  }

  test("valid active invocation binds SELF, chain, program, payload, and chunks") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)
    val e = evaluator(capability)
    val call = node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array[Byte](7, 8)),
      chunks(Array[Byte](4), Array[Byte](5, 6)))

    evalDirect(e, call) shouldBe true
    runtime.calls shouldBe 1
    runtime.observedChain shouldBe ChainDomainId
    runtime.observedProgram shouldBe profileId(9)
    runtime.observedPayload shouldBe Array[Byte](7, 8)
    runtime.observedChunks shouldBe Array(Array[Byte](4), Array[Byte](5, 6))
    runtime.observedContract shouldBe
      ProfileBlake2b256.hash(e.context.SELF.propositionBytes.toArray)

    runtime.result = false
    evalDirect(evaluator(capability), call) shouldBe false
  }

  test("runtime and cost-limit failures propagate instead of becoming false") {
    val runtime = new RecordingRuntime(profileId(1), thrown = new IllegalStateException("test"))
    val capability = activeSnapshot(runtime)
    val call = node(
      ByteArrayConstant(profileId(1)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(Array.empty[Byte]),
      chunks(Array[Byte](4), Array[Byte](5, 6)))
    an[IllegalStateException] shouldBe thrownBy(
      evalDirect(evaluator(capability), call))

    val limited = evaluator(capability, costLimit = JitCost(150))
    a[CostLimitException] shouldBe thrownBy(
      evalDirect(limited, node(ByteArrayConstant(profileId(1)))))
    runtime.calls shouldBe 1
  }

  test("ErgoLikeContext preserves and forwards the purpose-specific capability") {
    val runtime = new RecordingRuntime(profileId(1))
    val capability = activeSnapshot(runtime)
    val tree = ErgoTree.fromProposition(TrueSigmaProp)
    val box = createBox(1000000L, tree)
    val context = ErgoLikeContextTesting.dummy(
      box,
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withErgoTreeVersion(VersionContext.StarkVerificationVersion)
      .withStarkVerificationCapability(capability)
    context.withInitCost(0).starkVerificationCapability shouldBe capability
    context.withValidationSettings(context.validationSettings)
      .starkVerificationCapability shouldBe capability
    an[IllegalArgumentException] shouldBe thrownBy(
      context.withStarkVerificationCapability(null))
  }
}
