package sigma.eval {
  final class Eip0045ContinuationCensusRuntime(
      sourceProfileId: Array[Byte]) extends StarkProfileRuntime {
    var calls: Int = 0

    override private[sigma] def profileId: Array[Byte] = sourceProfileId
    override private[sigma] def exactProofBytes: Int = 1
    override private[sigma] def maxApplicationPayloadBytes: Int = 2
    override private[sigma] def canonicalProofChunkLengths: Array[Int] = Array(1)

    override private[sigma] def verify(
        chainDomainId: Array[Byte],
        programId: Array[Byte],
        contractId: Array[Byte],
        applicationPayload: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = {
      calls += 1
      true
    }
  }
}

package sigma.stark {
  import sigma.ast.{CostItem, JitCost, VerifyStarkEvaluationObserver}
  import sigma.ast.syntax.SValue
  import sigma.eval.Profiler

  object Eip0045FullRouteOperationProfiler {
    def canonicalChunkLengths: Array[Int] =
      profile.RawSealV1Decoder.canonicalChunkLengths

    def operationObserverClass: Class[_] = classOf[VerifierOperationObserver]

    def canonicalOperationIds: Vector[Int] =
      Vector(
        VerifierOperationObserver.MerkleTopPairHash,
        VerifierOperationObserver.MerkleQueryPairHash,
        VerifierOperationObserver.ContentHashCall,
        VerifierOperationObserver.ContentHashPermutation,
        VerifierOperationObserver.RngCommit,
        VerifierOperationObserver.RngElementDraw,
        VerifierOperationObserver.RngPermutation)

    def apply(
        recordRoute: String => Unit,
        recordOperation: Int => Unit): Profiler =
      new FullRouteOperationProfiler(recordRoute, recordOperation)

    private final class FullRouteOperationProfiler(
        recordRoute: String => Unit,
        recordOperation: Int => Unit)
        extends Profiler
        with VerifyStarkEvaluationObserver
        with VerifierOperationObserver {
      override def onBeforeNode(node: SValue): Unit = ()
      override def onAfterNode(node: SValue): Unit = ()
      override def addCostItem(costItem: CostItem, time: Long): Unit = ()
      override def addJitEstimation(
          script: String,
          cost: JitCost,
          actualTimeNano: Long): Unit = ()

      override def onProfileIdEvaluated(): Unit = recordRoute("profile-id-evaluated")
      override def onDispatchCharged(): Unit = recordRoute("dispatch-charged")
      override def onProfileIdValidated(): Unit = recordRoute("profile-id-validated")
      override def onProfileIdMaterialized(): Unit = recordRoute("profile-id-materialized")
      override def onByteComparison(): Unit = recordRoute("byte-compared")
      override def onEntryComparison(): Unit = recordRoute("entry-compared")
      override def onLookupCompleted(): Unit = recordRoute("lookup-completed")
      override def onActiveLifecycleSelected(): Unit =
        recordRoute("active-lifecycle-selected")
      override def onFixedCharged(): Unit = recordRoute("fixed-charged")
      override def onProgramIdEvaluated(): Unit = recordRoute("program-id-evaluated")
      override def onProgramIdValidated(): Unit = recordRoute("program-id-validated")
      override def onApplicationPayloadEvaluated(): Unit =
        recordRoute("application-payload-evaluated")
      override def onApplicationPayloadValidated(): Unit =
        recordRoute("application-payload-validated")
      override def onProofChunksEvaluated(): Unit = recordRoute("proof-chunks-evaluated")
      override def onProofChunkCountValidated(): Unit =
        recordRoute("proof-chunk-count-validated")
      override def onProofChunkValidated(): Unit = recordRoute("proof-chunk-validated")

      override def onProofChunkMaterialized(): Unit =
        recordRoute("proof-chunk-materialized")
      override def onProgramIdMaterialized(): Unit =
        recordRoute("program-id-materialized")
      override def onApplicationPayloadMaterialized(): Unit =
        recordRoute("application-payload-materialized")
      override def onSelfPropositionBytesMaterialized(): Unit =
        recordRoute("self-proposition-bytes-materialized")
      override def onContractIdBuilt(): Unit = recordRoute("contract-id-built")
      override def onStatementBuilt(): Unit = recordRoute("statement-built")
      override def onJournalDigestBuilt(): Unit = recordRoute("journal-digest-built")
      override def onTaggedStructDigestBuilt(): Unit =
        recordRoute("tagged-struct-digest-built")
      override def onOkClaimBuilt(): Unit = recordRoute("ok-claim-built")
      override def onRawVerifierEntered(): Unit = recordRoute("raw-verifier-entered")

      override def onOperation(operationId: Int): Unit =
        recordOperation(operationId)
    }
  }
}

package sigma.ast {
  import sigma.eval.Profiler

  object Eip0045EvaluatorRouteProfiler {
    def apply(record: String => Unit): Profiler = new RouteProfiler(record)

    private final class RouteProfiler(record: String => Unit)
        extends Profiler with VerifyStarkEvaluationObserver {
      override def onBeforeNode(node: sigma.ast.syntax.SValue): Unit = ()
      override def onAfterNode(node: sigma.ast.syntax.SValue): Unit = ()
      override def addCostItem(costItem: CostItem, time: Long): Unit = ()
      override def addJitEstimation(
          script: String,
          cost: JitCost,
          actualTimeNano: Long): Unit = ()

      override def onProfileIdEvaluated(): Unit = record("profile-id-evaluated")
      override def onDispatchCharged(): Unit = record("dispatch-charged")
      override def onProfileIdValidated(): Unit = record("profile-id-validated")
      override def onProfileIdMaterialized(): Unit = record("profile-id-materialized")
      override def onByteComparison(): Unit = record("byte-compared")
      override def onEntryComparison(): Unit = record("entry-compared")
      override def onLookupCompleted(): Unit = record("lookup-completed")
      override def onActiveLifecycleSelected(): Unit =
        record("active-lifecycle-selected")
      override def onFixedCharged(): Unit = record("fixed-charged")
      override def onProgramIdEvaluated(): Unit = record("program-id-evaluated")
      override def onProgramIdValidated(): Unit = record("program-id-validated")
      override def onApplicationPayloadEvaluated(): Unit =
        record("application-payload-evaluated")
      override def onApplicationPayloadValidated(): Unit =
        record("application-payload-validated")
      override def onProofChunksEvaluated(): Unit = record("proof-chunks-evaluated")
      override def onProofChunkCountValidated(): Unit =
        record("proof-chunk-count-validated")
      override def onProofChunkValidated(): Unit = record("proof-chunk-validated")

      override def onProofChunkMaterialized(): Unit =
        record("proof-chunk-materialized")
      override def onProgramIdMaterialized(): Unit =
        record("program-id-materialized")
      override def onApplicationPayloadMaterialized(): Unit =
        record("application-payload-materialized")
      override def onSelfPropositionBytesMaterialized(): Unit =
        record("self-proposition-bytes-materialized")
      override def onContractIdBuilt(): Unit = record("contract-id-built")
      override def onStatementBuilt(): Unit = record("statement-built")
      override def onJournalDigestBuilt(): Unit = record("journal-digest-built")
      override def onTaggedStructDigestBuilt(): Unit =
        record("tagged-struct-digest-built")
      override def onOkClaimBuilt(): Unit = record("ok-claim-built")
      override def onRawVerifierEntered(): Unit = record("raw-verifier-entered")
    }
  }
}

package sigmastate.interpreter {
  import java.io.ByteArrayOutputStream
  import java.lang.reflect.{InvocationTargetException, Method, Modifier}
  import java.nio.charset.StandardCharsets
  import org.ergoplatform.ErgoLikeContext
  import org.scalatest.funsuite.AnyFunSuite
  import org.scalatest.matchers.should.Matchers
  import sigma.VersionContext
  import sigma.ast.SCollection.SByteArray
  import sigma.ast._
  import sigma.ast.syntax.{SigmaPropValue, TrueSigmaProp}
  import sigma.data.TrivialProp
  import sigma.eval.{Eip0045ContinuationCensusRuntime, EvalSettings, Profiler, Risc0StockProfileRuntime, StarkProfileRuntime}
  import sigma.eval.StarkVerificationCapability
  import sigma.eval.StarkVerificationCapability._
  import sigma.exceptions.{CostLimitException, OpcodeUnavailableException}
  import sigma.interpreter.ContextExtension
  import sigma.serialization.ValueSerializer
  import sigma.serialization.Eip0045ReferenceContract
  import sigma.stark.Eip0045FullRouteOperationProfiler
  import sigma.stark.profile.{Risc0ProfilePackageLoader, Risc0RawSealVerifier}
  import sigma.validation.{ReplacedRule, ValidationException}
  import sigma.validation.ValidationRules.CheckPositionLimit
  import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
  import sigmastate.helpers.TestingHelpers.createBox
  import sigmastate.interpreter.Interpreter.{
    StarkPreflightContinuationObserver,
    StarkPreflightOperationObserver
  }

  class Eip0045PreflightContinuationCensusSpec
      extends AnyFunSuite with Matchers {
    private val ContinuationTaken = "continuation-taken"
    private val DirectPathSelected = "direct-path-selected"
    private val MaterializedPathSelected = "materialized-path-selected"
    private val AvailabilityChecked = "availability-checked"
    private val AvailabilityPassed = "availability-passed"
    private val ConstantReductionEntered = "constant-reduction-entered"
    private val DirectEvaluatorEntered = "direct-evaluator-entered"
    private val JitReductionEntered = "jit-reduction-entered"
    private val StructuralPlanBuilt = "structural-plan-built"
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
    private val ProofChunkMaterialized = "proof-chunk-materialized"
    private val ProgramIdMaterialized = "program-id-materialized"
    private val ApplicationPayloadMaterialized = "application-payload-materialized"
    private val SelfPropositionBytesMaterialized = "self-proposition-bytes-materialized"
    private val ContractIdBuilt = "contract-id-built"
    private val StatementBuilt = "statement-built"
    private val JournalDigestBuilt = "journal-digest-built"
    private val TaggedStructDigestBuilt = "tagged-struct-digest-built"
    private val OkClaimBuilt = "ok-claim-built"
    private val RawVerifierEntered = "raw-verifier-entered"
    private val FirstVerifierOperation = "first-verifier-operation"

    private val CanonicalActiveRouteEvents =
      Vector(
        ProfileIdEvaluated,
        DispatchCharged,
        ProfileIdValidated,
        ProfileIdMaterialized) ++
        Vector.fill(ProfileIdBytes)(ByteCompared) ++
        Vector(
          EntryCompared,
          LookupCompleted,
          ActiveLifecycleSelected,
          FixedCharged,
          ProgramIdEvaluated,
          ProgramIdValidated,
          ApplicationPayloadEvaluated,
          ApplicationPayloadValidated,
          ProofChunksEvaluated,
          ProofChunkCountValidated) ++
        Vector.fill(Eip0045FullRouteOperationProfiler.canonicalChunkLengths.length)(
          ProofChunkValidated) ++
        Vector(
          ProofChunkMaterialized,
          ProofChunkMaterialized,
          ProofChunkMaterialized,
          ProofChunkMaterialized,
          ProgramIdMaterialized,
          ApplicationPayloadMaterialized,
          SelfPropositionBytesMaterialized,
          ContractIdBuilt,
          StatementBuilt,
          JournalDigestBuilt,
          TaggedStructDigestBuilt,
          TaggedStructDigestBuilt,
          TaggedStructDigestBuilt,
          OkClaimBuilt,
          RawVerifierEntered)

    private val CanonicalDirectActivePrefix =
      Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        DirectEvaluatorEntered) ++ CanonicalActiveRouteEvents

    private val CanonicalMaterializedActivePrefix =
      Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        JitReductionEntered) ++ CanonicalActiveRouteEvents

    private val FullOperationCounts = Vector(217, 4050, 353, 1384, 12, 244, 32)
    private val PackageRoot = "/stark-kats/eip0045-profile-package/"
    private val DirectRoot = "/stark-kats/eip0045-direct/"
    private val ReferencePayload =
      "v18-full-route-operation-census".getBytes(StandardCharsets.UTF_8)

    private class RecordingObserver extends StarkPreflightContinuationObserver {
      private val recorded = scala.collection.mutable.ArrayBuffer.empty[String]
      def events: Vector[String] = recorded.toVector

      protected def record(event: String): Unit = recorded += event

      final def recordRouteEvent(event: String): Unit = record(event)

      override def onContinuationTaken(): Unit = record(ContinuationTaken)
      override def onDirectPathSelected(): Unit = record(DirectPathSelected)
      override def onMaterializedPathSelected(): Unit =
        record(MaterializedPathSelected)
      override def onAvailabilityChecked(): Unit = record(AvailabilityChecked)
      override def onAvailabilityPassed(): Unit = record(AvailabilityPassed)
      override def onConstantReductionEntered(): Unit =
        record(ConstantReductionEntered)
      override def onDirectEvaluatorEntered(): Unit =
        record(DirectEvaluatorEntered)
      override def onJitReductionEntered(): Unit = record(JitReductionEntered)
    }

    private class JoinedObserver
        extends RecordingObserver with StarkPreflightOperationObserver {
      override def onNodeInspected(): Unit = ()
      override def onChildrenRead(): Unit = ()
      override def onFramePushed(): Unit = ()
      override def onNodeReused(): Unit = ()
      override def onNodeRebuilt(): Unit = ()
      override def onProfileIdClassified(): Unit = ()
      override def onPlanBuilt(): Unit = record(StructuralPlanBuilt)
    }

    private final class ObserverSentinel extends RuntimeException

    private final class ThrowingObserver(
        target: String,
        sentinel: ObserverSentinel) extends RecordingObserver {
      override protected def record(event: String): Unit =
        if (event == target) throw sentinel else super.record(event)
    }

    private final class ThrowingJoinedObserver(
        target: String,
        sentinel: ObserverSentinel) extends JoinedObserver {
      override protected def record(event: String): Unit =
        if (event == target) throw sentinel else super.record(event)
    }

    private final class CensusInterpreter extends ErgoLikeTestInterpreter {
      var v4DeserializeCalls: Int = 0

      override protected def deserializeMeasuredV4(
          context: CTX,
          updateContext: CTX => Unit,
          scriptBytes: Array[Byte]): Value[SType] = {
        v4DeserializeCalls += 1
        super.deserializeMeasuredV4(context, updateContext, scriptBytes)
      }

      def propositionFor(
          tree: ErgoTree,
          context: ErgoLikeContext): SigmaPropValue =
        propositionFromErgoTree(tree, context)
    }

    private final class GenericCensusInterpreter extends Interpreter {
      type CTX = InterpreterContext
    }

    private final class NonErgoContext(
        private val underlying: InterpreterContext) extends InterpreterContext {
      override val extension: ContextExtension = underlying.extension
      override val validationSettings = underlying.validationSettings
      override val costLimit: Long = underlying.costLimit
      override val initCost: Long = underlying.initCost
      override def starkVerificationCapability: StarkVerificationCapability =
        underlying.starkVerificationCapability
      override def activatedScriptVersion: Byte = underlying.activatedScriptVersion
      override def withErgoTreeVersion(newVersion: Byte): InterpreterContext =
        new NonErgoContext(underlying.withErgoTreeVersion(newVersion))
      override def withCostLimit(newCostLimit: Long): InterpreterContext =
        new NonErgoContext(underlying.withCostLimit(newCostLimit))
      override def withInitCost(newCost: Long): InterpreterContext =
        new NonErgoContext(underlying.withInitCost(newCost))
      override def withExtension(newExtension: ContextExtension): InterpreterContext =
        new NonErgoContext(underlying.withExtension(newExtension))
      override def withValidationSettings(
          newVs: sigma.validation.SigmaValidationSettings): InterpreterContext =
        new NonErgoContext(underlying.withValidationSettings(newVs))
      override def toSigmaContext(): sigma.Context = underlying.toSigmaContext()
    }

    private def profileId(first: Int): Array[Byte] = {
      val result = new Array[Byte](ProfileIdBytes)
      result(0) = first.toByte
      result
    }

    private def profileIdLast(last: Int): Array[Byte] = {
      val result = new Array[Byte](ProfileIdBytes)
      result(result.length - 1) = last.toByte
      result
    }

    private def resourceBytes(path: String): Array[Byte] = {
      val in = getClass.getResourceAsStream(path)
      require(in != null, "missing test resource " + path)
      val out = new ByteArrayOutputStream()
      val buffer = new Array[Byte](8192)
      try {
        var read = in.read(buffer)
        while (read >= 0) {
          if (read > 0) out.write(buffer, 0, read)
          read = in.read(buffer)
        }
        out.toByteArray
      } finally {
        in.close()
        out.close()
      }
    }

    private lazy val loadedProfile = Risc0ProfilePackageLoader.load(
      resourceBytes(PackageRoot + "manifest.bin"),
      resourceBytes(PackageRoot + "algorithm.txt"),
      resourceBytes(PackageRoot + "constants.bin"),
      resourceBytes(PackageRoot + "profile-id.bin")) match {
      case Right(value) => value
      case Left(failure) => fail("frozen B1/B2/B3 package rejected: " + failure)
    }

    private lazy val rawSeal = resourceBytes(DirectRoot + "po2-15-raw-seal.bin")

    private def canonicalChunks(bytes: Array[Byte]): Array[Array[Byte]] = {
      val lengths = Eip0045FullRouteOperationProfiler.canonicalChunkLengths
      val result = new Array[Array[Byte]](lengths.length)
      var offset = 0
      var i = 0
      while (i < lengths.length) {
        result(i) = java.util.Arrays.copyOfRange(bytes, offset, offset + lengths(i))
        offset += lengths(i)
        i += 1
      }
      result
    }

    private def stockRuntime: Risc0StockProfileRuntime =
      right(Risc0StockProfileRuntime.fromLoadedProfile(loadedProfile))

    private def referenceExtension(proof: Array[Array[Byte]]): ContextExtension =
      ContextExtension(Map(
        0.toByte -> ConcreteCollection[SByteArray](
          proof.iterator.map(ByteArrayConstant(_)).toIndexedSeq,
          SByteArray),
        1.toByte -> ByteArrayConstant(ReferencePayload)))

    private def referenceCall: VerifyStark =
      VerifyStark(
        OptionGet(GetVar(0.toByte, SCollection(SByteArray))),
        OptionGet(GetVar(1.toByte, SByteArray)),
        ByteArrayConstant(Eip0045ReferenceContract.programId),
        ByteArrayConstant(Eip0045ReferenceContract.profileId))

    private def chunks(values: Array[Byte]*):
        Value[SCollection[SCollection[SByte.type]]] =
      ConcreteCollection[SByteArray](
        values.map(ByteArrayConstant(_)).toIndexedSeq,
        SByteArray).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

    private def executableNode(id: Array[Byte]): VerifyStark =
      VerifyStark(
        chunks(Array[Byte](4)),
        ByteArrayConstant(Array[Byte](7, 8)),
        ByteArrayConstant(profileId(9)),
        ByteArrayConstant(id))

    private def v4Tree(proposition: SigmaPropValue): ErgoTree =
      ErgoTree.fromProposition(
        ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
        proposition)

    private def contextFor(
        tree: ErgoTree,
        capability: StarkVerificationCapability,
        extension: ContextExtension = ContextExtension.empty): ErgoLikeContext =
      ErgoLikeContextTesting.dummy(
        createBox(1000000L, tree),
        activatedVersion = VersionContext.StarkVerificationVersion)
        .withExtension(extension)
        .withStarkVerificationCapability(capability)

    private def right[A](value: Either[ConstructionFailure, A]): A = value match {
      case Right(result) => result
      case Left(failure) => fail("unexpected capability rejection: " + failure)
    }

    private def activeSnapshot(runtime: StarkProfileRuntime): Snapshot = {
      val entry = right(active(runtime, fixedJit = 200))
      right(snapshot(
        Array.tabulate[Byte](ProfileIdBytes)(_.toByte),
        protocolGeneration = 7,
        HistoricalBlockValidation,
        dispatchJit = 100,
        Vector(entry)))
    }

    private def successful(interpreter: CensusInterpreter)(
        value: Either[Interpreter.ReductionResult, interpreter.StarkPreflightResult]):
        interpreter.StarkPreflightResult = value match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected preflight terminal: " + terminal)
    }

    private def classHierarchy(root: Class[_]): Vector[Class[_]] = {
      val classes = Vector.newBuilder[Class[_]]
      var current = root
      while (current ne null) {
        classes += current
        current = current.getSuperclass
      }
      classes.result()
    }

    private def interpreterMethodOwners: Vector[Class[_]] = {
      val legacyOwner = try {
        Vector(Class.forName("sigmastate.interpreter.Interpreter$class"))
      }
      catch {
        case _: ClassNotFoundException => Vector.empty
      }
      (classOf[Interpreter] +: legacyOwner).distinct
    }

    private def observedPreflightMethod: (Method, Boolean) = {
      val helperSuffix = "preflightV4Observed"
      val methods = interpreterMethodOwners.flatMap(_.getDeclaredMethods).filter {
        method =>
          val parameters = method.getParameterTypes
          method.getName.endsWith(helperSuffix) &&
            (parameters.length == 4 || parameters.length == 5) &&
            parameters.last == classOf[StarkPreflightOperationObserver] &&
            (parameters.length == 4 || parameters.head == classOf[Interpreter])
      }.distinct
      methods should have length 1
      val method = methods.head
      val receiverFirst = Modifier.isStatic(method.getModifiers)
      method.setAccessible(true)
      (method, receiverFirst)
    }

    private def observedPreflight(interpreter: CensusInterpreter)(
        tree: ErgoTree,
        context: ErgoLikeContext,
        observer: StarkPreflightOperationObserver):
        Either[Interpreter.ReductionResult, interpreter.StarkPreflightResult] = {
      val versionedContext =
        context.withErgoTreeVersion(tree.version).asInstanceOf[ErgoLikeContext]
      VersionContext.withVersions(
          versionedContext.activatedScriptVersion,
          tree.version) {
        val proposition = interpreter.propositionFor(tree, versionedContext)
        val (method, receiverFirst) = observedPreflightMethod
        val arguments =
          if (receiverFirst)
            Array[AnyRef](interpreter, tree, proposition, versionedContext, observer)
          else
            Array[AnyRef](tree, proposition, versionedContext, observer)
        try {
          method.invoke(if (receiverFirst) null else interpreter, arguments: _*)
            .asInstanceOf[
              Either[Interpreter.ReductionResult, interpreter.StarkPreflightResult]]
        }
        catch {
          case invocation: InvocationTargetException => throw invocation.getCause
        }
      }
    }

    private def observedContinuationMethod: (Method, Boolean) = {
      val helperSuffix = "continueClaimedV4Observed"
      val observerClass = classOf[StarkPreflightContinuationObserver]
      val methods = interpreterMethodOwners.flatMap(_.getDeclaredMethods).filter {
        method =>
          val parameters = method.getParameterTypes
          method.getName.endsWith(helperSuffix) &&
            (parameters.length == 2 || parameters.length == 3) &&
            parameters.last == observerClass &&
            (parameters.length == 2 || parameters.head == classOf[Interpreter])
      }.distinct
      methods should have length 1
      val method = methods.head
      val receiverFirst = Modifier.isStatic(method.getModifiers)
      method.setAccessible(true)
      (method, receiverFirst)
    }

    private def observedContinuation(interpreter: CensusInterpreter)(
        tree: ErgoTree,
        context: ErgoLikeContext,
        preflight: interpreter.StarkPreflightResult,
        observer: StarkPreflightContinuationObserver): Interpreter.ReductionResult = {
      val versionedContext =
        context.withErgoTreeVersion(tree.version).asInstanceOf[ErgoLikeContext]
      VersionContext.withVersions(
          versionedContext.activatedScriptVersion,
          tree.version) {
        val (method, receiverFirst) = observedContinuationMethod
        val arguments =
          if (receiverFirst)
            Array[AnyRef](interpreter, preflight, observer)
          else
            Array[AnyRef](preflight, observer)
        try {
          method.invoke(if (receiverFirst) null else interpreter, arguments: _*)
            .asInstanceOf[Interpreter.ReductionResult]
        }
        catch {
          case invocation: InvocationTargetException => throw invocation.getCause
        }
      }
    }

    private def observedRouteContinuationMethod: (Method, Boolean) = {
      val helperSuffix = "continueClaimedV4RouteObserved"
      val observerClass = classOf[StarkPreflightContinuationObserver]
      val methods = interpreterMethodOwners.flatMap(_.getDeclaredMethods).filter {
        method =>
          val parameters = method.getParameterTypes
          method.getName.endsWith(helperSuffix) &&
            (parameters.length == 3 || parameters.length == 4) &&
            parameters(parameters.length - 2) == observerClass &&
            parameters.last == classOf[Profiler] &&
            (parameters.length == 3 || parameters.head == classOf[Interpreter])
      }.distinct
      methods should have length 1
      val method = methods.head
      val receiverFirst = Modifier.isStatic(method.getModifiers)
      method.setAccessible(true)
      (method, receiverFirst)
    }

    private def observedRouteContinuation(interpreter: CensusInterpreter)(
        tree: ErgoTree,
        context: ErgoLikeContext,
        preflight: interpreter.StarkPreflightResult,
        observer: StarkPreflightContinuationObserver,
        profiler: Profiler): Interpreter.ReductionResult = {
      val versionedContext =
        context.withErgoTreeVersion(tree.version).asInstanceOf[ErgoLikeContext]
      VersionContext.withVersions(
          versionedContext.activatedScriptVersion,
          tree.version) {
        val (method, receiverFirst) = observedRouteContinuationMethod
        val arguments =
          if (receiverFirst)
            Array[AnyRef](interpreter, preflight, observer, profiler)
          else
            Array[AnyRef](preflight, observer, profiler)
        try {
          method.invoke(if (receiverFirst) null else interpreter, arguments: _*)
            .asInstanceOf[Interpreter.ReductionResult]
        }
        catch {
          case invocation: InvocationTargetException => throw invocation.getCause
        }
      }
    }

    private def observedGenericContinuation(
        interpreter: GenericCensusInterpreter)(
        tree: ErgoTree,
        context: InterpreterContext,
        preflight: interpreter.StarkPreflightResult,
        observer: StarkPreflightContinuationObserver): Interpreter.ReductionResult = {
      val versionedContext = context.withErgoTreeVersion(tree.version)
      VersionContext.withVersions(
          versionedContext.activatedScriptVersion,
          tree.version) {
        val (method, receiverFirst) = observedContinuationMethod
        val arguments =
          if (receiverFirst)
            Array[AnyRef](interpreter, preflight, observer)
          else
            Array[AnyRef](preflight, observer)
        try {
          method.invoke(if (receiverFirst) null else interpreter, arguments: _*)
            .asInstanceOf[Interpreter.ReductionResult]
        }
        catch {
          case invocation: InvocationTargetException => throw invocation.getCause
        }
      }
    }

    private def ordinaryContinuation(interpreter: CensusInterpreter)(
        tree: ErgoTree,
        context: ErgoLikeContext): Interpreter.ReductionResult =
      interpreter.preflightFullReduction(tree, context) match {
        case Right(preflight) => interpreter.continueFullReduction(preflight)
        case Left(terminal) => fail("unexpected preflight terminal: " + terminal)
      }

    private def directDeadTree(id: Array[Byte]): ErgoTree =
      v4Tree(If(TrueLeaf, TrueLeaf, executableNode(id)).toSigmaProp)

    private def materializedTree: ErgoTree =
      v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)

    private def materializedReferenceTree: ErgoTree =
      v4Tree(DeserializeContext(2, SBoolean).toSigmaProp)

    private def materializedContext(
        tree: ErgoTree,
        value: Value[_ <: SType],
        capability: StarkVerificationCapability): ErgoLikeContext =
      contextFor(
        tree,
        capability,
        ContextExtension(Map(
          1.toByte -> ByteArrayConstant(ValueSerializer.serialize(value)))))

    private def materializedReferenceContext(
        tree: ErgoTree,
        proof: Array[Array[Byte]]): ErgoLikeContext =
      contextFor(
        tree,
        activeSnapshot(stockRuntime),
        referenceExtension(proof).add(
          2.toByte -> ByteArrayConstant(ValueSerializer.serialize(referenceCall))))

    test("unavailable non-empty plans stop at the availability boundary") {
      val directInterpreter = new CensusInterpreter
      val directTree = directDeadTree(profileId(1))
      val directContext = contextFor(directTree, Unavailable)
      val directPreflight = successful(directInterpreter)(
        directInterpreter.preflightFullReduction(directTree, directContext))
      val directObserver = new RecordingObserver
      val directFailure = intercept[OpcodeUnavailableException] {
        observedContinuation(directInterpreter)(
          directTree,
          directContext,
          directPreflight,
          directObserver)
      }
      directObserver.events shouldBe Vector(
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked)

      val ordinaryDirectFailure = intercept[OpcodeUnavailableException] {
        ordinaryContinuation(new CensusInterpreter)(directTree, directContext)
      }
      directFailure.opCode shouldBe ordinaryDirectFailure.opCode
      directFailure.getMessage shouldBe ordinaryDirectFailure.getMessage

      val materializedInterpreter = new CensusInterpreter
      val selectedCall = executableNode(profileId(2))
      val selectedTree = materializedTree
      val selectedContext = materializedContext(
        selectedTree,
        selectedCall,
        Unavailable)
      val materializedPreflight = successful(materializedInterpreter)(
        materializedInterpreter.preflightFullReduction(selectedTree, selectedContext))
      materializedPreflight.plan.occurrences should have size 1
      materializedInterpreter.v4DeserializeCalls shouldBe 1
      val materializedObserver = new RecordingObserver
      val materializedFailure = intercept[OpcodeUnavailableException] {
        observedContinuation(materializedInterpreter)(
          selectedTree,
          selectedContext,
          materializedPreflight,
          materializedObserver)
      }
      materializedObserver.events shouldBe Vector(
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked)
      materializedInterpreter.v4DeserializeCalls shouldBe 1
      val ordinaryMaterializedFailure = intercept[OpcodeUnavailableException] {
        ordinaryContinuation(new CensusInterpreter)(selectedTree, selectedContext)
      }
      materializedFailure.opCode shouldBe ordinaryMaterializedFailure.opCode
      materializedFailure.getMessage shouldBe ordinaryMaterializedFailure.getMessage
    }

    test("an available dead branch enters the direct evaluator without a runtime call") {
      val id = profileId(3)
      val runtime = new Eip0045ContinuationCensusRuntime(id)
      val capability = activeSnapshot(runtime)
      val tree = directDeadTree(id)
      val context = contextFor(tree, capability)
      val interpreter = new CensusInterpreter
      val observer = new RecordingObserver

      val result = observedContinuation(interpreter)(
        tree,
        context,
        successful(interpreter)(interpreter.preflightFullReduction(tree, context)),
        observer)

      val ordinaryInterpreter = new CensusInterpreter
      val ordinaryResult = ordinaryContinuation(ordinaryInterpreter)(tree, context)
      result.value shouldBe ordinaryResult.value
      result.cost shouldBe ordinaryResult.cost
      result.value shouldBe TrivialProp.TrueProp
      observer.events shouldBe Vector(
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        DirectEvaluatorEntered)
      runtime.calls shouldBe 0

      runtime.calls shouldBe 0
    }

    test("the direct continuation joins the absent-profile evaluator route") {
      val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val tree = v4Tree(executableNode(profileIdLast(1)).toSigmaProp)
      val context = contextFor(tree, activeSnapshot(runtime))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      observer.events shouldBe Vector(StructuralPlanBuilt)

      var activeEvaluator: CErgoTreeEvaluator = null
      val profiler = Eip0045EvaluatorRouteProfiler { event =>
        if (event == ProfileIdEvaluated)
          activeEvaluator = CErgoTreeEvaluator.getCurrentEvaluator
        observer.recordRouteEvent(event)
      }
      val observed = observedRouteContinuation(interpreter)(
        tree,
        context,
        preflight,
        observer,
        profiler)
      val ordinary = ordinaryContinuation(new CensusInterpreter)(tree, context)

      observed.value shouldBe ordinary.value
      observed.cost shouldBe ordinary.cost
      observed.value shouldBe TrivialProp.FalseProp
      observer.events shouldBe (
        Vector(
          StructuralPlanBuilt,
          ContinuationTaken,
          DirectPathSelected,
          AvailabilityChecked,
          AvailabilityPassed,
          DirectEvaluatorEntered,
          ProfileIdEvaluated,
          DispatchCharged,
          ProfileIdValidated,
          ProfileIdMaterialized) ++
          Vector.fill(ProfileIdBytes)(ByteCompared) ++
          Vector(EntryCompared, LookupCompleted))
      activeEvaluator should not be null
      activeEvaluator.profiler should be theSameInstanceAs profiler
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      runtime.calls shouldBe 0
    }

    test("the materialized continuation joins the absent-profile evaluator route") {
      val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val tree = materializedTree
      val context = materializedContext(
        tree,
        executableNode(profileIdLast(1)),
        activeSnapshot(runtime))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      preflight.plan.occurrences should have size 1
      interpreter.v4DeserializeCalls shouldBe 1
      observer.events shouldBe Vector(StructuralPlanBuilt)

      var activeEvaluator: CErgoTreeEvaluator = null
      val profiler = Eip0045EvaluatorRouteProfiler { event =>
        if (event == ProfileIdEvaluated)
          activeEvaluator = CErgoTreeEvaluator.getCurrentEvaluator
        observer.recordRouteEvent(event)
      }
      val observed = observedRouteContinuation(interpreter)(
        tree,
        context,
        preflight,
        observer,
        profiler)
      val ordinary = ordinaryContinuation(new CensusInterpreter)(tree, context)

      observed.value shouldBe ordinary.value
      observed.cost shouldBe ordinary.cost
      observed.value shouldBe TrivialProp.FalseProp
      observer.events shouldBe (
        Vector(
          StructuralPlanBuilt,
          ContinuationTaken,
          MaterializedPathSelected,
          AvailabilityChecked,
          AvailabilityPassed,
          JitReductionEntered,
          ProfileIdEvaluated,
          DispatchCharged,
          ProfileIdValidated,
          ProfileIdMaterialized) ++
          Vector.fill(ProfileIdBytes)(ByteCompared) ++
          Vector(EntryCompared, LookupCompleted))
      activeEvaluator should not be null
      activeEvaluator.profiler should be theSameInstanceAs profiler
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      runtime.calls shouldBe 0
    }

    test("the direct continuation joins raw entry to the complete verifier-operation census") {
      val tree = Eip0045ReferenceContract.buildTree()
      val proof = canonicalChunks(rawSeal)
      val context = contextFor(
        tree,
        activeSnapshot(stockRuntime),
        referenceExtension(proof))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      observer.events shouldBe Vector(StructuralPlanBuilt)

      val operationCounts = Array.fill(FullOperationCounts.length)(0)
      var operationCallbacks = 0
      var activeEvaluator: CErgoTreeEvaluator = null
      val profiler = Eip0045FullRouteOperationProfiler(
        event => {
          if (event == ProfileIdEvaluated)
            activeEvaluator = CErgoTreeEvaluator.getCurrentEvaluator
          observer.recordRouteEvent(event)
        },
        operationId => {
          operationId should (be >= 1 and be <= operationCounts.length)
          if (operationCallbacks == 0)
            observer.recordRouteEvent(FirstVerifierOperation)
          operationCounts(operationId - 1) += 1
          operationCallbacks += 1
        })
      val observed = observedRouteContinuation(interpreter)(
        tree,
        context,
        preflight,
        observer,
        profiler)
      val ordinary = ordinaryContinuation(new CensusInterpreter)(tree, context)

      observed.value shouldBe ordinary.value
      observed.cost shouldBe ordinary.cost
      observed.value shouldBe TrivialProp.FalseProp
      CanonicalActiveRouteEvents.length shouldBe 65
      CanonicalDirectActivePrefix.length shouldBe 71
      operationCounts.toVector shouldBe FullOperationCounts
      operationCallbacks shouldBe FullOperationCounts.sum
      FullOperationCounts.sum shouldBe 6292
      (FullOperationCounts(0) + FullOperationCounts(1) +
        FullOperationCounts(3) + FullOperationCounts(6)) shouldBe 5683
      observer.events shouldBe (CanonicalDirectActivePrefix :+ FirstVerifierOperation)
      classOf[Risc0RawSealVerifier.Probe].isAssignableFrom(profiler.getClass) shouldBe false
      activeEvaluator should not be null
      activeEvaluator.profiler should be theSameInstanceAs profiler
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
    }

    test("the materialized continuation joins raw entry to the complete verifier-operation census") {
      val tree = materializedReferenceTree
      val proof = canonicalChunks(rawSeal)
      val context = materializedReferenceContext(tree, proof)
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      preflight.plan.occurrences should have size 1
      interpreter.v4DeserializeCalls shouldBe 1
      observer.events shouldBe Vector(StructuralPlanBuilt)

      val operationCounts = Array.fill(FullOperationCounts.length)(0)
      var operationCallbacks = 0
      var activeEvaluator: CErgoTreeEvaluator = null
      val profiler = Eip0045FullRouteOperationProfiler(
        event => {
          if (event == ProfileIdEvaluated)
            activeEvaluator = CErgoTreeEvaluator.getCurrentEvaluator
          observer.recordRouteEvent(event)
        },
        operationId => {
          operationId should (be >= 1 and be <= operationCounts.length)
          if (operationCallbacks == 0)
            observer.recordRouteEvent(FirstVerifierOperation)
          operationCounts(operationId - 1) += 1
          operationCallbacks += 1
        })
      val observed = observedRouteContinuation(interpreter)(
        tree,
        context,
        preflight,
        observer,
        profiler)
      val ordinary = ordinaryContinuation(new CensusInterpreter)(tree, context)

      observed.value shouldBe ordinary.value
      observed.cost shouldBe ordinary.cost
      observed.value shouldBe TrivialProp.FalseProp
      CanonicalActiveRouteEvents.length shouldBe 65
      CanonicalMaterializedActivePrefix.length shouldBe 71
      operationCounts.toVector shouldBe FullOperationCounts
      operationCallbacks shouldBe FullOperationCounts.sum
      FullOperationCounts.sum shouldBe 6292
      observer.events shouldBe
        (CanonicalMaterializedActivePrefix :+ FirstVerifierOperation)
      classOf[Risc0RawSealVerifier.Probe].isAssignableFrom(profiler.getClass) shouldBe false
      activeEvaluator should not be null
      activeEvaluator.profiler should be theSameInstanceAs profiler
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
    }

    test("the first full-route operation exception preserves identity and evaluator scope") {
      val tree = Eip0045ReferenceContract.buildTree()
      val context = contextFor(
        tree,
        activeSnapshot(stockRuntime),
        referenceExtension(canonicalChunks(rawSeal)))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      val sentinel = new ObserverSentinel
      val profiler = Eip0045FullRouteOperationProfiler(
        observer.recordRouteEvent,
        _ => throw sentinel)

      val failure = intercept[ObserverSentinel] {
        observedRouteContinuation(interpreter)(
          tree,
          context,
          preflight,
          observer,
          profiler)
      }

      failure should be theSameInstanceAs sentinel
      observer.events shouldBe CanonicalDirectActivePrefix
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
    }

    test("the first materialized full-route operation exception preserves identity and evaluator scope") {
      val tree = materializedReferenceTree
      val context = materializedReferenceContext(tree, canonicalChunks(rawSeal))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      preflight.plan.occurrences should have size 1
      interpreter.v4DeserializeCalls shouldBe 1
      observer.events shouldBe Vector(StructuralPlanBuilt)
      val sentinel = new ObserverSentinel
      val profiler = Eip0045FullRouteOperationProfiler(
        observer.recordRouteEvent,
        _ => throw sentinel)

      val failure = intercept[ObserverSentinel] {
        observedRouteContinuation(interpreter)(
          tree,
          context,
          preflight,
          observer,
          profiler)
      }

      failure should be theSameInstanceAs sentinel
      observer.events shouldBe CanonicalMaterializedActivePrefix
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
    }

    test("the full-route operation seam is integer-only and retained by no long-lived state") {
      val operationObserverClass =
        Eip0045FullRouteOperationProfiler.operationObserverClass
      val threadLocalClass = classOf[ThreadLocal[_]]
      val probeClass = classOf[Risc0RawSealVerifier.Probe]

      operationObserverClass.getDeclaredFields.toSeq shouldBe empty
      val operationMethods = operationObserverClass.getDeclaredMethods
      val operationCallbacks = operationMethods.filter(_.getParameterCount == 1)
      operationCallbacks.map(_.getName).toSeq shouldBe Seq("onOperation")
      Eip0045FullRouteOperationProfiler.canonicalOperationIds shouldBe (1 to 7)
      val operationMethod = operationCallbacks.head
      operationMethod.getParameterTypes.toSeq shouldBe Seq(java.lang.Integer.TYPE)
      operationMethod.getReturnType shouldBe java.lang.Void.TYPE

      Seq(
        classOf[Risc0RawSealVerifier],
        classOf[Risc0StockProfileRuntime]).foreach { owner =>
        owner.getDeclaredFields.foreach { field =>
          operationObserverClass.isAssignableFrom(field.getType) shouldBe false
          probeClass.isAssignableFrom(field.getType) shouldBe false
          threadLocalClass.isAssignableFrom(field.getType) shouldBe false
        }
      }

      val observedEntries = classOf[Risc0RawSealVerifier].getDeclaredMethods
        .filter(_.getName == "verifyObservedOperations")
      observedEntries should have length 1
      observedEntries.head.getParameterTypes.toSeq shouldBe Seq(
        classOf[Array[Array[Byte]]],
        classOf[Array[Byte]],
        operationObserverClass)
      val ordinaryEntries = classOf[Risc0RawSealVerifier].getDeclaredMethods
        .filter(method => method.getName == "verify" && method.getParameterCount == 2)
      ordinaryEntries should have length 1
    }

    test("evaluator-route callbacks propagate with identity and restore evaluator scope") {
      val canonicalEvents =
        Vector(
          StructuralPlanBuilt,
          ContinuationTaken,
          DirectPathSelected,
          AvailabilityChecked,
          AvailabilityPassed,
          DirectEvaluatorEntered,
          ProfileIdEvaluated,
          DispatchCharged,
          ProfileIdValidated,
          ProfileIdMaterialized) ++
          Vector.fill(ProfileIdBytes)(ByteCompared) ++
          Vector(EntryCompared, LookupCompleted)
      val targets = Vector(
        ProfileIdEvaluated,
        DispatchCharged,
        ProfileIdValidated,
        ProfileIdMaterialized,
        ByteCompared,
        EntryCompared,
        LookupCompleted)

      targets.foreach { target =>
        withClue(target + ": ") {
          val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
          val tree = v4Tree(executableNode(profileIdLast(1)).toSigmaProp)
          val context = contextFor(tree, activeSnapshot(runtime))
          val interpreter = new CensusInterpreter
          val sentinel = new ObserverSentinel
          val observer = new ThrowingJoinedObserver(target, sentinel)
          val preflight = successful(interpreter)(observedPreflight(interpreter)(
            tree,
            context,
            observer))
          val profiler = Eip0045EvaluatorRouteProfiler(observer.recordRouteEvent)

          val failure = intercept[ObserverSentinel] {
            observedRouteContinuation(interpreter)(
              tree,
              context,
              preflight,
              observer,
              profiler)
          }
          failure should be theSameInstanceAs sentinel
          observer.events shouldBe canonicalEvents.takeWhile(_ != target)
          CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
          runtime.calls shouldBe 0
        }
      }
    }

    test("materialized evaluator-route callbacks preserve identity and restore scope") {
      val canonicalEvents =
        Vector(
          StructuralPlanBuilt,
          ContinuationTaken,
          MaterializedPathSelected,
          AvailabilityChecked,
          AvailabilityPassed,
          JitReductionEntered,
          ProfileIdEvaluated,
          DispatchCharged,
          ProfileIdValidated,
          ProfileIdMaterialized) ++
          Vector.fill(ProfileIdBytes)(ByteCompared) ++
          Vector(EntryCompared, LookupCompleted)
      val targets = Vector(
        ProfileIdEvaluated,
        DispatchCharged,
        ProfileIdValidated,
        ProfileIdMaterialized,
        ByteCompared,
        EntryCompared,
        LookupCompleted)

      targets.foreach { target =>
        withClue(target + ": ") {
          val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
          val tree = materializedTree
          val context = materializedContext(
            tree,
            executableNode(profileIdLast(1)),
            activeSnapshot(runtime))
          val interpreter = new CensusInterpreter
          val sentinel = new ObserverSentinel
          val observer = new ThrowingJoinedObserver(target, sentinel)
          val preflight = successful(interpreter)(observedPreflight(interpreter)(
            tree,
            context,
            observer))
          val profiler = Eip0045EvaluatorRouteProfiler(observer.recordRouteEvent)

          val failure = intercept[ObserverSentinel] {
            observedRouteContinuation(interpreter)(
              tree,
              context,
              preflight,
              observer,
              profiler)
          }
          failure should be theSameInstanceAs sentinel
          observer.events shouldBe canonicalEvents.takeWhile(_ != target)
          CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
          runtime.calls shouldBe 0
        }
      }
    }

    test("the materialized evaluator route preserves its soft-fork boundary") {
      val nonSoftRuntime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val tree = materializedTree
      val nonSoftContext = materializedContext(
        tree,
        executableNode(profileIdLast(1)),
        activeSnapshot(nonSoftRuntime))
      val nonSoftInterpreter = new CensusInterpreter
      val nonSoftObserver = new JoinedObserver
      val nonSoftPreflight = successful(nonSoftInterpreter)(
        observedPreflight(nonSoftInterpreter)(
          tree,
          nonSoftContext,
          nonSoftObserver))
      val nonSoftError = ValidationException(
        "non-soft evaluator callback",
        CheckPositionLimit,
        Seq.empty[Any])
      nonSoftContext.validationSettings.isSoftFork(nonSoftError) shouldBe false
      val nonSoftProfiler = Eip0045EvaluatorRouteProfiler { event =>
        if (event == ProfileIdEvaluated)
          throw nonSoftError
        nonSoftObserver.recordRouteEvent(event)
      }

      val nonSoftFailure = intercept[ValidationException] {
        observedRouteContinuation(nonSoftInterpreter)(
          tree,
          nonSoftContext,
          nonSoftPreflight,
          nonSoftObserver,
          nonSoftProfiler)
      }
      nonSoftFailure should be theSameInstanceAs nonSoftError
      nonSoftObserver.events shouldBe Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        JitReductionEntered)
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      nonSoftRuntime.calls shouldBe 0

      val softRuntime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val softBaseContext = materializedContext(
        tree,
        executableNode(profileIdLast(1)),
        activeSnapshot(softRuntime))
      val softSettings = softBaseContext.validationSettings.updated(
        CheckPositionLimit.id,
        ReplacedRule((CheckPositionLimit.id + 1).toShort))
      val softContext = softBaseContext.withValidationSettings(softSettings)
      val softInterpreter = new CensusInterpreter
      val softObserver = new JoinedObserver
      val softPreflight = successful(softInterpreter)(
        observedPreflight(softInterpreter)(
          tree,
          softContext,
          softObserver))
      val expectedSoftForkCost = softPreflight.preflightBlockCost
      val softError = ValidationException(
        "soft evaluator callback",
        CheckPositionLimit,
        Seq.empty[Any])
      softContext.validationSettings.isSoftFork(softError) shouldBe true
      val softProfiler = Eip0045EvaluatorRouteProfiler { event =>
        if (event == ProfileIdEvaluated)
          throw softError
        softObserver.recordRouteEvent(event)
      }

      val softResult = observedRouteContinuation(softInterpreter)(
        tree,
        softContext,
        softPreflight,
        softObserver,
        softProfiler)
      softResult.value shouldBe TrivialProp.TrueProp
      softResult.cost shouldBe expectedSoftForkCost
      softObserver.events shouldBe Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        JitReductionEntered)
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      softRuntime.calls shouldBe 0
    }

    test("profile evaluation completes before the first joined evaluator event") {
      val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val call = VerifyStark(
        chunks(Array[Byte](4)),
        ByteArrayConstant(Array[Byte](7, 8)),
        ByteArrayConstant(profileId(9)),
        ByIndex(chunks(Array[Byte](1)), IntConstant(1)))
      val tree = v4Tree(call.toSigmaProp)
      val context = contextFor(tree, activeSnapshot(runtime))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      val profiler = Eip0045EvaluatorRouteProfiler(observer.recordRouteEvent)

      val observedFailure = intercept[IndexOutOfBoundsException] {
        observedRouteContinuation(interpreter)(
          tree,
          context,
          preflight,
          observer,
          profiler)
      }
      observer.events shouldBe Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        DirectEvaluatorEntered)
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      runtime.calls shouldBe 0

      val ordinaryFailure = intercept[IndexOutOfBoundsException] {
        ordinaryContinuation(new CensusInterpreter)(tree, context)
      }
      observedFailure.getClass shouldBe ordinaryFailure.getClass
      runtime.calls shouldBe 0
    }

    test("materialized profile evaluation completes before its first joined event") {
      val runtime = new Eip0045ContinuationCensusRuntime(profileId(0))
      val call = VerifyStark(
        chunks(Array[Byte](4)),
        ByteArrayConstant(Array[Byte](7, 8)),
        ByteArrayConstant(profileId(9)),
        ByIndex(chunks(Array[Byte](1)), IntConstant(1)))
      val tree = materializedTree
      val context = materializedContext(tree, call, activeSnapshot(runtime))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver
      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      val profiler = Eip0045EvaluatorRouteProfiler(observer.recordRouteEvent)

      val observedFailure = intercept[IndexOutOfBoundsException] {
        observedRouteContinuation(interpreter)(
          tree,
          context,
          preflight,
          observer,
          profiler)
      }
      observer.events shouldBe Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        JitReductionEntered)
      CErgoTreeEvaluator.getCurrentEvaluator shouldBe null
      runtime.calls shouldBe 0

      val ordinaryFailure = intercept[IndexOutOfBoundsException] {
        ordinaryContinuation(new CensusInterpreter)(tree, context)
      }
      observedFailure.getClass shouldBe ordinaryFailure.getClass
      runtime.calls shouldBe 0
    }

    test("empty plans pass through the constant and materialized JIT boundaries") {
      val directInterpreter = new CensusInterpreter
      val directTree = v4Tree(TrueSigmaProp)
      val directContext = contextFor(directTree, Unavailable)
      val directObserver = new RecordingObserver
      val directResult = observedContinuation(directInterpreter)(
        directTree,
        directContext,
        successful(directInterpreter)(directInterpreter.preflightFullReduction(
          directTree,
          directContext)),
        directObserver)
      val ordinaryDirectInterpreter = new CensusInterpreter
      val ordinaryDirectResult = ordinaryContinuation(ordinaryDirectInterpreter)(
        directTree,
        directContext)
      directResult.value shouldBe ordinaryDirectResult.value
      directResult.cost shouldBe ordinaryDirectResult.cost
      directResult.value shouldBe TrivialProp.TrueProp
      directObserver.events shouldBe Vector(
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        ConstantReductionEntered)

      val jitInterpreter = new CensusInterpreter
      val jitTree = materializedTree
      val jitContext = materializedContext(jitTree, TrueLeaf, Unavailable)
      val jitPreflight = successful(jitInterpreter)(
        jitInterpreter.preflightFullReduction(jitTree, jitContext))
      jitPreflight.plan.occurrences shouldBe empty
      jitInterpreter.v4DeserializeCalls shouldBe 1
      val jitObserver = new RecordingObserver
      val jitResult = observedContinuation(jitInterpreter)(
        jitTree,
        jitContext,
        jitPreflight,
        jitObserver)
      val ordinaryJitInterpreter = new CensusInterpreter
      val ordinaryJitResult = ordinaryContinuation(ordinaryJitInterpreter)(
        jitTree,
        jitContext)
      jitResult.value shouldBe ordinaryJitResult.value
      jitResult.cost shouldBe ordinaryJitResult.cost
      jitResult.value shouldBe TrivialProp.TrueProp
      jitObserver.events shouldBe Vector(
        ContinuationTaken,
        MaterializedPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        JitReductionEntered)
      jitInterpreter.v4DeserializeCalls shouldBe 1
    }

    test("the constant boundary event precedes the costed reducer") {
      val tree = v4Tree(TrueSigmaProp)
      val baseContext = contextFor(tree, Unavailable)
      val rejectedContext = baseContext.withCostLimit(baseContext.initCost)
      val interpreter = new CensusInterpreter
      val preflight = successful(interpreter)(
        interpreter.preflightFullReduction(tree, rejectedContext))
      val observer = new RecordingObserver

      val observedFailure = intercept[CostLimitException] {
        observedContinuation(interpreter)(
          tree,
          rejectedContext,
          preflight,
          observer)
      }
      observer.events shouldBe Vector(
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        ConstantReductionEntered)

      val ordinaryFailure = intercept[CostLimitException] {
        ordinaryContinuation(new CensusInterpreter)(tree, rejectedContext)
      }
      observedFailure.estimatedCost shouldBe ordinaryFailure.estimatedCost
      observedFailure.getMessage shouldBe ordinaryFailure.getMessage
    }

    test("the direct evaluator event follows a successful ErgoLikeContext cast") {
      val tree = v4Tree(If(TrueLeaf, TrueLeaf, FalseLeaf).toSigmaProp)
      val context = new NonErgoContext(contextFor(tree, Unavailable))
      val interpreter = new GenericCensusInterpreter
      val preflight = interpreter.preflightFullReduction(tree, context) match {
        case Right(result) => result
        case Left(terminal) => fail("unexpected preflight terminal: " + terminal)
      }
      val observer = new RecordingObserver

      intercept[ClassCastException] {
        observedGenericContinuation(interpreter)(
          tree,
          context,
          preflight,
          observer)
      }
      observer.events shouldBe Vector(
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed)
    }

    test("the structural plan event precedes continuation consumption") {
      val id = profileId(4)
      val runtime = new Eip0045ContinuationCensusRuntime(id)
      val tree = directDeadTree(id)
      val context = contextFor(tree, activeSnapshot(runtime))
      val interpreter = new CensusInterpreter
      val observer = new JoinedObserver

      val preflight = successful(interpreter)(observedPreflight(interpreter)(
        tree,
        context,
        observer))
      observer.events shouldBe Vector(StructuralPlanBuilt)

      observedContinuation(interpreter)(tree, context, preflight, observer).value shouldBe
        TrivialProp.TrueProp
      observer.events shouldBe Vector(
        StructuralPlanBuilt,
        ContinuationTaken,
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        DirectEvaluatorEntered)
      runtime.calls shouldBe 0
    }

    test("consumption is one-shot and observer exceptions keep object identity") {
      val interpreter = new CensusInterpreter
      val tree = v4Tree(TrueSigmaProp)
      val context = contextFor(tree, Unavailable)
      val preflight = successful(interpreter)(
        interpreter.preflightFullReduction(tree, context))
      val firstObserver = new RecordingObserver
      observedContinuation(interpreter)(
        tree,
        context,
        preflight,
        firstObserver).value shouldBe
        TrivialProp.TrueProp

      val secondObserver = new RecordingObserver
      val secondUse = intercept[IllegalStateException] {
        observedContinuation(interpreter)(tree, context, preflight, secondObserver)
      }
      secondUse.getMessage shouldBe
        "STARK preflight result has already been consumed"
      secondObserver.events shouldBe empty

      val takeInterpreter = new CensusInterpreter
      val takeTree = v4Tree(TrueSigmaProp)
      val takeContext = contextFor(takeTree, Unavailable)
      val takePreflight = successful(takeInterpreter)(
        takeInterpreter.preflightFullReduction(takeTree, takeContext))
      val takeSentinel = new ObserverSentinel
      val takeObserver = new ThrowingObserver(ContinuationTaken, takeSentinel)
      intercept[ObserverSentinel] {
        observedContinuation(takeInterpreter)(
          takeTree,
          takeContext,
          takePreflight,
          takeObserver)
      } should be theSameInstanceAs takeSentinel
      takeObserver.events shouldBe empty
      val afterTakeObserver = new RecordingObserver
      val afterTakeFailure = intercept[IllegalStateException] {
        observedContinuation(takeInterpreter)(
          takeTree,
          takeContext,
          takePreflight,
          afterTakeObserver)
      }
      afterTakeFailure.getMessage shouldBe
        "STARK preflight result has already been consumed"
      afterTakeObserver.events shouldBe empty

      val directTargets = Vector(
        DirectPathSelected,
        AvailabilityChecked,
        AvailabilityPassed,
        ConstantReductionEntered)
      directTargets.foreach { target =>
        withClue(target + ": ") {
          val candidate = new CensusInterpreter
          val candidateTree = v4Tree(TrueSigmaProp)
          val candidateContext = contextFor(candidateTree, Unavailable)
          val sentinel = new ObserverSentinel
          val failure = intercept[ObserverSentinel] {
            observedContinuation(candidate)(
              candidateTree,
              candidateContext,
              successful(candidate)(candidate.preflightFullReduction(
                candidateTree,
                candidateContext)),
              new ThrowingObserver(target, sentinel))
          }
          failure should be theSameInstanceAs sentinel
        }
      }

      val evaluatorId = profileId(5)
      val evaluatorRuntime = new Eip0045ContinuationCensusRuntime(evaluatorId)
      val evaluatorTree = v4Tree(executableNode(evaluatorId).toSigmaProp)
      val evaluatorContext = contextFor(
        evaluatorTree,
        activeSnapshot(evaluatorRuntime))
      val evaluatorInterpreter = new CensusInterpreter
      val evaluatorPreflight = successful(evaluatorInterpreter)(
        evaluatorInterpreter.preflightFullReduction(
          evaluatorTree,
          evaluatorContext))
      val evaluatorSentinel = new ObserverSentinel
      intercept[ObserverSentinel] {
        observedContinuation(evaluatorInterpreter)(
          evaluatorTree,
          evaluatorContext,
          evaluatorPreflight,
          new ThrowingObserver(DirectEvaluatorEntered, evaluatorSentinel))
      } should be theSameInstanceAs evaluatorSentinel
      evaluatorRuntime.calls shouldBe 0
      ordinaryContinuation(new CensusInterpreter)(
        evaluatorTree,
        evaluatorContext).value shouldBe TrivialProp.TrueProp
      evaluatorRuntime.calls shouldBe 1

      Vector(
        MaterializedPathSelected -> 6,
        JitReductionEntered -> 7).foreach {
        case (target, idFirst) => withClue(target + ": ") {
          val id = profileId(idFirst)
          val runtime = new Eip0045ContinuationCensusRuntime(id)
          val candidate = new CensusInterpreter
          val candidateTree = materializedTree
          val candidateContext = materializedContext(
            candidateTree,
            executableNode(id),
            activeSnapshot(runtime))
          val candidatePreflight = successful(candidate)(
            candidate.preflightFullReduction(candidateTree, candidateContext))
          val sentinel = new ObserverSentinel
          val failure = intercept[ObserverSentinel] {
            observedContinuation(candidate)(
              candidateTree,
              candidateContext,
              candidatePreflight,
              new ThrowingObserver(target, sentinel))
          }
          failure should be theSameInstanceAs sentinel
          runtime.calls shouldBe 0
          ordinaryContinuation(new CensusInterpreter)(
            candidateTree,
            candidateContext).value shouldBe TrivialProp.TrueProp
          runtime.calls shouldBe 1
        }
      }
    }

    private def ordinaryHelperMethod(
        suffix: String,
        instanceParameterCount: Int): Method = {
      val observerClass = classOf[StarkPreflightContinuationObserver]
      val methods = interpreterMethodOwners.flatMap(_.getDeclaredMethods).filter {
        method =>
          val parameters = method.getParameterTypes
          val receiverFirst = Modifier.isStatic(method.getModifiers)
          method.getName.endsWith(suffix) &&
            parameters.length ==
              instanceParameterCount + (if (receiverFirst) 1 else 0) &&
            (!receiverFirst || parameters.head == classOf[Interpreter]) &&
            !parameters.contains(observerClass)
      }.distinct
      withClue(suffix + ": ") {
        methods should have length 1
      }
      methods.head
    }

    test("continuation observation is payload-free and retained by no state") {
      val observerClass = classOf[StarkPreflightContinuationObserver]
      val threadLocalClass = Class.forName("java.lang.ThreadLocal")
      observerClass.getDeclaredFields.toSeq shouldBe empty
      observerClass.getDeclaredMethods.map(_.getName).toSet shouldBe Set(
        "onContinuationTaken",
        "onDirectPathSelected",
        "onMaterializedPathSelected",
        "onAvailabilityChecked",
        "onAvailabilityPassed",
        "onConstantReductionEntered",
        "onDirectEvaluatorEntered",
        "onJitReductionEntered")
      observerClass.getDeclaredMethods.foreach { method =>
        method.getParameterTypes.toSeq shouldBe empty
        method.getReturnType shouldBe java.lang.Void.TYPE
      }

      val interpreter = new CensusInterpreter
      val continuationClass = Class.forName(
        "sigmastate.interpreter.Interpreter$StarkContinuation")
      val stateOwners = (Seq(
        classOf[Interpreter],
        Interpreter.getClass,
        classOf[interpreter.StarkPreflightResult],
        continuationClass) ++ classHierarchy(interpreter.getClass)).distinct
      stateOwners.foreach { cls =>
        cls.getDeclaredFields.foreach { field =>
          observerClass.isAssignableFrom(field.getType) shouldBe false
          threadLocalClass.isAssignableFrom(field.getType) shouldBe false
        }
      }

      (interpreterMethodOwners ++ classHierarchy(interpreter.getClass)).distinct
        .foreach { cls =>
          val exposedObserved = cls.getDeclaredMethods.filter { method =>
            method.getName.contains("Observed") &&
              (Modifier.isPublic(method.getModifiers) || method.isSynthetic)
          }
          withClue(cls.getName + ": ") {
            exposedObserved shouldBe empty
          }
        }

      val (observed, receiverFirst) = observedContinuationMethod
      val (routeObserved, routeReceiverFirst) = observedRouteContinuationMethod
      val continuationObservedMethods = interpreterMethodOwners
        .flatMap(_.getDeclaredMethods)
        .filter(_.getParameterTypes.contains(observerClass))
        .distinct
      continuationObservedMethods.toSet shouldBe Set(observed, routeObserved)
      val traitObserved = classOf[Interpreter].getDeclaredMethods
        .filter(_.getName == "continueClaimedV4Observed")
      traitObserved.foreach { method =>
        Modifier.isAbstract(method.getModifiers) shouldBe false
      }
      if (!receiverFirst) {
        traitObserved should have length 1
        Modifier.isPrivate(traitObserved.head.getModifiers) shouldBe true
      }
      interpreter.getClass.getDeclaredMethods
        .filter(_.getName == "continueClaimedV4Observed") shouldBe empty
      val traitRouteObserved = classOf[Interpreter].getDeclaredMethods
        .filter(_.getName == "continueClaimedV4RouteObserved")
      traitRouteObserved.foreach { method =>
        Modifier.isAbstract(method.getModifiers) shouldBe false
      }
      if (!routeReceiverFirst) {
        traitRouteObserved should have length 1
        Modifier.isPrivate(traitRouteObserved.head.getModifiers) shouldBe true
      }
      interpreter.getClass.getDeclaredMethods
        .filter(_.getName == "continueClaimedV4RouteObserved") shouldBe empty
      val observedParameters = observed.getParameterTypes.toVector
      val preflightClass = classOf[interpreter.StarkPreflightResult]
      if (receiverFirst)
        observedParameters shouldBe Vector(
          classOf[Interpreter],
          preflightClass,
          observerClass)
      else
        observedParameters shouldBe Vector(preflightClass, observerClass)
      val routeObservedParameters = routeObserved.getParameterTypes.toVector
      if (routeReceiverFirst)
        routeObservedParameters shouldBe Vector(
          classOf[Interpreter],
          preflightClass,
          observerClass,
          classOf[Profiler])
      else
        routeObservedParameters shouldBe Vector(
          preflightClass,
          observerClass,
          classOf[Profiler])

      val ordinaryContinuationEntries = classOf[Interpreter].getDeclaredMethods
        .filter(_.getName == "continueFullReduction")
      ordinaryContinuationEntries should have length 1
      ordinaryContinuationEntries.head.getParameterTypes.toVector shouldBe
        Vector(preflightClass)
      val ordinaryClosureMethods = classOf[Interpreter].getDeclaredMethods
        .filter(_.getName == "$anonfun$continueFullReduction$1")
      if (scala.util.Properties.versionNumberString.startsWith("2.11.")) {
        val ordinaryClosure = Class.forName(
          "sigmastate.interpreter.Interpreter$$anonfun$continueFullReduction$1")
        ordinaryClosure.getDeclaredFields.foreach { field =>
          observerClass.isAssignableFrom(field.getType) shouldBe false
          threadLocalClass.isAssignableFrom(field.getType) shouldBe false
        }
        ordinaryClosure.getDeclaredMethods.foreach { method =>
          method.getParameterTypes should not contain observerClass
        }
      }
      else {
        ordinaryClosureMethods should have length 1
        ordinaryClosureMethods.head.getParameterTypes.toVector shouldBe Vector(
          classOf[Interpreter],
          continuationClass,
          preflightClass)
      }

      Vector(
        "continueFullReductionInternal" -> 1,
        "continueClaimedV4" -> 2,
        "enforceStarkAvailability" -> 2,
        "continueDirectV4" -> 4,
        "continueMaterializedV4" -> 3).foreach {
        case (suffix, parameterCount) =>
          ordinaryHelperMethod(suffix, parameterCount)
            .getParameterTypes should not contain observerClass
      }
      classOf[Interpreter].getDeclaredMethods
        .filter(_.getName == "continueFullReduction") should have length 1

      val ordinaryEvaluatorEntries = CErgoTreeEvaluator.getClass
        .getDeclaredMethods
        .filter(_.getName == "evalToCrypto")
      ordinaryEvaluatorEntries should have length 1
      ordinaryEvaluatorEntries.head.getParameterTypes.toVector shouldBe Vector(
        classOf[ErgoLikeContext],
        classOf[ErgoTree],
        classOf[EvalSettings])
      val observedEvaluatorEntries = CErgoTreeEvaluator.getClass
        .getDeclaredMethods
        .filter(_.getName == "evalToCryptoObserved")
      observedEvaluatorEntries shouldBe empty
      CErgoTreeEvaluator.getClass.getDeclaredMethods.filter { method =>
        Modifier.isPublic(method.getModifiers) &&
          method.getParameterTypes.contains(classOf[Profiler])
      } shouldBe empty

      val takeContinuationMethods = preflightClass.getDeclaredMethods
        .filter(_.getName.endsWith("takeContinuation"))
      takeContinuationMethods should have length 1
      takeContinuationMethods.head.getParameterTypes.toVector shouldBe empty
    }
  }
}
