/*
 * SPDX-License-Identifier: MIT
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.eval

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax.TrueSigmaProp
import sigma.stark.profile.RawSealV1Decoder.WrongChunkLength
import sigma.stark.profile.Risc0RawSealVerifier.{ClaimMismatch, TransportRejected, Verified}
import sigma.stark.profile.{ProfileBlake2b256, RawSealV1Decoder, Risc0ClaimBuilder, Risc0ProfilePackageLoader, StarkPreVerifierObserver}
import sigmastate.helpers.ErgoLikeContextTesting
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.{CErgoTreeEvaluator, CostAccumulator}

class Eip0045StockRuntimeNegativePathSpec extends AnyFunSuite with Matchers {
  import StarkVerificationCapability._

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

  private val CanonicalPreVerifierEvents = Vector(
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

  private val CanonicalRoutePrefix =
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
      Vector.fill(RawSealV1Decoder.canonicalChunkLengths.length)(ProofChunkValidated)

  private val CanonicalRouteEvents =
    CanonicalRoutePrefix ++ CanonicalPreVerifierEvents

  private class RecordingObserver extends StarkPreVerifierObserver {
    private val recorded = scala.collection.mutable.ArrayBuffer.empty[String]
    def events: Vector[String] = recorded.toVector

    protected final def record(event: String): Unit = recorded += event

    override def onProofChunkMaterialized(): Unit = record(ProofChunkMaterialized)
    override def onProgramIdMaterialized(): Unit = record(ProgramIdMaterialized)
    override def onApplicationPayloadMaterialized(): Unit =
      record(ApplicationPayloadMaterialized)
    override def onSelfPropositionBytesMaterialized(): Unit =
      record(SelfPropositionBytesMaterialized)
    override def onContractIdBuilt(): Unit = record(ContractIdBuilt)
    override def onStatementBuilt(): Unit = record(StatementBuilt)
    override def onJournalDigestBuilt(): Unit = record(JournalDigestBuilt)
    override def onTaggedStructDigestBuilt(): Unit = record(TaggedStructDigestBuilt)
    override def onOkClaimBuilt(): Unit = record(OkClaimBuilt)
    override def onRawVerifierEntered(): Unit = record(RawVerifierEntered)
  }

  private final class RecordingRouteObserver
      extends RecordingObserver with VerifyStarkEvaluationObserver {
    override def onProfileIdEvaluated(): Unit = record(ProfileIdEvaluated)
    override def onDispatchCharged(): Unit = record(DispatchCharged)
    override def onProfileIdValidated(): Unit = record(ProfileIdValidated)
    override def onProfileIdMaterialized(): Unit = record(ProfileIdMaterialized)
    override def onByteComparison(): Unit = record(ByteCompared)
    override def onEntryComparison(): Unit = record(EntryCompared)
    override def onLookupCompleted(): Unit = record(LookupCompleted)
    override def onActiveLifecycleSelected(): Unit = record(ActiveLifecycleSelected)
    override def onFixedCharged(): Unit = record(FixedCharged)
    override def onProgramIdEvaluated(): Unit = record(ProgramIdEvaluated)
    override def onProgramIdValidated(): Unit = record(ProgramIdValidated)
    override def onApplicationPayloadEvaluated(): Unit =
      record(ApplicationPayloadEvaluated)
    override def onApplicationPayloadValidated(): Unit =
      record(ApplicationPayloadValidated)
    override def onProofChunksEvaluated(): Unit = record(ProofChunksEvaluated)
    override def onProofChunkCountValidated(): Unit =
      record(ProofChunkCountValidated)
    override def onProofChunkValidated(): Unit = record(ProofChunkValidated)
  }

  private final class ObserverSentinel extends RuntimeException

  private final class ThrowingObserver(
      target: String,
      sentinel: ObserverSentinel) extends StarkPreVerifierObserver {
    private def observe(event: String): Unit =
      if (event == target) throw sentinel

    override def onProofChunkMaterialized(): Unit = observe(ProofChunkMaterialized)
    override def onProgramIdMaterialized(): Unit = observe(ProgramIdMaterialized)
    override def onApplicationPayloadMaterialized(): Unit =
      observe(ApplicationPayloadMaterialized)
    override def onSelfPropositionBytesMaterialized(): Unit =
      observe(SelfPropositionBytesMaterialized)
    override def onContractIdBuilt(): Unit = observe(ContractIdBuilt)
    override def onStatementBuilt(): Unit = observe(StatementBuilt)
    override def onJournalDigestBuilt(): Unit = observe(JournalDigestBuilt)
    override def onTaggedStructDigestBuilt(): Unit = observe(TaggedStructDigestBuilt)
    override def onOkClaimBuilt(): Unit = observe(OkClaimBuilt)
    override def onRawVerifierEntered(): Unit = observe(RawVerifierEntered)
  }

  private val PackageRoot = "/stark-kats/eip0045-profile-package/"
  private val DirectRoot = "/stark-kats/eip0045-direct/"
  private val ChainDomainId = Array.tabulate[Byte](ProfileIdBytes)(i => (i + 1).toByte)
  private val ProgramId = Array.tabulate[Byte](ProfileIdBytes)(i => (0xa0 + i).toByte)
  private val Payload = "stock-runtime-negative-path".getBytes(StandardCharsets.UTF_8)
  private val DispatchSentinel = 1709
  private val FixedSentinel = 2903
  private val StaticProfileIdEvalCost = 5

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
  private lazy val retainedClaim = resourceBytes(DirectRoot + "po2-15-claim-digest.bin")

  private def canonicalChunks(bytes: Array[Byte]): Array[Array[Byte]] = {
    val lengths = Array(65535, 65535, 65535, 26063)
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

  private def runtime = Risc0StockProfileRuntime.fromLoadedProfile(loadedProfile) match {
    case Right(value) => value
    case Left(failure) => fail("stock runtime rejected frozen profile: " + failure)
  }

  private final class LegacyRuntime extends StarkProfileRuntime {
    var verifyCalls = 0
    var lastContractId: Array[Byte] = null

    override private[sigma] def profileId: Array[Byte] = loadedProfile.profileId
    override private[sigma] def exactProofBytes: Int = loadedProfile.exactProofBytes
    override private[sigma] def maxApplicationPayloadBytes: Int =
      loadedProfile.maxApplicationPayloadBytes
    override private[sigma] def canonicalProofChunkLengths: Array[Int] =
      RawSealV1Decoder.canonicalChunkLengths
    override private[sigma] def verify(
        chainDomainId: Array[Byte],
        programId: Array[Byte],
        contractId: Array[Byte],
        applicationPayload: Array[Byte],
        proofChunks: Array[Array[Byte]]): Boolean = {
      verifyCalls += 1
      lastContractId = contractId.clone()
      false
    }
  }

  private def capabilityFor(selectedRuntime: StarkProfileRuntime) = {
    val entry = active(selectedRuntime, FixedSentinel) match {
      case Right(value) => value
      case Left(failure) => fail("active entry rejected: " + failure)
    }
    snapshot(
      ChainDomainId,
      protocolGeneration = 10,
      HistoricalBlockValidation,
      DispatchSentinel,
      Vector(entry)) match {
      case Right(value) => value
      case Left(failure) => fail("active snapshot rejected: " + failure)
    }
  }

  private def evaluatorFor(selectedCapability: StarkVerificationCapability): CErgoTreeEvaluator = {
    val settings = CErgoTreeEvaluator.DefaultEvalSettings
    val accumulator = new CostAccumulator(JitCost(0), Some(JitCost.fromBlockCost(1000000)))
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
      selectedCapability)
  }

  private def evaluator(): CErgoTreeEvaluator = evaluatorFor(capabilityFor(runtime))

  private def evalDirect(evaluator: CErgoTreeEvaluator, value: Value[_ <: SType]): Any =
    VersionContext.withVersions(
      VersionContext.StarkVerificationVersion,
      VersionContext.StarkVerificationVersion) {
      evaluator.eval(Map.empty, value)
    }

  private def evalObserved(
      evaluator: CErgoTreeEvaluator,
      value: VerifyStark,
      observer: StarkPreVerifierObserver): Any =
    VersionContext.withVersions(
      VersionContext.StarkVerificationVersion,
      VersionContext.StarkVerificationVersion) {
      value.evalObserved(Map.empty, observer)(evaluator)
    }

  private def evaluateWithCost(
      value: VerifyStark,
      observer: StarkPreVerifierObserver): (Any, Int) = {
    val e = evaluator()
    val before = e.getAccumulatedCost.value
    val result =
      if (observer eq null) evalDirect(e, value)
      else evalObserved(e, value, observer)
    (result, e.getAccumulatedCost.value - before)
  }

  private def proofValue(
      values: Array[Array[Byte]]): Value[SCollection[SCollection[SByte.type]]] =
    ConcreteCollection[SByteArray](
      values.iterator.map(ByteArrayConstant(_)).toIndexedSeq,
      SByteArray).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

  private def node(
      proof: Array[Array[Byte]],
      payload: Array[Byte] = Payload,
      programId: Array[Byte] = ProgramId): VerifyStark =
    VerifyStark(
      proofValue(proof),
      ByteArrayConstant(payload),
      ByteArrayConstant(programId),
      ByteArrayConstant(loadedProfile.profileId))

  private def buildBinding(
      chainDomainId: Array[Byte],
      programId: Array[Byte],
      contractId: Array[Byte],
      payload: Array[Byte]): Risc0ClaimBuilder.Binding =
    Risc0ClaimBuilder.build(
      loadedProfile,
      chainDomainId,
      programId,
      contractId,
      payload) match {
      case Right(value) => value
      case Left(failure) => fail("ErgoStatementV1 construction rejected: " + failure)
    }

  private def flipFirst(value: Array[Byte]): Array[Byte] = {
    val result = value.clone()
    result(0) = (result(0) ^ 1).toByte
    result
  }

  test("retained po2-15 claim accepts before the host-derived claim reaches ClaimMismatch") {
    val proof = canonicalChunks(rawSeal)
    loadedProfile.verifier.verify(proof, retainedClaim) shouldBe Right(Verified(1, 15))

    val e = evaluator()
    val contractId = ProfileBlake2b256.hash(e.context.SELF.propositionBytes.toArray)
    val binding = buildBinding(
      ChainDomainId,
      ProgramId,
      contractId,
      Payload)

    binding.statement.length shouldBe Risc0ClaimBuilder.StatementPrefixBytes + Payload.length
    binding.expectedClaim.sameElements(retainedClaim) shouldBe false

    loadedProfile.verifier.verify(proof, binding.expectedClaim) shouldBe
      Left(ClaimMismatch)

    evalDirect(e, node(proof)) shouldBe false
  }

  test("dispatch and fixed sentinels are charged before program payload and proof children") {
    val inaccessibleBytes = ConstantPlaceholder(0, SByteArray)
    val inaccessibleChunks = ConstantPlaceholder(0, SCollection(SByteArray))
      .asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]
    val call = VerifyStark(
      inaccessibleChunks,
      inaccessibleBytes,
      inaccessibleBytes,
      ByteArrayConstant(loadedProfile.profileId))
    val e = evaluator()
    val before = e.getAccumulatedCost.value

    an[IndexOutOfBoundsException] shouldBe thrownBy(evalDirect(e, call))
    e.getAccumulatedCost.value - before shouldBe
      StaticProfileIdEvalCost + DispatchSentinel + FixedSentinel
  }

  test("one host field mutation at a time changes the claim without changing proof transport") {
    val proof = canonicalChunks(rawSeal)
    loadedProfile.verifier.verify(proof, retainedClaim) shouldBe Right(Verified(1, 15))
    val e = evaluator()
    val contractId = ProfileBlake2b256.hash(e.context.SELF.propositionBytes.toArray)
    val baseline = buildBinding(ChainDomainId, ProgramId, contractId, Payload)
    val mutations = Vector(
      ("chain-domain-id", flipFirst(ChainDomainId), ProgramId, contractId, Payload),
      ("program-id", ChainDomainId, flipFirst(ProgramId), contractId, Payload),
      ("contract-id", ChainDomainId, ProgramId, flipFirst(contractId), Payload),
      ("application-payload", ChainDomainId, ProgramId, contractId, flipFirst(Payload)))

    mutations.foreach { case (label, chain, program, contract, payload) =>
      withClue(label + ": ") {
        val changedFields = Vector(
          !chain.sameElements(ChainDomainId),
          !program.sameElements(ProgramId),
          !contract.sameElements(contractId),
          !payload.sameElements(Payload)).count(identity)
        changedFields shouldBe 1

        val binding = buildBinding(chain, program, contract, payload)
        binding.statement.sameElements(baseline.statement) shouldBe false
        binding.expectedClaim.sameElements(baseline.expectedClaim) shouldBe false
        binding.expectedClaim.sameElements(retainedClaim) shouldBe false

        loadedProfile.verifier.verify(proof, binding.expectedClaim) shouldBe
          Left(ClaimMismatch)
      }
    }
  }

  test("one transport mutation is typed at the decoder and remains false at the active adapter") {
    val proof = canonicalChunks(rawSeal)
    val shortened = proof.map(_.clone())
    shortened(0) = java.util.Arrays.copyOf(shortened(0), shortened(0).length - 1)
    shortened(0).sameElements(proof(0)) shouldBe false
    shortened.iterator.drop(1).zip(proof.iterator.drop(1)).forall {
      case (left, right) => left.sameElements(right)
    } shouldBe true

    loadedProfile.verifier.verify(shortened, retainedClaim) shouldBe
      Left(TransportRejected(WrongChunkLength(0, 65535, 65534)))

    val e = evaluator()
    val before = e.getAccumulatedCost.value
    evalDirect(e, node(shortened)) shouldBe false
    e.getAccumulatedCost.value - before should be >=
      (DispatchSentinel + FixedSentinel)
  }

  test("canonical active path joins route and pre-verifier operation sequences") {
    val proof = canonicalChunks(rawSeal)
    val directEvaluator = evaluator()
    val contractId = ProfileBlake2b256.hash(
      directEvaluator.context.SELF.propositionBytes.toArray)
    val binding = buildBinding(ChainDomainId, ProgramId, contractId, Payload)
    loadedProfile.verifier.verify(proof, binding.expectedClaim) shouldBe Left(ClaimMismatch)

    val observer = new RecordingObserver
    val observed = evaluateWithCost(node(proof), observer)
    val unobserved = evaluateWithCost(node(proof), null)

    observed._1 shouldBe false
    observed shouldBe unobserved
    observer.events shouldBe CanonicalPreVerifierEvents

    val routeObserver = new RecordingRouteObserver
    val routeObserved = evaluateWithCost(node(proof), routeObserver)
    routeObserved shouldBe unobserved
    routeObserver.events shouldBe CanonicalRouteEvents
  }

  test("every program payload and proof shape guard freezes its route prefix") {
    val proof = canonicalChunks(rawSeal)
    val missingLastChunk = proof.take(proof.length - 1)
    val oversizedPayload = new Array[Byte](loadedProfile.maxApplicationPayloadBytes + 1)
    val shortProgramId = java.util.Arrays.copyOf(ProgramId, ProgramId.length - 1)
    val throughFixed = CanonicalRoutePrefix.takeWhile(_ != ProgramIdEvaluated)
    val throughProgramValidation =
      throughFixed ++ Vector(ProgramIdEvaluated, ProgramIdValidated)
    val throughPayloadValidation =
      throughProgramValidation ++
        Vector(ApplicationPayloadEvaluated, ApplicationPayloadValidated)
    val throughProofCountValidation =
      throughPayloadValidation ++
        Vector(ProofChunksEvaluated, ProofChunkCountValidated)
    val baseCases = Vector(
      ("short-program-id",
        node(proof, programId = shortProgramId),
        throughFixed :+ ProgramIdEvaluated),
      ("oversized-payload",
        node(proof, payload = oversizedPayload),
        throughProgramValidation :+ ApplicationPayloadEvaluated),
      ("missing-last-chunk",
        node(missingLastChunk),
        throughPayloadValidation :+ ProofChunksEvaluated))
    val chunkCases = proof.indices.map { index =>
      val shortened = proof.map(_.clone())
      shortened(index) = java.util.Arrays.copyOf(
        shortened(index),
        shortened(index).length - 1)
      ("short-chunk-" + index,
        node(shortened),
        throughProofCountValidation ++ Vector.fill(index)(ProofChunkValidated))
    }
    val cases = baseCases ++ chunkCases

    cases.foreach { case (label, value, expectedRouteEvents) =>
      withClue(label + ": ") {
        val observer = new RecordingObserver
        val observed = evaluateWithCost(value, observer)
        val unobserved = evaluateWithCost(value, null)
        observed._1 shouldBe false
        observed shouldBe unobserved
        observer.events shouldBe empty

        val routeObserver = new RecordingRouteObserver
        val routeObserved = evaluateWithCost(value, routeObserver)
        routeObserved shouldBe unobserved
        routeObserver.events shouldBe expectedRouteEvents
      }
    }
  }

  test("pre-verifier observer exceptions propagate with object identity") {
    val proof = canonicalChunks(rawSeal)
    CanonicalPreVerifierEvents.distinct.foreach { event =>
      withClue(event + ": ") {
        val sentinel = new ObserverSentinel
        val observed = intercept[ObserverSentinel] {
          evalObserved(evaluator(), node(proof), new ThrowingObserver(event, sentinel))
        }
        observed should be theSameInstanceAs sentinel
      }
    }
  }

  test("observer seam preserves legacy descriptors and retains no observer state") {
    val observerClass = classOf[StarkPreVerifierObserver]
    val routeObserverClass = classOf[VerifyStarkEvaluationObserver]
    val threadLocalClass = Class.forName("java.lang.ThreadLocal")
    observerClass.getDeclaredFields.toSeq shouldBe empty
    observerClass.getDeclaredMethods.map(_.getName).toSet shouldBe Set(
      "onProofChunkMaterialized",
      "onProgramIdMaterialized",
      "onApplicationPayloadMaterialized",
      "onSelfPropositionBytesMaterialized",
      "onContractIdBuilt",
      "onStatementBuilt",
      "onJournalDigestBuilt",
      "onTaggedStructDigestBuilt",
      "onOkClaimBuilt",
      "onRawVerifierEntered")
    observerClass.getDeclaredMethods.foreach { method =>
      method.getParameterTypes.toSeq shouldBe empty
      method.getReturnType shouldBe java.lang.Void.TYPE
    }
    routeObserverClass.getDeclaredFields.toSeq shouldBe empty
    routeObserverClass.getDeclaredMethods.map(_.getName).toSet shouldBe Set(
      "onProfileIdEvaluated",
      "onDispatchCharged",
      "onProfileIdValidated",
      "onProfileIdMaterialized",
      "onLookupCompleted",
      "onActiveLifecycleSelected",
      "onFixedCharged",
      "onProgramIdEvaluated",
      "onProgramIdValidated",
      "onApplicationPayloadEvaluated",
      "onApplicationPayloadValidated",
      "onProofChunksEvaluated",
      "onProofChunkCountValidated",
      "onProofChunkValidated")
    routeObserverClass.getDeclaredMethods.foreach { method =>
      method.getParameterTypes.toSeq shouldBe empty
      method.getReturnType shouldBe java.lang.Void.TYPE
    }

    Seq(
      classOf[VerifyStark],
      classOf[Risc0StockProfileRuntime],
      Risc0ClaimBuilder.getClass,
      classOf[Risc0ClaimBuilder.Binding]).foreach { clazz =>
      val retained = clazz.getDeclaredFields.filter { field =>
        val lowerName = field.getName.toLowerCase(java.util.Locale.ROOT)
        observerClass.isAssignableFrom(field.getType) ||
          threadLocalClass.isAssignableFrom(field.getType) ||
          lowerName.contains("observer") || lowerName.contains("probe")
      }
      withClue(clazz.getName + ": ") {
        retained.toSeq shouldBe empty
      }
    }

    val legacyEval = classOf[VerifyStark].getDeclaredMethods.filter(_.getName == "eval")
    legacyEval should have length 1
    legacyEval.head.getParameterTypes should have length 2
    val observedEval = classOf[VerifyStark].getDeclaredMethods
      .filter(_.getName == "evalObserved")
    observedEval should have length 1
    observedEval.head.getParameterTypes should have length 3
    observedEval.head.getParameterTypes.apply(1) shouldBe observerClass

    val runtimeVerify = classOf[StarkProfileRuntime].getDeclaredMethods
      .filter(_.getName == "verify")
    runtimeVerify should have length 1
    runtimeVerify.head.getParameterTypes should have length 5
    runtimeVerify.head.getReturnType shouldBe java.lang.Boolean.TYPE
    classOf[StarkProfileRuntime].getDeclaredMethods
      .filter(_.getName == "verifyObserved") shouldBe empty

    val claimBuildArities = Risc0ClaimBuilder.getClass.getDeclaredMethods
      .filter(_.getName == "build")
      .map(_.getParameterTypes.length)
      .sorted
      .toSeq
    claimBuildArities shouldBe Seq(5, 6)

    val observedSeams = Seq(
      classOf[VerifyStark] -> "evalObserved",
      classOf[Risc0StockProfileRuntime] -> "verifyObserved",
      Risc0ClaimBuilder.getClass -> "buildObserved")
    observedSeams.foreach { case (clazz, methodName) =>
      clazz.getDeclaredMethods.exists { method =>
        method.getName.startsWith(methodName + "$default$")
      } shouldBe false
    }
    an[ClassNotFoundException] shouldBe thrownBy(
      Class.forName("sigma.stark.profile.StarkPreVerifierObserver$"))
    an[ClassNotFoundException] shouldBe thrownBy(
      Class.forName("sigma.ast.VerifyStarkEvaluationObserver$"))
  }

  test("legacy runtime shape needs only the historical verify method") {
    val legacyRuntime = new LegacyRuntime
    val proof = canonicalChunks(rawSeal)
    val directEvaluator = evaluatorFor(capabilityFor(legacyRuntime))
    val expectedContractId = ProfileBlake2b256.hash(
      directEvaluator.context.SELF.propositionBytes.toArray)

    evalDirect(
      directEvaluator,
      node(proof)) shouldBe false
    legacyRuntime.verifyCalls shouldBe 1
    legacyRuntime.lastContractId should contain theSameElementsInOrderAs expectedContractId

    val observer = new RecordingObserver
    evalObserved(
      evaluatorFor(capabilityFor(legacyRuntime)),
      node(proof),
      observer) shouldBe false
    legacyRuntime.verifyCalls shouldBe 2
    legacyRuntime.lastContractId should contain theSameElementsInOrderAs expectedContractId
    observer.events shouldBe CanonicalPreVerifierEvents.take(8)

    val routeObserver = new RecordingRouteObserver
    evalObserved(
      evaluatorFor(capabilityFor(legacyRuntime)),
      node(proof),
      routeObserver) shouldBe false
    legacyRuntime.verifyCalls shouldBe 3
    routeObserver.events shouldBe
      (CanonicalRoutePrefix ++ CanonicalPreVerifierEvents.take(8))
  }
}
