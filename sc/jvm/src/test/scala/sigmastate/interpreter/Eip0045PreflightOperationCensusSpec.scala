package sigmastate.interpreter

import java.lang.reflect.{InvocationTargetException, Method, Modifier}
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import org.ergoplatform.validation.ValidationRules.{CheckValidOpCode, currentSettings}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax.{SigmaPropValue, TrueSigmaProp}
import sigma.data.TrivialProp
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.eval.StarkVerificationCapability.{ProfileIdBytes, Unavailable}
import sigma.exceptions.{CostLimitException, OpcodeUnavailableException}
import sigma.interpreter.ContextExtension
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.{CaseObjectSerialization, ValueSerializer}
import sigma.validation.ChangedRule
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers.createBox
import sigmastate.interpreter.Interpreter.{StaticStarkProfileId, StarkPreflightOperationObserver}

class Eip0045PreflightOperationCensusSpec extends AnyFunSuite with Matchers {
  private val NodeInspected = "node-inspected"
  private val ChildrenRead = "children-read"
  private val FramePushed = "frame-pushed"
  private val NodeReused = "node-reused"
  private val NodeRebuilt = "node-rebuilt"
  private val ProfileIdClassified = "profile-id-classified"
  private val PlanBuilt = "plan-built"

  private val CanonicalDirectEvents = Vector(
    NodeInspected, ChildrenRead, FramePushed,
    NodeInspected, ChildrenRead, FramePushed,
    NodeInspected, ChildrenRead, FramePushed,
    NodeInspected, ChildrenRead, ChildrenRead, NodeReused,
    NodeInspected, ChildrenRead,
    NodeInspected, ChildrenRead,
    NodeInspected, ChildrenRead, ChildrenRead, NodeReused, ProfileIdClassified,
    ChildrenRead, NodeReused, PlanBuilt)

  private class RecordingObserver extends StarkPreflightOperationObserver {
    private val recorded = scala.collection.mutable.ArrayBuffer.empty[String]
    def events: Vector[String] = recorded.toVector

    protected def record(event: String): Unit = recorded += event

    override def onNodeInspected(): Unit = record(NodeInspected)
    override def onChildrenRead(): Unit = record(ChildrenRead)
    override def onFramePushed(): Unit = record(FramePushed)
    override def onNodeReused(): Unit = record(NodeReused)
    override def onNodeRebuilt(): Unit = record(NodeRebuilt)
    override def onProfileIdClassified(): Unit = record(ProfileIdClassified)
    override def onPlanBuilt(): Unit = record(PlanBuilt)
  }

  private final class ObserverSentinel extends RuntimeException

  private final class ThrowingObserver(
      target: String,
      sentinel: ObserverSentinel) extends RecordingObserver {
    override protected def record(event: String): Unit =
      if (event == target) throw sentinel else super.record(event)
  }

  private final class CensusInterpreter extends ErgoLikeTestInterpreter {
    def propositionFor(
        tree: ErgoTree,
        context: ErgoLikeContext): SigmaPropValue =
      propositionFromErgoTree(tree, context)
  }

  private val interpreter = new CensusInterpreter

  private def profileId(first: Int): Array[Byte] = {
    val result = new Array[Byte](ProfileIdBytes)
    result(0) = first.toByte
    result
  }

  private def chunks(values: Array[Byte]*): Value[SCollection[SCollection[SByte.type]]] =
    ConcreteCollection[SByteArray](
      values.map(ByteArrayConstant(_)).toIndexedSeq,
      SByteArray).asInstanceOf[Value[SCollection[SCollection[SByte.type]]]]

  private def canonicalNode: VerifyStark =
    VerifyStark(
      chunks(Array[Byte](4)),
      ByteArrayConstant(Array[Byte](7, 8)),
      ByteArrayConstant(profileId(9)),
      ByteArrayConstant(profileId(1)))

  private val FutureBoolCode: OpCode =
    OpCode @@ (TypeCodes.LastConstantCode + 57).toByte

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

  private def v4Tree(proposition: SigmaPropValue): ErgoTree =
    ErgoTree.fromProposition(
      ErgoTree.defaultHeaderWithVersion(VersionContext.StarkVerificationVersion),
      proposition)

  private def contextFor(
      tree: ErgoTree,
      extension: ContextExtension = ContextExtension.empty): ErgoLikeContext =
    ErgoLikeContextTesting.dummy(
      createBox(1000000L, tree),
      activatedVersion = VersionContext.StarkVerificationVersion)
      .withExtension(extension)
      .withStarkVerificationCapability(Unavailable)

  private def successful(
      value: Either[Interpreter.ReductionResult, interpreter.StarkPreflightResult]):
      interpreter.StarkPreflightResult =
    value match {
      case Right(result) => result
      case Left(terminal) => fail("unexpected preflight terminal: " + terminal)
    }

  private def observedPreflight(
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
        case invocation: InvocationTargetException =>
          throw invocation.getCause
      }
    }
  }

  private def observedPreflightMethod: (Method, Boolean) = {
    val helperSuffix = "preflightV4Observed"
    val legacyOwners: Vector[Class[_]] = try {
      Vector(Class.forName("sigmastate.interpreter.Interpreter$class"))
    }
    catch {
      case _: ClassNotFoundException => Vector.empty
    }
    val owners = classOf[Interpreter] +: legacyOwners
    val methods = owners.flatMap(_.getDeclaredMethods).filter { method =>
      val parameters = method.getParameterTypes
      method.getName.endsWith(helperSuffix) &&
        (parameters.length == 4 || parameters.length == 5) &&
        parameters.last == classOf[StarkPreflightOperationObserver] &&
        (parameters.length == 4 || parameters.head == classOf[Interpreter])
    }.distinct
    methods should have length 1
    val method = methods.head
    val receiverFirst = Modifier.isStatic(method.getModifiers)
    method.getParameterTypes should have length (if (receiverFirst) 5 else 4)
    if (receiverFirst)
      method.getParameterTypes.head shouldBe classOf[Interpreter]
    method.setAccessible(true)
    (method, receiverFirst)
  }

  private def staticIds(result: interpreter.StarkPreflightResult): Vector[Vector[Byte]] =
    result.plan.occurrences.map(_.profileId match {
      case static: StaticStarkProfileId => static.bytes.toVector
      case other => fail("expected static profile id, got " + other)
    }).toVector

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

  private def ordinaryHelperMethod(
      suffix: String,
      instanceParameterCount: Int): Method = {
    val observerClass = classOf[StarkPreflightOperationObserver]
    val methods = interpreterMethodOwners.flatMap(_.getDeclaredMethods).filter { method =>
      val parameters = method.getParameterTypes
      val receiverFirst = Modifier.isStatic(method.getModifiers)
      method.getName.endsWith(suffix) &&
        parameters.length == instanceParameterCount + (if (receiverFirst) 1 else 0) &&
        (!receiverFirst || parameters.head == classOf[Interpreter]) &&
        !parameters.contains(observerClass)
    }.distinct
    withClue(suffix + ": ") {
      methods should have length 1
    }
    methods.head
  }

  test("direct four-child materialization freezes the source-level operation sequence") {
    val tree = v4Tree(canonicalNode.toSigmaProp)
    val context = contextFor(tree)
    val observer = new RecordingObserver

    val observed = successful(
      observedPreflight(tree, context, observer))
    val ordinary = successful(interpreter.preflightFullReduction(tree, context))

    observer.events shouldBe CanonicalDirectEvents
    staticIds(observed) shouldBe staticIds(ordinary)
    staticIds(observed) shouldBe Vector(profileId(1).toVector)
    observed.preflightBlockCost shouldBe ordinary.preflightBlockCost
    observed.preflightBlockCost shouldBe context.initCost

    val observedFailure = intercept[OpcodeUnavailableException] {
      interpreter.continueFullReduction(observed)
    }
    val ordinaryFailure = intercept[OpcodeUnavailableException] {
      interpreter.continueFullReduction(ordinary)
    }
    observedFailure.opCode shouldBe ordinaryFailure.opCode
    observedFailure.getMessage shouldBe ordinaryFailure.getMessage
  }

  test("empty-plan verdict and cost are unchanged by observation") {
    val tree = v4Tree(TrueSigmaProp)
    val context = contextFor(tree)
    val observer = new RecordingObserver
    val observed = successful(
      observedPreflight(tree, context, observer))
    val ordinary = successful(interpreter.preflightFullReduction(tree, context))

    observer.events shouldBe Vector(NodeInspected, ChildrenRead, PlanBuilt)
    observed.plan.occurrences shouldBe empty
    ordinary.plan.occurrences shouldBe empty
    observed.preflightBlockCost shouldBe ordinary.preflightBlockCost
    interpreter.continueFullReduction(observed) shouldBe
      interpreter.continueFullReduction(ordinary)
    interpreter.continueFullReduction(successful(
      interpreter.preflightFullReduction(tree, context))).value shouldBe
      TrivialProp.TrueProp
  }

  test("observed deserialize outer cost boundary matches ordinary preflight") {
    val tree = v4Tree(DeserializeRegister(
      ErgoBox.R4,
      SBoolean,
      Some(TrueLeaf)).toSigmaProp)
    val baseContext = contextFor(tree)
    val attemptedCost =
      baseContext.initCost + tree.bytes.length.toLong * interpreter.CostPerTreeByte
    val rejectedContext = baseContext.withCostLimit(attemptedCost - 1L)

    val observedFailure = intercept[CostLimitException] {
      observedPreflight(tree, rejectedContext, new RecordingObserver)
    }
    val ordinaryFailure = intercept[CostLimitException] {
      interpreter.preflightFullReduction(tree, rejectedContext)
    }
    observedFailure.estimatedCost shouldBe ordinaryFailure.estimatedCost
    observedFailure.estimatedCost shouldBe attemptedCost
    observedFailure.getMessage shouldBe ordinaryFailure.getMessage

    val acceptedContext = baseContext.withCostLimit(attemptedCost)
    val observed = successful(
      observedPreflight(tree, acceptedContext, new RecordingObserver))
    val ordinary = successful(
      interpreter.preflightFullReduction(tree, acceptedContext))
    observed.plan.occurrences shouldBe ordinary.plan.occurrences
    observed.preflightBlockCost shouldBe ordinary.preflightBlockCost
    observed.preflightBlockCost shouldBe attemptedCost

    val overflowContext = baseContext
      .withInitCost(Long.MaxValue)
      .withCostLimit(Long.MaxValue)
    val observedOverflow = intercept[ArithmeticException] {
      observedPreflight(tree, overflowContext, new RecordingObserver)
    }
    val ordinaryOverflow = intercept[ArithmeticException] {
      interpreter.preflightFullReduction(tree, overflowContext)
    }
    observedOverflow.getClass shouldBe ordinaryOverflow.getClass
    observedOverflow.getMessage shouldBe ordinaryOverflow.getMessage
  }

  test("observed deserialize soft-fork terminal matches ordinary preflight") {
    val unsupportedBytes = futureBoolBytes()
    val tree = v4Tree(DeserializeContext(2, SBoolean).toSigmaProp)
    val extension = ContextExtension(Map(
      2.toByte -> ByteArrayConstant(unsupportedBytes)))
    val validationSettings = currentSettings.updated(
      CheckValidOpCode.id,
      ChangedRule(Array(FutureBoolCode)))
    val context = contextFor(tree, extension)
      .withValidationSettings(validationSettings)

    val observed = observedPreflight(tree, context, new RecordingObserver)
    val ordinary = interpreter.preflightFullReduction(tree, context)
    (observed, ordinary) match {
      case (Left(observedTerminal), Left(ordinaryTerminal)) =>
        observedTerminal.value shouldBe ordinaryTerminal.value
        observedTerminal.value shouldBe TrivialProp.TrueProp
        observedTerminal.cost shouldBe ordinaryTerminal.cost
      case other => fail("expected matching soft-fork terminals, got " + other)
    }
  }

  test("observer exceptions preserve object identity at every direct event") {
    val tree = v4Tree(canonicalNode.toSigmaProp)
    val context = contextFor(tree)
    val targets = Vector(
      NodeInspected,
      ChildrenRead,
      FramePushed,
      NodeReused,
      ProfileIdClassified,
      PlanBuilt)

    targets.foreach { target =>
      withClue(target + ": ") {
        val sentinel = new ObserverSentinel
        val observed = intercept[ObserverSentinel] {
          observedPreflight(
            tree,
            context,
            new ThrowingObserver(target, sentinel))
        }
        observed should be theSameInstanceAs sentinel
      }
    }

    val selectedBytes = ValueSerializer.serialize(TrueLeaf)
    val rebuiltTree = v4Tree(DeserializeContext(1, SBoolean).toSigmaProp)
    val rebuiltContext = contextFor(
      rebuiltTree,
      ContextExtension(Map(1.toByte -> ByteArrayConstant(selectedBytes))))
    val rebuiltSentinel = new ObserverSentinel
    val rebuiltFailure = intercept[ObserverSentinel] {
      observedPreflight(
        rebuiltTree,
        rebuiltContext,
        new ThrowingObserver(NodeRebuilt, rebuiltSentinel))
    }
    rebuiltFailure should be theSameInstanceAs rebuiltSentinel
  }

  test("observer seam is payload-free and retained by no interpreter state") {
    val observerClass = classOf[StarkPreflightOperationObserver]
    val threadLocalClass = Class.forName("java.lang.ThreadLocal")
    observerClass.getDeclaredFields.toSeq shouldBe empty
    observerClass.getDeclaredMethods.map(_.getName).toSet shouldBe Set(
      "onNodeInspected",
      "onChildrenRead",
      "onFramePushed",
      "onNodeReused",
      "onNodeRebuilt",
      "onProfileIdClassified",
      "onPlanBuilt")
    observerClass.getDeclaredMethods.foreach { method =>
      method.getParameterTypes.toSeq shouldBe empty
      method.getReturnType shouldBe java.lang.Void.TYPE
    }

    (Seq(
      classOf[Interpreter],
      Interpreter.getClass,
      classOf[interpreter.StarkPreflightResult]) ++
      classHierarchy(interpreter.getClass)).distinct.foreach { cls =>
      cls.getDeclaredFields.foreach { field =>
        observerClass.isAssignableFrom(field.getType) shouldBe false
        threadLocalClass.isAssignableFrom(field.getType) shouldBe false
      }
    }
    val updaterClass = Class.forName(
      "sigmastate.interpreter.Interpreter$StarkPreflightContextUpdater")
    updaterClass.getDeclaredFields.foreach { field =>
      observerClass.isAssignableFrom(field.getType) shouldBe false
      threadLocalClass.isAssignableFrom(field.getType) shouldBe false
    }

    val ordinary = classOf[Interpreter].getDeclaredMethods
      .filter(_.getName == "preflightFullReduction")
    ordinary should have length 1
    ordinary.head.getParameterTypes should have length 2
    val ordinaryClosureMethods = classOf[Interpreter].getDeclaredMethods
      .filter(_.getName.startsWith("$anonfun$preflightFullReduction$"))
    if (scala.util.Properties.versionNumberString.startsWith("2.11.")) {
      val ordinaryClosure = Class.forName(
        "sigmastate.interpreter.Interpreter$$anonfun$preflightFullReduction$1")
      ordinaryClosure.getDeclaredFields.foreach { field =>
        observerClass.isAssignableFrom(field.getType) shouldBe false
      }
    }
    else {
      ordinaryClosureMethods should have length 1
      ordinaryClosureMethods.head.getParameterTypes should not contain observerClass
    }
    (interpreterMethodOwners ++ classHierarchy(interpreter.getClass)).distinct
      .foreach { cls =>
        val publicObserved = cls.getDeclaredMethods.filter { method =>
          Modifier.isPublic(method.getModifiers) && method.getName.contains("Observed")
        }
        withClue(cls.getName + ": ") {
          publicObserved shouldBe empty
        }
      }
    val (observed, receiverFirst) = observedPreflightMethod
    val traitObserved = classOf[Interpreter].getDeclaredMethods
      .filter(_.getName == "preflightV4Observed")
    traitObserved.foreach { method =>
      Modifier.isAbstract(method.getModifiers) shouldBe false
    }
    if (!receiverFirst) {
      traitObserved should have length 1
      Modifier.isPrivate(traitObserved.head.getModifiers) shouldBe true
    }
    interpreter.getClass.getDeclaredMethods
      .filter(_.getName == "preflightV4Observed") shouldBe empty
    observed.getParameterTypes should have length (if (receiverFirst) 5 else 4)
    if (receiverFirst)
      observed.getParameterTypes.head shouldBe classOf[Interpreter]
    observed.getParameterTypes.last shouldBe observerClass
  }

  test("ordinary preflight keeps the historical observer-free helper graph") {
    val observerClass = classOf[StarkPreflightOperationObserver]
    Vector(
      "structuralChildren" -> 1,
      "rebuildWithChildren" -> 2,
      "classifyProfileId" -> 2,
      "materializeV4" -> 3,
      "preflightV4" -> 3).foreach { case (suffix, parameterCount) =>
      ordinaryHelperMethod(suffix, parameterCount)
        .getParameterTypes should not contain observerClass
    }

    Vector("fullReduction" -> 3, "preflightFullReduction" -> 2)
      .foreach { case (name, parameterCount) =>
        val entries = classOf[Interpreter].getDeclaredMethods.filter { method =>
          method.getName == name && method.getParameterTypes.length == parameterCount
        }
        withClue(name + ": ") {
          entries should have length 1
        }
        entries.head.getParameterTypes should not contain observerClass
      }

    if (scala.util.Properties.versionNumberString.startsWith("2.11.")) {
      Vector(
        "sigmastate.interpreter.Interpreter$$anonfun$fullReduction$1",
        "sigmastate.interpreter.Interpreter$$anonfun$preflightFullReduction$1")
        .foreach { name =>
          Class.forName(name).getDeclaredFields.foreach { field =>
            observerClass.isAssignableFrom(field.getType) shouldBe false
          }
        }
    }
    else {
      classOf[Interpreter].getDeclaredMethods.filter { method =>
        method.getName.startsWith("$anonfun$fullReduction$") ||
          method.getName.startsWith("$anonfun$preflightFullReduction$")
      }.foreach { method =>
        method.getParameterTypes should not contain observerClass
      }
    }

    (interpreterMethodOwners ++ classHierarchy(interpreter.getClass)).distinct
      .foreach { cls =>
        cls.getDeclaredFields.foreach { field =>
          observerClass.isAssignableFrom(field.getType) shouldBe false
        }
      }
  }
}
