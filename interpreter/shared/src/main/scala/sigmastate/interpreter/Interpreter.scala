package sigmastate.interpreter

import debox.cfor
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.validation.ValidationRules._
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax._
import sigma.ast._
import sigma.data.{CAND, COR, CSigmaProp, CTHRESHOLD, ProveDHTuple, ProveDlog, SigmaBoolean, TrivialProp}
import sigma.kiama.rewriting.Rewriter.{everywherebu, rule, strategy}
import sigma.kiama.rewriting.Strategy
import sigma.serialization.SigSerializer._
import sigma.serialization.{SigSerializer, SigmaSerializer, ValueSerializer}
import sigma.validation.{SigmaValidationSettings, ValidationException}
import sigma.validation.ValidationRules.trySoftForkable
import sigmastate.FiatShamirTree._
import sigmastate._
import sigmastate.crypto.DLogProtocol.{DLogProver, FirstDLogProverMessage}
import sigmastate.crypto._
import sigmastate.eval.{CProfiler, addCostChecked, msgCostLimitError}
import sigmastate.interpreter.CErgoTreeEvaluator.fixedCostOp
import sigmastate.interpreter.Interpreter._
import sigma.ast.syntax.ValueOps
import sigma.eval.StarkVerificationCapability.Unavailable
import sigma.eval.{EvalSettings, Profiler, SigmaDsl}
import sigma.exceptions.{CostLimitException, InterpreterException, OpcodeUnavailableException, StarkOpcodeErgoTreeVersionException}
import sigma.interpreter.ProverResult
import sigma.util.CollectionUtil
import sigma.util.Extensions._
import sigmastate.utils.Helpers._

import scala.util.{Success, Try}
import scala.collection.mutable.ArrayBuffer

/** Base (verifying) interpreter of ErgoTrees.
  * Can perform:
  * - ErgoTree evaluation (aka reduction) to sigma proposition (aka
  *  SigmaBoolean) in the given context.
  * - verification of ErgoTree in the given context.
  *
  * NOTE: In version v5.0 this interpreter contains two alternative implementations.
  * 1) Old implementation from v4.x which is based on AOT costing
  * 2) New implementation added in v5.0 which is based on JIT costing (see methods
  *    with JITC suffix).
  *
  * Both implementations are equivalent in v5.0, but have different performance
  * as result they produce different cost estimations.
  *
  * The interpreter has evaluationMode which defines how it should execute scripts.
  * @see verify, fullReduction
  */
trait Interpreter {

  /** Type of context data used by this interpreter to access blockchain and transaction data. */
  type CTX <: InterpreterContext

  type ProofT = UncheckedTree

  /** Force initialization of reflection. */
  private val _ = InterpreterReflection

  /** Evaluation settings used by [[CErgoTreeEvaluator]] which is used by this
    * interpreter to perform fullReduction.
    */
  protected def evalSettings: EvalSettings = CErgoTreeEvaluator.DefaultEvalSettings

  /** Logs the given message string. Can be overridden in the derived interpreter classes
    * to redefine the default behavior. */
  protected def logMessage(msg: String) = {
    println(msg)
  }

  protected def logMessage(msg: String, t: Throwable) = {
    println(msg)
    t.printStackTrace(System.out)
  }

  /** The cost of Value[T] deserialization is O(n), where n is the length of its bytes
    * array. To evaluate [[DeserializeContext]] and
    * [[DeserializeRegister]] we add the following cost of deserialization
    * for each byte.
    */
  val CostPerByteDeserialized = 2

  /** The cost of substituting [[DeserializeContext]] and
    * [[DeserializeRegister]] nodes with the deserialized expression is
    * O(n), where n is the number of bytes in ErgoTree.
    * The following is the cost added for each ErgoTree.bytes.
    */
  val CostPerTreeByte = 2

  /** Deserializes given script bytes using ValueSerializer (i.e. assuming expression tree format).
    * It also measures tree complexity adding to the total estimated cost of script execution.
    * The new returned context contains increased `initCost` and should be used for further processing.
    *
    * The method SHOULD be called only inside trySoftForkable scope, to make deserialization soft-forkable.
    *
    * NOTE: While ErgoTree is always of type SigmaProp, ValueSerializer can serialize expression of any type.
    * So it cannot be replaced with ErgoTreeSerializer here.
    */
  protected def deserializeMeasured(context: CTX, scriptBytes: Array[Byte]): (CTX, Value[SType]) = {
    val r = SigmaSerializer.startReader(scriptBytes)
    val script = ValueSerializer.deserialize(r)  // Why ValueSerializer? read NOTE above
    val scriptComplexity = java7.compat.Math.multiplyExact(scriptBytes.length, CostPerByteDeserialized)

    val currCost = addCostChecked(context.initCost, scriptComplexity, context.costLimit)
    val ctx1 = context.withInitCost(currCost).asInstanceOf[CTX]
    (ctx1, script)
  }

  /** V4+ deserialization order used by the complete materialization preflight.
    * The byte charge is committed to `updateContext` before parsing so a
    * soft-fork parse terminal cannot discard work which was already admitted.
    */
  protected def deserializeMeasuredV4(
      context: CTX,
      updateContext: CTX => Unit,
      scriptBytes: Array[Byte]): Value[SType] = {
    val scriptComplexity = java7.compat.Math.multiplyExact(
      scriptBytes.length,
      CostPerByteDeserialized)
    val currCost = addCostChecked(context.initCost, scriptComplexity, context.costLimit)
    val ctx1 = context.withInitCost(currCost).asInstanceOf[CTX]
    updateContext(ctx1)

    val r = SigmaSerializer.startReader(scriptBytes)
    ValueSerializer.deserialize(r)
  }

  /** @param updateContext  call back to setup new context (with updated cost limit) to be passed next time */
  protected def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray]@unchecked if eba.tpe == SByteArray =>
            val scriptBytes = eba.value.toArray
            val (ctx1, script) = deserializeMeasured(context, scriptBytes)
            updateContext(ctx1)

            CheckDeserializedScriptType(d, script)
            Some(script)
          case _ =>
            None
        }
      else
        None
    case _ => None
  }

  /** V4+ counterpart of [[substDeserialize]]. Implementations must use
    * [[deserializeMeasuredV4]] so every selected occurrence is charged before
    * its bytes are parsed. A [[DeserializeRegister]] implementation returns
    * only a value selected from the register; its syntactic default is handled
    * centrally by [[materializeV4]] so a selected value cannot hide that
    * default from whole-input preflight.
    */
  protected def substDeserializeV4(
      context: CTX,
      updateContext: CTX => Unit,
      node: SValue): Option[SValue] = node match {
    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray]@unchecked if eba.tpe == SByteArray =>
            val script = deserializeMeasuredV4(context, updateContext, eba.value.toArray)
            CheckDeserializedScriptType(d, script)
            Some(script)
          case _ =>
            None
        }
      else
        None
    case _ => None
  }

  /** Extracts proposition for ErgoTree handling soft-fork condition.
    * @note soft-fork handler */
  protected def propositionFromErgoTree(ergoTree: ErgoTree, context: CTX): SigmaPropValue = {
    val validationSettings = context.validationSettings
    val prop = ergoTree.root match {
      case Right(_) =>
        ergoTree.toProposition(ergoTree.isConstantSegregation)
      case Left(UnparsedErgoTree(_, error)) if validationSettings.isSoftFork(error) =>
        TrueSigmaProp
      case Left(UnparsedErgoTree(_, error)) =>
        throw new InterpreterException(
          "Script has not been recognized due to ValidationException, and it cannot be accepted as soft-fork.", Some(error))
    }
    prop
  }

  private def isTopLevelSoftFork(ergoTree: ErgoTree, context: CTX): Boolean =
    ergoTree.root match {
      case Left(UnparsedErgoTree(_, error)) =>
        context.validationSettings.isSoftFork(error)
      case Right(_) =>
        false
    }

  /** Same as applyDeserializeContext, but returns SigmaPropValue instead of BoolValue.
    * This is necessary because new interpreter, while ultimately produces the same
    * results as the old interpreter, it is implemented differently internally.
    */
  private def applyDeserializeContextJITC(
      context: CTX,
      exp: Value[SType],
      onInserted: SValue => Unit): (SigmaPropValue, CTX) = {
    val currContext = new MutableCell(context)
    val substRule = strategy[Any] { case x: SValue =>
      val replacement = substDeserialize(
        currContext.value,
        { ctx: CTX => currContext.value = ctx },
        x)
      replacement.foreach(onInserted)
      replacement
    }
    val Some(substTree: SValue) = everywherebu(substRule)(exp)
    val res = toValidScriptTypeJITC(substTree)
    (res, currContext.value)
  }

  /** Structurally scans all AST fields, including non-evaluated branches,
    * function bodies and optional deserialization defaults.
    */
  private def containsVerifyStark(root: SValue): Boolean = {
    val pending = ArrayBuffer.empty[SValue]
    pending += root
    while (pending.nonEmpty) {
      val current = pending.remove(pending.length - 1)
      if (current.isInstanceOf[VerifyStark])
        return true

      val children = structuralChildren(current)
      var index = children.length - 1
      while (index >= 0) {
        pending += children(index)
        index -= 1
      }
    }
    false
  }

  private final class MaterializationFrame(
      val original: SValue,
      val children: Array[SValue],
      val starkOccurrenceIndex: Int,
      val selectedResultChild: Int = -1) {
    var nextChild: Int = 0
  }

  private final class StarkPreflightContextUpdater(
      contextCell: MutableCell[CTX]) extends Function1[CTX, Unit] {
    override def apply(updated: CTX): Unit = contextCell.value = updated
  }

  private final class StarkContinuation(
      val ergoTree: ErgoTree,
      val context: CTX,
      val proposition: SigmaPropValue,
      val useDirectErgoTree: Boolean)

  /** Opaque v4+ continuation. The public surface exposes only the immutable
    * structural plan; the charged context and materialized AST remain bound to
    * this interpreter instance and can be consumed exactly once.
    */
  final class StarkPreflightResult private[Interpreter] (
      val plan: StarkPreflightPlan,
      private[Interpreter] val continuation: StarkContinuation) {
    /** Accumulated block-cost units after structural materialization and before
      * evaluator/cryptographic execution. This includes the incoming context
      * cost, the outer substitution charge and every selected byte occurrence.
      */
    val preflightBlockCost: Long = continuation.context.initCost

    private[this] var isPending = true

    private[Interpreter] def takeContinuation(): StarkContinuation = synchronized {
      if (!isPending)
        throw new IllegalStateException("STARK preflight result has already been consumed")
      isPending = false
      continuation
    }
  }

  /** Direct Value children are owned by the same serializer that defines the
    * consensus wire shape. This deliberately excludes Product reflection,
    * generic collection traversal and a second hand-maintained AST schema.
    */
  private def structuralChildren(term: SValue): Array[SValue] =
    ValueSerializer
      .getSerializer(term.opCode)
      .valueChildren(term)
      .map(_.asInstanceOf[SValue])
      .toArray

  private def rebuildWithChildren(
      original: SValue,
      children: Array[SValue]): SValue = {
    val serializer = ValueSerializer.getSerializer(original.opCode)
    val originalChildren = serializer.valueChildren(original)
    var unchanged = originalChildren.length == children.length
    var i = 0
    while (unchanged && i < children.length) {
      unchanged = originalChildren(i).asInstanceOf[AnyRef] eq
        children(i).asInstanceOf[AnyRef]
      i += 1
    }
    // Keeping an untouched node is both allocation-free and semantically
    // important: structural preflight may classify an already constructed
    // malformed shape without asking a validating deserialization builder to
    // accept that shape a second time. Any actual child replacement still
    // passes through the serializer-owned checked rebuild path.
    if (unchanged) original
    else
      serializer
        .rebuildValue(original, children.toIndexedSeq)
        .asInstanceOf[SValue]
  }

  private def classifyProfileId(
      profileId: SValue,
      constants: IndexedSeq[Constant[SType]]): StarkProfileIdClassification = {
    def classifyConstant(value: SValue): StarkProfileIdClassification = value match {
      case ByteArrayConstant(bytes) if bytes.length == VerifyStark.DigestBytes =>
        StaticStarkProfileId(bytes.toArray)
      case _: Constant[_] =>
        MalformedStarkProfileId
      case _ =>
        MalformedStarkProfileId
    }

    profileId match {
      case constant: Constant[_] =>
        classifyConstant(constant)
      case placeholder: ConstantPlaceholder[_] =>
        if (placeholder.tpe != SByteArray ||
            placeholder.id < 0 || placeholder.id >= constants.length)
          MalformedStarkProfileId
        else
          classifyConstant(constants(placeholder.id))
      case _ =>
        DynamicStarkProfileId
    }
  }

  /** Iterative, occurrence-based v4+ materialization. There is deliberately no
    * identity set or cycle/depth cap: a cyclic byte expansion creates fresh
    * charged occurrences until the ordinary cost limit rejects it.
    */
  private def materializeV4(
      root: SValue,
      contextCell: MutableCell[CTX],
      constants: IndexedSeq[Constant[SType]]): (SValue, StarkPreflightPlan) = {
    val stack = ArrayBuffer.empty[MaterializationFrame]
    val classifications = ArrayBuffer.empty[StarkProfileIdClassification]
    var current: SValue = root
    var finalResult: SValue = null
    var finished = false

    while (!finished) {
      var inspectReplacement = true
      while (inspectReplacement) {
        val registerDefault = current match {
          case d: DeserializeRegister[_] =>
            d.default.map(_.asInstanceOf[SValue])
          case _ =>
            None
        }
        substDeserializeV4(
          contextCell.value,
          { updated: CTX => contextCell.value = updated },
          current) match {
          case Some(replacement) =>
            registerDefault match {
              case Some(defaultValue) =>
                // Register bytes determine the value retained in the
                // materialized AST, but the syntactic default remains part
                // of whole-input closure. The selected bytes have already
                // been charged and parsed; materialize the shadowed default
                // first, then the selected subtree, and retain child 1.
                stack += new MaterializationFrame(
                  current,
                  Array[SValue](defaultValue, replacement),
                  starkOccurrenceIndex = -1,
                  selectedResultChild = 1)
                current = defaultValue
              case None =>
                current = replacement
            }
          case None =>
            registerDefault match {
              case Some(defaultValue) =>
                // Missing or wrong-typed register input selects the
                // ordinary syntactic default as the materialized value.
                current = defaultValue
              case None =>
                inspectReplacement = false
            }
        }
      }

      val occurrenceIndex = current match {
        case _: VerifyStark =>
          val index = classifications.length
          // Filled when this occurrence has been completely materialized, so
          // a profileId supplied by nested deserialization can become static.
          classifications += null
          index
        case _ =>
          -1
      }

      val children = structuralChildren(current)
      if (children.nonEmpty) {
        val frame = new MaterializationFrame(current, children, occurrenceIndex)
        stack += frame
        current = children(0)
      }
      else {
        var completed = current
        if (occurrenceIndex >= 0) {
          val call = completed.asInstanceOf[VerifyStark]
          classifications(occurrenceIndex) = classifyProfileId(call.profileId, constants)
        }

        var ascending = true
        while (ascending && !finished) {
          if (stack.isEmpty) {
            finalResult = completed
            finished = true
          }
          else {
            val frame = stack.last
            frame.children(frame.nextChild) = completed
            frame.nextChild += 1
            if (frame.nextChild < frame.children.length) {
              current = frame.children(frame.nextChild)
              ascending = false
            }
            else {
              stack.remove(stack.length - 1)
              completed =
                if (frame.selectedResultChild >= 0)
                  frame.children(frame.selectedResultChild)
                else
                  rebuildWithChildren(frame.original, frame.children)
              if (frame.starkOccurrenceIndex >= 0) {
                val call = completed.asInstanceOf[VerifyStark]
                classifications(frame.starkOccurrenceIndex) =
                  classifyProfileId(call.profileId, constants)
              }
            }
          }
        }
      }
    }

    val occurrences = classifications.iterator.map(StarkPreflightOccurrence).toVector
    (finalResult, StarkPreflightPlan(occurrences))
  }

  private def structuralChildrenObserved(
      term: SValue,
      observer: StarkPreflightOperationObserver): Array[SValue] = {
    val children = structuralChildren(term)
    if (observer ne null)
      observer.onChildrenRead()
    children
  }

  private def rebuildWithChildrenObserved(
      original: SValue,
      children: Array[SValue],
      observer: StarkPreflightOperationObserver): SValue = {
    val serializer = ValueSerializer.getSerializer(original.opCode)
    val originalChildren = serializer.valueChildren(original)
    if (observer ne null)
      observer.onChildrenRead()
    var unchanged = originalChildren.length == children.length
    var i = 0
    while (unchanged && i < children.length) {
      unchanged = originalChildren(i).asInstanceOf[AnyRef] eq
        children(i).asInstanceOf[AnyRef]
      i += 1
    }
    // Keeping an untouched node is both allocation-free and semantically
    // important: structural preflight may classify an already constructed
    // malformed shape without asking a validating deserialization builder to
    // accept that shape a second time. Any actual child replacement still
    // passes through the serializer-owned checked rebuild path.
    if (unchanged) {
      if (observer ne null)
        observer.onNodeReused()
      original
    }
    else {
      val rebuilt = serializer
        .rebuildValue(original, children.toIndexedSeq)
        .asInstanceOf[SValue]
      if (observer ne null)
        observer.onNodeRebuilt()
      rebuilt
    }
  }

  private def classifyProfileIdObserved(
      profileId: SValue,
      constants: IndexedSeq[Constant[SType]],
      observer: StarkPreflightOperationObserver): StarkProfileIdClassification = {
    def classifyConstant(value: SValue): StarkProfileIdClassification = value match {
      case ByteArrayConstant(bytes) if bytes.length == VerifyStark.DigestBytes =>
        StaticStarkProfileId(bytes.toArray)
      case _: Constant[_] =>
        MalformedStarkProfileId
      case _ =>
        MalformedStarkProfileId
    }

    val classification = profileId match {
      case constant: Constant[_] =>
        classifyConstant(constant)
      case placeholder: ConstantPlaceholder[_] =>
        if (placeholder.tpe != SByteArray ||
            placeholder.id < 0 || placeholder.id >= constants.length)
          MalformedStarkProfileId
        else
          classifyConstant(constants(placeholder.id))
      case _ =>
        DynamicStarkProfileId
    }
    if (observer ne null)
      observer.onProfileIdClassified()
    classification
  }

  /** Iterative, occurrence-based v4+ materialization. There is deliberately no
    * identity set or cycle/depth cap: a cyclic byte expansion creates fresh
    * charged occurrences until the ordinary cost limit rejects it.
    */
  private def materializeV4Observed(
      root: SValue,
      contextCell: MutableCell[CTX],
      constants: IndexedSeq[Constant[SType]],
      observer: StarkPreflightOperationObserver): (SValue, StarkPreflightPlan) = {
    val stack = ArrayBuffer.empty[MaterializationFrame]
    val classifications = ArrayBuffer.empty[StarkProfileIdClassification]
    var current: SValue = root
    var finalResult: SValue = null
    var finished = false
    val updateContext = new StarkPreflightContextUpdater(contextCell)

    while (!finished) {
      var inspectReplacement = true
      while (inspectReplacement) {
        val registerDefault = current match {
          case d: DeserializeRegister[_] =>
            d.default match {
              case Some(value) => Some(value.asInstanceOf[SValue])
              case None => None
            }
          case _ =>
            None
        }
        substDeserializeV4(
          contextCell.value,
          updateContext,
          current) match {
          case Some(replacement) =>
            registerDefault match {
              case Some(defaultValue) =>
                // Register bytes determine the value retained in the
                // materialized AST, but the syntactic default remains part
                // of whole-input closure. The selected bytes have already
                // been charged and parsed; materialize the shadowed default
                // first, then the selected subtree, and retain child 1.
                stack += new MaterializationFrame(
                  current,
                  Array[SValue](defaultValue, replacement),
                  starkOccurrenceIndex = -1,
                  selectedResultChild = 1)
                if (observer ne null)
                  observer.onFramePushed()
                current = defaultValue
              case None =>
                current = replacement
            }
          case None =>
            registerDefault match {
              case Some(defaultValue) =>
                // Missing or wrong-typed register input selects the
                // ordinary syntactic default as the materialized value.
                current = defaultValue
              case None =>
                inspectReplacement = false
            }
        }
      }

      if (observer ne null)
        observer.onNodeInspected()
      val occurrenceIndex = current match {
        case _: VerifyStark =>
          val index = classifications.length
          // Filled when this occurrence has been completely materialized, so
          // a profileId supplied by nested deserialization can become static.
          classifications += null
          index
        case _ =>
          -1
      }

      val children = structuralChildrenObserved(current, observer)
      if (children.nonEmpty) {
        val frame = new MaterializationFrame(current, children, occurrenceIndex)
        stack += frame
        if (observer ne null)
          observer.onFramePushed()
        current = children(0)
      }
      else {
        var completed = current
        if (occurrenceIndex >= 0) {
          val call = completed.asInstanceOf[VerifyStark]
          classifications(occurrenceIndex) =
            classifyProfileIdObserved(call.profileId, constants, observer)
        }

        var ascending = true
        while (ascending && !finished) {
          if (stack.isEmpty) {
            finalResult = completed
            finished = true
          }
          else {
            val frame = stack.last
            frame.children(frame.nextChild) = completed
            frame.nextChild += 1
            if (frame.nextChild < frame.children.length) {
              current = frame.children(frame.nextChild)
              ascending = false
            }
            else {
              stack.remove(stack.length - 1)
              completed =
                if (frame.selectedResultChild >= 0)
                  frame.children(frame.selectedResultChild)
                else
                  rebuildWithChildrenObserved(frame.original, frame.children, observer)
              if (frame.starkOccurrenceIndex >= 0) {
                val call = completed.asInstanceOf[VerifyStark]
                classifications(frame.starkOccurrenceIndex) =
                  classifyProfileIdObserved(call.profileId, constants, observer)
              }
            }
          }
        }
      }
    }

    val occurrences = ArrayBuffer.empty[StarkPreflightOccurrence]
    var occurrenceIndex = 0
    while (occurrenceIndex < classifications.length) {
      occurrences += StarkPreflightOccurrence(classifications(occurrenceIndex))
      occurrenceIndex += 1
    }
    val plan = StarkPreflightPlan(occurrences.toVector)
    if (observer ne null)
      observer.onPlanBuilt()
    (finalResult, plan)
  }

  private def rejectLegacyVerifyStark(ergoTreeVersion: Byte): Nothing =
    throw new StarkOpcodeErgoTreeVersionException(
      ergoTreeVersion.toInt,
      Interpreter.VerifyStarkMinErgoTreeVersion,
      s"VerifyStark requires ErgoTree version ${Interpreter.VerifyStarkMinErgoTreeVersion} or higher; got $ergoTreeVersion")

  private def rejectUnavailableVerifyStark(): Nothing =
    throw new OpcodeUnavailableException(
      VerifyStark.opCode.toByte & 0xff,
      "Opcode 0xB9 is unavailable before network activation")

  /** This method uses the new JIT costing with direct ErgoTree execution. It is used in
    * both prover and verifier to compute SigmaProp value.
    * As the first step the cost of computing the `exp` expression in the given context is
    * estimated.
    * If cost is above limit then exception is returned and `exp` is not executed
    * else `exp` is computed in the given context and the resulting SigmaBoolean returned.
    *
    * @param context        the context in which `exp` should be executed
    * @param exp            expression to be executed in the given `context`
    * @return result of script reduction
    * @see `ReductionResult`
    */
  protected def reduceToCryptoJITC(context: CTX, exp: SigmaPropValue): Try[ReductionResult] = Try {
    implicit val vs = context.validationSettings
    trySoftForkable[ReductionResult](whenSoftFork = WhenSoftForkReductionResult(context.initCost)) {

      val (resProp, cost) = {
        val ctx = context.asInstanceOf[ErgoLikeContext]
        CErgoTreeEvaluator.eval(ctx, ErgoTree.EmptyConstants, exp, evalSettings) match {
          case (p: sigma.SigmaProp, c) => (p, c)
          case (res, _) =>
            sys.error(s"Invalid result type of $res: expected SigmaProp when evaluating $exp")
        }
      }

      ReductionResult(SigmaDsl.toSigmaBoolean(resProp), cost.toLong)
    }
  }

  /** Full reduction of contract proposition given in the ErgoTree form to a SigmaBoolean value
    * which encodes either a sigma-protocol proposition or a boolean (true or false) value.
    *
    * Works as follows:
    * 1) parse ErgoTree instance into a typed AST
    * 2) go bottom-up the tree to replace DeserializeContext nodes only
    * 3) estimate cost and reduce the AST to a SigmaBoolean instance (either sigma-tree or
    * trivial boolean value)
    *
    * @param ergoTree input ErgoTree expression to reduce
    * @param ctx      context used in reduction
    * @param env      script environment
    * @return reduction result as a pair of sigma boolean and the accumulated cost counter
    *         after reduction
    */
  def fullReduction(ergoTree: ErgoTree,
                    ctx: CTX,
                    env: ScriptEnv): ReductionResult = {
    val context = ctx.withErgoTreeVersion(ergoTree.version).asInstanceOf[CTX]
    VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
      val topLevelSoftFork = isTopLevelSoftFork(ergoTree, context)
      val prop = propositionFromErgoTree(ergoTree, context)
      val isLegacyStarkVersion =
        ergoTree.version < Interpreter.VerifyStarkMinErgoTreeVersion
      if (isLegacyStarkVersion) {
        val seenVerifyStark = containsVerifyStark(prop)
        prop match {
          case SigmaPropConstant(p) =>
            if (seenVerifyStark)
              rejectLegacyVerifyStark(ergoTree.version)
            reduceSigmaPropConstant(context, p)
          case _ if !ergoTree.hasDeserialize =>
            if (seenVerifyStark)
              rejectLegacyVerifyStark(ergoTree.version)
            val ergoContext = context.asInstanceOf[ErgoLikeContext]
            VersionContext.withVersions(ergoContext.activatedScriptVersion, ergoTree.version) {
              CErgoTreeEvaluator.evalToCrypto(ergoContext, ergoTree, evalSettings)
            }
          case _ =>
            reductionWithDeserialize(ergoTree, prop, context, env, seenVerifyStark)
        }
      }
      else if (topLevelSoftFork) {
        // Keep the historical true/outcome and constant-evaluation cost, but
        // expose a global unparsed soft fork as a terminal rather than a
        // forgeable empty continuation.
        reduceTrueSigmaPropConstant(context)
      }
      else {
        preflightV4(ergoTree, prop, context) match {
          case Left(terminal) => terminal
          case Right(preflight) => continueFullReductionInternal(preflight)
        }
      }
    }
  }

  private def reduceSigmaPropConstant(context: CTX, proposition: sigma.SigmaProp): ReductionResult = {
    val sb = SigmaDsl.toSigmaBoolean(proposition)

    // NOTE, evaluator cost unit needs to be scaled to the cost unit of context
    val evalCost = Eval_SigmaPropConstant.costKind.cost.toBlockCost
    val resCost = addCostChecked(context.initCost, evalCost, context.costLimit)
    ReductionResult(sb, resCost)
  }

  private def reduceTrueSigmaPropConstant(context: CTX): ReductionResult =
    reduceSigmaPropConstant(context, TrueSigmaProp.value)

  /** Performs the complete v4+ structural phase without evaluating user logic
    * or invoking a STARK runtime. A soft-fork terminal is returned as `Left`;
    * only a complete materialization produces an inspectable continuation.
    */
  def preflightFullReduction(
      ergoTree: ErgoTree,
      ctx: CTX): Either[ReductionResult, StarkPreflightResult] = {
    if (ergoTree.version < Interpreter.VerifyStarkMinErgoTreeVersion)
      throw new IllegalArgumentException(
        s"STARK preflight requires ErgoTree version ${Interpreter.VerifyStarkMinErgoTreeVersion} or higher; got ${ergoTree.version}")

    // This public entry point must preserve the same outer version gate as
    // verify(). In particular, callers cannot use preflight + continuation to
    // execute v4 before activation, and stale nodes retain the ordinary
    // future-version terminal without receiving a continuation token.
    checkSoftForkCondition(ergoTree, ctx) match {
      case Some((accepted, cost)) =>
        Left(ReductionResult(
          if (accepted) TrivialProp.TrueProp else TrivialProp.FalseProp,
          cost))
      case None =>
        val context = ctx.withErgoTreeVersion(ergoTree.version).asInstanceOf[CTX]
        VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
          if (isTopLevelSoftFork(ergoTree, context)) {
            // Global unparsed soft forks are ordinary terminal reductions. They do
            // not expose an empty plan or a continuation token.
            Left(reduceTrueSigmaPropConstant(context))
          }
          else {
            val prop = propositionFromErgoTree(ergoTree, context)
            preflightV4(ergoTree, prop, context)
          }
      }
    }
  }

  /** Consumes a successful v4+ preflight exactly once. Availability is checked
    * against the complete plan before any evaluator or cryptographic runtime is
    * entered. Generation/purpose admission remains a trusted node policy.
    */
  def continueFullReduction(preflight: StarkPreflightResult): ReductionResult = {
    val continuation = preflight.takeContinuation()
    VersionContext.withVersions(
        continuation.context.activatedScriptVersion,
        continuation.ergoTree.version) {
      continueClaimedV4(continuation, preflight.plan)
    }
  }

  private def continueFullReductionInternal(
      preflight: StarkPreflightResult): ReductionResult = {
    val continuation = preflight.takeContinuation()
    continueClaimedV4(continuation, preflight.plan)
  }

  private def continueClaimedV4(
      continuation: StarkContinuation,
      plan: StarkPreflightPlan): ReductionResult = {
    if (continuation.useDirectErgoTree)
      continueDirectV4(
        continuation.ergoTree,
        continuation.context,
        continuation.proposition,
        plan)
    else
      continueMaterializedV4(
        continuation.context,
        continuation.proposition,
        plan)
  }

  private def enforceStarkAvailability(context: CTX, plan: StarkPreflightPlan): Unit =
    if (plan.occurrences.nonEmpty && context.starkVerificationCapability == Unavailable)
      rejectUnavailableVerifyStark()

  private def continueDirectV4(
      ergoTree: ErgoTree,
      context: CTX,
      proposition: SigmaPropValue,
      plan: StarkPreflightPlan): ReductionResult = {
    enforceStarkAvailability(context, plan)
    proposition match {
      case SigmaPropConstant(p) =>
        reduceSigmaPropConstant(context, p)
      case _ =>
        val ergoContext = context.asInstanceOf[ErgoLikeContext]
        CErgoTreeEvaluator.evalToCrypto(ergoContext, ergoTree, evalSettings)
    }
  }

  private def continueMaterializedV4(
      context: CTX,
      proposition: SigmaPropValue,
      plan: StarkPreflightPlan): ReductionResult = {
    enforceStarkAvailability(context, plan)
    reduceToCryptoJITC(context, proposition).getOrThrow
  }

  private def continueClaimedV4Observed(
      preflight: StarkPreflightResult,
      observer: StarkPreflightContinuationObserver): ReductionResult = {
    val continuation = preflight.takeContinuation()
    observer.onContinuationTaken()
    val useDirectErgoTree = continuation.useDirectErgoTree
    if (useDirectErgoTree)
      observer.onDirectPathSelected()
    else
      observer.onMaterializedPathSelected()
    observer.onAvailabilityChecked()
    enforceStarkAvailability(continuation.context, preflight.plan)
    observer.onAvailabilityPassed()

    if (useDirectErgoTree) {
      continuation.proposition match {
        case SigmaPropConstant(p) =>
          observer.onConstantReductionEntered()
          reduceSigmaPropConstant(continuation.context, p)
        case _ =>
          val ergoContext = continuation.context.asInstanceOf[ErgoLikeContext]
          observer.onDirectEvaluatorEntered()
          CErgoTreeEvaluator.evalToCrypto(
            ergoContext,
            continuation.ergoTree,
            evalSettings)
      }
    }
    else {
      observer.onJitReductionEntered()
      reduceToCryptoJITC(
        continuation.context,
        continuation.proposition).getOrThrow
    }
  }

  /** Validation-only join from the observed direct continuation boundary into
    * the evaluator's existing per-call profiler carrier. The ordinary helper
    * graph remains observer-free.
    */
  private def continueClaimedV4RouteObserved(
      preflight: StarkPreflightResult,
      observer: StarkPreflightContinuationObserver,
      evaluatorProfiler: Profiler): ReductionResult = {
    val continuation = preflight.takeContinuation()
    observer.onContinuationTaken()
    val useDirectErgoTree = continuation.useDirectErgoTree
    if (useDirectErgoTree)
      observer.onDirectPathSelected()
    else
      observer.onMaterializedPathSelected()
    observer.onAvailabilityChecked()
    enforceStarkAvailability(continuation.context, preflight.plan)
    observer.onAvailabilityPassed()

    if (useDirectErgoTree) {
      continuation.proposition match {
        case SigmaPropConstant(p) =>
          observer.onConstantReductionEntered()
          reduceSigmaPropConstant(continuation.context, p)
        case _ =>
          val ergoContext = continuation.context.asInstanceOf[ErgoLikeContext]
          observer.onDirectEvaluatorEntered()
          val costAccumulator = new CostAccumulator(
            initialCost = JitCost.fromBlockCost(ergoContext.initCost.toIntExact),
            costLimit = Some(JitCost.fromBlockCost(ergoContext.costLimit.toIntExact)))
          val evaluator = new CErgoTreeEvaluator(
            ergoContext.toSigmaContext(),
            continuation.ergoTree.constants,
            costAccumulator,
            evaluatorProfiler,
            evalSettings,
            ergoContext.starkVerificationCapability)
          val result = evaluator.eval(
            Map(),
            continuation.ergoTree.toProposition(replaceConstants = false))
          val sigmaBoolean = result match {
            case sigmaProp: CSigmaProp => sigmaProp.wrappedValue
            case value: SigmaBoolean  => value
            case _ => sys.error(s"Expected SigmaBoolean but was: $result")
          }
          ReductionResult(
            sigmaBoolean,
            costAccumulator.totalCost.toBlockCost)
      }
    }
    else {
      observer.onJitReductionEntered()
      try {
        val ergoContext = continuation.context.asInstanceOf[ErgoLikeContext]
        val costAccumulator = new CostAccumulator(
          initialCost = JitCost.fromBlockCost(ergoContext.initCost.toIntExact),
          costLimit = Some(JitCost.fromBlockCost(ergoContext.costLimit.toIntExact)))
        val evaluator = new CErgoTreeEvaluator(
          ergoContext.toSigmaContext(),
          ErgoTree.EmptyConstants,
          costAccumulator,
          evaluatorProfiler,
          evalSettings,
          ergoContext.starkVerificationCapability)
        val result = evaluator.eval(Map(), continuation.proposition)
        val sigmaProp = result match {
          case value: sigma.SigmaProp => value
          case value =>
            sys.error(
              s"Invalid result type of $value: expected SigmaProp when evaluating ${continuation.proposition}")
        }
        ReductionResult(
          SigmaDsl.toSigmaBoolean(sigmaProp),
          costAccumulator.totalCost.toBlockCost.toLong)
      }
      catch {
        case error: ValidationException =>
          if (continuation.context.validationSettings.isSoftFork(error))
            WhenSoftForkReductionResult(continuation.context.initCost)
          else
            throw error
      }
    }
  }

  private def preflightV4(
      ergoTree: ErgoTree,
      proposition: SigmaPropValue,
      context: CTX): Either[ReductionResult, StarkPreflightResult] = {
    val contextCell = new MutableCell(context)
    if (!ergoTree.hasDeserialize) {
      val (materialized, plan) = materializeV4(
        proposition,
        contextCell,
        ergoTree.constants)
      val validProposition = toValidScriptTypeJITC(materialized)
      Right(new StarkPreflightResult(
        plan,
        new StarkContinuation(
          ergoTree,
          contextCell.value,
          validProposition,
          useDirectErgoTree = true)))
    }
    else {
      implicit val vs: SigmaValidationSettings = context.validationSettings
      val deserializeSubstitutionCost = java7.compat.Math.multiplyExact(
        ergoTree.bytes.length,
        CostPerTreeByte)
      val currCost = addCostChecked(
        context.initCost,
        deserializeSubstitutionCost,
        context.costLimit)
      contextCell.value = context.withInitCost(currCost).asInstanceOf[CTX]

      val materialized = trySoftForkable[
        Either[ReductionResult, (SigmaPropValue, StarkPreflightPlan)]](
        whenSoftFork = Left(WhenSoftForkReductionResult(contextCell.value.initCost))) {
        val (root, plan) = materializeV4(
          proposition,
          contextCell,
          ergoTree.constants)
        Right((toValidScriptTypeJITC(root), plan))
      }

      materialized match {
        case Left(terminal) => Left(terminal)
        case Right((root, plan)) =>
          Right(new StarkPreflightResult(
            plan,
            new StarkContinuation(
              ergoTree,
              contextCell.value,
              root,
              useDirectErgoTree = false)))
      }
    }
  }

  private def preflightV4Observed(
      ergoTree: ErgoTree,
      proposition: SigmaPropValue,
      context: CTX,
      observer: StarkPreflightOperationObserver): Either[ReductionResult, StarkPreflightResult] = {
    val contextCell = new MutableCell(context)
    if (!ergoTree.hasDeserialize) {
      val materializedResult = materializeV4Observed(
        proposition,
        contextCell,
        ergoTree.constants,
        observer)
      val materialized = materializedResult._1
      val plan = materializedResult._2
      val validProposition = toValidScriptTypeJITC(materialized)
      Right(new StarkPreflightResult(
        plan,
        new StarkContinuation(
          ergoTree,
          contextCell.value,
          validProposition,
          useDirectErgoTree = true)))
    }
    else {
      implicit val vs: SigmaValidationSettings = context.validationSettings
      val deserializeSubstitutionCost = java7.compat.Math.multiplyExact(
        ergoTree.bytes.length,
        CostPerTreeByte)
      val currCost = java7.compat.Math.addExact(
        context.initCost,
        deserializeSubstitutionCost)
      if (currCost > context.costLimit)
        throw new CostLimitException(
          currCost,
          msgCostLimitError(currCost, context.costLimit))
      contextCell.value = context.withInitCost(currCost).asInstanceOf[CTX]

      val materialized: Either[
          ReductionResult,
          (SigmaPropValue, StarkPreflightPlan)] = try {
        val materializedResult = materializeV4Observed(
          proposition,
          contextCell,
          ergoTree.constants,
          observer)
        val root = materializedResult._1
        val plan = materializedResult._2
        Right((toValidScriptTypeJITC(root), plan))
      }
      catch {
        case error: ValidationException =>
          if (vs.isSoftFork(error))
            Left(WhenSoftForkReductionResult(contextCell.value.initCost))
          else
            throw error
      }

      materialized match {
        case Left(terminal) => Left(terminal)
        case Right((root, plan)) =>
          Right(new StarkPreflightResult(
            plan,
            new StarkContinuation(
              ergoTree,
              contextCell.value,
              root,
              useDirectErgoTree = false)))
      }
    }
  }

  /** Full reduction of contract proposition given in the ErgoTree form to a SigmaBoolean value
    * which encodes either a sigma-protocol proposition or a boolean (true or false) value.
    * See other overload for details.
    */
  def fullReduction(ergoTree: ErgoTree, ctx: CTX): ReductionResult = {
    fullReduction(ergoTree, ctx, Interpreter.emptyEnv)
  }

  /** Performs reduction of proposition which contains deserialization operations. */
  private def reductionWithDeserialize(ergoTree: ErgoTree,
                                       prop: SigmaPropValue,
                                       context: CTX,
                                       env: ScriptEnv,
                                       initiallySeenVerifyStark: Boolean): ReductionResult = {
    implicit val vs: SigmaValidationSettings = context.validationSettings
    val res = VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
      val enforceLegacyStarkVersion =
        ergoTree.version < Interpreter.VerifyStarkMinErgoTreeVersion
      val seenVerifyStark = new MutableCell(initiallySeenVerifyStark)
      val deserializeSubstitutionCost = java7.compat.Math.multiplyExact(ergoTree.bytes.length, CostPerTreeByte)
      val currCost = addCostChecked(context.initCost, deserializeSubstitutionCost, context.costLimit)
      val context1 = context.withInitCost(currCost).asInstanceOf[CTX]
      val (propTree, context2) = trySoftForkable[(SigmaPropValue, CTX)](whenSoftFork = (TrueSigmaProp, context1)) {
        // Before V6 protocol activation, the deserialization cost was not added to the total cost
        // Since V6, it is fixed, so deserialization cost is added
        // This change is okay for older clients (< V6), as they are getting cost which is less than for new clients
        // So when new clients are accepting V6 block with deserialization costs added, V5 clients will accept them also
        // And blocks are generated by V6 clients (90+% at least) after V6 activation
        applyDeserializeContextJITC(if (VersionContext.current.isV6Activated) {
          context1
        } else {
          context
        }, prop, { inserted: SValue =>
          if (enforceLegacyStarkVersion && containsVerifyStark(inserted))
            seenVerifyStark.value = true
        })
      }

      // This check deliberately follows the historical substitution pass. It
      // therefore preserves deterministic substitution cost, parse and type
      // failures, while a later ordinary soft-fork fallback cannot erase a
      // VerifyStark already observed in the prepared or inserted structure.
      if (enforceLegacyStarkVersion && seenVerifyStark.value)
        rejectLegacyVerifyStark(ergoTree.version)

      // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
      // and the rest of the verification is also trivial
      reduceToCryptoJITC(context2, propTree).getOrThrow
    }

    res
  }

  /** Adds the cost to verify sigma protocol proposition.
    * This is AOT part of JITC-based interpreter, it predicts the cost of crypto
    * verification, which is asymptotically much faster and protects from spam scripts.
    *
    * @param reductionRes result of JIT-based reduction
    * @param baseCost     base cost of verification prior to calling this method
    * @param costLimit    total cost limit to check and raise exception if exceeded
    * @return computed baseCost + crypto verification cost
    * @throws CostLimitException if cost limit is exceeded
    */
  protected def addCryptoCost(reductionRes: SigmaBoolean, baseCost: Long, costLimit: Long): Long = {
    val cryptoCost = estimateCryptoVerifyCost(reductionRes).toBlockCost // scale JitCost to tx cost

    // Note, baseCost should be already scaled
    val fullCost = addCostChecked(baseCost, cryptoCost, costLimit)
    fullCost
  }

  /** Checks the possible soft-fork condition.
    *
    * @param ergoTree contract which needs to be executed
    * @param context  evaluation context to use for detecting soft-fork condition
    * @return `None`, if no soft-fork has been detected and ErgoTree execution can proceed normally
    *         `Some(true -> context.initCost)`, if soft-fork has been detected, but we
    *         cannot proceed with ErgoTree, however can accept relying on 90% of upgraded
    *         nodes (due to activation has already been done).
    * @throws InterpreterException when cannot proceed and no activation yet.
    */
  protected def checkSoftForkCondition(ergoTree: ErgoTree, context: CTX): Option[VerificationResult] = {
    // TODO v6.0: the condition below should be revised if necessary (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/904)
    // The following conditions define behavior which depend on the version of ergoTree
    // This works in addition to more fine-grained soft-forkability mechanism implemented
    // using ValidationRules (see trySoftForkable method call here and in reduceToCrypto).

    if (context.activatedScriptVersion > VersionContext.MaxSupportedScriptVersion) {
      // The activated protocol exceeds capabilities of this interpreter.
      // NOTE: this path should never be taken for validation of candidate blocks
      // in which case Ergo node should always pass Interpreter.MaxSupportedScriptVersion
      // as the value of ErgoLikeContext.activatedScriptVersion.
      // see also ErgoLikeContext ScalaDoc.

      // Currently more than 90% of nodes has already switched to a higher version,
      // thus we can accept without verification, but only if we cannot verify
      // the given ergoTree
      if (ergoTree.version > VersionContext.MaxSupportedScriptVersion) {
        // We accept the box spending and rely on 90% of all the other nodes.
        // Thus, the old node will stay in sync with the network.
        return Some(true -> context.initCost)
      }
      // otherwise, we can verify the box spending and thus, proceed normally

    } else {
      // activated version is within the supported range [0..MaxSupportedScriptVersion]
      // in addition, ErgoTree version should never exceed the currently activated protocol

      if (ergoTree.version > context.activatedScriptVersion) {
        throw new InterpreterException(
          s"ErgoTree version ${ergoTree.version} is higher than activated ${context.activatedScriptVersion}")
      }
    }
    None // proceed normally
  }

  /** Executes the script in a given context.
    * Step 1: Deserialize context variables
    * Step 2: Evaluate expression and produce SigmaProp value, which is zero-knowledge
    *         statement (see also `SigmaBoolean`).
    * Step 3: Verify that the proof is presented to satisfy SigmaProp conditions.
    *
    * NOTE, ergoTree.complexity is not added to the cost when v5.0 is activated
    *
    * @param env      environment of system variables used by the interpreter internally
    * @param ergoTree ErgoTree expression to execute in the given context and verify its
    *                 result
    * @param context  the context in which `exp` should be executed
    * @param proof    The proof of knowledge of the secrets which is expected by the
    *                 resulting SigmaProp
    * @param message  message bytes, which are used in verification of the proof
    * @return verification result or Exception.
    *         If if the estimated cost of execution of the `exp` exceeds the limit (given
    *         in `context`), then exception if thrown and packed in Try.
    *         If the first component is false, then:
    *         1) script executed to false or
    *         2) the given proof failed to validate resulting SigmaProp conditions.
    * @see `reduceToCrypto`
    */
  def verify(env: ScriptEnv,
             ergoTree: ErgoTree,
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = {
    val res = Try {
      checkSoftForkCondition(ergoTree, context) match {
        case Some(resWhenSoftFork) => return Success(resWhenSoftFork)
        case None => // proceed normally
      }
      VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
        val reduced = fullReduction(ergoTree, context, env)
        reduced.value match {
          case TrivialProp.TrueProp => (true, reduced.cost)
          case TrivialProp.FalseProp => (false, reduced.cost)
          case _ =>
            val fullCost = addCryptoCost(reduced.value, reduced.cost, context.costLimit)

            val ok = if (evalSettings.isMeasureOperationTime) {
              val E = CErgoTreeEvaluator.forProfiling(verifySignatureProfiler, evalSettings)
              verifySignature(reduced.value, message, proof)(E)
            } else {
              verifySignature(reduced.value, message, proof)(null)
            }
            (ok, fullCost)
        }
      }
    }
    res
  }

  // Perform Verifier Steps 4-6
  private def checkCommitments(sp: UncheckedSigmaTree, message: Array[Byte])(implicit E: CErgoTreeEvaluator): Boolean = {
    // Perform Verifier Step 4
    val newRoot = computeCommitments(sp).get.asInstanceOf[UncheckedSigmaTree]
    val bytes = CollectionUtil.concatArrays(FiatShamirTree.toBytes(newRoot), message)
    /**
      * Verifier Steps 5-6: Convert the tree to a string `s` for input to the Fiat-Shamir hash function,
      * using the same conversion as the prover in 7
      * Accept the proof if the challenge at the root of the tree is equal to the Fiat-Shamir hash of `s`
      * (and, if applicable,  the associated data). Reject otherwise.
      */
    val expectedChallenge = CryptoFunctions.hashFn(bytes)
    java.util.Arrays.equals(newRoot.challenge.toArray, expectedChallenge)
  }

  /**
    * Verifier Step 4: For every leaf node, compute the commitment a from the challenge e and response $z$,
    * per the verifier algorithm of the leaf's Sigma-protocol.
    * If the verifier algorithm of the Sigma-protocol for any of the leaves rejects, then reject the entire proof.
    */
  val computeCommitments: Strategy = everywherebu(rule[Any] {
    case c: UncheckedConjecture => c // Do nothing for internal nodes

    case sn: UncheckedSchnorr =>
      implicit val E = CErgoTreeEvaluator.getCurrentEvaluator
      fixedCostOp(ComputeCommitments_Schnorr) {
        val a = DLogProver.computeCommitment(sn.proposition, sn.challenge, sn.secondMessage)
        sn.copy(commitmentOpt = Some(FirstDLogProverMessage(a)))
      }

    case dh: UncheckedDiffieHellmanTuple =>
      implicit val E = CErgoTreeEvaluator.getCurrentEvaluator
      fixedCostOp(ComputeCommitments_DHT) {
        val (a, b) = DiffieHellmanTupleProver.computeCommitment(dh.proposition, dh.challenge, dh.secondMessage)
        dh.copy(commitmentOpt = Some(FirstDHTupleProverMessage(a, b)))
      }

    case _: UncheckedSigmaTree => ???
  })

  def verify(ergoTree: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(Interpreter.emptyEnv, ergoTree, ctxv, proverResult.proof, message)
  }

  def verify(env: ScriptEnv,
             ergoTree: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(env, ergoTree, ctxv, proverResult.proof, message)
  }

  def verify(ergoTree: ErgoTree,
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(Interpreter.emptyEnv, ergoTree, context, SigSerializer.toProofBytes(proof), message)
  }

  /**
    * Verify a signature on given (arbitrary) message for a given public key.
    *
    * @param sigmaTree public key (represented as a tree)
    * @param message   message
    * @param signature signature for the message
    * @param E         optional evaluator (can be null) which is used for profiling of operations.
    *                  When `E` is `null`, then profiling is turned-off and has no effect on
    *                  the execution.
    * @return whether signature is valid or not
    */
  def verifySignature(sigmaTree: SigmaBoolean,
                      message: Array[Byte],
                      signature: Array[Byte])(implicit E: CErgoTreeEvaluator): Boolean = {
    // Perform Verifier Steps 1-3
    try {
      SigSerializer.parseAndComputeChallenges(sigmaTree, signature) match {
        case NoProof => false
        case sp: UncheckedSigmaTree =>
          // Perform Verifier Steps 4-6
          checkCommitments(sp, message)
      }
    } catch {
      case t: Throwable =>
        // TODO cover with tests
        //  NOTE, property("handle improper signature") doesn't lead to exception
        //  because the current implementation of parseAndComputeChallenges doesn't throw
        //  an exception
        logMessage("Improper signature: ", t);
        false
    }
  }

}

object Interpreter {
  final val VerifyStarkMinErgoTreeVersion: Int =
    VersionContext.StarkVerificationVersion.toInt

  private[interpreter] trait StarkPreflightOperationObserver {
    def onNodeInspected(): Unit
    def onChildrenRead(): Unit
    def onFramePushed(): Unit
    def onNodeReused(): Unit
    def onNodeRebuilt(): Unit
    def onProfileIdClassified(): Unit
    def onPlanBuilt(): Unit
  }

  private[interpreter] trait StarkPreflightContinuationObserver {
    def onContinuationTaken(): Unit
    def onDirectPathSelected(): Unit
    def onMaterializedPathSelected(): Unit
    def onAvailabilityChecked(): Unit
    def onAvailabilityPassed(): Unit
    def onConstantReductionEntered(): Unit
    def onDirectEvaluatorEntered(): Unit
    def onJitReductionEntered(): Unit
  }

  /** Structural classification of a materialized VerifyStark profileId.
    * Classification never evaluates the expression.
    */
  sealed trait StarkProfileIdClassification

  /** Exact, structurally static 32-byte profile identifier. */
  final class StaticStarkProfileId private[interpreter] (sourceBytes: Array[Byte])
      extends StarkProfileIdClassification {
    private val storedBytes = sourceBytes.clone()

    /** Returns a defensive copy. */
    def bytes: Array[Byte] = storedBytes.clone()

    override def equals(other: Any): Boolean = other match {
      case that: StaticStarkProfileId =>
        java.util.Arrays.equals(storedBytes, that.storedBytes)
      case _ => false
    }

    override def hashCode(): Int = java.util.Arrays.hashCode(storedBytes)

    override def toString: String =
      s"StaticStarkProfileId(${storedBytes.length} bytes)"
  }

  private[interpreter] object StaticStarkProfileId {
    def apply(bytes: Array[Byte]): StaticStarkProfileId =
      new StaticStarkProfileId(bytes)
  }

  /** The profile expression requires evaluator execution. */
  case object DynamicStarkProfileId extends StarkProfileIdClassification

  /** A constant/placeholder shape exists, but it cannot resolve to an exact
    * 32-byte profile identifier.
    */
  case object MalformedStarkProfileId extends StarkProfileIdClassification

  /** One materialized VerifyStark occurrence in canonical DFS order. */
  final case class StarkPreflightOccurrence(
      profileId: StarkProfileIdClassification)

  /** Immutable trusted-host plan produced after complete v4+ materialization.
    * Generation- and purpose-specific admission policy belongs to the node,
    * outside Sigma evaluation.
    */
  final class StarkPreflightPlan private[interpreter] (
      val occurrences: IndexedSeq[StarkPreflightOccurrence])

  private[interpreter] object StarkPreflightPlan {
    def apply(occurrences: IndexedSeq[StarkPreflightOccurrence]): StarkPreflightPlan =
      new StarkPreflightPlan(occurrences.toVector)
  }

  /** Result of Box.ergoTree verification procedure (see `verify` method).
    * The first component is the value of Boolean type which represents a result of
    * SigmaProp condition verification via sigma protocol.
    * The second component is the estimated cost of contract execution. */
  type VerificationResult = (Boolean, Long)

  /** Result of ErgoTree reduction procedure by JIT-based interpreter (see `fullReduction`,
    * `reduceToCrypto` and friends).
    *
    * @param value the value of SigmaProp type which represents a logical statement
    *              verifiable via sigma protocol.
    * @param cost  the estimated cost of the contract execution (in block's scale).
    */
  case class ReductionResult(value: SigmaBoolean, cost: Long)

  /** Represents properties of interpreter invocation. */
  type ScriptEnv = Map[String, Any]

  /** Empty interpreter properties. */
  val emptyEnv: ScriptEnv = Map.empty[String, Any]

  /** Property name used to store script name. */
  val ScriptNameProp = "ScriptName"

  /** Initial cost of instantiating an interpreter and creating ErgoLikeContext.
    * Added once per transaction.
    */
  val interpreterInitCost = 10000

  /** The result of script reduction when soft-fork condition is detected by the old node,
    * in which case the script is reduced to the trivial true proposition and takes up 0 cost.
    */
  def WhenSoftForkReductionResult(cost: Long): ReductionResult = ReductionResult(TrivialProp.TrueProp, cost)

  /** Represents the cost of computing DLogInteractiveProver.computeCommitment. */
  final val ComputeCommitments_Schnorr = OperationCostInfo(
    FixedCost(JitCost(3400)), NamedDesc("ComputeCommitments_Schnorr"))

  /** Represents the cost of computing DiffieHellmanTupleInteractiveProver.computeCommitment. */
  final val ComputeCommitments_DHT = OperationCostInfo(
    FixedCost(JitCost(6450)), NamedDesc("ComputeCommitments_DHT"))

  /** Represents the cost spent by JIT evaluator on a simple ErgoTree containing
    * SigmaPropConstant.
    * It doesn't include cost of crypto verification.
    */
  final val Eval_SigmaPropConstant = OperationCostInfo(
    FixedCost(JitCost(50)), NamedDesc("Eval_SigmaPropConstant"))

  /** Verification cost of each ProveDlog node of SigmaBoolean proposition tree. */
  final val ProveDlogVerificationCost =
    ParseChallenge_ProveDlog.costKind.cost +
    ComputeCommitments_Schnorr.costKind.cost +
    ToBytes_Schnorr.costKind.cost

  /** Verification cost of each ProveDHTuple node of SigmaBoolean proposition tree. */
  final val ProveDHTupleVerificationCost =
    ParseChallenge_ProveDHT.costKind.cost +
    ComputeCommitments_DHT.costKind.cost +
    ToBytes_DHT.costKind.cost

  /** Computes the estimated cost of verification of sigma proposition.
    * The cost is estimated ahead of time, without actually performing expencive crypto
    * operations.
    * @param sb sigma proposition
    * @return estimated cost of verification of the given proposition in JIT scale
    */
  def estimateCryptoVerifyCost(sb: SigmaBoolean): JitCost = {
    /** Recursively compute the total cost of the given children. */
    def childrenCost(children: Seq[SigmaBoolean]): JitCost = {
      val childrenArr = children.toArray
      val nChildren = childrenArr.length
      var sum = JitCost(0)
      cfor(0)(_ < nChildren, _ + 1) { i =>
        val c = estimateCryptoVerifyCost(childrenArr(i))
        sum = sum + c
      }
      sum
    }
    sb match {
      case _: ProveDlog => ProveDlogVerificationCost
      case _: ProveDHTuple => ProveDHTupleVerificationCost

      case and: CAND =>
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childrenC = childrenCost(and.children)
        nodeC + childrenC

      case or: COR =>
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childrenC = childrenCost(or.children)
        nodeC + childrenC

      case th: CTHRESHOLD =>
        val nChildren = th.children.length
        val nCoefs = nChildren - th.k
        val parseC = ParsePolynomial.costKind.cost(nCoefs)
        val evalC = EvaluatePolynomial.costKind.cost(nCoefs) * nChildren
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childernC = childrenCost(th.children)
        parseC + evalC + nodeC + childernC
      case _ =>
        JitCost(0)  // the cost of trivial proposition
    }
  }

  /** An instance of profiler used to measure cost parameters of verifySignature
    * operations.
    */
  val verifySignatureProfiler = new CProfiler

  private def toValidScriptTypeJITC(exp: SValue): SigmaPropValue = exp match {
    case v: Value[SBoolean.type]@unchecked if v.tpe == SBoolean => v.toSigmaProp
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp
    case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

}
