package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import sigma.ast._
import sigma.ast.syntax._
import sigmastate.eval.{CAvlTreeVerifier, CProfiler}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigma.{AvlTree, Coll, Colls, Context, Header, VersionContext}
import sigma.util.Extensions._
import debox.{cfor, Buffer => DBuffer}
import scorex.crypto.authds.ADKey
import sigma.ast.SAvlTreeMethods._
import sigma.ast.SHeaderMethods.checkPowMethod
import sigma.ast.SType
import sigma.data.{CSigmaProp, KeyValueColl, SigmaBoolean}
import sigma.eval.{AvlTreeVerifier, ErgoTreeEvaluator, EvalSettings, Profiler}
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigmastate.interpreter.CErgoTreeEvaluator.fixedCostOp

import scala.collection.compat.immutable.ArraySeq
import scala.util.{DynamicVariable, Failure, Success}

/** Result of JITC evaluation with costing. */
case class JitEvalResult[A](value: A, cost: JitCost)

/** Implements a simple and fast direct-style interpreter of ErgoTrees.
  *
  * ### Motivation
  * [[ErgoTree]] is a simple declarative intermediate representation for Ergo contracts. It is
  * designed to be compact in serialized form and directly executable, i.e. no additional
  * transformation is necessary before it can be efficiently executed.
  *
  * This class implements a big-step recursive interpreter that works directly with
  * ErgoTree HOAS. Because of this the evaluator is very simple and follows denotational
  * semantics of ErgoTree (see https://ergoplatform.org/docs/ErgoTree.pdf). Or, the other
  * way around, this implementation of ErgoTreeEvaluator is purely functional with
  * immutable data structures and can be used as definition of ErgoTree's semantics.
  *
  * ### Implementation
  * ErgoTreeEvaluator takes ErgoTree directly as it is deserialized as part of a
  * transaction. No additional transformation is performed.
  * ErgoTree is interpreted directly and all the intermediate data is stored in the
  * runtime types.
  * The runtime types are such types as [[sigma.Coll]],
  * [[sigma.SigmaProp]], [[sigma.AvlTree]], [[BigInt]], etc.
  * It also use immutable Map to keep current [[DataEnv]] of computed [[ValDef]]s, as
  * result only addition is used from the map, and deletion is essentially a garbage
  * collection.
  *
  * ### Performance
  * Since this interpreter directly works with SigmaDsl types (Coll, BigInt, SigmaProp
  * etc), it turns out to be very fast. Since it also does JIT style costing instead of
  * AOT style, it is 5-6x faster than existing implementation.
  *
  * @param context   Represents blockchain data context for ErgoTree evaluation
  * @param constants Segregated constants from ErgoTree, to lookup them from
  *                  [[ConstantPlaceholder]] evaluation.
  * @param coster    Accumulates computation costs.
  * @param profiler  Performs operations profiling and time measurements (if enabled in settings).
  * @param settings  Settings to be used during evaluation.
  */
class CErgoTreeEvaluator(
  val context: Context,
  val constants: Seq[Constant[SType]],
  protected val coster: CostAccumulator,
  val profiler: Profiler,
  val settings: EvalSettings) extends ErgoTreeEvaluator {

  override def createTreeVerifier(tree: AvlTree, proof: Coll[Byte]): AvlTreeVerifier =
    CAvlTreeVerifier(tree, proof)

  /** Creates [[sigma.eval.AvlTreeVerifier]] for the given tree and proof. */
  def createVerifier(tree: AvlTree, proof: Coll[Byte]) = {
    // the cost of tree reconstruction from proof is O(proof.length)
    addSeqCost(CreateAvlVerifier_Info, proof.length) { () =>
      CAvlTreeVerifier(tree, proof)
    }
  }

  override def contains_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean = {
    val bv     = createVerifier(tree, proof)
    val nItems = bv.treeHeight
    var res = false
    // the cost of tree lookup is O(bv.treeHeight)
    addSeqCost(LookupAvlTree_Info, nItems) { () =>
      res = bv.performLookup(ADKey @@ key.toArray) match {
        case Success(r) => r match {
          case Some(_) => true
          case _ => false
        }
        case Failure(_) => false
      }
    }
    res
  }

  override def get_eval(mc: MethodCall, tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = {
    val bv     = createVerifier(tree, proof)
    val nItems = bv.treeHeight

    // the cost of tree lookup is O(bv.treeHeight)
    addSeqCost(LookupAvlTree_Info, nItems) { () =>
      bv.performLookup(ADKey @@ key.toArray) match {
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
        case Failure(_) => syntax.error(s"Tree proof is incorrect $tree")
      }
    }
  }

  override def getMany_eval(
      mc: MethodCall,
      tree: AvlTree,
      keys: Coll[Coll[Byte]],
      proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = {
    val bv = createVerifier(tree, proof)
    val nItems = bv.treeHeight
    keys.map { key =>
      // the cost of tree lookup is O(bv.treeHeight)
      addSeqCost(LookupAvlTree_Info, nItems) { () =>
        bv.performLookup(ADKey @@ key.toArray) match {
          case Success(r) => r match {
            case Some(v) => Some(Colls.fromArray(v))
            case _ => None
          }
          case Failure(_) => syntax.error(s"Tree proof is incorrect $tree")
        }
      }
    }
  }

  override def insert_eval(mc: MethodCall, tree: AvlTree, entries: KeyValueColl, proof: Coll[Byte]): Option[AvlTree] = {
    addCost(isInsertAllowed_Info)
    if (!tree.isInsertAllowed) {
      None
    } else {
      val bv     = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)
      entries.forall { case (key, value) =>
        var res = true
        // the cost of tree lookup is O(bv.treeHeight)
        addSeqCost(InsertIntoAvlTree_Info, nItems) { () =>
          val insertRes = bv.performInsert(key.toArray, value.toArray)
          // TODO v6.0: throwing exception is not consistent with update semantics
          //  however it preserves v4.0 semantics (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/908)
          if (insertRes.isFailure) {
            syntax.error(s"Incorrect insert for $tree (key: $key, value: $value, digest: ${tree.digest}): ${insertRes.failed.get}}")
          }
          res = insertRes.isSuccess
        }
        res
      }
      bv.digest match {
        case Some(d) =>
          addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  override def update_eval(
      mc: MethodCall, tree: AvlTree,
      operations: KeyValueColl, proof: Coll[Byte]): Option[AvlTree] = {
    addCost(isUpdateAllowed_Info)
    if (!tree.isUpdateAllowed) {
      None
    } else {
      val bv     = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)

      // here we use forall as looping with fast break on first failed tree oparation
      operations.forall { case (key, value) =>
        var res = true
        // the cost of tree update is O(bv.treeHeight)
        addSeqCost(UpdateAvlTree_Info, nItems) { () =>
          val updateRes = bv.performUpdate(key.toArray, value.toArray)
          res = updateRes.isSuccess
        }
        res
      }
      bv.digest match {
        case Some(d) =>
          addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  override def remove_eval(
      mc: MethodCall, tree: AvlTree,
      operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree] = {
    addCost(isRemoveAllowed_Info)
    if (!tree.isRemoveAllowed) {
      None
    } else {
      val bv     = createVerifier(tree, proof)
      // when the tree is empty we still need to add the insert cost
      val nItems = Math.max(bv.treeHeight, 1)
      cfor(0)(_ < operations.length, _ + 1) { i =>
        addSeqCost(RemoveAvlTree_Info, nItems) { () =>
          val key = operations(i).toArray
          bv.performRemove(key)
        }
      }
      addCost(digest_Info)
      bv.digest match {
        case Some(d) =>
          addCost(updateDigest_Info)
          Some(tree.updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  /** Evaluates the given expression in the given data environment. */
  def eval(env: DataEnv, exp: SValue): Any = {
    VersionContext.checkVersions(context.activatedScriptVersion, context.currentErgoTreeVersion)
    CErgoTreeEvaluator.currentEvaluator.withValue(this) {
      exp.evalTo[Any](env)(this)
    }
  }

  /** Evaluates the given expression in the given data environment and accrue the cost
    * into the `coster` of this evaluator.
    * @return the value of the expression and the total accumulated cost in the coster.
    *         The returned cost includes the initial cost accumulated in the `coster`
    *         prior to calling this method. */
  def evalWithCost[A](env: DataEnv, exp: SValue): JitEvalResult[A] = {
    val res = eval(env, exp)
    val cost = coster.totalCost
    JitEvalResult(res.asInstanceOf[A], cost)
  }

  /** Trace of cost items accumulated during execution of `eval` method. Call
    * `clearTrace` method before each `eval` invocation. */
  private lazy val costTrace: DBuffer[CostItem] = {
    DBuffer.ofSize[CostItem](1000)
  }

  /** Returns currently accumulated JIT cost in this evaluator. */
  def getAccumulatedCost: JitCost = coster.totalCost

  /** Returns the currently accumulated trace of cost items in this evaluator.
    * A new array is allocated and returned, the evaluator state is unaffected.
    */
  def getCostTrace(): Seq[CostItem] = {
    costTrace.toArray()
  }

  /** Clears the accumulated trace of this evaluator. */
  def clearTrace() = {
    costTrace.clear()
  }

  override def addCost(costKind: FixedCost, opDesc: OperationDesc): Unit = {
    coster.add(costKind.cost)
    if (settings.costTracingEnabled) {
      costTrace += FixedCostItem(opDesc, costKind)
    }
  }

  override def addCost(costInfo: OperationCostInfo[FixedCost]): Unit = {
    addCost(costInfo.costKind, costInfo.opDesc)
  }

  override def addTypeBasedCost[R](costKind: TypeBasedCost,
                             tpe: SType, opDesc: OperationDesc)(block: () => R): R = {
    var costItem: TypeBasedCostItem = null
    if (settings.costTracingEnabled) {
      costItem = TypeBasedCostItem(opDesc, costKind, tpe)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime) {
      if (costItem == null) {
        costItem = TypeBasedCostItem(opDesc, costKind, tpe)
      }
      val start = System.nanoTime()
      val cost = costKind.costFunc(tpe) // should be measured as part of the operation
      coster.add(cost)
      val res = block()
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
      res
    } else {
      val cost = costKind.costFunc(tpe)
      coster.add(cost)
      block()
    }
  }

  /** @hotspot don't beautify the code */
  override def addFixedCost(costKind: FixedCost, opDesc: OperationDesc)(block: => Unit): Unit = {
    var costItem: FixedCostItem = null
    if (settings.costTracingEnabled) {
      costItem = FixedCostItem(opDesc, costKind)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime) {
      if (costItem == null) {
        costItem = FixedCostItem(opDesc, costKind)
      }
      val start = System.nanoTime()
      coster.add(costKind.cost)
      val _ = block
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
    } else {
      coster.add(costKind.cost)
      block
    }
  }

  override def addFixedCost(costInfo: OperationCostInfo[FixedCost])(block: => Unit): Unit = {
    addFixedCost(costInfo.costKind, costInfo.opDesc)(block)
  }

  /** @hotspot don't beautify the code */
  override def addSeqCostNoOp(costKind: PerItemCost, nItems: Int, opDesc: OperationDesc): Unit = {
    var costItem: SeqCostItem = null
    if (settings.costTracingEnabled) {
      costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
    val cost = costKind.cost(nItems)
    coster.add(cost)
  }

  override def addSeqCost[R](costKind: PerItemCost, nItems: Int, opDesc: OperationDesc)(block: () => R): R = {
    var costItem: SeqCostItem = null
    if (settings.costTracingEnabled) {
      costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime) {
      if (costItem == null) {
        costItem = SeqCostItem(opDesc, costKind, nItems)
      }
      val start = System.nanoTime()
      val cost = costKind.cost(nItems) // should be measured as part of the operation
      coster.add(cost)
      val res = block()
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
      res
    } else {
      val cost = costKind.cost(nItems)
      coster.add(cost)
      block()
    }
  }

  override def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
                         (block: () => R): R = {
    addSeqCost(costInfo.costKind, nItems, costInfo.opDesc)(block)
  }

  override def addSeqCost(costKind: PerItemCost, opDesc: OperationDesc)(block: () => Int): Unit = {
    var costItem: SeqCostItem = null
    var nItems = 0
    if (settings.isMeasureOperationTime) {
      val start = System.nanoTime()
      nItems = block()
      val cost = costKind.cost(nItems) // should be measured as part of the operation
      coster.add(cost)
      val end = System.nanoTime()

      costItem = SeqCostItem(opDesc, costKind, nItems)
      profiler.addCostItem(costItem, end - start)
    } else {
      nItems = block()
      val cost = costKind.cost(nItems)
      coster.add(cost)
    }
    if (settings.costTracingEnabled) {
      if (costItem == null)
        costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
  }

  override def addSeqCost(costInfo: OperationCostInfo[PerItemCost])(block: () => Int): Unit = {
    addSeqCost(costInfo.costKind, costInfo.opDesc)(block)
  }
}

object CErgoTreeEvaluator {

  /** Size of data block in bytes. Used in JIT cost calculations.
    * @see [[NEQ]],
    */
  val DataBlockSize: Int = 512

  /** Empty data environment. */
  val EmptyDataEnv: DataEnv = Map.empty

  /** A profiler which is used by default if [[EvalSettings.isMeasureOperationTime]] is enabled. */
  val DefaultProfiler = new CProfiler

  /** Default global [[EvalSettings]] instance. */
  val DefaultEvalSettings = EvalSettings(
    isMeasureOperationTime = false,
    isMeasureScriptTime = false)

  /** Evaluator currently is being executed on the current thread.
    * This variable is set in a single place, specifically in the `eval` method of
    * [[CErgoTreeEvaluator]].
 *
    * @see getCurrentEvaluator
    */
  private[sigmastate] val currentEvaluator = new DynamicVariable[CErgoTreeEvaluator](null)

  /** Returns a current evaluator for the current thread. */
  def getCurrentEvaluator: CErgoTreeEvaluator = currentEvaluator.value

  /** Creates a new [[CErgoTreeEvaluator]] instance with the given profiler and settings.
    * The returned evaluator can be used to initialize the `currentEvaluator` variable.
    * As a result, cost-aware operations (code blocks) can be implemented, even when those
    * operations don't involve ErgoTree evaluation.
    * As an example, see methods in [[sigmastate.SigSerializer]] and
    * [[sigmastate.FiatShamirTree]] where cost-aware code blocks are used.
    */
  def forProfiling(profiler: CProfiler, evalSettings: EvalSettings): CErgoTreeEvaluator = {
    val acc = new CostAccumulator(
      initialCost = JitCost(0),
      costLimit = Some(JitCost.fromBlockCost(evalSettings.scriptCostLimitInEvaluator)))
    new CErgoTreeEvaluator(
      context = null,
      constants = ArraySeq.empty,
      acc, profiler, evalSettings.copy(profilerOpt = Some(profiler)))
  }

  /** Executes [[FixedCost]] code `block` and use the given evaluator `E` to perform
    * profiling and cost tracing.
    * This helper method allows implementation of cost-aware code blocks by using
    * thread-local instance of [[CErgoTreeEvaluator]].
    * If the `currentEvaluator` [[DynamicVariable]] is not initialized (equals to null),
    * then the block is executed with minimal overhead.
    *
    * @param costInfo operation descriptor
    * @param block    block of code to be executed (given as lazy by-name argument)
    * @param E        evaluator to be used (or null if it is not available on the
    *                 current thread), in which case the method is equal to the
    *                 `block` execution.
    * @return result of code block execution
    * HOTSPOT: don't beautify the code
    * Note, `null` is used instead of Option to avoid allocations.
    */
  def fixedCostOp[R](costInfo: OperationCostInfo[FixedCost])
                              (block: => R)(implicit E: ErgoTreeEvaluator): R = {
    if (E != null) {
      var res: R = null.asInstanceOf[R]
      E.addFixedCost(costInfo) {
        res = block
      }
      res
    } else
      block
  }

  /** Executes [[PerItemCost]] code `block` and use the given evaluator `E` to perform
    * profiling and cost tracing.
    * This helper method allows implementation of cost-aware code blocks by using
    * thread-local instance of [[CErgoTreeEvaluator]].
    * If the `currentEvaluator` [[DynamicVariable]] is not initialized (equals to null),
    * then the block is executed with minimal overhead.
    *
    * @param costInfo operation descriptor
    * @param nItems   number of data items in the operation
    * @param block    block of code to be executed (given as lazy by-name argument)
    * @param E        evaluator to be used (or null if it is not available on the
    *                 current thread), in which case the method is equal to the
    *                 `block` execution.
    * @return result of code block execution
    * HOTSPOT: don't beautify the code
    * Note, `null` is used instead of Option to avoid allocations.
    */
  def perItemCostOp[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
                      (block: () => R)(implicit E: ErgoTreeEvaluator): R = {
    if (E != null) {
      E.addSeqCost(costInfo, nItems)(block)
    } else
      block()
  }

  /** Evaluate the given [[ErgoTree]] in the given Ergo context using the given settings.
    * The given ErgoTree is evaluated as-is and is not changed during evaluation.
    *
    * @param context      [[ErgoLikeContext]] used for script execution
    * @param ergoTree     script represented as [[ErgoTree]]
    * @param evalSettings evaluation settings
    * @return a sigma protocol proposition (as [[SigmaBoolean]]) and accumulated JIT cost estimation.
    */
  def evalToCrypto(context: ErgoLikeContext, ergoTree: ErgoTree, evalSettings: EvalSettings): ReductionResult = {
    val (res, cost) = eval(context, ergoTree.constants, ergoTree.toProposition(replaceConstants = false), evalSettings)
    val sb = res match {
      case sp: CSigmaProp => sp.wrappedValue
      case sb: SigmaBoolean => sb
      case _ => error(s"Expected SigmaBoolean but was: $res")
    }
    ReductionResult(sb, cost)
  }

  /** Evaluate the given expression in the given Ergo context using the given settings.
    * The given Value is evaluated as-is and is not changed during evaluation.
    *
    * @param context      [[ErgoLikeContext]] used for script execution
    * @param constants    collection of segregated constants which can be refered by
    *                     [[ConstantPlaceholder]]s in `exp`
    * @param exp          ErgoTree expression represented as [[Value]]
    * @param evalSettings evaluation settings
    * @return 1) the result of evaluating `exp` in a given context and
    *         2) an accumulated JIT cost estimation.
    */
  def eval(context: ErgoLikeContext,
           constants: Seq[Constant[SType]],
           exp: SValue,
           evalSettings: EvalSettings): (Any, Int) = {
    val costAccumulator = new CostAccumulator(
      initialCost = JitCost.fromBlockCost(context.initCost.toIntExact),
      costLimit = Some(JitCost.fromBlockCost(context.costLimit.toIntExact)))
    val sigmaContext = context.toSigmaContext()
    eval(sigmaContext, costAccumulator, constants, exp, evalSettings)
  }

  /** Evaluate the given expression in the given Ergo context using the given settings.
    * The given Value is evaluated as-is and is not changed during evaluation.
    *
    * @param sigmaContext    [[sigma.Context]] instance used for script execution
    * @param costAccumulator [[CostAccumulator]] instance used for accumulating costs
    * @param constants       collection of segregated constants which can be refered by
    *                        [[ConstantPlaceholder]]s in `exp`
    * @param exp             ErgoTree expression represented as [[sigma.ast.Value]]
    * @param evalSettings    evaluation settings
    * @return 1) the result of evaluating `exp` in a given context and
    *         2) an accumulated JIT cost estimation.
    */
  def eval(sigmaContext: Context,
           costAccumulator: CostAccumulator,
           constants: Seq[Constant[SType]],
           exp: SValue,
           evalSettings: EvalSettings): (Any, Int) = {
    val evaluator = new CErgoTreeEvaluator(
      sigmaContext, constants, costAccumulator, DefaultProfiler, evalSettings)
    val res       = evaluator.eval(Map(), exp)
    val cost = costAccumulator.totalCost.toBlockCost // scale to block cost
    (res, cost)
  }

  def error(msg: String) = sys.error(msg)
}


