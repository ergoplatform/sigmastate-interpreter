package sigma.eval

import sigma.{AvlTree, Coll, Context, Header}
import sigma.ast.{Constant, FixedCost, MethodCall, OperationCostInfo, OperationDesc, PerItemCost, SType, TypeBasedCost}
import sigma.data.KeyValueColl

abstract class ErgoTreeEvaluator {
  /** Adds the given cost to the `coster`. If tracing is enabled, creates a new cost item
    * with the given operation.
    *
    * @param costKind the cost to be added to `coster` for each item
    * @param nItems   the number of items
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost
    * @tparam R result type of the operation
    * @hotspot don't beautify the code
    */
  def addSeqCost[R](costKind: PerItemCost, nItems: Int, opDesc: OperationDesc)
      (block: () => R): R

  /** Adds the cost to the `coster`. See the other overload for details. */
  def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
      (block: () => R): R

  /** Adds the cost to the `coster`. If tracing is enabled, creates a new cost item with
    * the given operation descriptor and cost kind. If time measuring is enabled also
    * performs profiling.
    *
    * WARNING: The cost is accumulated AFTER the block is executed.
    * Each usage of this method should be accompanied with a proof of why this cannot lead
    * to unbounded execution (see all usages).
    *
    * @param costKind the cost descriptor to be used to compute the cost based on the
    *                 actual number of items returned by the `block`
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost descriptors, returns the
    *                 actual number of items processed
    * @hotspot don't beautify the code
    */
  def addSeqCost(costKind: PerItemCost, opDesc: OperationDesc)(block: () => Int): Unit

  /** Adds the cost to the `coster`. See the other overload for details. */
  def addSeqCost(costInfo: OperationCostInfo[PerItemCost])(block: () => Int): Unit

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    *
    * @param costKind kind of the cost to be added to `coster`
    * @param opDesc   operation descriptor to associate the cost with (when costTracingEnabled)
    */
  def addCost(costKind: FixedCost, opDesc: OperationDesc): Unit

  def addCost(costInfo: OperationCostInfo[FixedCost]): Unit

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    *
    * @param costKind kind of the cost to be added to `coster`
    * @param opDesc   the operation descriptor to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost
    */
  def addFixedCost(costKind: FixedCost, opDesc: OperationDesc)(block: => Unit): Unit

  def addFixedCost(costInfo: OperationCostInfo[FixedCost])(block: => Unit): Unit

  /** Adds the given cost to the `coster`. If tracing is enabled, creates a new cost item
    * with the given operation.
    *
    * @param costKind the cost to be added to `coster` for each item
    * @param nItems   the number of items
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    */
  def addSeqCostNoOp(costKind: PerItemCost, nItems: Int, opDesc: OperationDesc): Unit

  /** Add the cost given by the cost descriptor and the type to the accumulator and
    * associate it with this operation descriptor.
    *
    * @param costKind descriptor of the cost
    * @param tpe      specific type for which the cost should be computed by this descriptor
    *                 (see costFunc method)
    * @param opDesc   operation which is associated with this cost
    */
  def addTypeBasedCost[R](
      costKind: TypeBasedCost,
      tpe: SType, opDesc: OperationDesc)(block: () => R): R

  /** Settings to be used during evaluation. */
  def settings: EvalSettings

  /** Performs operations profiling and time measurements (if enabled in settings). */
  def profiler: Profiler

  /** Segregated constants from ErgoTree, to lookup them from
    * [[ConstantPlaceholder]] evaluation.
    */
  def constants: Seq[Constant[SType]]

  /** Represents blockchain data context for ErgoTree evaluation. */
  def context: Context

  /** Create an instance of [[AvlTreeVerifier]] for the given tree and proof. */
  def createTreeVerifier(tree: AvlTree, proof: Coll[Byte]): AvlTreeVerifier

  /** Implements evaluation of AvlTree.contains method call ErgoTree node. */
  def contains_eval(
      mc: MethodCall,
      tree: AvlTree,
      key: Coll[Byte],
      proof: Coll[Byte]): Boolean

  /** Implements evaluation of AvlTree.get method call ErgoTree node. */
  def get_eval(
      mc: MethodCall,
      tree: AvlTree,
      key: Coll[Byte],
      proof: Coll[Byte]): Option[Coll[Byte]]

  /** Implements evaluation of AvlTree.getMany method call ErgoTree node. */
  def getMany_eval(
      mc: MethodCall,
      tree: AvlTree,
      keys: Coll[Coll[Byte]],
      proof: Coll[Byte]): Coll[Option[Coll[Byte]]]

  /** Implements evaluation of AvlTree.insert method call ErgoTree node. */
  def insert_eval(
      mc: MethodCall,
      tree: AvlTree,
      entries: KeyValueColl,
      proof: Coll[Byte]): Option[AvlTree]

  /** Implements evaluation of AvlTree.update method call ErgoTree node. */
  def update_eval(
      mc: MethodCall, tree: AvlTree,
      operations: KeyValueColl, proof: Coll[Byte]): Option[AvlTree]

  /** Implements evaluation of AvlTree.remove method call ErgoTree node. */
  def remove_eval(
      mc: MethodCall, tree: AvlTree,
      operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree]
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]
}