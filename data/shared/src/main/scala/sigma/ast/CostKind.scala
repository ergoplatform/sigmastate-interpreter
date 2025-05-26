package sigma.ast

import scala.runtime.Statics

/** Cost descriptor of a single operation, usually associated with
  * [[OperationDesc]].
  */
sealed abstract class CostKind

/** Descriptor of the simple fixed cost.
  * @param cost  given cost of the operation */
case class FixedCost(cost: JitCost) extends CostKind {
  override def hashCode(): Int = cost.value
}

/** Cost of operation over collection of the known length.
  * See for example [[Exists]], [[MapCollection]].
  * @param baseCost cost of operation factored out of the loop iterations
  * @param perChunkCost cost associated with each chunk of items
  * @param chunkSize number of items in a chunk
  */
case class PerItemCost(baseCost: JitCost, perChunkCost: JitCost, chunkSize: Int) extends CostKind {
  /** Compute number of chunks necessary to cover the given number of items. */
  def chunks(nItems: Int) = (nItems - 1) / chunkSize + 1

  /** Computes the cost for the given number of items. */
  def cost (nItems: Int): JitCost = {
    val nChunks = chunks(nItems)
    baseCost + (perChunkCost * nChunks)
  }

  /** This override is necessary to avoid JitCost instances allocation in the default
    * generated code for case class.
    */
  override def hashCode(): Int = {
    var var1 = -889275714
    var1 = Statics.mix(var1, this.baseCost.value)
    var1 = Statics.mix(var1, this.perChunkCost.value)
    var1 = Statics.mix(var1, this.chunkSize)
    Statics.finalizeHash(var1, 3)
  }
}

/** Descriptor of the cost which depends on type. */
abstract class TypeBasedCost extends CostKind {
  /** Returns cost value depending on the given type. */
  def costFunc(tpe: SType): JitCost
}

/** Cost of operation cannot be described using fixed set of parameters.
  * In this case the operation cost is a sum of sub-operation costs.
  * See [[EQ]], [[NEQ]]. */
case object DynamicCost extends CostKind



