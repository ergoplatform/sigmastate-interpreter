package sigma.ast

import sigma.Coll

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

/**
  * Cost of  converting numeric value to the numeric value of the given type, i.e. Byte -> Int
  */
object NumericCastCostKind extends TypeBasedCost {
  override def costFunc(targetTpe: SType): JitCost = targetTpe match {
    case SBigInt => JitCost(30)
    case _ => JitCost(10)
  }
}

/**
  * Cost of Global.powHit method, which is dependent on few parameters, see cost() function description
  */
object PowHitCostKind extends CostKind {
  /**
    * @param k - k parameter of Autolykos 2 (number of inputs in k-sum problem)"
    * @param msg - message to calculate Autolykos hash 2 for
    * @param nonce - used to pad the message to get Proof-of-Work hash function output with desirable properties
    * @param h - PoW protocol specific padding for table uniqueness (e.g. block height in Ergo)
    * @return cost of custom Autolykos2 hash function invocation
    */
  def cost(k: Int, msg: Coll[Byte], nonce: Coll[Byte], h: Coll[Byte]): JitCost = {
    val chunkSize = CalcBlake2b256.costKind.chunkSize
    val perChunkCost = CalcBlake2b256.costKind.perChunkCost
    val baseCost = 300

    // the heaviest part inside is k + 1 Blake2b256 invocations
    val c = baseCost + (k + 1) * ((msg.length + nonce.length + h.length) / chunkSize + 1) * perChunkCost.value
    JitCost(c)
  }
}



