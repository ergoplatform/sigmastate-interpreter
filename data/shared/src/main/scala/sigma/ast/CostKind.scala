package sigma.ast

import sigma.Coll
import sigma.pow.Autolykos2PowValidation

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
  def chunks(nItems: Int) = {
    require(nItems >= 0, s"Negative nItems: $nItems")
    (nItems - 1) / chunkSize + 1
  }

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
    case SUnsignedBigInt => JitCost(30)
    case _ => JitCost(10)
  }
}

/**
  * Cost of Global.powHit method, which is dependent on few parameters, see cost() function description
  */
object PowHitCostKind extends CostKind {

  /** Cost of Autolykos 2 hashing for the given input lengths, including the
    * constant 8 KiB `M` payload hashed on each of the `k + 1` iterations.
    */
  def costForAutolykos2(k: Int, msgLength: Int, nonceLength: Int, hLength: Int): JitCost = {
    val chunkSize = CalcBlake2b256.costKind.chunkSize
    val perChunkCost = CalcBlake2b256.costKind.perChunkCost
    val baseCost = 500
    val payloadLength = msgLength + nonceLength + hLength + Autolykos2PowValidation.M.length
    JitCost(baseCost + (k + 1) * (payloadLength / chunkSize + 1) * perChunkCost.value)
  }

  /** Cost of Autolykos 2 header proof-of-work validation.
    *
    * This is 15,515 JIT units: `500 + 33 * ((32 + 8 + 4 + 8192) / 128 + 1) * 7`.
    */
  val Autolykos2CheckPowCost: JitCost = costForAutolykos2(32, 32, 8, 4)

  /**
    * @param k - k parameter of Autolykos 2 (number of inputs in k-sum problem)"
    * @param msg - message to calculate Autolykos hash 2 for
    * @param nonce - used to pad the message to get Proof-of-Work hash function output with desirable properties
    * @param h - PoW protocol specific padding for table uniqueness (e.g. block height in Ergo)
    * @return cost of custom Autolykos2 hash function invocation
    */
  def cost(k: Int, msg: Coll[Byte], nonce: Coll[Byte], h: Coll[Byte]): JitCost =
    costForAutolykos2(k, msg.length, nonce.length, h.length)
}



