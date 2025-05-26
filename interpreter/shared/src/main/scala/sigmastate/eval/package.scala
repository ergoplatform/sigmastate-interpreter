package sigmastate

import org.ergoplatform.ErgoBox
import sigma._
import sigma.data.{AvlTreeData, SigmaBoolean}
import sigma.eval.SigmaDsl
import sigma.exceptions.CostLimitException

import scala.language.implicitConversions

package object eval {
  /** Implicit conversions between Dsl type and the type wrapped by the corresponding type Dsl type.
    * Here BigInt is Dsl type and BigInteger is wrapped type.
    * @see `sigma.CBigInt`
    */
  implicit def sigmaBooleanToSigmaProp(p: SigmaBoolean): SigmaProp = SigmaDsl.SigmaProp(p)
  implicit def sigmaPropToSigmaBoolean(p: SigmaProp): SigmaBoolean = SigmaDsl.toSigmaBoolean(p)

  implicit def avlTreeDataToAvlTree(p: AvlTreeData): AvlTree = SigmaDsl.avlTree(p)
  implicit def avlTreeToAvlTreeData(p: AvlTree): AvlTreeData = SigmaDsl.toAvlTreeData(p)

  implicit def ergoBoxToBox(p: ErgoBox): Box = SigmaDsl.Box(p)
  implicit def boxToErgoBox(p: Box): ErgoBox = SigmaDsl.toErgoBox(p)

  def msgCostLimitError(
      cost: Long,
      limit: Long) = s"Estimated execution cost $cost exceeds the limit $limit"

  /** Helper method to accumulate cost while checking limit.
    *
    * @param current current cost value
    * @param delta additional cost to add to the current value
    * @param limit total cost limit
    * @param msgSuffix use case-specific error message suffix
    * @return new increased cost when it doesn't exceed the limit
    * @throws CostLimitException
    */
  def addCostChecked(
      current: Long,
      delta: Long,
      limit: Long,
      msgSuffix: => String = ""): Long = {
    val newCost = java7.compat.Math.addExact(current, delta)
    if (newCost > limit) {
      throw new CostLimitException(
        estimatedCost = newCost,
        message = {
          val suffix = if (msgSuffix.isEmpty) "" else s": $msgSuffix"
          msgCostLimitError(newCost, limit) + suffix
        }
      )
    }
    newCost
  }
}
