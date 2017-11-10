package sigmastate.utxo

import sigmastate._
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.UtxoContext.Height

case class BoxMetadata(creationHeight: Height)

case class BowWithMetadata(box: SigmaStateBox, metadata: BoxMetadata)

case class UtxoContext(currentHeight: Height,
                       spendingTransaction: SigmaStateTransaction,
                       self: BowWithMetadata,
                       override val extension: ContextExtension = ContextExtension(Map())
                      ) extends Context[UtxoContext] {
  override def withExtension(newExtension: ContextExtension): UtxoContext = this.copy(extension = newExtension)
}

object UtxoContext {
  type Height = Long
}

trait SelfVariable[V <: Value] extends Variable[V] {
  override def cost: Int = Cost.SelfVariableDeclaration
}

case object SelfHeight extends SelfVariable[IntLeaf]

case object SelfAmount extends SelfVariable[IntLeaf]

case object SelfScript extends SelfVariable[PropLeaf]

case object OutputAmount extends Variable[IntLeaf] {
  override val cost: Int = Cost.OutputAmount
}

case object OutputScript extends Variable[PropLeaf] {
  override val cost: Int = Cost.OutputScript
}

case object TxOutBytes extends Variable[ByteArrayLeaf] {
  override val cost: Int = Cost.TxOutBytes
}