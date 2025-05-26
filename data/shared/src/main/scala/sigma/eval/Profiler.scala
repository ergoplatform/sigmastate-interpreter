package sigma.eval

import sigma.ast.{CostItem, JitCost}
import sigma.ast.syntax.SValue

abstract class Profiler {
  /** Called from evaluator immediately before the evaluator start recursive evaluation of
    * the given node.
    */
  def onBeforeNode(node: SValue): Unit

  /** Called from evaluator immediately after the evaluator finishes recursive evaluation
    * of the given node.
    */
  def onAfterNode(node: SValue): Unit

  /** Add new measurement point to the stats of this profiler.
    *
    * @param costItem executed cost item
    * @param time     time spent to execute this cost item
    */
  def addCostItem(costItem: CostItem, time: Long): Unit

  /** Adds estimated cost and actual measured time data point to the StatCollection for
    * the given script.
    */
  def addJitEstimation(script: String, cost: JitCost, actualTimeNano: Long): Unit
}
