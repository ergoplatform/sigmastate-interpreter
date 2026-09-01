package sigmastate.eval

import debox.{cfor, sp, Buffer => DBuffer, Map => DMap}
import sigma.ast.{CostItem, FixedCost, FixedCostItem, JitCost, SMethod, SeqCostItem, TypeBasedCostItem}
import sigma.ast.TypeCodes.LastConstantCode
import sigma.ast.syntax._
import sigma.util.Extensions.ByteOps
import sigma.ast.{MethodCall, PropertyCall}
import sigma.eval.Profiler
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.ValueSerializer
import sigma.serialization.ValueSerializer.getSerializer

import scala.reflect.ClassTag

/** Holds a series of profile measurements associated with a key.
  * Allows to compute simple statistic data.
  * @tparam V type of the measured numeric value
  */
abstract class StatHolder[@sp (Long, Double) V] {
  /** How many data points have been collected */
  def count: Int

  /** Sum of all data points */
  def sum: V

  /** Returns arithmetic average value. */
  def avg: V

  /** Returns arithmetic mean value (excluding 10% of smallest and 10% of highest values).
    */
  def mean: (V, Int)
}

/** Collects profiler measured data points associated with keys.
  * Group points by key into [[StatHolder]]s.
  * @tparam K type of the mapping key
  * @tparam V type of the measured numeric value
  */
class StatCollection[@sp(Int) K, @sp(Long, Double) V]
  (implicit n: Integral[V], ctK: ClassTag[K], ctV: ClassTag[V]) {

  /** Sum all values in `buf` using the given Numeric. */
  private def sumAll[A](buf: DBuffer[A], n: Numeric[A]): A = {
    val limit     = buf.length
    var result: A = n.zero
    cfor(0)(_ < limit, _ + 1) { i =>
      result = n.plus(result, buf.elems(i))
    }
    result
  }

  private def calcAvg(buf: DBuffer[V]): V = {
    n.quot(sumAll(buf, n), n.fromInt(buf.length))
  }

  // NOTE: this class is mutable so better to keep it private
  private class StatHolderImpl extends StatHolder[V] {
    final val NumMaxPoints = 10000

    val dataPoints: DBuffer[V] = DBuffer.ofSize[V](256)

    def addPoint(point: V) = {
      // collect data points until the threshold
      if (dataPoints.length < NumMaxPoints) {
        dataPoints += point
      }
    }

    override def count: Int = dataPoints.length
    override def sum: V = sumAll(dataPoints, n)
    override def avg: V = calcAvg(dataPoints)

    override def mean: (V, Int) = {
      val nCropped = dataPoints.length / 10
      if (nCropped == 0) {
        (calcAvg(dataPoints), dataPoints.length)
      }
      else {
        val sorted = dataPoints.toArray()
        sorted.sorted
        val slice = sorted.slice(nCropped, sorted.length - nCropped)
        (calcAvg(DBuffer.fromArray(slice)), slice.length)
      }
    }
  }

  /** Timings of op codes. For performance debox.Map is used, which keeps keys unboxed. */
  private val opStat = DMap[K, StatHolderImpl]()

  /** Returns arithmetic mean value (excluding 10% of smallest and 10% of highest values)
    * for the given key.
    */
  final def getMean(key: K): Option[(V, Int)] = opStat.get(key).map(_.mean)

  /** Update measurement stats for a given operation. */
  final def addPoint(key: K, point: V) = {
    val item = opStat.getOrElse(key, null)
    if (item != null) {
      item.addPoint(point)
    } else {
      val item = new StatHolderImpl
      item.addPoint(point)
      opStat(key) = item
    }
  }

  /** Maps each entry of the collected mapping to a new array of values using the given
    * function.
    */
  final def mapToArray[@sp C: ClassTag](f: (K, StatHolder[V]) => C): Array[C] = {
    opStat.mapToArray(f)
  }
}

/** A simple profiler to measure average execution times of ErgoTree operations. */
class CProfiler extends Profiler {

  // NOTE: this class is mutable so better to keep it private
  private class OpStat(
    /** The node on the evaluation stack. */
    val node: SValue,
    /** The time then this node evaluation was started. */
    val outerStart: Long,
    /** The accumulated time of evaluating all the sub-nodes */
    var innerTime: Long,
    /** The time then this nodes evaluation finished */
    val outerEnd: Long
  )

  /** If every recursive evaluation of every Value is marked with
    * [[onBeforeNode()]]/[[onAfterNode()]], then this stack corresponds to the stack of
    * recursive invocations of the evaluator. */
  private var opStack: List[OpStat] = Nil

  /** Called from evaluator immediately before the evaluator start recursive evaluation of
    * the given node.
    */
  def onBeforeNode(node: SValue) = {
    val t = System.nanoTime()
    opStack = new OpStat(node, t, 0, t) :: opStack
  }

  /** Called from evaluator immediately after the evaluator finishes recursive evaluation
    * of the given node.
    */
  def onAfterNode(node: SValue) = {
    val t = System.nanoTime()

    val op = opStack.head   // always non empty at this point
    opStack = opStack.tail  // pop current op
    assert(op.node.opCode == node.opCode, s"Inconsistent stack at ${op :: opStack}")

    val opFullTime = t - op.outerStart  // full time spent in this op

    // add this time to parent's innerTime (if any parent)
    if (opStack.nonEmpty) {
      val parent = opStack.head
      parent.innerTime += opFullTime
    } else {
      // we are on top level, do nothing
    }

    val opSelfTime = opFullTime - op.innerTime

    // update timing stats
    addOpTime(node.opCode, opSelfTime)
  }

  /** Timings of op codes. For performance debox implementation of Map is used. */
  private val opStat = new StatCollection[Int, Long]()

  /** Update time measurement stats for a given operation. */
  @inline private final def addOpTime(op: OpCode, time: Long) = {
    opStat.addPoint(OpCode.raw(op), time)
  }

  /** Timings of method calls */
  private val mcStat = new StatCollection[Int, Long]()

  /** Wrapper class which implements special equality between CostItem instances,
   * suitable for collecting of the statistics. */
  class CostItemKey(val costItem: CostItem) {
    override def hashCode(): Int = costItem match {
      case sci: SeqCostItem => 31 * sci.opDesc.hashCode + sci.chunks
      case _ => costItem.hashCode()
    }

    override def equals(obj: scala.Any): Boolean = (obj != null) &&
      (this.eq(obj.asInstanceOf[AnyRef]) || {
        (obj match {
          case that: CostItemKey =>
            this.costItem match {
              case sciThis: SeqCostItem  =>
                that.costItem match {
                  case sciThat: SeqCostItem =>
                    sciThis.opDesc == sciThat.opDesc && sciThis.chunks == sciThat.chunks
                  case _ => false
                }
              case _ => this.costItem == that.costItem
            }
          case _ => false
        })
      })

  }

  /** Timings of cost items */
  private val costItemsStat = new StatCollection[CostItemKey, Long]()

  override def addCostItem(costItem: CostItem, time: Long) = {
    costItemsStat.addPoint(new CostItemKey(costItem), time)
  }

  /** Estimation cost for each script */
  private val estimationCostStat = new StatCollection[String, Int]()
  /** Estimation cost for each script */
  private val measuredTimeStat = new StatCollection[String, Long]()

  /** Returns relative error between estimated and actual values. */
  def relativeError(est: Double, act: Double): Double = {
    val delta = Math.abs(est - act)
    delta / act
  }

  /** Adds estimated cost and actual measured time data point to the StatCollection for
    * the given script.
    */
  def addEstimation(script: String, cost: Int, actualTimeNano: Long) = {
    estimationCostStat.addPoint(script, cost)
    measuredTimeStat.addPoint(script, actualTimeNano)
  }

  /** Adds estimated cost and actual measured time data point to the StatCollection for
    * the given script.
    */
  def addJitEstimation(script: String, cost: JitCost, actualTimeNano: Long) = {
    addEstimation(script, cost.value, actualTimeNano)
  }

  /** Suggests a cost value for a given operation time.
    * @return suggested cost in JIT scale. */
  def suggestCost(time: Long): Int = {
    ((time - 1) / 100 + 1).toInt
  }

  /** Prints the operation timing table using collected execution profile information.
    */
  def generateReport(): String = {
    val opCodeLines = opStat.mapToArray { case (key, stat) =>
      val (time, count) = stat.mean
      val opCode = OpCode @@ key.toByte
      if (ValueSerializer.serializers.get(opCode).isEmpty) {
        // SoftForkabilitySpecification contain tests with unsupported operations
        // so return null here and then filter it out
        return null
      }
      val ser = getSerializer(opCode)
      val opDesc = ser.opDesc
      val (opName, cost) = opDesc.costKind match {
        case FixedCost(c) if opDesc != MethodCall && opDesc != PropertyCall =>
          (opDesc.typeName, c.value)
        case _ => ("", 0)
      }
      val suggestedCost = suggestCost(time)
      val warn = if (suggestedCost > cost) "!!!" else ""
      val comment = s"count: $count, suggestedCost: $suggestedCost, actualCost: $cost$warn"
      (opName, (opCode.toUByte - LastConstantCode).toString, time, comment)
    }.filter(line => line != null && line._1.nonEmpty)
      .sortBy(_._3)(Ordering[Long].reverse)

    val mcLines = mcStat.mapToArray { case (key, stat) =>
      val methodId = (key & 0xFF).toByte
      val typeId = (key >> 8).toByte
      val (time, count) = stat.mean
      val m = SMethod.fromIds(typeId, methodId)
      val typeName = m.objType.typeName
      (s"$typeName.${m.name}", typeId, methodId, time, count.toString)
    }.sortBy(r => (r._2,r._3))(Ordering[(Byte,Byte)].reverse)

    val ciLines = costItemsStat.mapToArray { case (ciKey, stat) =>
      val (name, timePerItem, time, comment) = {
        val (time, count) = stat.mean
        val suggestedCost = suggestCost(time)
        val warn = if (suggestedCost > ciKey.costItem.cost.value) "!!!" else ""
        ciKey.costItem match {
          case ci: FixedCostItem =>
            val comment = s"count: $count, suggested: $suggestedCost, actCost: ${ci.cost}$warn"
            (ci.opName, time, time, comment)
          case ci: TypeBasedCostItem =>
            val comment = s"count: $count, suggested: $suggestedCost, actCost: ${ci.cost}$warn"
            (ci.opName, time, time, comment)
          case ci @ SeqCostItem(_, costKind, _) =>
            val nChunks = ci.chunks
            val timePerChunk = if (nChunks > 0) time / nChunks else time
            val name = s"${ci.opName}(nChunks: $nChunks)"
            val comment = s"count: $count, suggested: $suggestedCost, actCost: ${ci.cost}$warn, kind: $costKind"
            (name, timePerChunk, time, comment)
        }
      }
      (name, timePerItem, time, comment)
    }.sortBy({ case (name, tpi, _, _) => (name, tpi)})(Ordering[(String, Long)])

    val estLines = estimationCostStat.mapToArray { case (script, stat) =>
      val (cost, count) = stat.mean
      val (timeNano, _) = measuredTimeStat.getMean(script).get
      val actualTimeMicro = timeNano.toDouble / 100
      val actualCost = cost.toDouble
      val error = relativeError(actualCost, actualTimeMicro)
      (script, error, cost, Math.round(actualTimeMicro), count.toString)
    }.sortBy(_._2)(Ordering[Double].reverse)


    val rows = opCodeLines
        .map { case (opName, _, time, comment) =>
          val key = s"$opName".padTo(26, ' ')
          s"$key -> time: $time ns, $comment "
        }
        .mkString("\n")

    val mcRows = mcLines
        .map { case (opName, typeId, methodId, time, count) =>
          val key = s"($typeId.toByte, $methodId.toByte)".padTo(25, ' ')
          s"$key -> $time,  // count = $count, $opName "
        }
        .mkString("\n")

    val ciRows = ciLines
        .map { case (opName, timePerItem, time, comment) =>
          val key = s"$opName".padTo(40, ' ')
          val totalTime = if (time != timePerItem) s"($time)" else ""
          s"$key -> $timePerItem${totalTime} ns, $comment"
        }
        .mkString("\n")

    val estRows = estLines
        .map { case (opName, error, cost, time, count) =>
          val key = s"$opName".padTo(30, ' ')
          val warn = if (cost < time) "!!!" else ""
          val err = f"$error%4.4f"
          s"$key -> ($err, $cost$warn, $time),  // count = $count "
        }
        .mkString("\n")

    s"""
      |-----------
      |$rows
      |-----------
      |$mcRows
      |-----------
      |$ciRows
      |-----------
      |$estRows
      |-----------
     """.stripMargin
  }

}

