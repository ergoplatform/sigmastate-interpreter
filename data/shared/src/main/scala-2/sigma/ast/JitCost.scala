package sigma.ast

/** Represents cost estimation computed by JITC interpreter.
  * The JITC costs use 10x more accurate scale comparing to block cost values.
  *
  * @see toBlockCost
  */
// TODO make 'value` private[sigma] after code moved from sigmastate package
case class JitCost private[sigma](val value: Int) extends AnyVal {
  /** Adds two cost values. */
  def +(y: JitCost): JitCost =
    new JitCost(java7.compat.Math.addExact(value, y.value))

  /** Multiplies this cost to the given integer. */
  def *(n: Int): JitCost =
    new JitCost(java7.compat.Math.multiplyExact(value, n))

  /** Divides this cost by the given integer. */
  def /(n: Int): JitCost =
    new JitCost(value / n)

  /** Return true if this value > y.value in the normal Int ordering. */
  def >(y: JitCost): Boolean = value > y.value

  /** Return true if this value >= y.value in the normal Int ordering. */
  def >=(y: JitCost): Boolean = value >= y.value

  /** Scales JitCost back to block cost value. This is inverse to JitCost.fromBlockCost. */
  def toBlockCost: Int = value / 10
}

object JitCost {
  /** Scales the given block cost to the JitCost scale. This is inverse to toBlockCost */
  def fromBlockCost(blockCost: Int): JitCost =
    new JitCost(java7.compat.Math.multiplyExact(blockCost, 10))
}

