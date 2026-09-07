package sigma.ast

/** Represents cost estimation computed by JITC interpreter.
  * The JITC costs use 10x more accurate scale comparing to block cost values.
  *
  * @see toBlockCost
  */
opaque type JitCost = Int

object JitCost {
  def apply(value: Int): JitCost = value

  /** Extracts the cost value, preserving the Scala 2 case-class pattern syntax. */
  def unapply(cost: JitCost): Some[Int] = Some(cost)

  /** Scales the given block cost to the JitCost scale. This is inverse to toBlockCost */
  def fromBlockCost(blockCost: Int): JitCost =
    java7.compat.Math.multiplyExact(blockCost, 10)

  extension (cost: JitCost) {
    // TODO make 'value` private[sigma] after code moved from sigmastate package
    def value: Int = cost

    /** Copies the cost value, preserving the Scala 2 case-class copy syntax. */
    def copy(value: Int = cost): JitCost = value

    /** Adds two cost values. */
    def +(y: JitCost): JitCost =
      java7.compat.Math.addExact(cost, y)

    /** Multiplies this cost to the given integer. */
    def *(n: Int): JitCost =
      java7.compat.Math.multiplyExact(cost, n)

    /** Divides this cost by the given integer. */
    def /(n: Int): JitCost = cost / n

    /** Return true if this value > y.value in the normal Int ordering. */
    def >(y: JitCost): Boolean = cost > y

    /** Return true if this value >= y.value in the normal Int ordering. */
    def >=(y: JitCost): Boolean = cost >= y

    /** Scales JitCost back to block cost value. This is inverse to JitCost.fromBlockCost. */
    def toBlockCost: Int = cost / 10
  }
}
