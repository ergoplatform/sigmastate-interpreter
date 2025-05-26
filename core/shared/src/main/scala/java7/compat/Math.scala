package java7.compat

/**
  * Contains methods introduced since Java 1.8 which are not available in Java 1.7.
  * Using this methods supports compatibility with Java 1.7 in non-JVM contexts like
  * RoboVM.
  * The implementations are copies from JDK 1.8 sources.
  * <p>
  * See
  * <a href="https://github.com/ScorexFoundation/sigmastate-interpreter/issues/735">this issue</a>
  */
object Math {
  /**
    * Returns the sum of its arguments,
    * throwing an exception if the result overflows an {@code int}.
    *
    * @param x the first value
    * @param y the second value
    * @return the result
    * @throws ArithmeticException if the result overflows an int
    */
  def addExact(x: Int, y: Int): Int = {
    val r = x + y
    // HD 2-12 Overflow iff both arguments have the opposite sign of the result
    if (((x ^ r) & (y ^ r)) < 0) {
      throw new ArithmeticException("integer overflow")
    }
    r
  }

  /**
    * Returns the sum of its arguments,
    * throwing an exception if the result overflows a {@code long}.
    *
    * @param x the first value
    * @param y the second value
    * @return the result
    * @throws ArithmeticException if the result overflows a long
    */
  def addExact(x: Long, y: Long): Long = {
    val r = x + y
    if (((x ^ r) & (y ^ r)) < 0) {
      throw new ArithmeticException("long overflow")
    }
    r
  }

  /**
    * Returns the difference of the arguments,
    * throwing an exception if the result overflows an {@code int}.
    *
    * @param x the first value
    * @param y the second value to subtract from the first
    * @return the result
    * @throws ArithmeticException if the result overflows an int
    */
  def subtractExact(x: Int, y: Int): Int = {
    val r = x - y
    // HD 2-12 Overflow iff the arguments have different signs and
    // the sign of the result is different than the sign of x
    if (((x ^ y) & (x ^ r)) < 0) {
      throw new ArithmeticException("integer overflow")
    }
    r
  }

  /**
    * Returns the difference of the arguments,
    * throwing an exception if the result overflows a {@code long}.
    *
    * @param x the first value
    * @param y the second value to subtract from the first
    * @return the result
    * @throws ArithmeticException if the result overflows a long
    */
  def subtractExact(x: Long, y: Long) = {
    val r = x - y
    // HD 2-12 Overflow iff the arguments have different signs and
    // the sign of the result is different than the sign of x
    if (((x ^ y) & (x ^ r)) < 0) {
      throw new ArithmeticException("long overflow")
    }
    r
  }

  /**
    * Returns the product of the arguments,
    * throwing an exception if the result overflows an {@code int}.
    *
    * @param x the first value
    * @param y the second value
    * @return the result
    * @throws ArithmeticException if the result overflows an int
    */
  def multiplyExact(x: Int, y: Int) = {
    val r = x.toLong * y.toLong
    if (r.toInt != r) {
      throw new ArithmeticException("integer overflow")
    }
    r.toInt
  }

  /**
    * Returns the product of the arguments,
    * throwing an exception if the result overflows a {@code long}.
    *
    * @param x the first value
    * @param y the second value
    * @return the result
    * @throws ArithmeticException if the result overflows a long
    */
  def multiplyExact(x: Long, y: Long) = {
    val r = x * y
    val ax = java.lang.Math.abs(x)
    val ay = java.lang.Math.abs(y)
    if ((ax | ay) >>> 31 != 0) {
      // Some bits greater than 2^31 that might cause overflow
      // Check the result using the divide operator
      // and check for the special case of Long.MIN_VALUE * -1
      if (((y != 0) && (r / y != x)) || (x == Long.MinValue && y == -1)) {
        throw new ArithmeticException("long overflow")
      }
    }
    r
  }
}
