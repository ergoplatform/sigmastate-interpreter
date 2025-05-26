package sigma.util

import debox.{cfor, Buffer => DBuffer}
import sigma.crypto.{CryptoFacade, Ecp}
import sigma.data._
import sigma.{AvlTree, GroupElement, SigmaProp}

import java.math.BigInteger
import java.nio.ByteBuffer

object Extensions {
  implicit class BooleanOps(val b: Boolean) extends AnyVal {
    /** Convert true to 1 and false to 0 */
    def toByte: Byte = if (b) 1 else 0
  }

  /** HOTSPOT:  it is used in deserialization so we avoid allocation by any means. */
  @inline final def toUByte(b: Byte) = b & 0xFF

  implicit class ByteOps(val b: Byte) extends AnyVal {
    @inline def toUByte: Int = Extensions.toUByte(b)

    def addExact(b2: Byte): Byte = {
      val r = b + b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    def subtractExact(b2: Byte): Byte = {
      val r = b - b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    def multiplyExact(b2: Byte): Byte = {
      val r = b * b2
      if (r < Byte.MinValue || r > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      r.toByte
    }

    /** Absolute value of this numeric value. */
    def toAbs: Byte = if (b < 0) (-b).toByte else b
  }

  implicit class ShortOps(val x: Short) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def addExact(y: Short): Short = {
      val r = x + y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }

    def subtractExact(y: Short): Short = {
      val r = x - y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }

    def multiplyExact(y: Short): Short = {
      val r = x * y
      if (r < Short.MinValue || r > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      r.toShort
    }

    /** Absolute value of this numeric value. */
    def toAbs: Short = if (x < 0) (-x).toShort else x
  }

  implicit class IntOps(val x: Int) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def toShortExact: Short = {
      if (x < Short.MinValue || x > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      x.toShort
    }

    /** Absolute value of this numeric value. */
    def toAbs: Int = if (x < 0) -x else x
  }

  implicit class LongOps(val x: Long) extends AnyVal {
    def toByteExact: Byte = {
      if (x < Byte.MinValue || x > Byte.MaxValue)
        throw new ArithmeticException("Byte overflow")
      x.toByte
    }

    def toShortExact: Short = {
      if (x < Short.MinValue || x > Short.MaxValue)
        throw new ArithmeticException("Short overflow")
      x.toShort
    }

    def toIntExact: Int = {
      if (x < Int.MinValue || x > Int.MaxValue)
        throw new ArithmeticException("Int overflow")
      x.toInt
    }

    /** Absolute value of this numeric value. */
    def toAbs: Long = if (x < 0) -x else x
  }

  implicit class BigIntegerOps(val x: BigInteger) extends AnyVal {

    /**
      * Converts this {@code BigInteger} to a {@code long}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code long} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code long}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code long}.
      * @see BigInteger#longValue
      * @since 1.8
      */
    def toLongExact: Long = {
      if (x.bitLength <= 63) x.longValue
      else
        throw new ArithmeticException("BigInteger out of long range")
    }

    /**
      * Converts this {@code BigInteger} to an {@code int}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code int} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to an {@code int}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in an {@code int}.
      * @see BigInteger#intValue
      */
    def toIntExact: Int = {
      if (x.bitLength <= 31) x.intValue
      else
        throw new ArithmeticException("BigInteger out of int range")
    }

    /**
      * Converts this {@code BigInteger} to a {@code short}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code short} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code short}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code short}.
      * @see BigInteger#shortValue
      */
    def toShortExact: Short = {
      if (x.bitLength <= 31) {
        val value = x.intValue
        if (value >= Short.MinValue && value <= Short.MaxValue)
          return x.shortValue
      }
      throw new ArithmeticException("BigInteger out of short range")
    }

    /**
      * Converts this {@code BigInteger} to a {@code byte}, checking
      * for lost information.  If the value of this {@code BigInteger}
      * is out of the range of the {@code byte} type, then an
      * {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} converted to a {@code byte}.
      * @throws ArithmeticException if the value of {@code this} will
      *                             not exactly fit in a {@code byte}.
      * @see BigInteger#byteValue
      * @since 1.8
      */
    def toByteExact: Byte = {
      if (x.bitLength <= 31) {
        val value = x.intValue
        if (value >= Byte.MinValue && value <= Byte.MaxValue)
          return x.byteValue
      }
      throw new ArithmeticException("BigInteger out of byte range")
    }

    /** Checks this {@code BigInteger} can be cast to 256 bit two's-compliment representation. */
    @inline def fitsIn256Bits: Boolean = {
      // Comparing with 255 is correct because bitLength() method excludes the sign bit.
      // For example, these are the boundary values:
      // (new BigInteger("80" + "00" * 31, 16)).bitLength() = 256
      // (new BigInteger("7F" + "ff" * 31, 16)).bitLength() = 255
      // (new BigInteger("-7F" + "ff" * 31, 16)).bitLength() = 255
      // (new BigInteger("-80" + "00" * 31, 16)).bitLength() = 255
      // (new BigInteger("-80" + "00" * 30 + "01", 16)).bitLength() = 256
      x.bitLength() <= 255
    }

    /** Checks this {@code BigInteger} can be cast to 256 bit two's-compliment representation,
      * checking for lost information. If the value of this {@code BigInteger}
      * is out of the range of the 256 bits, then an {@code ArithmeticException} is thrown.
      *
      * @return this {@code BigInteger} if the check is successful
      * @throws ArithmeticException if the value of {@code this} will
      * not exactly fit in a 256 bit range.
      * @see BigInteger#longValueExact
      */
    @inline final def to256BitValueExact: BigInteger = {
      if (fitsIn256Bits) x
      else
        throw new ArithmeticException("BigInteger out of 256 bit range");
    }

    /** Converts `x` to [[sigma.BigInt]] */
    def toBigInt: sigma.BigInt = CBigInt(x)
  }

  implicit class BigIntOps(val x: sigma.BigInt) extends AnyVal {
    /** Converts `x` to [[java.math.BigInteger]] */
    def toBigInteger: BigInteger = x.asInstanceOf[CBigInt].wrappedValue
  }

  implicit class OptionOps[T](val opt: Option[T]) extends AnyVal {
    /** Elvis operator for Option. See https://en.wikipedia.org/wiki/Elvis_operator*/
    def ?:(whenNone: => T): T = if (opt.isDefined) opt.get else whenNone
  }

  implicit class ProductOps(val source: Product) extends AnyVal {
    def toArray: Array[Any] = {
      val arity = source.productArity
      val res = new Array[Any](arity)
      var i = 0
      while (i < arity) {
        res(i) = source.productElement(i)
        i += 1
      }
      res
    }
  }

  implicit class ByteBufferOps(val buf: ByteBuffer) extends AnyVal {
    def toBytes: Array[Byte] = {
      val res = new Array[Byte](buf.position())
      buf.array().copyToArray(res, 0, res.length)
      res
    }
    def getBytes(size: Int): Array[Byte] = {
      if (size > buf.remaining)
        throw new IllegalArgumentException(s"Not enough bytes in the ByteBuffer: $size")
      val res = new Array[Byte](size)
      buf.get(res)
      res
    }
    def getOption[T](getValue: => T): Option[T] = {
      val tag = buf.get()
      if (tag != 0)
        Some(getValue)
      else
        None
    }
  }

  /** Syntactic sugar for postfix assertions and the value pass through
    * Example:
    * val positiveValue = x.ensuring(_ > 0, x => s"the value is not positive: $x")
    */
  implicit final class Ensuring[A](private val self: A) extends AnyVal {
    /** Ensures that the given predicate holds for this value.
      * @param cond the predicate used to test this value.
      * @param msg the error message to be used if the predicate does not hold.
      * @return this value, if it satisfies the given predicate `p`.
      * @throws AssertionError if the predicate does not hold.
      */
    def ensuring(cond: A => Boolean, msg: A => Any): A = {
      assert(cond(self), msg(self))
      self
    }
  }

  implicit class EcpOps(val source: Ecp) extends AnyVal {
    /** Extracts [[sigma.GroupElement]] from the Ecp instance. */
    def toGroupElement: GroupElement = CGroupElement(source)

    /** Shortened String representation of `source` GroupElement. */
    def showECPoint: String = {
      if (source.toGroupElement.isIdentity) {
        "IDENTITY"
      }
      else {
        CryptoFacade.showPoint(source)
      }
    }
  }

  implicit class GroupElementOps(val source: GroupElement) extends AnyVal {
    def toECPoint: Ecp = source.asInstanceOf[CGroupElement].wrappedValue
    /** Shortened String representation of `source` GroupElement. */
    def showToString: String = toECPoint.showECPoint
  }

  implicit class DBufferOps[A](val buf: DBuffer[A]) extends AnyVal {
    /** Sum all values in `buf` using the given Numeric. */
    def sumAll(implicit n: Numeric[A]): A = {
      val limit     = buf.length
      var result: A = n.zero
      cfor(0)(_ < limit, _ + 1) { i =>
        result = n.plus(result, buf.elems(i))
      }
      result
    }
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    /** Wraps SigmaBoolean into SigmaProp. */
    def toSigmaProp: SigmaProp = CSigmaProp(sb)

    /** Human readable string representation of the proposition. */
    def showToString: String = sb match {
      case ProveDlog(v) =>
        s"ProveDlog(${v.showECPoint})"
      case ProveDHTuple(gv, hv, uv, vv) =>
        s"ProveDHTuple(${gv.showECPoint}, ${hv.showECPoint}, ${uv.showECPoint}, ${vv.showECPoint})"
      case _ => sb.toString
    }
  }

  implicit class SigmaPropOps(val sb: SigmaProp) extends AnyVal {
    /** Extracts [[sigma.SigmaBoolean]] from the SigmaProp instance. */
    def toSigmaBoolean: SigmaBoolean = sb.asInstanceOf[CSigmaProp].wrappedValue
  }

  implicit class AvlTreeDataOps(val treeData: AvlTreeData) extends AnyVal {
    /** Wrap [[sigma.AvlTreeData]] to [[sigma.AvlTree]]. */
    def toAvlTree: AvlTree= CAvlTree(treeData)
  }

  implicit class CoreAvlTreeOps(val tree: AvlTree) extends AnyVal {
    /** Extracts [[sigma.AvlTreeData]] from the AvlTree instance. */
    def toAvlTreeData: AvlTreeData = tree.asInstanceOf[CAvlTree].wrappedValue
  }
}
