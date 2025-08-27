package sigma.data

import sigma.{BigInt, Coll, Colls, UnsignedBigInt}
import sigma.crypto.BigIntegers
import sigma.util.Extensions.BigIntegerOps

import java.math.BigInteger


/** A default implementation of [[UnsignedBigInt]] interface.
  *
  * @see [[UnsignedBigInt]] for detailed descriptions
  */
case class CUnsignedBigInt(override val wrappedValue: BigInteger) extends UnsignedBigInt with WrapperOf[BigInteger] {

  if (wrappedValue.compareTo(BigInteger.ZERO) < 0) {
    throw new ArithmeticException(s"Attempt to create unsigned value from negative big integer $wrappedValue")
  }

  if (wrappedValue.bitLength() > 256) {
    throw new ArithmeticException(s"Too big unsigned big int value $wrappedValue")
  }

  override def toByte: Byte = wrappedValue.toByteExact

  override def toShort: Short = wrappedValue.toShortExact

  override def toInt: Int = wrappedValue.toIntExact

  override def toLong: Long = wrappedValue.toLongExact

  override def toBytes: Coll[Byte] = Colls.fromArray(BigIntegers.asUnsignedByteArray(wrappedValue))

  override def compareTo(that: UnsignedBigInt): Int =
    wrappedValue.compareTo(that.asInstanceOf[CUnsignedBigInt].wrappedValue)

  override def add(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.add(that.asInstanceOf[CUnsignedBigInt].wrappedValue).toUnsignedBigIntValueExact)

  override def subtract(that: UnsignedBigInt): UnsignedBigInt = {
    CUnsignedBigInt(wrappedValue.subtract(that.asInstanceOf[CUnsignedBigInt].wrappedValue).toUnsignedBigIntValueExact)
  }

  override def multiply(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.multiply(that.asInstanceOf[CUnsignedBigInt].wrappedValue).toUnsignedBigIntValueExact)

  override def divide(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.divide(that.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def mod(m: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.mod(m.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def min(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.min(that.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def max(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.max(that.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def and(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.and(that.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def or(that: UnsignedBigInt): UnsignedBigInt = CUnsignedBigInt(wrappedValue.or(that.asInstanceOf[CUnsignedBigInt].wrappedValue))

  override def modInverse(m: UnsignedBigInt): UnsignedBigInt = {
    CUnsignedBigInt(wrappedValue.modInverse(m.asInstanceOf[CUnsignedBigInt].wrappedValue))
  }

  override def plusMod(that: UnsignedBigInt, m: UnsignedBigInt): UnsignedBigInt = {
    val thatBi = that.asInstanceOf[CUnsignedBigInt].wrappedValue
    val mBi = m.asInstanceOf[CUnsignedBigInt].wrappedValue
    CUnsignedBigInt(wrappedValue.add(thatBi).mod(mBi))
  }

  override def subtractMod(that: UnsignedBigInt, m: UnsignedBigInt): UnsignedBigInt = {
    val thatBi = that.asInstanceOf[CUnsignedBigInt].wrappedValue
    val mBi = m.asInstanceOf[CUnsignedBigInt].wrappedValue
    CUnsignedBigInt(wrappedValue.subtract(thatBi).mod(mBi))
  }

  override def multiplyMod(that: UnsignedBigInt, m: UnsignedBigInt): UnsignedBigInt = {
    val thatBi = that.asInstanceOf[CUnsignedBigInt].wrappedValue
    val mBi = m.asInstanceOf[CUnsignedBigInt].wrappedValue
    CUnsignedBigInt(wrappedValue.multiply(thatBi).mod(mBi))
  }

  /**
    * @return a big integer whose value is `this xor that`
    */
  def xor(that: UnsignedBigInt): UnsignedBigInt = {
    CUnsignedBigInt(wrappedValue.xor(that.asInstanceOf[CUnsignedBigInt].wrappedValue))
  }

  override def shiftLeft(n: Int): UnsignedBigInt = CUnsignedBigInt(wrappedValue.shiftLeft(n))

  override def shiftRight(n: Int): UnsignedBigInt = CUnsignedBigInt(wrappedValue.shiftRight(n))

  override def bitwiseInverse(): UnsignedBigInt = {
    val bytes = BigIntegers.asUnsignedByteArray(32, wrappedValue)
    val res: Array[Byte] = bytes.map(b => (~b & 0xff).toByte)
    CUnsignedBigInt(BigIntegers.fromUnsignedByteArray(res))
  }

  override def toSigned(): BigInt = {
    CBigInt(wrappedValue.toSignedBigIntValueExact)
  }

}

