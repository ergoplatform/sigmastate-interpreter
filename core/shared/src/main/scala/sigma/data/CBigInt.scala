package sigma.data

import sigma.util.Extensions.BigIntegerOps
import sigma.{BigInt, Coll, Colls, UnsignedBigInt}

import java.math.BigInteger

/** A default implementation of [[BigInt]] interface.
  *
  * @see [[BigInt]] for detailed descriptions
  */
case class CBigInt(override val wrappedValue: BigInteger) extends BigInt with WrapperOf[BigInteger] {

  override def toByte: Byte = wrappedValue.toByteExact

  override def toShort: Short = wrappedValue.toShortExact

  override def toInt: Int = wrappedValue.toIntExact

  override def toLong: Long = wrappedValue.toLongExact

  override def toBytes: Coll[Byte] = Colls.fromArray(wrappedValue.toByteArray)

  override def toAbs: BigInt = CBigInt(wrappedValue.abs())

  override def compareTo(that: BigInt): Int =
    wrappedValue.compareTo(that.asInstanceOf[CBigInt].wrappedValue)

  override def signum: Int = wrappedValue.signum()

  override def add(that: BigInt): BigInt = CBigInt(wrappedValue.add(that.asInstanceOf[CBigInt].wrappedValue).toSignedBigIntValueExact)

  override def subtract(that: BigInt): BigInt = CBigInt(wrappedValue.subtract(that.asInstanceOf[CBigInt].wrappedValue).toSignedBigIntValueExact)

  override def multiply(that: BigInt): BigInt = CBigInt(wrappedValue.multiply(that.asInstanceOf[CBigInt].wrappedValue).toSignedBigIntValueExact)

  override def divide(that: BigInt): BigInt = CBigInt(wrappedValue.divide(that.asInstanceOf[CBigInt].wrappedValue))

  override def mod(m: BigInt): BigInt = CBigInt(wrappedValue.mod(m.asInstanceOf[CBigInt].wrappedValue))

  override def remainder(that: BigInt): BigInt = CBigInt(wrappedValue.remainder(that.asInstanceOf[CBigInt].wrappedValue))

  override def min(that: BigInt): BigInt = CBigInt(wrappedValue.min(that.asInstanceOf[CBigInt].wrappedValue))

  override def max(that: BigInt): BigInt = CBigInt(wrappedValue.max(that.asInstanceOf[CBigInt].wrappedValue))

  override def negate(): BigInt = CBigInt(wrappedValue.negate().toSignedBigIntValueExact)

  override def and(that: BigInt): BigInt = CBigInt(wrappedValue.and(that.asInstanceOf[CBigInt].wrappedValue))

  override def or(that: BigInt): BigInt = CBigInt(wrappedValue.or(that.asInstanceOf[CBigInt].wrappedValue))

  override def xor(that: BigInt): BigInt = CBigInt(wrappedValue.xor(that.asInstanceOf[CBigInt].wrappedValue))

  override def shiftLeft(n: Int): BigInt = CBigInt(wrappedValue.shiftLeft(n).toSignedBigIntValueExact)

  override def shiftRight(n: Int): BigInt = CBigInt(wrappedValue.shiftRight(n).toSignedBigIntValueExact)

  def toUnsigned: UnsignedBigInt = {
    if(this.wrappedValue.compareTo(BigInteger.ZERO) < 0){
      throw new ArithmeticException("BigInteger argument for .toUnsigned is negative in");
    } else {
      CUnsignedBigInt(this.wrappedValue)
    }
  }

  def toUnsignedMod(m: UnsignedBigInt): UnsignedBigInt = {
    CUnsignedBigInt(this.wrappedValue.mod(m.asInstanceOf[CUnsignedBigInt].wrappedValue))
  }
}

/** A default implementation of [[BigInt]] interface.
  *
  * @see [[BigInt]] for detailed descriptions
  */
case class CUnsignedBigInt(override val wrappedValue: BigInteger) extends UnsignedBigInt with WrapperOf[BigInteger] {

  override def toByte: Byte = wrappedValue.toByteExact

  override def toShort: Short = wrappedValue.toShortExact

  override def toInt: Int = wrappedValue.toIntExact

  override def toLong: Long = wrappedValue.toLongExact

  override def toBytes: Coll[Byte] = Colls.fromArray(wrappedValue.toByteArray)

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

  override def shiftLeft(n: Int): UnsignedBigInt = CUnsignedBigInt(wrappedValue.shiftLeft(n).toUnsignedBigIntValueExact)

  override def shiftRight(n: Int): UnsignedBigInt = CUnsignedBigInt(wrappedValue.shiftRight(n).toUnsignedBigIntValueExact)

  override def toSigned(): BigInt = {
    CBigInt(wrappedValue.toSignedBigIntValueExact)
  }
}
