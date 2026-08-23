package sigma.data

import sigma.util.Extensions.BigIntegerOps
import sigma.{BigInt, Coll, Colls, UnsignedBigInt, VersionContext}

import java.math.BigInteger

/** A default implementation of [[BigInt]] interface.
  *
  * @see [[BigInt]] for detailed descriptions
  */
case class CBigInt(override val wrappedValue: BigInteger) extends BigInt with WrapperOf[BigInteger] {

  // the check is identical to `fitsIn256Bits` in Extensions
  // the check is used in >= 3 trees as there are ways to ask for big int
  // deserialization now aside of register / context var deserialization
  // e.g. Header w. Autolykos v1 deserialization
  if (VersionContext.current.isV3OrLaterErgoTreeVersion && wrappedValue.bitLength() > 255) {
    throw new ArithmeticException(s"Too big bigint value $wrappedValue")
  }

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

  // there is no need to do .toSignedBigIntValueExact check, as this method is introduced in trees v3,
  // and for trees v3, the check done in constructor
  override def xor(that: BigInt): BigInt = CBigInt(wrappedValue.xor(that.asInstanceOf[CBigInt].wrappedValue))

  // there is no need to do .toSignedBigIntValueExact check, as this method is introduced in trees v3,
  // and for trees v3, the check done in constructor
  override def shiftLeft(n: Int): BigInt = CBigInt(wrappedValue.shiftLeft(n).toSignedBigIntValueExact)

  // there is no need to do .toSignedBigIntValueExact check, as this method is introduced in trees v3,
  // and for trees v3, the check done in constructor
  override def shiftRight(n: Int): BigInt = CBigInt(wrappedValue.shiftRight(n).toSignedBigIntValueExact)

  override def toUnsigned: UnsignedBigInt = {
    CUnsignedBigInt(this.wrappedValue)
  }

  override def toUnsignedMod(m: UnsignedBigInt): UnsignedBigInt = {
    CUnsignedBigInt(this.wrappedValue.mod(m.asInstanceOf[CUnsignedBigInt].wrappedValue))
  }

}

