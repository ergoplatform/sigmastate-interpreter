package sigma.data

import sigma.util.Extensions.BigIntegerOps
import sigma.{BigInt, Coll, Colls}

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

  override def add(that: BigInt): BigInt = CBigInt(wrappedValue.add(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def subtract(that: BigInt): BigInt = CBigInt(wrappedValue.subtract(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def multiply(that: BigInt): BigInt = CBigInt(wrappedValue.multiply(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def divide(that: BigInt): BigInt = CBigInt(wrappedValue.divide(that.asInstanceOf[CBigInt].wrappedValue))

  override def mod(m: BigInt): BigInt = CBigInt(wrappedValue.mod(m.asInstanceOf[CBigInt].wrappedValue))

  override def remainder(that: BigInt): BigInt = CBigInt(wrappedValue.remainder(that.asInstanceOf[CBigInt].wrappedValue))

  override def min(that: BigInt): BigInt = CBigInt(wrappedValue.min(that.asInstanceOf[CBigInt].wrappedValue))

  override def max(that: BigInt): BigInt = CBigInt(wrappedValue.max(that.asInstanceOf[CBigInt].wrappedValue))

  override def negate(): BigInt = CBigInt(wrappedValue.negate().to256BitValueExact)

  override def and(that: BigInt): BigInt = CBigInt(wrappedValue.and(that.asInstanceOf[CBigInt].wrappedValue))

  override def or(that: BigInt): BigInt = CBigInt(wrappedValue.or(that.asInstanceOf[CBigInt].wrappedValue))
}
