package sigma.data

import sigma.AnyValue
import sigma.data.OverloadHack.Overloaded1

import scala.annotation.unused

/** Default implementation of AnyValue interface. */
case class CAnyValue[A](value: A, tVal: RType[Any]) extends AnyValue {
  def tA: RType[A] = tVal.asInstanceOf[RType[A]]

  override def toString = s"TestValue($value)"
}

object CAnyValue {
  def apply[A](value: A)(implicit t: RType[A], @unused o: Overloaded1): CAnyValue[A] =
    new CAnyValue(value, t.asInstanceOf[RType[Any]])
}