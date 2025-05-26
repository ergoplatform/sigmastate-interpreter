package sigma.compiler.ir.wrappers
import _root_.sigma.data.RType
/** Base class for wrappers of such types as WOption, WRType etc.
  * Used in graph IR to implement method invocation.
  */
trait WrapSpecBase extends WrapSpec {
}

/** Wrappers spec for Option */
class OptionWrapSpec extends WrapSpecBase {
  def get[A](xs: Option[A]): A = xs.get
  def getOrElse[A](xs: Option[A], default: => A): A = xs.getOrElse(default)
  def map[A,B](xs: Option[A], f: A => B): Option[B] = xs.map(f)
  def filter[A](xs: Option[A], f: A => Boolean): Option[A] = xs.filter(f)
  def isDefined[A](xs: Option[A]): Boolean  = xs.isDefined
};

/** Wrappers spec for RType */
class RTypeWrapSpec extends WrapSpecBase {
  def name[T](d: RType[T]): String = d.name
}
