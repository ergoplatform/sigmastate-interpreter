package sigma.data

/** Scala specific trick to appease erasure of methods argument types.
  * Example usage:
  * def m1(l: List[Int])(implicit o: Overloaded1)
  * def m2(l: List[String])(implicit o: Overloaded2)
  * Without the implicit arguments the methods would have identical signatures
  * after erasure, which is a compilation time error.
  */
object OverloadHack {
  trait Overloaded
  class Overloaded1 extends Overloaded { override def toString = "O1"}
  class Overloaded2 extends Overloaded { override def toString = "O2"}
  class Overloaded3 extends Overloaded { override def toString = "O3"}
  implicit val overloaded1: Overloaded1 = new Overloaded1
  implicit val overloaded2: Overloaded2 = new Overloaded2
  implicit val overloaded3: Overloaded3 = new Overloaded3
}