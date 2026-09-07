package scalan

import sigma.data.RType

class TypeDescsTests extends BaseCtxTests {

  lazy val ctx = new TestContext with TestLibrary
  import ctx._
  import Coll._
  import Liftables._

  test("Implicit conversion from RType to Elem") {
    val eInt: Elem[Int] = sigma.IntType
    eInt shouldBe IntElement

    val ePair: Elem[(Int, Coll[Byte])] = RType[(Int, SColl[Byte])]
    ePair shouldBe element[(Int, Coll[Byte])]
  }
}
