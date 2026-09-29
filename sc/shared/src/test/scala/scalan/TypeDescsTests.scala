package scalan

class TypeDescsTests extends BaseCtxTests {

  lazy val ctx = new TestContext with TestLibrary
  import ctx._
  import Liftables._

  test("Implicit conversion from RType to Elem") {
    val eInt: Elem[Int] = sigma.IntType
    eInt shouldBe IntElement
  }
}
