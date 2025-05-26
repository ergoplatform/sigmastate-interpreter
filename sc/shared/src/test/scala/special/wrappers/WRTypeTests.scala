package special.wrappers

import sigma.data.RType

import scala.language.reflectiveCalls

class WRTypeTests extends WrappersTests {

  lazy val ctx = new WrappersCtx
  import ctx._
  import Coll._
  import WRType._
  import EnvRep._
  import Liftables._

  test("invokeUnlifted") {
    val ty = RType[Int]
    check(ty, { env: EnvRep[WRType[Int]] => for { xs <- env } yield xs.name }, ty.name)
  }

  test("Implicit conversion from RType to Elem") {
    val eInt: Elem[Int] = sigma.IntType
    eInt shouldBe IntElement

    val ePair: Elem[(Int, Coll[Byte])] = RType[(Int, SColl[Byte])]
    ePair shouldBe element[(Int, Coll[Byte])]
  }
}
