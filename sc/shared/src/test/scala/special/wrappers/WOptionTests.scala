package special.wrappers

import scala.language.reflectiveCalls

class WOptionTests extends WrappersTests {

  test("invokeUnlifted") {
    val ctx = new WrappersCtx
    import ctx._
    import WOption._
    import EnvRep._

    val opt = Option(1)
    check(opt, { env: EnvRep[WOption[Int]] => for { xs <- env } yield xs.get }, opt.get)
    check(opt, { env: EnvRep[WOption[Int]] => for { xs <- env } yield xs.isDefined }, opt.isDefined)

    val none: Option[Int] = None
    val th = () => 10
    check(none, { env: EnvRep[WOption[Int]] => for { xs <- env; thL <- lifted(th) } yield xs.getOrElse(thL) }, none.getOrElse(th()))

    val p = (x: Int) => x == 2
    check(opt, { env: EnvRep[WOption[Int]] => for { xs <- env; pL <- lifted(p) } yield  xs.filter(pL) }, opt.filter(p))

    val inc = (x: Int) => x + 1

    check(opt, { env: EnvRep[WOption[Int]] => for { xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, opt.map(inc))
  }
}
