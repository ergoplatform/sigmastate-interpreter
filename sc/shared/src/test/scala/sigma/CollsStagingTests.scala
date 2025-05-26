package sigma

import scala.language.reflectiveCalls
import special.wrappers.WrappersTests
import scalan._
import sigma.compiler.ir.IRContext
import sigma.data.CollOverArrayBuilder

class CollsStagingTests extends WrappersTests {
  val printDebugInfo: Boolean = false

  class Ctx extends TestContext with TestLibrary {
    import Coll._
    import CollBuilder._
    lazy val t2 = fun { (c: Ref[Coll[Long]]) =>
      c.map(fun { x => x + 1L })
    }
    lazy val t3 = fun { (x: Ref[Int]) =>
      val b = colBuilder
      b.fromItems(x, x + 1, x + 2)
    }
  }

  test("Coll methods") {
    val ctx = new Ctx
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

  test("invokeTransformedAdapterMethodCall") {
    val ctx = new Ctx {
      useAlphaEquality = true
      keepOriginalFunc = false
    }
    import ctx._
    import Coll._
    val f = fun { col: Ref[Coll[Int]] =>  col.length }
    val g = fun { col: Ref[Coll[Int]] => f(col) }
    val exp = fun { col: Ref[Coll[Int]] => col.length }
    emit("graphs", f, g, exp)
    g shouldBe exp
  }

  test("invokeUnlifted for Col") {
    val ctx = new WrappersCtx with IRContext
    import ctx._
    import Coll._
    import CollBuilder._
    import EnvRep._
    import Liftables._

    val Cols: SCollBuilder = new CollOverArrayBuilder
    val arr = Array(1, 2, 3)
    val col = Cols.fromArray(arr)

    check(col, { env: EnvRep[Coll[Int]] => for {xs <- env; arg <- lifted(2) } yield xs.apply(arg) }, col.apply(2))

    val inc = (x: Int) => x + 1
    check(col, { env: EnvRep[Coll[Int]] => for { xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, col.map(inc))
  }

  test("invokeUnlifted for method of Ctor") {
    val ctx = new WrappersCtx with IRContext
    import ctx._
    import Coll._
    import CollBuilder._

    val Cols: SCollBuilder = new CollOverArrayBuilder
    val colData = Cols.replicate(10, 10)
    val colSym = colBuilder.replicate(10, 10)
    val resSym = colSym.append(colSym)
    val resData = colData.append(colData)
    val env = Map[Sym, AnyRef]()
    val resEnvSym = EnvRep.add(colSym -> colData.asInstanceOf[AnyRef])
    val (resEnv, _) = resEnvSym.run(env)
    resSym match {
      case Def(mc: MethodCall) =>
        val res = invokeUnlifted(colSym.elem, mc, resEnv)
        res shouldBe resData
    }
  }
}
