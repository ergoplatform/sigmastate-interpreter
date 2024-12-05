package sigma

import org.scalatest.BeforeAndAfterAll
import scalan.{BaseCtxTests, BaseLiftableTests}
import sigma.compiler.ir.IRContext
import sigma.data.TrivialProp
import sigma.eval.Extensions.toAnyValue
import sigmastate.eval._

import scala.language.reflectiveCalls

class SigmaDslStaginTests extends BaseCtxTests with ErgoScriptTestkit with BaseLiftableTests with BeforeAndAfterAll {
  class Ctx extends TestContext with IRContext with LiftableTestKit {
  }

  test("invokeUnlifted") {
    val cake = new Ctx
    import cake._
    import Box._
    import Coll._
    import Context._
    import EnvRep._
    import Liftables._
    import SigmaDslBuilder._
    import SigmaProp._

    val dsl: SSigmaDslBuilder = sigma.eval.SigmaDsl
    type RSigmaDslBuilder = cake.SigmaDslBuilder
    type RContext = cake.Context
    type RBox = cake.Box
    type RSigmaProp = cake.SigmaProp
    val boxA1 = newAliceBox(100)
    val boxA2 = newAliceBox(200)
    val ctx: SContext = newContext(10, boxA1, VersionContext.MaxSupportedScriptVersion, VersionContext.MaxSupportedScriptVersion)
      .withInputs(boxA2)
      .withVariables(Map(1 -> toAnyValue(30), 2 -> toAnyValue(40)))

    cake.check(dsl,  { env: EnvRep[RSigmaDslBuilder] =>
      for { dsl <- env; arg <- lifted(true) } yield dsl.sigmaProp(arg) }, dsl.sigmaProp(true))

    cake.check(ctx, { env: EnvRep[RContext] => for { obj <- env } yield obj.SELF }, ctx.SELF)
    cake.check(ctx, { env: EnvRep[RContext] =>
      for { obj <- env; id <- lifted(1.toByte) } yield obj.getVar[Int](id) }, ctx.getVar[Int](1))

    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.value }, boxA1.value)
    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env } yield obj.creationInfo }, boxA1.creationInfo)
    cake.check(boxA1, { env: EnvRep[RBox] => for { obj <- env; arg <- lifted(1) } yield obj.getReg[Coll[Byte]](arg) }, boxA1.getReg[sigma.Coll[Byte]](1))

  }

}
