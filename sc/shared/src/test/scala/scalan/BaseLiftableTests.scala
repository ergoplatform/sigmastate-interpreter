package scalan

import sigma.compiler.ir.IRContext

trait BaseLiftableTests { self: BaseCtxTests =>

  trait LiftableTestKit { scalan: IRContext =>
    import Liftables._

    /** Check the MethodCall reified in f can be mapped to unlifted method which can be invoked.*/
    def check[ST, T](obj: ST, f: EnvRep[T] => EnvRep[_], expected: Any)(implicit lT: Liftable[ST,T]) = {
      val objSym: Ref[T] = liftConst(obj)
      val env = Map[Sym, AnyRef]()
      val resEnvSym = f(EnvRep.add(objSym -> obj.asInstanceOf[AnyRef]))
      val (resEnv, resSym) = resEnvSym.run(env)
      resSym match {
        case Def(mc: MethodCall) =>
          val res = invokeUnlifted(objSym.elem, mc, resEnv)
          res shouldBe expected
      }
    }
  }

}
