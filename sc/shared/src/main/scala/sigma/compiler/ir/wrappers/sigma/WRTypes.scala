package sigma.compiler.ir.wrappers.sigma

import sigma.compiler.ir.{Base, IRContext}


  /** IR representation of RType. */
  trait WRTypes extends Base { self: IRContext =>
    trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      def name: Ref[String]
    };
    trait WRTypeCompanion
  }