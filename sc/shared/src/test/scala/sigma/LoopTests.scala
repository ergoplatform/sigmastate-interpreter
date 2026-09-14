package sigma

import sigma.ast.{SBoxMethods, SCollectionMethods, SMethod}
import sigmastate.helpers.CompilerTestingCommons

class LoopTests extends CompilerTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext
  import IR._

  property("Test nested loop") {
    import Coll._
    import Box._
    /** A call node carrying its descriptor, as GraphBuilding builds them. */
    def call[R](receiver: Sym, m: SMethod, args: Sym*)(implicit eR: Elem[R]): Ref[R] =
      asRep[R](mkMethodCall(receiver, MethodCallee(m), args, Map(), eR))

    // variable to capture internal lambdas
    var proj2: Ref[((Coll[Byte], Int)) => Int] = null
    var sumFold: Ref[((Int, Int)) => Int] = null
    var pred: Ref[(((Coll[Byte], Int), sigma.Box)) => Boolean] = null
    var total: Ref[Int] = null

    val f = fun { in: Ref[(Coll[sigma.Box], Coll[(Coll[Byte], Int)])] =>
      val Pair(outputs, spenders) = in
      proj2 = fun { e: Ref[(Coll[Byte], Int)] => e._2 }
      val ratios = call[Coll[Int]](spenders, SCollectionMethods.MapMethod, proj2)
      sumFold = fun { in: Ref[(Int, Int)] => in._1 + in._2 }
      total = call[Int](ratios, SCollectionMethods.FoldMethod, toRep(0), sumFold)
      pred = fun { e: Ref[(((Coll[Byte], Int), sigma.Box))] =>
        val ratio = e._1._2
        val box = e._2
        val share = total * ratio
        call[Long](box, SBoxMethods.ValueMethod) >= share.toLong
      }
      val zipped = call[Coll[((Coll[Byte], Int), sigma.Box)]](spenders, SCollectionMethods.ZipMethod, outputs)
      call[Boolean](zipped, SCollectionMethods.ForallMethod, pred)
    }
    val Def(l: Lambda[_,_]) = f
    assert(l.freeVars.contains(proj2))
    assert(l.freeVars.contains(sumFold))
    val Def(p: Lambda[_,_]) = pred
    assert(p.freeVars.contains(total))
    emit("f", f)
  }
}
