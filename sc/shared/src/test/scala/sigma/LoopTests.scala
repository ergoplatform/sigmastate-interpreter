package sigma

import sigmastate.helpers.CompilerTestingCommons

class LoopTests extends CompilerTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext
  import IR._

  property("Test nested loop") {
    import Coll._
    import Context._; import Box._
    // variable to capture internal lambdas
    var proj2: Ref[((Coll[Byte], Int)) => Int] = null
    var sumFold: Ref[((Int, Int)) => Int] = null
    var pred: Ref[(((Coll[Byte], Int), Box)) => Boolean] = null
    var total: Ref[Int] = null

    val f = fun { in: Ref[(Context, Coll[(Coll[Byte], Int)])] =>
      val Pair(ctx, spenders) = in
      val feeBox = ctx.OUTPUTS(0)
      proj2 = fun { e: Ref[(Coll[Byte], Int)] => e._2 }
      val ratios = spenders.map(proj2)
      sumFold = fun { in: Ref[(Int, Int)] => in._1 + in._2 }
      total = ratios.foldLeft(0, sumFold)
      pred = fun { e: Ref[(((Coll[Byte], Int), Box))] =>
        val ratio = e._1._2
        val box = e._2
        val share = total * ratio
        box.value >= share.toLong
      }
      val validOuts = spenders.zip(ctx.OUTPUTS).forall(pred)
      validOuts
    }
    val Def(l: Lambda[_,_]) = f
    assert(l.freeVars.contains(proj2))
    assert(l.freeVars.contains(sumFold))
    val Def(p: Lambda[_,_]) = pred
    assert(p.freeVars.contains(total))
    emit("f", f)
  }
}

