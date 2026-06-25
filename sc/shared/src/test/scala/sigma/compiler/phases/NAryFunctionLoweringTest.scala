package sigma.compiler.phases

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.VersionContext
import sigma.ast._
import sigma.ast.syntax.SValue

class NAryFunctionLoweringTest extends AnyPropSpec with Matchers {

  private def id(n: String, t: SType): Ident = Ident(n, t)

  /** Run `block` with the v6 protocol version active.
    *
    * `NAryFunctionLowering` gates the Apply rewrite behind v6+, so any unit
    * test that constructs an n-arg `Apply` must be wrapped in this. Tests that
    * only exercise the (always-on) Lambda rewrite can run at any version.
    */
  private def atV6[A](block: => A): A =
    VersionContext.withVersions(
      activatedVersion = VersionContext.V6SoftForkVersion,
      ergoTreeVersion = VersionContext.V6SoftForkVersion)(block)

  property("1-arg lambda is unchanged") {
    val lam = Lambda(IndexedSeq("x" -> SInt), SInt, Some(id("x", SInt)))
    NAryFunctionLowering.lower(lam) shouldBe lam
  }

  property("2-arg lambda lowers to single tuple-arg with Block of two projections") {
    val body = id("a", SInt)
    val lam = Lambda(IndexedSeq("a" -> SInt, "b" -> SInt), SInt, Some(body))

    val lowered = NAryFunctionLowering.lower(lam).asInstanceOf[Lambda]
    lowered.args.length shouldBe 1

    val (tupName, tupTpe) = lowered.args.head
    tupTpe shouldBe STuple(SInt, SInt)

    val block = lowered.body.get.asInstanceOf[Block]
    block.bindings should have size 2
    val Seq(va, vb) = block.bindings
    va.name shouldBe "a"
    va.body shouldBe SelectField(Ident(tupName, tupTpe).asInstanceOf[Value[STuple]], 1.toByte)
    vb.name shouldBe "b"
    vb.body shouldBe SelectField(Ident(tupName, tupTpe).asInstanceOf[Value[STuple]], 2.toByte)
    block.result shouldBe body
  }

  property("3-arg lambda lowers to right-nested pair type and chained SelectFields") {
    val body = id("a", SInt)
    val lam = Lambda(IndexedSeq("a" -> SInt, "b" -> SInt, "c" -> SInt), SInt, Some(body))

    val lowered = NAryFunctionLowering.lower(lam).asInstanceOf[Lambda]
    val (tupName, tupTpe) = lowered.args.head
    tupTpe shouldBe STuple(SInt, STuple(SInt, SInt))

    val block = lowered.body.get.asInstanceOf[Block]
    val tupRef = Ident(tupName, tupTpe).asInstanceOf[Value[STuple]]
    val proj0 = SelectField(tupRef, 1.toByte)
    val proj1 = SelectField(SelectField(tupRef, 2.toByte).asInstanceOf[Value[STuple]], 1.toByte)
    val proj2 = SelectField(SelectField(tupRef, 2.toByte).asInstanceOf[Value[STuple]], 2.toByte)

    block.bindings.map(_.body) shouldBe Seq(proj0, proj1, proj2)
    block.bindings.map(_.name) shouldBe Seq("a", "b", "c")
  }

  property("4-arg lambda projections match the right-nested pair shape") {
    val lam = Lambda(
      IndexedSeq("a" -> SLong, "b" -> SLong, "c" -> SLong, "d" -> SLong),
      SLong,
      Some(id("a", SLong)))

    val lowered = NAryFunctionLowering.lower(lam).asInstanceOf[Lambda]
    val (tupName, tupTpe) = lowered.args.head
    tupTpe shouldBe STuple(SLong, STuple(SLong, STuple(SLong, SLong)))

    val tupRef = Ident(tupName, tupTpe).asInstanceOf[Value[STuple]]
    def sel2(v: SValue): Value[STuple] =
      SelectField(v.asInstanceOf[Value[STuple]], 2.toByte).asInstanceOf[Value[STuple]]
    val expected = Seq(
      SelectField(tupRef, 1.toByte),
      SelectField(sel2(tupRef), 1.toByte),
      SelectField(sel2(sel2(tupRef)), 1.toByte),
      SelectField(sel2(sel2(tupRef)), 2.toByte)
    )
    val block = lowered.body.get.asInstanceOf[Block]
    block.bindings.map(_.body) shouldBe expected
  }

  property("n-arg Apply is tupled into a single right-nested pair argument") {
    atV6 {
      val f = Ident("f", SFunc(IndexedSeq(SInt, SInt, SInt), SInt))
      val a1 = IntConstant(1)
      val a2 = IntConstant(2)
      val a3 = IntConstant(3)
      val app = Apply(f, IndexedSeq(a1, a2, a3))
      val lowered = NAryFunctionLowering.lower(app).asInstanceOf[Apply]
      lowered.func shouldBe f
      lowered.args.length shouldBe 1
      lowered.args.head shouldBe Tuple(IndexedSeq(a1, Tuple(IndexedSeq(a2, a3))))
    }
  }

  property("1-arg Apply is unchanged") {
    val f = Ident("f", SFunc(IndexedSeq(SInt), SInt))
    val app = Apply(f, IndexedSeq(IntConstant(7)))
    NAryFunctionLowering.lower(app) shouldBe app
  }

  property("lowering of n-arg Apply is gated to v6+") {
    val f = Ident("f", SFunc(IndexedSeq(SInt, SInt), SInt))
    val app = Apply(f, IndexedSeq(IntConstant(1), IntConstant(2)))
    // Pre-v6: throw a clear BuilderException.
    VersionContext.withVersions(activatedVersion = 2, ergoTreeVersion = 2) {
      val ex = the [sigma.exceptions.BuilderException] thrownBy
        NAryFunctionLowering.lower(app)
      ex.getMessage should include ("require ErgoTree v6+")
    }
    // v6+: the rewrite succeeds.
    atV6 {
      NAryFunctionLowering.lower(app).asInstanceOf[Apply].args.length shouldBe 1
    }
  }

  property("Lambda rewrite is not gated — n-ary lambdas lower at every version") {
    // The Lambda rule is plumbing required by HOF lambdas (e.g. fold's
    // 2-arg lambda) and must work at every protocol version.
    val lam = Lambda(IndexedSeq("a" -> SInt, "b" -> SInt), SInt, Some(id("a", SInt)))
    VersionContext.withVersions(activatedVersion = 2, ergoTreeVersion = 2) {
      val lowered = NAryFunctionLowering.lower(lam).asInstanceOf[Lambda]
      lowered.args.length shouldBe 1
      lowered.args.head._2 shouldBe STuple(SInt, SInt)
    }
  }

  property("lowering is idempotent") {
    val lam = Lambda(IndexedSeq("a" -> SInt, "b" -> SInt, "c" -> SInt), SInt, Some(id("a", SInt)))
    val once = NAryFunctionLowering.lower(lam)
    val twice = NAryFunctionLowering.lower(once)
    twice shouldBe once
  }

  property("1-arg lambda whose param is already an STuple passes through unchanged") {
    val tupTpe = STuple(SInt, SInt)
    val lam = Lambda(
      IndexedSeq("t" -> tupTpe),
      SInt,
      Some(SelectField(id("t", tupTpe).asInstanceOf[Value[STuple]], 1.toByte)))
    NAryFunctionLowering.lower(lam) shouldBe lam
  }

  property("sibling n-ary lambdas get distinct fresh tuple names") {
    val lam1 = Lambda(IndexedSeq("a" -> SInt, "b" -> SInt), SInt, Some(id("a", SInt)))
    val lam2 = Lambda(IndexedSeq("c" -> SInt, "d" -> SInt), SInt, Some(id("c", SInt)))
    val pair = Tuple(IndexedSeq(lam1, lam2))
    val lowered = NAryFunctionLowering.lower(pair).asInstanceOf[Tuple]
    val l1 = lowered.items(0).asInstanceOf[Lambda]
    val l2 = lowered.items(1).asInstanceOf[Lambda]
    l1.args.head._1 should not equal l2.args.head._1
  }

  property("mixed-type 3-arg lambda nests pairs left-to-right by argument order") {
    val lam = Lambda(
      IndexedSeq("a" -> SInt, "b" -> SLong, "c" -> SBoolean),
      SBoolean,
      Some(id("c", SBoolean)))
    val lowered = NAryFunctionLowering.lower(lam).asInstanceOf[Lambda]
    val (_, tupTpe) = lowered.args.head
    tupTpe shouldBe STuple(SInt, STuple(SLong, SBoolean))
  }

  property("nested lambdas lower independently and inner-lambda shadowing is preserved") {
    atV6 {
      val inner = Lambda(
        IndexedSeq("c" -> SInt, "d" -> SInt),
        SInt,
        Some(id("c", SInt)))
      val outer = Lambda(
        IndexedSeq("a" -> SInt, "b" -> SInt),
        SInt,
        Some(Apply(inner, IndexedSeq(id("a", SInt), id("b", SInt)))))

      val lowered = NAryFunctionLowering.lower(outer).asInstanceOf[Lambda]
      lowered.args.length shouldBe 1
      val outerBlock = lowered.body.get.asInstanceOf[Block]
      outerBlock.bindings.map(_.name) shouldBe Seq("a", "b")

      val innerApp = outerBlock.result.asInstanceOf[Apply]
      val innerLam = innerApp.func.asInstanceOf[Lambda]
      innerLam.args.length shouldBe 1
      val innerBlock = innerLam.body.get.asInstanceOf[Block]
      innerBlock.bindings.map(_.name) shouldBe Seq("c", "d")
    }
  }
}
