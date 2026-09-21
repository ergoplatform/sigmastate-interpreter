package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast._
import sigma.ast.syntax.SValue
import sigma.exceptions.GraphBuildingException
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.helpers.SigmaPPrint

/** Pins what makes two call nodes the same node, i.e. what common-subexpression elimination may
  * merge: the descriptor, the receiver, the arguments, the explicit type arguments and the
  * structural result type. Each case failed on an earlier revision of the descriptor-carrying node.
  */
class CallIdentitySpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def compileV6(code: String): SValue =
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion)(compile(Map(), code))

  private def check(what: String, code: String, expected: SValue): Unit = withClue(s"$what: ") {
    val actual = compileV6(code)
    if (actual != expected) SigmaPPrint.pprintln(actual, width = 100)
    actual shouldBe expected
  }

  /** All `MethodCall` nodes of the tree. */
  private def methodCalls(v: Any): Seq[MethodCall] = v match {
    case mc: MethodCall => mc +: mc.productIterator.flatMap(methodCalls).toSeq
    case _: SMethod | _: SType => Seq()
    case xs: Iterable[_] => xs.toSeq.flatMap(methodCalls)
    case p: Product => p.productIterator.flatMap(methodCalls).toSeq
    case _ => Seq()
  }

  property("an inferable type argument written out gives the same node as the inferred form") {
    check("Global.serialize",
      "{ val x = getVar[Int](1).get; sigmaProp(Global.serialize(x) == Global.serialize[Int](x)) }",
      BoolToSigmaProp(TrueLeaf))
    check("Coll.updated",
      "{ val xs = getVar[Coll[Int]](1).get; sigmaProp(xs.updated(0, 1) == xs.updated[Int](0, 1)) }",
      BoolToSigmaProp(TrueLeaf))
  }

  property("an inferable type argument written out is not kept in the tree") {
    // `compile` asserts the serialization round trip, which only holds if the substitution is empty:
    // the serializer writes the explicit type arguments of a method and nothing else
    Seq(
      "{ sigmaProp(Global.serialize[Int](getVar[Int](1).get).size > 0) }",
      "{ sigmaProp(getVar[Int](1).filter[Int]({ (x: Int) => x > 0 }).isDefined) }"
    ).foreach { code =>
      withClue(code) { methodCalls(compileV6(code)).map(_.typeSubst) should contain only Map.empty }
    }
  }

  property("empty collections of different function types are different nodes") {
    // both element types render as `Int => Long => Byte`; identity must be the structural type
    val curried = SFunc(SInt, SFunc(SLong, SByte))
    val uncurried = SFunc(SFunc(SInt, SLong), SByte)
    compileV6("(Coll[(Int => Long) => Byte](), Coll[Int => Long => Byte]())").tpe shouldBe
      STuple(SCollection(uncurried), SCollection(curried))
    compileV6("(Coll[Int => Long => Byte](), Coll[(Int => Long) => Byte]())").tpe shouldBe
      STuple(SCollection(curried), SCollection(uncurried))
    // an unused `a` must not prime the node of `b` with the wrong element type
    compileV6(
      """{
        |  val a = Coll[(Int => Long) => Byte]()
        |  val b = Coll[Int => Long => Byte]()
        |  b.getOrElse(0, { (x: Int) => { (y: Long) => 1.toByte } })(1)(2L) == 1.toByte
        |}""".stripMargin)
    check("identically typed empty collections still merge", "{ sigmaProp(Coll[Int]() == Coll[Int]()) }", BoolToSigmaProp(TrueLeaf))
  }

  property("the shared numeric methods under v5 fail in graph building as before") {
    // the v5 copies keep the SNumericTypeMethods object as container; the IR never supported them
    VersionContext.withVersions(2.toByte, 2.toByte) {
      Seq("toBytes", "toBits").foreach { name =>
        val e = the[GraphBuildingException] thrownBy compile(Map(), s"{ sigmaProp(getVar[Long](1).get.$name.size > 0) }")
        e.getMessage should include ("Type Long doesn't have methods")
      }
    }
  }
}
