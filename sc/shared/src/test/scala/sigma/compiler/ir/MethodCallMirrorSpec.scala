package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast.{MethodCall, SGlobalMethods, SInt, SMethod, SType}
import sigma.ast.SType.tT
import sigmastate.helpers.CompilerTestingCommons

/** Pins that the explicit type arguments of a `MethodCall` (`fromBigEndianBytes[Int]`, `some[Int]`,
  * `none[Int]`) survive every graph path that rebuilds nodes (lambda unfolding, thunk inlining,
  * lazy boolean operands, option defaults) and reach the ErgoTree with their substitution intact.
  * `MethodCall.mirror` used to drop `typeSubst`; that path is not reachable from ErgoScript today
  * (this spec passes unchanged at 5633d00e3, before the fix), so the spec guards the invariant
  * rather than a fixed defect: a future rewrite that mirrors such a node would fail here.
  */
class MethodCallMirrorSpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** All `MethodCall` nodes of the tree, in traversal order. */
  private def methodCalls(v: Any): Seq[MethodCall] = v match {
    case mc: MethodCall => mc +: mc.productIterator.flatMap(methodCalls).toSeq
    case _: SMethod | _: SType => Seq()
    case xs: Iterable[_] => xs.toSeq.flatMap(methodCalls)
    case xs: Array[_] => xs.toSeq.flatMap(methodCalls)
    case p: Product => p.productIterator.flatMap(methodCalls).toSeq
    case _ => Seq()
  }

  /** Compiles `code` under v6 and asserts that every call of each `expected` global method in the
    * tree carries `Map(T -> tpe)` as its explicit type substitution. */
  private def compilesV6(code: String, expected: (SMethod, SType)*): Unit =
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion) {
      val tree = compile(Map(), code) // checkCompilerResult asserts the serialization round trip
      val calls = methodCalls(tree)
      for ((method, tpe) <- expected) {
        val found = calls.filter(mc => mc.method.objType == SGlobalMethods && mc.method.methodId == method.methodId)
        withClue(s"${method.name} calls in $tree:") { found should not be empty }
        found.foreach(_.typeSubst shouldBe Map(tT -> tpe))
      }
    }

  property("explicit type args survive lambda application") {
    compilesV6(
      """{
        |  val f = { (b: Coll[Byte]) => fromBigEndianBytes[Int](b) }
        |  f(getVar[Coll[Byte]](1).get) + f(getVar[Coll[Byte]](2).get) > 0
        |}""".stripMargin,
      SGlobalMethods.FromBigEndianBytesMethod -> SInt)
  }

  property("explicit type args survive a lambda returned from a lambda") {
    compilesV6(
      """{
        |  val g = { (n: Int) => { (b: Coll[Byte]) => fromBigEndianBytes[Int](b) + n } }
        |  g(1)(getVar[Coll[Byte]](1).get) > 0
        |}""".stripMargin,
      SGlobalMethods.FromBigEndianBytesMethod -> SInt)
  }

  property("explicit type args survive lazy operands and option defaults") {
    compilesV6(
      """{
        |  val b = getVar[Coll[Byte]](1).get
        |  (HEIGHT > 1 && fromBigEndianBytes[Int](b) > 0) &&
        |    getVar[Int](2).getOrElse(fromBigEndianBytes[Int](b)) > 0
        |}""".stripMargin,
      SGlobalMethods.FromBigEndianBytesMethod -> SInt)
    compilesV6(
      """{
        |  val o = getVar[Int](1)
        |  Global.some[Int](o.getOrElse(1)).isDefined && Global.none[Int]().isDefined == false
        |}""".stripMargin,
      SGlobalMethods.someMethod -> SInt, SGlobalMethods.noneMethod -> SInt)
  }
}
