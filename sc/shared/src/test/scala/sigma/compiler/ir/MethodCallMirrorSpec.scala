package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast.{MethodCall, SGlobalMethods, SInt, SMethod, SType}
import sigma.ast.SType.tT
import sigmastate.helpers.CompilerTestingCommons

/** Pins that the explicit type arguments of a `MethodCall` (`fromBigEndianBytes[Int]`, `some[Int]`,
  * `none[Int]`) reach the ErgoTree with their substitution intact through every graph path that
  * rebuilds nodes: lambda unfolding, thunk inlining, lazy boolean operands, option defaults and
  * the map-fusion rule, which mirrors both lambda bodies. `MethodCall.mirror` used to drop
  * `typeSubst`; TreeBuilding now takes the substitution from the node, so a regression there
  * shows up below as a tree without the type argument. The last property exercises `mirror`
  * directly.
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

  property("explicit type args survive the map-fusion rule, which mirrors the lambda bodies") {
    compilesV6(
      """{
        |  val xs = getVar[Coll[Coll[Byte]]](1).get
        |  xs.map({ (b: Coll[Byte]) => fromBigEndianBytes[Int](b) }).map({ (y: Int) => y + 1 })(0) > 0
        |}""".stripMargin,
      SGlobalMethods.FromBigEndianBytesMethod -> SInt)
  }

  property("MethodCall.mirror keeps the type substitution") {
    import IR.{ByteElement, IntElement, MapTransformer, MethodCallee, collElement, mkMethodCall, sigmaDslBuilderElement, toLazyElem, variable}
    val global = variable[sigma.SigmaDslBuilder]
    val bytes = variable[sigma.Coll[Byte]]
    val bytes2 = variable[sigma.Coll[Byte]]
    val subst = Map(tT -> (SInt: SType))
    val call = mkMethodCall(global, MethodCallee(SGlobalMethods.FromBigEndianBytesMethod), Seq(bytes), subst, IntElement)
    val mirrored = call.node.mirror(new MapTransformer(bytes -> bytes2))
    mirrored should not be call
    mirrored.node.asInstanceOf[IR.MethodCall].typeSubst shouldBe subst
  }
}
