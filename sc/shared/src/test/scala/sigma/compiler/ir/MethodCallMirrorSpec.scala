package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigmastate.helpers.CompilerTestingCommons

/** Probes whether a `MethodCall` with explicit type arguments can reach
  * `MethodCall.mirror` from ErgoScript. Until `mirror` was fixed it rebuilt the node without
  * `typeSubst`, so a mirrored `fromBigEndianBytes[T]` / `some[T]` call would fail in
  * `TreeBuilding` with a missing type substitution. Every script here must compile and
  * round-trip; the cases cover the paths that mirror graph nodes (lambda unfolding, thunk
  * inlining, lazy boolean operands, option defaults).
  */
class MethodCallMirrorSpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def compilesV6(code: String): Unit =
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion) {
      compile(Map(), code) // checkCompilerResult asserts the serialization round trip
    }

  property("explicit type args survive lambda application") {
    compilesV6(
      """{
        |  val f = { (b: Coll[Byte]) => fromBigEndianBytes[Int](b) }
        |  f(getVar[Coll[Byte]](1).get) + f(getVar[Coll[Byte]](2).get) > 0
        |}""".stripMargin)
  }

  property("explicit type args survive a lambda returned from a lambda") {
    compilesV6(
      """{
        |  val g = { (n: Int) => { (b: Coll[Byte]) => fromBigEndianBytes[Int](b) + n } }
        |  g(1)(getVar[Coll[Byte]](1).get) > 0
        |}""".stripMargin)
  }

  property("explicit type args survive lazy operands and option defaults") {
    compilesV6(
      """{
        |  val b = getVar[Coll[Byte]](1).get
        |  (HEIGHT > 1 && fromBigEndianBytes[Int](b) > 0) &&
        |    getVar[Int](2).getOrElse(fromBigEndianBytes[Int](b)) > 0
        |}""".stripMargin)
    compilesV6(
      """{
        |  val o = getVar[Int](1)
        |  Global.some[Int](o.getOrElse(1)).isDefined && Global.none[Int]().isDefined == false
        |}""".stripMargin)
  }
}
