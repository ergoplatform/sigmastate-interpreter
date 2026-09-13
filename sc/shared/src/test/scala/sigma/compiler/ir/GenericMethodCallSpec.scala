package sigma.compiler.ir

import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast.{ErgoTree, MethodCall, Value}
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.helpers.CompilerTestingCommons

/** Methods that have no dedicated ErgoTree node must flow through the compiler as plain
  * `MethodCall` nodes with no per-method code in `sc`: script → ErgoTree → bytes → ErgoTree.
  * Runs on JVM and JS (spec success criterion 4).
  */
class GenericMethodCallSpec extends CompilerTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** (method name expected in the tree, script producing a SigmaProp). */
  val cases: Seq[(String, String)] = Seq(
    "checkPow"   -> "{ sigmaProp(getVar[Header](1).get.checkPow) }",
    "digest"     -> "{ sigmaProp(getVar[AvlTree](1).get.digest.size > 0) }",
    "modInverse" -> "{ val x = getVar[UnsignedBigInt](1).get; sigmaProp(x.modInverse(getVar[UnsignedBigInt](2).get) == x) }",
    "some"       -> "{ sigmaProp(Global.some[Int](getVar[Int](1).get).isDefined) }"
  )

  /** All nodes of an ErgoTree expression, in pre-order. */
  private def nodes(v: SValue): Seq[SValue] = {
    def walk(x: Any): Seq[SValue] = x match {
      case s: Value[_] => s +: s.productIterator.toSeq.flatMap(walk(_))
      case xs: Iterable[_] => xs.toSeq.flatMap(walk)
      case _ => Seq.empty
    }
    walk(v)
  }

  property("methods without a dedicated node compile to MethodCall and round-trip through bytes") {
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion) {
      cases.foreach { case (name, code) =>
        withClue(s"$name: ") {
          val prop = compile(Map(), code)
          val calls = nodes(prop).collect { case mc: MethodCall if mc.method.name == name => mc }
          calls should have size 1
          // a v6 method id only deserializes under a v3 tree header (with the size bit, as v6 requires)
          val header = ErgoTree.setSizeBit(ErgoTree.headerWithVersion(ErgoTree.ZeroHeader, V6SoftForkVersion))
          val tree = ErgoTree.withSegregation(header, prop.asSigmaProp)
          val bytes = DefaultSerializer.serializeErgoTree(tree)
          DefaultSerializer.deserializeErgoTree(bytes) shouldBe tree
        }
      }
    }
  }
}
