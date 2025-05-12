package sigmastate.lang

import org.scalatest.matchers.should.Matchers
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.ast._
import sigma.crypto.CryptoConstants
import sigma.data.{CAnyValue, CSigmaDslBuilder, ProveDHTuple, ProveDlog, SigmaBoolean}
import sigma.util.Extensions.BigIntegerOps
import sigma._
import sigmastate.helpers.NegativeTesting
import sigmastate.interpreter.Interpreter.ScriptEnv

import java.math.BigInteger

trait LangTests extends Matchers with NegativeTesting {

  def BoolIdent(name: String): Value[SBoolean.type] = Ident(name).asValue[SBoolean.type]
  def IntIdent(name: String): Value[SLong.type] = Ident(name).asValue[SLong.type]
  def ByteIdent(name: String): Value[SByte.type] = Ident(name).asValue[SByte.type]
  def ByteArrayIdent(name: String): Value[SByteArray] = Ident(name).asValue[SByteArray]
  def GEIdent(name: String): Value[SGroupElement.type] = Ident(name).asValue[SGroupElement.type]
  def SigmaPropIdent(name: String): Value[SSigmaProp.type] = Ident(name).asValue[SSigmaProp.type]
  def BigIntIdent(name: String): Value[SBigInt.type] = Ident(name).asValue[SBigInt.type]

  def plus(l: SValue, r: SValue, tpe: SType = NoType): MethodCallLike =
    MethodCallLike(l, "+", IndexedSeq(r), tpe)

  val EV: ScriptEnv = Map()

  val arr1 = Array[Byte](1, 2)
  val arr2 = Array[Byte](10, 20)
  val dlog = CryptoConstants.dlogGroup
  val ecp1 = dlog.generator
  val ecp2 = dlog.multiplyGroupElements(ecp1, ecp1)
  val ecp3 = dlog.multiplyGroupElements(ecp2, ecp2)
  val ecp4 = dlog.multiplyGroupElements(ecp3, ecp3)
  val g1 = CSigmaDslBuilder.GroupElement(ecp1)
  val g2 = CSigmaDslBuilder.GroupElement(ecp2)
  val g3 = CSigmaDslBuilder.GroupElement(ecp3)
  val g4 = CSigmaDslBuilder.GroupElement(ecp4)

  protected val n1: BigInt = BigInt(10).underlying().toBigInt
  protected val n2: BigInt = BigInt(20).underlying().toBigInt
  protected val bigIntegerArr1: Coll[BigInt] = Colls.fromItems(n1, n2)
  protected val big: BigInteger = BigInt(Long.MaxValue).underlying().pow(2)
  protected val p1: SigmaBoolean = ProveDlog(ecp1)
  protected val p2: SigmaBoolean = ProveDlog(ecp2)
  protected val dht1: SigmaBoolean = ProveDHTuple(ecp1, ecp2, ecp3, ecp4)

  // to support both JVM and JS we need to wrap numeric values into CAnyValue
  val env = Map(
    "x" -> CAnyValue(10), "y" -> CAnyValue(11), "c1" -> true, "c2" -> false,
    "height1" -> 100L, "height2" -> 200L,
    "b1" -> CAnyValue(1.toByte),
    "b2" -> CAnyValue(2.toByte),
    "arr1" -> arr1,
    "arr2" -> arr2,
    "col1" -> ConcreteCollection.fromItems(LongConstant(1), LongConstant(2)),
    "col2" -> ConcreteCollection.fromItems(LongConstant(10), LongConstant(20)),
    "g1" -> g1,
    "g2" -> g2,
    "p1" -> p1,
    "p2" -> p2,
    "n1" -> n1,
    "n2" -> n2,
    "big" -> big,
    "bigIntArr1" -> bigIntegerArr1
  )

  /** Parses string to SType tree */
  def ty(s: String): SType = SigmaParser.parseType(s)

  def assertSrcCtxForAllNodes(tree: SValue): Unit = {
    import sigma.kiama.rewriting.Rewriter._
    rewrite(everywherebu(rule[Any] {
      case node: SValue =>
        withClue(s"Missing sourceContext for $node") { node.sourceContext.isDefined shouldBe true }
        node
    }))(tree)
  }

  /** Execute the given `block` having `version` as both activated and ErgoTree version. */
  def runWithVersion[T](version: Byte)(block: => T): T = {
    VersionContext.withVersions(version, version)(block)
  }

}
