package sigmastate.serialization

import debox.cfor
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import scorex.util.encode.Base16
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast._
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.OpCodes.PlusCode

/** Generators and pre-built fixtures shared by the serialization benchmark suites.
  *
  * The benchmarks intentionally split two size ranges:
  *
  *   - `treeDepths`  — small, for nested AST structures where depth grows the tree
  *                     non-linearly (ArithOp, logical AND). Capped well below
  *                     `SigmaConstants.MaxTreeDepth`.
  *   - `collSizes`   — larger, for flat structures (Coll[Int]) where size is the
  *                     count of leaf items.
  *
  * Trees and their serialized byte forms are built once per size by Scalameter's
  * lazy `Gen` machinery; the closure passed to `using(...) in { ... }` runs many
  * times per measurement and should NOT rebuild fixtures inside.
  */
trait SerializationBenchmarkGens { suite: Bench[Double] =>

  /** Exponential depths for nested AST benchmarks: 5, 15, 45. Stays below
    * default tree depth limit even after wrapping in `BoolToSigmaProp(EQ(...))`. */
  val treeDepths: Gen[Int] = Gen.exponential("depth")(5, 45, 3)

  /** Collection sizes that keep the resulting `ErgoTree` below
    * `SigmaConstants.MaxPropositionBytes` (4096) once serialized — each
    * `IntConstant` may use up to 5 bytes of VLQ encoding plus per-item overhead. */
  val collSizes: Gen[Int] = Gen.enumeration("size")(10, 100, 500)

  /** A real v3 ErgoTree hex sample (with upcast) used by the spec tests. Small,
    * deterministic, and tied to a concrete on-chain shape. */
  val v3SampleHex: String = "0b15ea02e4dc650cfe020300020008b20e020102020100"
  val v3SampleBytes: Array[Byte] = Base16.decode(v3SampleHex).get

  /** ErgoTree wrapping a deeply nested `ArithOp(+)` chain of `IntConstant`s,
    * compared for equality so the result type is `SigmaProp`. */
  def buildArithOpTree(depth: Int): ErgoTree = {
    var expr: Value[SInt.type] = IntConstant(0)
    cfor(0)(_ < depth, _ + 1) { i =>
      expr = ArithOp(expr, IntConstant(i), PlusCode)
    }
    val prop = BoolToSigmaProp(EQ(expr, IntConstant(depth)))
    ErgoTree.withSegregation(ZeroHeader, prop)
  }

  /** ErgoTree wrapping a deep nested `AND` of pairwise booleans. */
  def buildLogicalAndTree(depth: Int): ErgoTree = {
    var expr: Value[SBoolean.type] = TrueLeaf
    cfor(0)(_ < depth, _ + 1) { _ =>
      expr = AND(ConcreteCollection(IndexedSeq(expr, TrueLeaf), SBoolean))
    }
    ErgoTree.withSegregation(ZeroHeader, BoolToSigmaProp(expr))
  }

  /** ErgoTree carrying a single `Coll[Int]` constant of the requested length. */
  def buildCollIntTree(size: Int): ErgoTree = {
    val items = new Array[Value[SInt.type]](size)
    cfor(0)(_ < size, _ + 1) { i => items(i) = IntConstant(i) }
    val coll = ConcreteCollection(items.toIndexedSeq, SInt)
    val prop = BoolToSigmaProp(EQ(SizeOf(coll), IntConstant(size)))
    ErgoTree.withSegregation(ZeroHeader, prop)
  }

  val arithOpTrees: Gen[ErgoTree] = treeDepths.map(buildArithOpTree)
  val logicalAndTrees: Gen[ErgoTree] = treeDepths.map(buildLogicalAndTree)
  val collIntTrees: Gen[ErgoTree] = collSizes.map(buildCollIntTree)

  val arithOpTreeBytes: Gen[Array[Byte]] =
    arithOpTrees.map(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree)
  val logicalAndTreeBytes: Gen[Array[Byte]] =
    logicalAndTrees.map(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree)
  val collIntTreeBytes: Gen[Array[Byte]] =
    collIntTrees.map(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree)
}
