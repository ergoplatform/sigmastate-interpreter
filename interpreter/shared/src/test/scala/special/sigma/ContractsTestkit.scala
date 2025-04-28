package sigma

import sigma.Extensions.ArrayOps
import sigma.ast.ErgoTree
import sigma.ast.syntax.TrueSigmaProp
import sigma.data.{AvlTreeData, RType}
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._
import sigma.data._

trait ContractsTestkit {

  val Colls = new CollOverArrayBuilder
  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()
  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)
  val emptyAvlTree = new CAvlTree(AvlTreeData.dummy)
  val noHeaders = CSigmaDslBuilder.Colls.emptyColl[Header]
  val dummyPreHeader: PreHeader = null

  /** Create collection from array of items */
  def collection[T: RType](items: T*): Coll[T] = Colls.fromArray(items.toArray)

  val AliceId = Array[Byte](1) // 0x0001

  def newAliceBox(value: Long): Box = {
    val ergoBox = testBox(value,
      ErgoTree.fromProposition(TrueSigmaProp),
      creationHeight = 0, additionalTokens = Seq(), additionalRegisters = Map())
    new CBox(ergoBox)
  }

  def testContext(
      inputs: Array[Box], outputs: Array[Box], height: Int, self: Box,
      tree: AvlTree, minerPk: Array[Byte], activatedScriptVersion: Byte,
      currErgoTreeVersion: Byte, vars: ContextVarsMap) =
    new CContext(
      noInputs.toColl, noHeaders, dummyPreHeader,
      inputs.toColl, outputs.toColl, height, self, inputs.indexOf(self), tree,
      minerPk.toColl, vars, activatedScriptVersion, currErgoTreeVersion)

  def newContext(
      height: Int,
      self: Box,
      activatedScriptVersion: Byte,
      currErgoTreeVersion: Byte,
      vars: ContextVarsMap): CContext = {
    testContext(
      noInputs, noOutputs, height, self, emptyAvlTree, dummyPubkey,
      activatedScriptVersion, currErgoTreeVersion, vars)
  }

  implicit class TestContextOps(ctx: CContext) {
    def withInputs(inputs: Box*) = ctx.copy(inputs = inputs.toArray.toColl)

    def withOutputs(outputs: Box*) = ctx.copy(outputs = outputs.toArray.toColl)
  }
}
