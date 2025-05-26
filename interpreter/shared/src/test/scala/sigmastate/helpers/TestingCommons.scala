package sigmastate.helpers

import org.scalatest.propspec.AnyPropSpec
import sigmastate.TestsBase
import sigmastate.helpers.TestingHelpers.createBox
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import sigma.eval.SigmaDsl
import scorex.crypto.hash.Blake2b256
import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import org.scalatest.matchers.should.Matchers
import sigma.ast.syntax.GroupElementConstant
import sigma.crypto.EcPointType

trait TestingCommons extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with NegativeTesting
    with TestsBase {
  def fakeSelf: ErgoBox = createBox(0, TrueTree)

  def fakeContext: ErgoLikeContext =
    ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val fakeMessage = Blake2b256("Hello World")

  implicit def grElemConvert(leafConstant: GroupElementConstant): EcPointType =
    SigmaDsl.toECPoint(leafConstant.value).asInstanceOf[EcPointType]
}
