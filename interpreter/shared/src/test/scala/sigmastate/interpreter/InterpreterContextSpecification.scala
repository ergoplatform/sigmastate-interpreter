package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import scorex.util.encode.Base16
import sigma.ast.{BoolToSigmaProp, ErgoTree, GT, Height, IntConstant, SType}
import sigma.interpreter.{ContextExtension, ProverResult, SigmaMap}
import sigmastate.helpers.TestingHelpers.testBox
import sigmastate.helpers.{ErgoLikeContextTesting, ErgoLikeTestInterpreter, TestingCommons}

class InterpreterContextSpecification extends TestingCommons {

  private def v(i: Int): sigma.ast.EvaluatedValue[_ <: SType] = IntConstant(i)

  private def baseCtx: ErgoLikeContext =
    ErgoLikeContextTesting.dummy(fakeSelf, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

  property("withBindings adds bindings to an empty extension") {
    val ctx = baseCtx.withBindings(5.toByte -> v(5), 7.toByte -> v(7))
    val ext = ctx.extension
    ext.values.size shouldBe 2
    ext.values.iterator.toList shouldBe List((5.toByte, v(5)), (7.toByte, v(7)))
    ext.get(5) shouldBe Some(v(5))
    ext.get(7) shouldBe Some(v(7))
  }

  property("withBindings resolves duplicate new bindings to first position and last value") {
    val ctx = baseCtx.withBindings(
      5.toByte -> v(50),
      7.toByte -> v(70),
      5.toByte -> v(55)
    )
    val ext = ctx.extension
    ext.values.size shouldBe 2
    ext.values.iterator.toList shouldBe List((5.toByte, v(55)), (7.toByte, v(70)))
    ext.get(5) shouldBe Some(v(55))
  }

  property("withBindings merges with existing extension keeping existing order and updating value") {
    val ext0 = ContextExtension(SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[_ <: SType]](
      7.toByte -> v(70),
      5.toByte -> v(50)
    )))
    val ctx = baseCtx.withExtension(ext0).withBindings(5.toByte -> v(55), 9.toByte -> v(99))
    val ext = ctx.extension
    ext.values.size shouldBe 3
    // existing order is preserved; key 5 keeps its position and gets the new value
    ext.values.iterator.toList shouldBe List((7.toByte, v(70)), (5.toByte, v(55)), (9.toByte, v(99)))
    ext.get(5) shouldBe Some(v(55))
    ext.get(7) shouldBe Some(v(70))
    ext.get(9) shouldBe Some(v(99))
  }

  property("withExtension replaces the extension completely") {
    val ext0 = ContextExtension(SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[_ <: SType]](5.toByte -> v(5))))
    val ext1 = ContextExtension(SigmaMap(Map[Byte, sigma.ast.EvaluatedValue[_ <: SType]](7.toByte -> v(7))))
    val ctx = baseCtx.withExtension(ext0).withExtension(ext1)
    ctx.extension shouldBe ext1
    ctx.extension.values.iterator.toList shouldBe List((7.toByte, v(7)))
  }

  property("withBindings on large extension normalizes to SigmaMap hash-trie order") {
    val ctx = baseCtx.withBindings(
      10.toByte -> v(10),
      20.toByte -> v(20),
      30.toByte -> v(30),
      40.toByte -> v(40),
      50.toByte -> v(50)
    )
    ctx.extension.values.size shouldBe 5
    ctx.extension.values.iterator.toList.map(_._1) shouldBe Seq[Byte](10, 20, 50, 40, 30)
  }

  property("toSigmaContext and verify reject unread malformed extension variable (0100830104a3)") {
    val malformedBytes = Base16.decode("0100830104a3").get
    val malformedExt = ContextExtension.serializer.fromBytes(malformedBytes)
    val verifier = new ErgoLikeTestInterpreter
    val prop = BoolToSigmaProp(GT(Height, IntConstant(-1)))

    for (actVer <- Seq[Byte](2, 3); treeVer <- Seq[Byte](0, 1, 2, 3) if treeVer <= actVer) {
      val tree = ErgoTree.fromProposition(ErgoTree.defaultHeaderWithVersion(treeVer), prop)
      val box = testBox(10, tree, creationHeight = 0)
      val ctx = ErgoLikeContextTesting.dummy(box, actVer).withErgoTreeVersion(treeVer)

      val directErr = the[AssertionError] thrownBy ctx.withExtension(malformedExt).toSigmaContext()
      directErr.getMessage should include("Value Height$(163) doesn't conform to type")
      directErr.getMessage should include("EvaluatedValue")

      val pr = ProverResult(Array.emptyByteArray, malformedExt)
      val res = verifier.verify(tree, ctx, pr, fakeMessage)
      res.isFailure shouldBe true
      res.failed.get shouldBe a[AssertionError]
      res.failed.get.getMessage should include("Value Height$(163) doesn't conform to type")
      res.failed.get.getMessage should include("EvaluatedValue")
    }
  }

  property("toSigmaContext rejects unread malformed variable in multi-entry small and large SigmaMap") {
    val malformedVal = ContextExtension.serializer.fromBytes(Base16.decode("0100830104a3").get).get(0).get

    val smallExt = ContextExtension(SigmaMap(
      Array[Byte](1, 2, 3),
      Array[sigma.ast.EvaluatedValue[_ <: SType]](v(1), v(2), malformedVal)
    ))
    an[AssertionError] should be thrownBy baseCtx.withExtension(smallExt).toSigmaContext()

    val largeExt = ContextExtension(SigmaMap(
      Array[Byte](10, 20, 30, 40, 50),
      Array[sigma.ast.EvaluatedValue[_ <: SType]](v(10), v(20), v(30), v(40), malformedVal)
    ))
    an[AssertionError] should be thrownBy baseCtx.withExtension(largeExt).toSigmaContext()
  }
}
