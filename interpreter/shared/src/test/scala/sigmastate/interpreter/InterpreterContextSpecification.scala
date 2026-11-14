package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import sigma.ast.{IntConstant, SType}
import sigma.interpreter.{ContextExtension, SigmaMap}
import sigmastate.helpers.{ErgoLikeContextTesting, TestingCommons}

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
}
