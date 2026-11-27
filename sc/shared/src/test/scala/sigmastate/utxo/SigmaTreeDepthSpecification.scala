package sigmastate.utxo

import sigma.SigmaTreeDepthExceeded
import sigma.ast.syntax._
import sigma.ast.{AtLeast, ByteArrayConstant}
import sigma.data.{CAND, COR, CSigmaProp, SigmaBoolean}
import sigma.interpreter.ContextExtension
import sigmastate._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ErgoLikeContextTesting, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.interpreter.Interpreter

/** Tests for the consensus-level depth bound on runtime SigmaBoolean trees.
  * Without the bound, a script like
  * `getVar[Coll[Byte]](1).get.fold(pk, (acc, b) => acc && pk)` builds a CAND chain
  * whose depth equals the extension length, and deep trees overflow the stack in
  * recursive verification passes (computeCommitments, estimateCryptoVerifyCost),
  * making the verdict depend on the node's JVM stack size.
  */
class SigmaTreeDepthSpecification extends CompilerTestingCommons with CompilerCrossVersionProps {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** Left-nested CAND chain of the given depth: ((pk && pk) && ...) && pk */
  private def andChain(pk: SigmaBoolean, depth: Int): SigmaBoolean = {
    var res = pk
    var i = 1
    while (i < depth) {
      res = CAND.normalized(Seq(res, pk))
      i += 1
    }
    res
  }

  /** Left-nested COR chain of the given depth. */
  private def orChain(pk: SigmaBoolean, depth: Int): SigmaBoolean = {
    var res = pk
    var i = 1
    while (i < depth) {
      res = COR.normalized(Seq(res, pk))
      i += 1
    }
    res
  }

  property("CAND/COR normalized allow trees up to MaxDepth and reject deeper ones") {
    val pk = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    andChain(pk, SigmaBoolean.MaxDepth).depth shouldBe SigmaBoolean.MaxDepth
    orChain(pk, SigmaBoolean.MaxDepth).depth shouldBe SigmaBoolean.MaxDepth
    a[SigmaTreeDepthExceeded] should be thrownBy andChain(pk, SigmaBoolean.MaxDepth + 1)
    a[SigmaTreeDepthExceeded] should be thrownBy orChain(pk, SigmaBoolean.MaxDepth + 1)
  }

  property("AtLeast.reduce rejects trees deeper than MaxDepth") {
    val pk = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    val deep = andChain(pk, SigmaBoolean.MaxDepth)
    // 1 < bound < children.length so that CTHRESHOLD is constructed directly
    a[SigmaTreeDepthExceeded] should be thrownBy AtLeast.reduce(2, Seq(deep, pk, pk))
  }

  property("proof for MaxDepth tree is generated and verified, also on a small-stack thread") {
    val prover = new ErgoLikeTestProvingInterpreter()
    val pk = prover.dlogSecrets.head.publicImage
    val tree = mkTestErgoTree(andChain(pk, SigmaBoolean.MaxDepth))
    val box = testBox(10, tree, 0)
    val ctx = ErgoLikeContextTesting.dummy(box, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)

    val pr = prover.prove(tree, ctx, fakeMessage).get
    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(tree, ctx, pr, fakeMessage).get._1 shouldBe true

    // propBytes at the limit must not overflow (serializer recursion is depth-bounded now)
    CSigmaProp(andChain(pk, SigmaBoolean.MaxDepth)).propBytes.length should be > 0

    // verify on a thread with 1M stack: with the depth bound the verdict must not
    // depend on the stack size
    @volatile var res = false
    @volatile var error: Throwable = null
    val t = new Thread(null, new Runnable {
      def run(): Unit = {
        try {
          res = verifier.verify(tree, ctx, pr, fakeMessage).get._1
        } catch {
          case e: Throwable => error = e
        }
      }
    }, "small-stack-verifier", 1024 * 1024)
    t.start()
    t.join()
    if (error != null) throw error
    res shouldBe true
  }

  property("fold-built CAND chain from context extension is depth-limited") {
    val prover = new ErgoLikeTestProvingInterpreter()
    val pk = prover.dlogSecrets.head.publicImage
    val verifier = new ErgoLikeTestInterpreter
    // depth of the resulting SigmaBoolean = extension length + 1
    val ergoTree = mkTestErgoTree(compile(Map("pk" -> pk),
      "getVar[Coll[Byte]](1).get.fold(pk, {(acc: SigmaProp, b: Byte) => acc && pk})").asSigmaProp)
    val box = testBox(10, ergoTree, 0)

    def ctxWithExt(extLen: Int) = ErgoLikeContextTesting.dummy(box, activatedVersionInTests)
        .withErgoTreeVersion(ergoTreeVersionInTests)
        .withExtension(ContextExtension(Map(1.toByte -> ByteArrayConstant(Array.fill(extLen)(0: Byte)))))

    // extension length 109 -> depth 110, accepted and provable
    val pr = prover.prove(ergoTree, ctxWithExt(SigmaBoolean.MaxDepth - 1), fakeMessage).get
    verifier.verify(ergoTree, ctxWithExt(SigmaBoolean.MaxDepth - 1), pr, fakeMessage).get._1 shouldBe true

    // extension length 110 -> depth 111, rejected deterministically by both prover and verifier
    val ctxOver = ctxWithExt(SigmaBoolean.MaxDepth)
    a[SigmaTreeDepthExceeded] should be thrownBy
      prover.prove(ergoTree, ctxOver, fakeMessage).get
    // NOTE: verify(ergoTree, ctx, proverResult, msg) would replace the context extension
    // with pr.extension (the 109-byte one stored by the prover), so use the overload
    // taking raw proof bytes which keeps ctxOver's extension.
    a[SigmaTreeDepthExceeded] should be thrownBy
      verifier.verify(Interpreter.emptyEnv, ergoTree, ctxOver, pr.proof, fakeMessage).get
  }
}
