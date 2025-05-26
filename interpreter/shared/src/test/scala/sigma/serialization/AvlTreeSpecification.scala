package sigma.serialization

import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.Colls
import sigma.data.{AvlTreeData, AvlTreeFlags}
import sigma.ast.AvlTreeConstant
import sigma.Extensions.ArrayOps
import sigma.eval.SigmaDsl
import sigmastate.eval.Extensions.AvlTreeOps

class AvlTreeSpecification extends SerializationSpecification {

  private val flags = Array(
    AvlTreeFlags(false, false, false),
    AvlTreeFlags(false, false, true),
    AvlTreeFlags(false, true, false),
    AvlTreeFlags(false, true, true),
    AvlTreeFlags(true, false, false),
    AvlTreeFlags(true, false, true),
    AvlTreeFlags(true, true, false),
    AvlTreeFlags(true, true, true)
  )

  property("roundtrip for all the possible AVL tree flags") {
    flags.foreach { f =>
      AvlTreeFlags(AvlTreeFlags.serializeFlags(f)) shouldBe f
    }
  }

  property("roundtrip for an AVL tree") {
    forAll(avlTreeGen) { t =>
      val v = AvlTreeConstant(t)
      roundTripTest(v)
    }
  }

  property("insert") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val digest = avlProver.digest
    val flags = AvlTreeFlags(true, false, false)
    val treeData = new AvlTreeData(Colls.fromArray(digest), flags, 32, None)
    val tree = SigmaDsl.avlTree(treeData)

    val k = Blake2b256.hash("1")
    val v = k
    avlProver.performOneOperation(Insert(ADKey @@@ k, ADValue @@@ v))
    val proof = avlProver.generateProof()

    val resTree = tree.insert(Array(k.toColl -> v.toColl).toColl, proof.toColl).get
    val resTreeAgain = tree.insert(Array(k.toColl -> v.toColl).toColl, proof.toColl).get

    // It is possible to insert wrong key. However, verifier is getting different digest then,
    // and this is to be checked independently.
    val wrongKey = Blake2b256.hash("2")
    val resTreeWrong = tree.insert(Array(wrongKey.toColl -> v.toColl).toColl, proof.toColl).get

    resTree shouldBe resTreeAgain
    resTree should not be resTreeWrong
  }

}
