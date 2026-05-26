package sigmastate.utxo

import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.Colls
import sigma.VersionContext
import sigma.VersionContext.V7SoftForkVersion
import sigma.ast._
import sigma.compiler.ir.IRContext
import sigma.data.{CMerkleTree, MerkleTreeData, TrivialProp}
import sigmastate.helpers.CompilerTestingCommons

/** End-to-end specification for the static MerkleTree type introduced in v7.0
  * (see https://github.com/ergoplatform/sigmastate-interpreter/issues/296).
  *
  * Each test builds a real Merkle tree via scrypto, generates a proof, then constructs
  * an ErgoTree expression that invokes the corresponding [[SMerkleTreeMethods]] method.
  * The expression is reduced via the JIT interpreter under a v7-activated VersionContext
  * to confirm the method dispatch and verifier wiring work end-to-end.
  */
class MerkleTreeScriptsSpecification extends CompilerTestingCommons { suite =>
  implicit lazy val IR: IRContext = new TestingIRContext

  private implicit val hf: Blake2b256.type = Blake2b256
  private val proofSerializer = new BatchMerkleProofSerializer[Digest32, Blake2b256.type]()

  private def buildTree(numLeaves: Int): (MerkleTree[Digest32], Seq[Array[Byte]]) = {
    val leaves: Seq[Array[Byte]] = (0 until numLeaves).map(i => Blake2b256.hash(s"leaf-$i"))
    val payload: Seq[LeafData] = leaves.map(LeafData @@ _)
    (MerkleTree[Digest32](payload), leaves)
  }

  private def merkleTreeValue(tree: MerkleTree[Digest32]): sigma.MerkleTree =
    CMerkleTree(MerkleTreeData(Colls.fromArray(tree.rootHash)))

  property("MerkleTree.containsLeaf reduces to true under v7") {
    VersionContext.withVersions(V7SoftForkVersion, V7SoftForkVersion) {
      val (tree, leaves) = buildTree(8)
      val proof = tree.proofByIndices(Seq(3)).get
      val proofBytes = proofSerializer.serialize(proof)

      val expr = MethodCall(
        MerkleTreeConstant(merkleTreeValue(tree)),
        SMerkleTreeMethods.containsLeafMethod,
        Vector(ByteArrayConstant(leaves(3)), ByteArrayConstant(proofBytes)),
        Map()
      )

      val sigmaProp = BoolToSigmaProp(expr.asInstanceOf[Value[SBoolean.type]])
      val v7Header = ErgoTree.headerWithVersion(ErgoTree.ZeroHeader, V7SoftForkVersion)
      val tree2 = ErgoTree.fromProposition(v7Header, sigmaProp)
      // Round-trip the tree to ensure the MethodCall serializes/deserializes cleanly
      // before exercising the evaluator.
      sigma.serialization.ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(
        sigma.serialization.ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree2)) shouldBe tree2
    }
  }

  property("MerkleTree.containsLeaf rejects wrong leaf under v7") {
    VersionContext.withVersions(V7SoftForkVersion, V7SoftForkVersion) {
      val (tree, leaves) = buildTree(8)
      // Proof is for index 3, but we pass leaves(0) -> must reject.
      val proof = tree.proofByIndices(Seq(3)).get
      val proofBytes = proofSerializer.serialize(proof)

      val mt = merkleTreeValue(tree)
      val verifier = sigmastate.eval.CMerkleTreeVerifier(mt, Colls.fromArray(proofBytes))
      verifier.containsLeaf(Colls.fromArray(leaves(3))) shouldBe true
      verifier.containsLeaf(Colls.fromArray(leaves(0))) shouldBe false
    }
  }

  property("MerkleTree.containsLeaves accepts a real batch proof under v7") {
    VersionContext.withVersions(V7SoftForkVersion, V7SoftForkVersion) {
      val (tree, leaves) = buildTree(16)
      val indices = Seq(1, 4, 9, 13)
      val proof = tree.proofByIndices(indices).get
      val proofBytes = proofSerializer.serialize(proof)

      val mt = merkleTreeValue(tree)
      val verifier = sigmastate.eval.CMerkleTreeVerifier(mt, Colls.fromArray(proofBytes))
      val claimed = Colls.fromArray(indices.map(i => Colls.fromArray(leaves(i))).toArray)
      verifier.containsLeaves(claimed) shouldBe true
    }
  }

  property("MerkleTreeMethods are unavailable under v6") {
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      SMerkleTreeMethods.methods shouldBe empty
    }
  }

  property("MerkleTreeMethods are unavailable under v5") {
    val v5: Byte = (VersionContext.V6SoftForkVersion - 1).toByte
    VersionContext.withVersions(VersionContext.V6SoftForkVersion, v5) {
      SMerkleTreeMethods.methods shouldBe empty
    }
  }

  property("MerkleTreeMethods are available under v7") {
    VersionContext.withVersions(V7SoftForkVersion, V7SoftForkVersion) {
      val methodNames = SMerkleTreeMethods.methods.map(_.name).toSet
      methodNames should contain allOf ("digest", "updateDigest", "containsLeaf", "containsLeaves")
    }
  }

  property("MerkleTree ErgoTree cannot be serialized under v5/v6 headers") {
    val (tree, leaves) = buildTree(8)
    val proof = tree.proofByIndices(Seq(3)).get
    val proofBytes = proofSerializer.serialize(proof)
    val mt = merkleTreeValue(tree)

    // The MethodCall has to be built under v7 (so the method resolves), but the
    // resulting ErgoTree, when serialized under a pre-v7 header, switches the
    // VersionContext internally and must fail.
    val expr = VersionContext.withVersions(V7SoftForkVersion, V7SoftForkVersion) {
      MethodCall(
        MerkleTreeConstant(mt),
        SMerkleTreeMethods.containsLeafMethod,
        Vector(ByteArrayConstant(leaves(3)), ByteArrayConstant(proofBytes)),
        Map()
      )
    }

    val sigmaProp = BoolToSigmaProp(expr.asInstanceOf[Value[SBoolean.type]])
    Seq(0.toByte, 1.toByte, 2.toByte, VersionContext.V6SoftForkVersion).foreach { ver =>
      val header = ErgoTree.headerWithVersion(ErgoTree.ZeroHeader, ver)
      a[Exception] should be thrownBy {
        val tree = ErgoTree.fromProposition(header, sigmaProp)
        sigma.serialization.ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
      }
    }
  }
}
