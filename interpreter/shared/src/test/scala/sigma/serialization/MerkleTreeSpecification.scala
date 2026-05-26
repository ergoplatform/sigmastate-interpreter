package sigma.serialization

import org.scalacheck.{Arbitrary, Gen}
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.authds.merkle.serialization.BatchMerkleProofSerializer
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.VersionContext
import sigma.VersionContext.V7SoftForkVersion
import sigma.Colls
import sigma.ast.MerkleTreeConstant
import sigma.data.{CMerkleTree, MerkleTreeData}

class MerkleTreeSpecification extends SerializationSpecification {

  private val ver7 = V7SoftForkVersion

  /** Generator: 32 random bytes used as a Merkle tree root digest. */
  private def digestGen: Gen[Array[Byte]] =
    Gen.listOfN(MerkleTreeData.DigestSize, Arbitrary.arbitrary[Byte]).map(_.toArray)

  property("MerkleTreeData roundtrip") {
    forAll(digestGen) { digestBytes =>
      val data = MerkleTreeData(Colls.fromArray(digestBytes))
      val w = SigmaSerializer.startWriter()
      MerkleTreeData.serializer.serialize(data, w)
      val parsed = MerkleTreeData.serializer.parse(SigmaSerializer.startReader(w.toBytes))
      parsed shouldBe data
    }
  }

  property("MerkleTreeConstant serialization roundtrip under v7") {
    forAll(digestGen) { digestBytes =>
      val tree = CMerkleTree(MerkleTreeData(Colls.fromArray(digestBytes)))
      val v = MerkleTreeConstant(tree)
      roundTripTest(v, Some(ver7))
    }
  }

  property("BatchMerkleProof verifies for a real Merkle tree") {
    implicit val hf: Blake2b256.type = Blake2b256
    val leaves: Seq[Array[Byte]] = (0 until 8).map(i => Blake2b256.hash(s"leaf-$i"))
    val payload: Seq[LeafData] = leaves.map(LeafData @@ _)
    val tree = MerkleTree[Digest32](payload)
    val serializer = new BatchMerkleProofSerializer[Digest32, Blake2b256.type]()(hf)

    // Build a single-leaf batch proof.
    val proof = tree.proofByIndices(Seq(3)).get
    val proofBytes = serializer.serialize(proof)

    VersionContext.withVersions(ver7, ver7) {
      val mt = CMerkleTree(MerkleTreeData(Colls.fromArray(tree.rootHash)))
      // Construct verifier and check it accepts the leaf at index 3:
      val verifier = sigmastate.eval.CMerkleTreeVerifier(mt, Colls.fromArray(proofBytes))
      verifier.containsLeaf(Colls.fromArray(leaves(3))) shouldBe true
      // wrong leaf -> rejected
      verifier.containsLeaf(Colls.fromArray(leaves(0))) shouldBe false
    }
  }

  property("malformed proof bytes do not throw, just return false") {
    val digest = Array.fill[Byte](MerkleTreeData.DigestSize)(7.toByte)
    val mt = CMerkleTree(MerkleTreeData(Colls.fromArray(digest)))
    val garbage = Array.fill[Byte](16)(0.toByte) // too short to be a valid proof
    val verifier = sigmastate.eval.CMerkleTreeVerifier(mt, Colls.fromArray(garbage))
    verifier.containsLeaf(Colls.fromArray(Array.fill[Byte](4)(1.toByte))) shouldBe false
    verifier.containsLeaves(Colls.fromArray(Array(Colls.fromArray(Array.fill[Byte](4)(1.toByte))))) shouldBe false
  }

  property("batch proof verifies multiple leaves") {
    implicit val hf: Blake2b256.type = Blake2b256
    val leaves: Seq[Array[Byte]] = (0 until 16).map(i => Blake2b256.hash(s"item-$i"))
    val payload: Seq[LeafData] = leaves.map(LeafData @@ _)
    val tree = MerkleTree[Digest32](payload)
    val serializer = new BatchMerkleProofSerializer[Digest32, Blake2b256.type]()(hf)

    val indices = Seq(1, 4, 9)
    val proof = tree.proofByIndices(indices).get
    val proofBytes = serializer.serialize(proof)

    VersionContext.withVersions(ver7, ver7) {
      val mt = CMerkleTree(MerkleTreeData(Colls.fromArray(tree.rootHash)))
      val verifier = sigmastate.eval.CMerkleTreeVerifier(mt, Colls.fromArray(proofBytes))
      val claimed = Colls.fromArray(indices.map(i => Colls.fromArray(leaves(i))).toArray)
      verifier.containsLeaves(claimed) shouldBe true

      // wrong leaf in batch -> rejected
      val wrongClaim = Colls.fromArray(
        Array(Colls.fromArray(leaves(1)), Colls.fromArray(leaves(2)), Colls.fromArray(leaves(9))))
      verifier.containsLeaves(wrongClaim) shouldBe false
    }
  }

  // ---------------------------------------------------------------------------
  // Pre-v7 negative tests
  //
  // MerkleTree must not be reachable from v5/v6 contexts. These assertions pin
  // each gating layer separately so future changes don't silently re-expose it.
  // ---------------------------------------------------------------------------

  private val ver6: Byte = VersionContext.V6SoftForkVersion
  private val ver5: Byte = (VersionContext.V6SoftForkVersion - 1).toByte

  property("SMerkleTree.typeCode does not deserialize in v5/v6") {
    val w = SigmaSerializer.startWriter()
    w.put(sigma.ast.SMerkleTree.typeCode)
    val bytes = w.toBytes
    Seq(ver5, ver6).foreach { v =>
      val activated = if (v < VersionContext.V6SoftForkVersion) ver6 else v
      VersionContext.withVersions(activated, v) {
        // TypeSerializer dispatches to NoType (after CheckTypeCode) for unknown codes
        // under pre-v7; validation rule yields ValidationException.
        a[Exception] should be thrownBy
          TypeSerializer.deserialize(SigmaSerializer.startReader(bytes))
      }
    }
  }

  property("MerkleTreeData does not serialize in v5/v6 via CoreDataSerializer") {
    val mt = CMerkleTree(MerkleTreeData(Colls.fromArray(Array.fill[Byte](32)(0.toByte))))
    Seq(ver5, ver6).foreach { v =>
      val activated = if (v < VersionContext.V6SoftForkVersion) ver6 else v
      VersionContext.withVersions(activated, v) {
        val w = SigmaSerializer.startWriter()
        a[SerializerException] should be thrownBy
          DataSerializer.serialize[sigma.ast.SMerkleTree.type](mt, sigma.ast.SMerkleTree, w)
      }
    }
  }

  property("Constant[SMerkleTree] does not round-trip in v5/v6") {
    val mt = CMerkleTree(MerkleTreeData(Colls.fromArray(Array.fill[Byte](32)(0.toByte))))
    val c = sigma.ast.MerkleTreeConstant(mt)
    Seq(ver5, ver6).foreach { v =>
      val activated = if (v < VersionContext.V6SoftForkVersion) ver6 else v
      VersionContext.withVersions(activated, v) {
        a[Exception] should be thrownBy ValueSerializer.serialize(c)
      }
    }
  }

  property("SMerkleTreeMethods is empty in v5/v6") {
    Seq(ver5, ver6).foreach { v =>
      val activated = if (v < VersionContext.V6SoftForkVersion) ver6 else v
      VersionContext.withVersions(activated, v) {
        sigma.ast.SMerkleTreeMethods.methods shouldBe empty
      }
    }
  }

  property("SMerkleTreeMethods is populated in v7") {
    VersionContext.withVersions(ver7, ver7) {
      sigma.ast.SMerkleTreeMethods.methods.map(_.name).toSet shouldBe
        Set("digest", "updateDigest", "containsLeaf", "containsLeaves")
    }
  }
}
