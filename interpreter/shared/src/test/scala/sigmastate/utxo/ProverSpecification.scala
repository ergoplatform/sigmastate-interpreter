package sigmastate.utxo

import org.ergoplatform.ErgoLikeInterpreter
import scorex.crypto.hash.Blake2b256
import sigma.crypto.SecP256K1Group
import sigma.data.{CAND, COR, CTHRESHOLD, SigmaBoolean, TrivialProp}
import sigma.exceptions.InterpreterException
import sigmastate._
import sigmastate.crypto.DLogProtocol.FirstDLogProverMessage
import sigmastate.crypto.FirstDHTupleProverMessage
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, TestingCommons}
import sigmastate.interpreter.{HintsBag, ProverInterpreter}

class ProverSpecification extends TestingCommons {

  property("generateCommitments") {

    val prover = new ErgoLikeTestProvingInterpreter

    val pk: SigmaBoolean = prover.dlogSecrets.head.publicImage
    val pk2: SigmaBoolean = prover.dhSecrets.head.publicImage

    prover.generateCommitmentsFor(pk, Seq(pk2)).hints.isEmpty shouldBe true

    val pk3 = CAND(Seq(pk, pk2))
    prover.generateCommitmentsFor(pk, Seq(pk3)).hints.isEmpty shouldBe true
    prover.generateCommitmentsFor(pk3, Seq(pk3)).hints.isEmpty shouldBe true

    val h = prover.generateCommitmentsFor(pk, Seq(pk))
    h.hints.nonEmpty shouldBe true
    h.realCommitments.head.position shouldBe NodePosition.CryptoTreePrefix
    h.ownCommitments.head.position shouldBe NodePosition.CryptoTreePrefix

    h.realCommitments.head.commitment shouldBe h.ownCommitments.head.commitment

    val a = h.realCommitments.head.commitment.asInstanceOf[FirstDLogProverMessage]
    val r = h.ownCommitments.head.secretRandomness

    // g^r == a
    SecP256K1Group.exponentiate(SecP256K1Group.generator, r) shouldBe a.ecData

    val h2 = prover.generateCommitmentsFor(pk3, Seq(pk))
    h2.hints.size shouldBe 2

    h2.realCommitments.head.position shouldBe NodePosition(Seq(0,0))
    h2.ownCommitments.head.position shouldBe NodePosition(Seq(0,0))

    //DH
    val h3 = prover.generateCommitmentsFor(pk2, Seq(pk2))
    h3.hints.nonEmpty shouldBe true
    h3.realCommitments.head.position shouldBe NodePosition.CryptoTreePrefix
    h3.ownCommitments.head.position shouldBe NodePosition.CryptoTreePrefix

    h3.realCommitments.head.commitment shouldBe h3.ownCommitments.head.commitment

    h3.realCommitments.head.commitment.isInstanceOf[FirstDHTupleProverMessage] shouldBe true
  }

  property("setPositions - and") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = CAndUnproven(CAND(Seq(pk0, pk1)), None, false, Seq(child0, child1), parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

  property("setPositions - or") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = COrUnproven(COR(Seq(pk0, pk1)), None, false, Seq(child0, child1), parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

  property("setPositions - threshold") {
    val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage

    val parentPos = NodePosition(Seq(0,0))
    val child0 = UnprovenSchnorr(pk0, None, None, None, false, NodePosition(Seq(1)))
    val child1 = UnprovenSchnorr(pk1, None, None, None, false, NodePosition(Seq(0)))

    val c0 = CThresholdUnproven(CTHRESHOLD(1, Seq(pk0, pk1)), None, false, 1, Seq(child0, child1), None, parentPos)
    val c1 = prover.setPositions(c0)

    c1.children.head.asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 0))

    c1.children(1).asInstanceOf[UnprovenSchnorr].position shouldBe NodePosition(Seq(0, 0, 1))
  }

  val message = Blake2b256("some message based on tx content")

  def checkProof(sb: SigmaBoolean)(implicit prover: ProverInterpreter) = {
    val verifier = new ErgoLikeInterpreter()
    val proof = prover.generateProof(sb, message, HintsBag.empty)
    val ok = verifier.verifySignature(sb, message, proof)(null)
    ok shouldBe true
  }

  def checkUnrealRoot(sb: SigmaBoolean)(implicit prover: ProverInterpreter) = {
    assertExceptionThrown(
      checkProof(sb),
      { case e: AssertionError => e.getMessage.contains("Tree root should be real but was")
        case _ => false })
  }

  property("proof/verify completeness") {
    implicit val prover = new ErgoLikeTestProvingInterpreter
    val pk0 = prover.dlogSecrets(0).publicImage
    val pk1 = prover.dlogSecrets(1).publicImage
    val dht0 = prover.dhSecrets(0).publicImage

    val otherProver = new ErgoLikeTestProvingInterpreter
    val pkUnknown0 = otherProver.dlogSecrets(0).publicImage
    val pkUnknown1 = otherProver.dlogSecrets(1).publicImage
    val pkUnknown2 = otherProver.dlogSecrets(2).publicImage

    assertExceptionThrown(
      checkProof(TrivialProp.FalseProp),
      { case e: InterpreterException =>
        e.getMessage.contains("Script reduced to false")
        case _ => false })

    checkProof(pk0)
    checkProof(dht0)

    checkProof(CAND(Array(pk0)))
    checkUnrealRoot(CAND(Array(pkUnknown0)))

    checkProof(CAND(Array(pk0, pk1)))
    checkUnrealRoot(CAND(Array(pk0, pkUnknown0)))
    checkUnrealRoot(CAND(Array(pkUnknown0, pk0)))
    checkUnrealRoot(CAND(Array(pkUnknown0, pkUnknown1)))

    checkProof(COR(Array(pk0)))
    checkUnrealRoot(COR(Array(pkUnknown0)))

    checkProof(COR(Array(pk0, pk1)))
    checkProof(COR(Array(pk0, pkUnknown0)))
    checkProof(COR(Array(pkUnknown0, pk0)))
    checkUnrealRoot(COR(Array(pkUnknown0, pkUnknown1)))

    checkProof(CTHRESHOLD(1, Array(pk0)))
    checkUnrealRoot(CTHRESHOLD(1, Array(pkUnknown0)))

    checkProof(CTHRESHOLD(1, Array(pk0, pk1)))
    checkProof(CTHRESHOLD(1, Array(pk0, pkUnknown1)))
    checkProof(CTHRESHOLD(1, Array(pkUnknown1, pk0)))
    checkUnrealRoot(CTHRESHOLD(1, Array(pkUnknown0, pkUnknown1)))

    checkProof(CTHRESHOLD(1, Array(pk0, pk1, dht0)))
    checkProof(CTHRESHOLD(1, Array(pk0, pkUnknown0, dht0)))
    checkProof(CTHRESHOLD(1, Array(pkUnknown0, pkUnknown1, dht0)))
    checkProof(CTHRESHOLD(1, Array(pkUnknown0, dht0, pkUnknown1)))
    checkProof(CTHRESHOLD(1, Array(dht0, pkUnknown0, pkUnknown1)))
    checkUnrealRoot(CTHRESHOLD(1, Array(pkUnknown0, pkUnknown1, pkUnknown2)))

    checkProof(CTHRESHOLD(2, Array(pk0, pk1, dht0)))
    checkProof(CTHRESHOLD(2, Array(pkUnknown0, pk0, dht0)))
    checkProof(CTHRESHOLD(2, Array(pk0, pkUnknown0, dht0)))
    checkProof(CTHRESHOLD(2, Array(pk0, dht0, pkUnknown0)))
    checkUnrealRoot(CTHRESHOLD(2, Array(pk0, pkUnknown0, pkUnknown1)))
    checkUnrealRoot(CTHRESHOLD(2, Array(pkUnknown0, pk0, pkUnknown1)))
    checkUnrealRoot(CTHRESHOLD(2, Array(pkUnknown0, pkUnknown1, pk0)))

    checkProof(CTHRESHOLD(3, Array(pk0, pk1, dht0)))
    checkUnrealRoot(CTHRESHOLD(3, Array(pkUnknown0, pk0, dht0)))
    checkUnrealRoot(CTHRESHOLD(3, Array(pk0, pkUnknown0, dht0)))
    checkUnrealRoot(CTHRESHOLD(3, Array(pk0, dht0, pkUnknown0)))
  }
}
