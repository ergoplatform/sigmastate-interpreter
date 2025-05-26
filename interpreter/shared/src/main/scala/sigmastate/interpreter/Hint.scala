package sigmastate.interpreter

import sigma.data.{SigmaBoolean, SigmaLeaf}
import java.math.BigInteger
import sigmastate.{NodePosition, UncheckedTree}
import sigmastate.crypto.FirstProverMessage
import sigmastate.crypto.VerifierMessage.Challenge

/**
  * A hint for a prover which helps the prover to prove a statement. For example, if the statement is "pk1 && pk2",
  * and the prover knows only a secret for the public key pk1, the prover fails on proving without a hint. But if the
  * prover knows that pk2 is known to another party, the prover may prove the statement (with an empty proof for "pk2").
  */
trait Hint {
  /**
    * Public image of a secret
    */
  def image: SigmaLeaf

  /**
    * A hint is related to a subtree (or a leaf) of a tree. This field encodes a position in the tree.
    * See `NodePosition` ScalaDoc for details.
    */
  val position: NodePosition
}

/**
  * A hint which is indicating that a secret associated with its public image "image" is already proven.
  */
abstract class SecretProven extends Hint {
  /**
    * Challenge used for a proof
    */
  def challenge: Challenge

  /**
    * Proof in a tree form
    */
  def uncheckedTree: UncheckedTree
}

/**
  * A hint which contains a proof-of-knowledge for a secret associated with its public image "image",
  * with also the mark that the proof is real.
  */
case class RealSecretProof(image: SigmaLeaf,
                           challenge: Challenge,
                           uncheckedTree: UncheckedTree,
                           override val position: NodePosition) extends SecretProven

/**
  * A hint which contains a proof-of-knowledge for a secret associated with its public image "image",
  * with also the mark that the proof is simulated (not real).
  */
case class SimulatedSecretProof(image: SigmaLeaf,
                                challenge: Challenge,
                                uncheckedTree: UncheckedTree,
                                override val position: NodePosition) extends SecretProven


/**
  * A family of hints which are about a correspondence between a public image of a secret image and prover's commitment
  * to randomness ("a" in a sigma protocol).
  */
abstract class CommitmentHint extends Hint {
  /** Commitment to randomness (first message in a sigma protocol) */
  def commitment: FirstProverMessage
}

/**
  * A hint which a commitment to randomness associated with a public image of a secret, as well as randomness itself.
  * Please note that this randomness should be kept in secret by the prover.
  *
  * @param image      - image of a secret
  * @param secretRandomness - randomness
  * @param commitment - commitment to randomness used while proving knowledge of the secret
  */
case class OwnCommitment(override val image: SigmaLeaf,
                         secretRandomness: BigInteger,
                         commitment: FirstProverMessage,
                         override val position: NodePosition) extends CommitmentHint

/**
  * A hint which contains a commitment to randomness associated with a public image of a secret.
  *
  * @param image      - image of a secret
  * @param commitment - commitment to randomness used while proving knowledge of the secret
  */
case class RealCommitment(override val image: SigmaLeaf,
                          commitment: FirstProverMessage,
                          override val position: NodePosition) extends CommitmentHint

/**
  * A hint which contains a commitment to randomness associated with a public image of a secret.
  *
  * @param image      - image of a secret
  * @param commitment - commitment to randomness used while proving knowledge of the secret
  */
case class SimulatedCommitment(override val image: SigmaLeaf,
                               commitment: FirstProverMessage,
                               override val position: NodePosition) extends CommitmentHint


/**
  * Collection of hints to be used by a prover
  *
  * @param hints - hints stored in the bag
  */
case class HintsBag(hints: Seq[Hint]) {

  lazy val realProofs: Seq[RealSecretProof] = hints.collect { case osp: RealSecretProof => osp }
  lazy val simulatedProofs: Seq[SimulatedSecretProof] = hints.collect { case osp: SimulatedSecretProof => osp }

  lazy val proofs: Seq[SecretProven] = realProofs ++ simulatedProofs

  lazy val commitments: Seq[CommitmentHint] = hints.collect { case ch: CommitmentHint => ch }
  lazy val realCommitments: Seq[RealCommitment] = hints.collect { case rc: RealCommitment => rc }
  lazy val ownCommitments: Seq[OwnCommitment] = hints.collect { case oc: OwnCommitment => oc }

  lazy val realImages: Seq[SigmaBoolean] = realProofs.map(_.image) ++ realCommitments.map(_.image)

  def addHint(hint: Hint): HintsBag = HintsBag(hint +: hints)

  def addHints(newHints: Hint*): HintsBag = HintsBag(newHints ++ hints)

  def ++(other: HintsBag): HintsBag = HintsBag(other.hints ++ hints)

  override def toString: String = s"HintsBag(${hints.mkString("\n")})"

}

object HintsBag {

  val empty = HintsBag(Seq.empty)

}
