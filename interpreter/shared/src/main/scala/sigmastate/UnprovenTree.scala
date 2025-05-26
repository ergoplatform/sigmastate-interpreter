package sigmastate

import debox.cfor
import sigma.data.{CAND, COR, CTHRESHOLD, ProveDHTuple, ProveDlog, SigmaBoolean, SigmaLeaf}
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.{ErgoTree, FixedCost, JitCost, NamedDesc, OperationCostInfo, SigmaPropConstant}
import sigma.eval.ErgoTreeEvaluator
import sigmastate.crypto.DLogProtocol.FirstDLogProverMessage
import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto.{FirstDHTupleProverMessage, FirstProverMessage, GF2_192_Poly}
import sigmastate.interpreter.CErgoTreeEvaluator.fixedCostOp
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigma.serialization.{SigmaByteWriter, SigmaSerializer}

import java.math.BigInteger

object ConjectureType extends Enumeration {
  val AndConjecture = Value(0)
  val OrConjecture = Value(1)
  val ThresholdConjecture = Value(2)
}

//Proof tree

trait ProofTree extends Product

trait ProofTreeLeaf extends ProofTree {
  val proposition: SigmaLeaf
  val commitmentOpt: Option[FirstProverMessage]
}

trait ProofTreeConjecture extends ProofTree {
  val conjectureType: ConjectureType.Value
  val children: Seq[ProofTree]
}

/**
  * Data type which encodes position of a node in a tree.
  *
  * Position is encoded like following (the example provided is for CTHRESHOLD(2, Seq(pk1, pk2, pk3 && pk4)) :
  *
  *            0
  *          / | \
  *         /  |  \
  *       0-0 0-1 0-2
  *               /|
  *              / |
  *             /  |
  *            /   |
  *          0-2-0 0-2-1
  *
  * So a hint associated with pk1 has a position "0-0", pk4 - "0-2-1" .
  *
  * Please note that "0" prefix is for a crypto tree. There are several kinds of trees during evaluation.
  * Initial mixed tree (ergoTree) would have another prefix.
  *
  * @param positions - positions from root (inclusive) in top-down order
  */
case class NodePosition(positions: Seq[Int]) {

  def child(childIdx: Int): NodePosition = NodePosition(positions :+ childIdx)

  override def toString: String = positions.mkString("-")
}

object NodePosition {
  /**
    * Prefix to encode node positions in a crypto tree.
    */
  val CryptoTreePrefix = NodePosition(Seq(0))

  /**
    * Prefix to encode node positions in an ErgoTree instance.
    */
  val ErgoTreePrefix = NodePosition(Seq(1))
}

/**
  * A node of a sigma-tree used by the prover. See ProverInterpreter comments and the
  * ErgoScript white-paper https://ergoplatform.org/docs/ErgoScript.pdf , Appendix A, for details
  */
sealed trait UnprovenTree extends ProofTree {

  /**
    * Position of the node in the tree, see comments for `position` field in
    * `sigmastate.interpreter.Hint`
    */
  val position: NodePosition

  /**
    * Node's sigma-protocol statement to be proven.
    */
  val proposition: SigmaBoolean

  /**
    * Whether the node represents simulated sigma-protocol
    */
  val simulated: Boolean

  def real: Boolean = !simulated

  /**
    * Challenge used by the prover.
    */
  val challengeOpt: Option[Challenge]

  def withChallenge(challenge: Challenge): UnprovenTree

  def withSimulated(newSimulated: Boolean): UnprovenTree

  def withPosition(updatedPosition: NodePosition): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree with ProofTreeLeaf

sealed trait UnprovenConjecture extends UnprovenTree with ProofTreeConjecture

case class CAndUnproven(override val proposition: CAND,
                        override val challengeOpt: Option[Challenge] = None,
                        override val simulated: Boolean,
                        children: Seq[ProofTree],
                        override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.AndConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition): UnprovenTree = this.copy(position = updatedPosition)
}

case class COrUnproven(override val proposition: COR,
                       override val challengeOpt: Option[Challenge] = None,
                       override val simulated: Boolean,
                       children: Seq[ProofTree],
                       override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.OrConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition): UnprovenTree = this.copy(position = updatedPosition)
}

/**
  * Unproven threshold k-out-n conjecture. k secrets will be proven, (n-k) simulated.
  * For details on challenge and polynomial used in this case, see [CramerDamgardSchoenmakers94].
  */
case class CThresholdUnproven(override val proposition: CTHRESHOLD,
                       override val challengeOpt: Option[Challenge] = None,
                       override val simulated: Boolean,
                       k: Integer,
                       children: Seq[ProofTree],
                       polynomialOpt: Option[GF2_192_Poly],
                       override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  require(k >= 0 && k <= children.length, "Wrong k value")
  require(children.size <= 255) // Our polynomial arithmetic can take only byte inputs

  override val conjectureType = ConjectureType.ThresholdConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)

  def withPolynomial(newPolynomial: GF2_192_Poly) = this.copy(polynomialOpt = Some(newPolynomial))
}


case class UnprovenSchnorr(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Challenge] = None,
                           override val simulated: Boolean,
                           override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenLeaf {

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)
}

case class UnprovenDiffieHellmanTuple(override val proposition: ProveDHTuple,
                                      override val commitmentOpt: Option[FirstDHTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Challenge] = None,
                                      override val simulated: Boolean,
                                      override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenLeaf {
  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)
}

object FiatShamirTree {
  /** Prefix byte which is put before the other ProofTreeConjecture serialized bytes. */
  val internalNodePrefix: Byte = 0

  /** Prefix byte which is put before the other ProofTreeLeaf serialized bytes. */
  val leafPrefix: Byte = 1

  /** Represents cost of serializing UncheckedSchnorr or UnprovenSchnorr node of ProofTree. */
  final val ToBytes_Schnorr = OperationCostInfo(
    FixedCost(JitCost(570)), NamedDesc("ToBytes_Schnorr"))

  /** Represents cost of serializing UncheckedDiffieHellmanTuple or
    * UnprovenDiffieHellmanTuple node of ProofTree.
    */
  final val ToBytes_DHT = OperationCostInfo(
    FixedCost(JitCost(680)), NamedDesc("ToBytes_DHT"))

  /** Represents cost of serializing ProofTreeConjecture node of ProofTree. */
  final val ToBytes_ProofTreeConjecture = OperationCostInfo(
    FixedCost(JitCost(15)), NamedDesc("ToBytes_ProofTreeConjecture"))

  /** Prover Step 7: Convert the tree to a byte array `s` for input to the Fiat-Shamir hash
    * function.
    * See the other overload for detailed docs. */
  def toBytes(tree: ProofTree)(implicit E: ErgoTreeEvaluator): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    toBytes(tree, w)
    w.toBytes
  }

  /** Prover Step 7: Convert the tree to a byte array `s` for input to the Fiat-Shamir hash
    * function. The conversion should be such that the tree can be unambiguously parsed and
    * restored given the array.
    * For each non-leaf node, the string should contain its type (OR or AND).
    * For each leaf node, the string should contain the Sigma-protocol statement being
    * proven and the commitment.
    * The string should not contain information on whether a node is marked "real" or
    * "simulated", and should not contain challenges, responses and/or the real/simulated
    * flag for any node.
    *
    * @param tree the tree to take commitments from
    * @param w    writer which is used for serialization
    * @param E     optional evaluator (can be null) which is used for profiling of operations.
    *              When `E` is `null`, then profiling is turned-off and has no effect on
    *              the execution.
    *
    * HOTSPOT: don't beautify the code
    */
  def toBytes(tree: ProofTree, w: SigmaByteWriter)
             (implicit E: ErgoTreeEvaluator): Unit = tree match {
    case l: ProofTreeLeaf =>
      val costInfo = l match {
        case _: UncheckedSchnorr | _: UnprovenSchnorr => ToBytes_Schnorr
        case _: UncheckedDiffieHellmanTuple | _: UnprovenDiffieHellmanTuple => ToBytes_DHT
      }
      fixedCostOp(costInfo) {
        val propTree = ErgoTree.withSegregation(ZeroHeader, SigmaPropConstant(l.proposition))
        val propBytes = DefaultSerializer.serializeErgoTree(propTree)
        val commitmentBytes = l.commitmentOpt.get.bytes
        w.put(leafPrefix)
        w.putShortBytes(propBytes.length.toShort)
        w.putBytes(propBytes)
        w.putShortBytes(commitmentBytes.length.toShort)
        w.putBytes(commitmentBytes)
      }

    case c: ProofTreeConjecture =>
      fixedCostOp(ToBytes_ProofTreeConjecture) {
        w.put(internalNodePrefix)
        w.put(c.conjectureType.id.toByte)
        c match {
          case unproven: CThresholdUnproven =>
            w.put(unproven.k.toByte)
          case unchecked: CThresholdUncheckedNode =>
            w.put(unchecked.k.toByte)
          case _ =>
        }
        val childrenCount = c.children.length.toShort
        w.putShortBytes(childrenCount)
      }

      val cs = c.children.toArray
      cfor(0)(_ < cs.length, _ + 1) { i =>
        val child = cs(i)
        toBytes(child, w)
      }
  }
}
