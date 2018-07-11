package sigmastate.interpreter

import java.util

import org.bitbucket.inkytonik.kiama.attribution.AttributionCore
import scapi.sigma.SigmaProtocolPrivateInput
import scapi.sigma.DLogProtocol._
import sigmastate._
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues, Helpers}
import sigmastate.utils.Extensions._
import Values._

import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, everywheretd, rule}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma.VerifierMessage.Challenge
import scapi.sigma._
import scorex.utils.Random
import sigmastate.serialization.Serializer

/**
  * Proof generated by a prover along with possible context extensions
  */
class ProverResult(val proof: Array[Byte], val extension: ContextExtension) {
  override def hashCode(): Int = util.Arrays.hashCode(proof) * 31 + extension.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: ProverResult =>
      util.Arrays.equals(proof, obj.proof) && extension == obj.extension
    case _ => false
  }
}

object ProverResult {

  def apply(proof: Array[Byte], extension: ContextExtension): ProverResult =
    new ProverResult(proof, extension)

  object serializer extends Serializer[ProverResult, ProverResult] {

    override def serializeBody(obj: ProverResult, w: ByteWriterSigmaValues): Unit = {
      w.putUShort(obj.proof.length.toShort)
      w.putBytes(obj.proof)
      ContextExtension.serializer.serializeBody(obj.extension, w)

    }

    override def parseBody(r: ByteReaderSigmaValues): ProverResult = {
      val sigBytesCount = r.getUShort()
      val proofBytes = r.getBytes(sigBytesCount)
      val ce = ContextExtension.serializer.parseBody(r)
      ProverResult(proofBytes, ce)
    }
  }
}

case class CostedProverResult(override val proof: Array[Byte],
                              override val extension: ContextExtension,
                              cost: Long) extends ProverResult(proof, extension)

/**
  * Interpreter with enhanced functionality to prove statements.
  */
trait ProverInterpreter extends Interpreter with AttributionCore {
  import Interpreter._

  override type ProofT = UncheckedTree

  val secrets: Seq[SigmaProtocolPrivateInput[_, _]]

  def contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = Map()

  val knownExtensions = ContextExtension(contextExtenders)

  /**
    * The comments in this section are taken from the algorithm for the
    * Sigma-protocol prover as described in the white paper
    *
    */
  // todo: if we are concerned about timing attacks against the prover, we should make sure that this code
  // todo: takes the same amount of time regardless of which nodes are real and which nodes are simulated
  // todo: In particular, we should avoid the use of exists and forall, because they short-circuit the evaluation
  // todo: once the right value is (or is not) found. We should also make all loops look similar, the same
  // todo: amount of copying is done regardless of what's real or simulated,
  // todo: real vs. simulated computations take the same time, etc.
  protected def prove(unprovenTree: UnprovenTree, message: Array[Byte]): ProofT = {

    // Prover Step 1: Mark as real everything the prover can prove
    val step1 = markReal(unprovenTree).get.asInstanceOf[UnprovenTree]

    // Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
    // to perform the proof. Abort.
    assert(step1.real, s"Tree root should be real but was $step1")

    // Prover Step 3: Change some "real" nodes to "simulated" to make sure each node
    // has the right number of simulated children.
    val step3 = polishSimulated(step1).get.asInstanceOf[UnprovenTree]

    // Prover Steps 4, 5, and 6 together: find challenges for simulated nodes; simulate simulated leaves;
    // compute commitments for real leaves
    val step6 = simulateAndCommit(step3).get.asInstanceOf[UnprovenTree]

    // Prover Steps 7: convert the relevant information in the tree (namely, tree structure, node types,
    // the statements being proven and commitments at the leaves)
    // to a string
    val s = FiatShamirTree.toBytes(step6)

    // Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    // and the message being signed.
    val rootChallenge = Challenge @@ CryptoFunctions.hashFn(s ++ message)
    val step8 = step6.withChallenge(rootChallenge)

    // Prover Step 9: complete the proof by computing challenges at real nodes and additionally responses at real leaves
    val step9 = proving(step8).get.asInstanceOf[ProofTree]

    // Syntactic step that performs a type conversion only
    convertToUnchecked(step9)
  }

  def prove(exp: Value[SBoolean.type], context: CTX, message: Array[Byte]): Try[ProverResult] = Try {
    val (reducedProp, cost) = reduceToCrypto(context.withExtension(knownExtensions), exp).get
    val proofTree = reducedProp match {
      case bool: BooleanConstant =>
        bool match {
          case TrueLeaf => NoProof
          case _ => ???
        }
      case _ =>
        val ct = convertToUnproven(reducedProp.asInstanceOf[SigmaBoolean])
        prove(ct, message)
    }
    // Prover Step 10: output the right information into the proof
    val proof = SigSerializer.toBytes(proofTree)
    CostedProverResult(proof, knownExtensions, cost)
  }

  /**
    * Prover Step 1: This step will mark as "real" every node for which the prover can produce a real proof.
    * This step may mark as "real" more nodes than necessary if the prover has more than the minimal
    * necessary number of witnesses (for example, more than one child of an OR).
    * This will be corrected in the next step. In a bottom-up traversal of the tree, do the following for each node:
    */
  val markReal: Strategy = everywherebu(rule[UnprovenTree] {
    case and: CAndUnproven =>
      // If the node is AND, mark it "real" if all of its children are marked real; else mark it "simulated"
      val simulated = and.children.exists(_.asInstanceOf[UnprovenTree].simulated)
      and.copy(simulated = simulated)
    case or: COrUnproven =>
      // If the node is OR, mark it "real" if at least one child is marked real; else mark it "simulated"
      val simulated = or.children.forall(_.asInstanceOf[UnprovenTree].simulated)
      or.copy(simulated = simulated)
    case su: UnprovenSchnorr =>
      // If the node is a leaf, mark it "real'' if the witness for it is available; else mark it "simulated"
      val secretKnown = secrets.exists {
        case in: DLogProverInput => in.publicImage == su.proposition
        case _ => false
      }
      su.copy(simulated = !secretKnown)
    case dhu: UnprovenDiffieHellmanTuple =>
      // If the node is a leaf, mark it "real" if the witness for it is available; else mark it "simulated"
      val secretKnown = secrets.exists {
        case in: DiffieHellmanTupleProverInput => in.publicImage == dhu.proposition
        case _ => false
      }
      dhu.copy(simulated = !secretKnown)
    case t =>
      error(s"Don't know how to markSimulated($t)")
  })

  /**
    * Prover Step 3: This step will change some "real" nodes to "simulated" to make sure each node has
    * the right number of simulated children.
    * In a top-down traversal of the tree, do the following for each node:
    */
  val polishSimulated: Strategy = everywheretd(rule[UnprovenTree] {
    case and: CAndUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      if (and.simulated) and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      else and
    case or: COrUnproven =>
      // If the node is marked "simulated", mark all of its children ``simulated''
      if (or.simulated) {
        or.copy(children = or.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      } else {
        // If the node is OR marked "real",  mark all but one of its children "simulated"
        // (the node is guaranteed by step 1 to have at least one "real" child).
        // Which particular child is left "real" is not important for security;
        // the choice can be guided by efficiency or convenience considerations.
        val newChildren = or.children.foldLeft((Seq[UnprovenTree](), false)) { case ((children, realFound), child) =>
          val cut = child.asInstanceOf[UnprovenTree]
          (realFound, cut.real) match {
            case (true, true) => (children :+ cut.withSimulated(true), true)
            case (true, false) => (children :+ cut, true)
            case (false, true) => (children :+ cut, true)
            case (false, false) => (children :+ cut, false)
          }
        }._1
        or.copy(children = newChildren)
      }
    case su: UnprovenSchnorr => su
    case dhu: UnprovenDiffieHellmanTuple => dhu
    case _ => ???
  })

  /**
    * Prover Step 4: In a top-down traversal of the tree, compute the challenges e for simulated children of every node
    * Prover Step 5: For every leaf marked "simulated", use the simulator of the Sigma-protocol for that leaf
    * to compute the commitment $a$ and the response z, given the challenge e that is already stored in the leaf.
    * Prover Step 6: For every leaf marked "real", use the first prover step of the Sigma-protocol for that leaf to
    * compute the commitment a.
    */
  val simulateAndCommit: Strategy = everywheretd(rule[ProofTree] {
    // Step 4 part 1: If the node is marked "real", then each of its simulated children gets  a fresh uniformly random challenge
    // in {0,1}^t.
    case and: CAndUnproven if and.real => and // A real AND node has no simulated children

    case or: COrUnproven if or.real =>
      val newChildren = or.children.cast[UnprovenTree].map(c =>
        if (c.real) c
        else c.withChallenge(Challenge @@ Random.randomBytes(CryptoFunctions.soundnessBytes))
      )
      or.copy(children = newChildren)

    // Step 4 part 2: If the node is marked "simulated", let e_0 be the challenge computed for it.
    // All of its children are simulated, and thus we compute challenges for all
    // of them, as follows:
    case and: CAndUnproven if and.simulated =>
      // If the node is AND, then all of its children get e_0 as the challenge
      assert(and.challengeOpt.isDefined)
      val challenge = and.challengeOpt.get
      val newChildren = and.children.cast[UnprovenTree].map(_.withChallenge(challenge))
      and.copy(children = newChildren)

    case or: COrUnproven if or.simulated =>
      // If the node is OR, then each of its children except one gets a fresh uniformly random
      // challenge in {0,1}^t. The remaining child gets a challenge computed as an XOR of the challenges of all
      // the other children and e_0.
      assert(or.challengeOpt.isDefined)
      val unprovenChildren = or.children.cast[UnprovenTree]
      val t = unprovenChildren.tail.map(_.withChallenge(Challenge @@ Random.randomBytes(CryptoFunctions.soundnessBytes)))
      val toXor: Seq[Array[Byte]] = or.challengeOpt.get +: t.map(_.challengeOpt.get)
      val xoredChallenge = Challenge @@ Helpers.xor(toXor: _*)
      val h = unprovenChildren.head.withChallenge(xoredChallenge)
      or.copy(children = h +: t)

    case su: UnprovenSchnorr =>
      if (su.simulated) {
        // Step 5 (simulated leaf -- complete the simulation)
        assert(su.challengeOpt.isDefined)
        val (fm, sm) = DLogInteractiveProver.simulate(su.proposition, su.challengeOpt.get)
        UncheckedSchnorr(su.proposition, Some(fm), su.challengeOpt.get, sm)
      } else {
        // Step 6 (real leaf -- compute the commitment a)
        val (r, commitment) = DLogInteractiveProver.firstMessage(su.proposition)
        su.copy(commitmentOpt = Some(commitment), randomnessOpt = Some(r))
      }

    case dhu: UnprovenDiffieHellmanTuple =>
      if (dhu.simulated) {
        // Step 5 (simulated leaf -- complete the simulation)
        assert(dhu.challengeOpt.isDefined)
        val (fm, sm) = DiffieHellmanTupleInteractiveProver.simulate(dhu.proposition, dhu.challengeOpt.get)
        UncheckedDiffieHellmanTuple(dhu.proposition, Some(fm), dhu.challengeOpt.get, sm)
      } else {
        // Step 6 (real leaf -- compute the commitment a)
        val (r, fm) = DiffieHellmanTupleInteractiveProver.firstMessage(dhu.proposition)
        dhu.copy(commitmentOpt = Some(fm), randomnessOpt = Some(r))
      }

    case a: Any => error(s"Don't know how to challengeSimulated($a)")
  })

  def extractChallenge(pt: ProofTree): Option[Array[Byte]] = pt match {
    case upt: UnprovenTree => upt.challengeOpt
    case sn: UncheckedSchnorr => Some(sn.challenge)
    case dh: UncheckedDiffieHellmanTuple => Some(dh.challenge)
    case _ => ???
  }

  /**
    * Prover Step 9: Perform a top-down traversal of only the portion of the tree marked "real" in order to compute
    * the challenge e for every node marked "real" below the root and, additionally, the response z for every leaf
    * marked "real"
    */
  val proving: Strategy = everywheretd(rule[ProofTree] {
    // If the node is a non-leaf marked real whose challenge is e_0, proceed as follows:
    case and: CAndUnproven if and.real =>
      assert(and.challengeOpt.isDefined)
      // If the node is AND, let each of its children have the challenge e_0
      val andChallenge = and.challengeOpt.get
      and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withChallenge(andChallenge)))

    case or: COrUnproven if or.real =>
      // If the node is OR, it has only one child marked "real".
      // Let this child have the challenge equal to the XOR of the challenges of all the other children and e_0
      assert(or.challengeOpt.isDefined)
      val rootChallenge = or.challengeOpt.get
      val challenge = Challenge @@ Helpers.xor(rootChallenge +: or.children.flatMap(extractChallenge): _*)

      or.copy(children = or.children.map {
        case r: UnprovenTree if r.real => r.withChallenge(challenge)
        case p: ProofTree => p
      })

    // If the node is a leaf marked "real", compute its response according to the second prover step
    // of the Sigma-protocol given the commitment, challenge, and witness
    case su: UnprovenSchnorr if su.real =>
      assert(su.challengeOpt.isDefined)
      val privKey = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)
        .get.asInstanceOf[DLogProverInput]
      val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, su.challengeOpt.get)
      UncheckedSchnorr(su.proposition, None, su.challengeOpt.get, z)

    case dhu: UnprovenDiffieHellmanTuple if dhu.real =>
      assert(dhu.challengeOpt.isDefined)
      val privKey = secrets
        .filter(_.isInstanceOf[DiffieHellmanTupleProverInput])
        .find(_.asInstanceOf[DiffieHellmanTupleProverInput].publicImage == dhu.proposition)
        .get.asInstanceOf[DiffieHellmanTupleProverInput]
      val z = DiffieHellmanTupleInteractiveProver.secondMessage(privKey, dhu.randomnessOpt.get, dhu.challengeOpt.get)
      UncheckedDiffieHellmanTuple(dhu.proposition, None, dhu.challengeOpt.get, z)


    case sn: UncheckedSchnorr => sn

    case dh: UncheckedDiffieHellmanTuple => dh

    case ut: UnprovenTree => ut

    case a: Any => println(a); ???
  })


  //converts SigmaTree => UnprovenTree
  val convertToUnproven: SigmaBoolean => UnprovenTree = attr {
    case CAND(sigmaTrees) =>
      CAndUnproven(CAND(sigmaTrees), None, simulated = false, sigmaTrees.map(convertToUnproven))
    case COR(children) =>
      COrUnproven(COR(children), None, simulated = false, children.map(convertToUnproven))
    case ci: ProveDlog =>
      UnprovenSchnorr(ci, None, None, None, simulated = false)
    case dh: ProveDiffieHellmanTuple =>
      UnprovenDiffieHellmanTuple(dh, None, None, None, simulated = false)
  }

  //converts ProofTree => UncheckedSigmaTree
  val convertToUnchecked: ProofTree => UncheckedSigmaTree = attr {
    case and: CAndUnproven =>
      CAndUncheckedNode(and.challengeOpt.get,  and.children.map(convertToUnchecked))
    case or: COrUnproven =>
      COrUncheckedNode(or.challengeOpt.get,  or.children.map(convertToUnchecked))
    case s: UncheckedSchnorr => s
    case d: UncheckedDiffieHellmanTuple => d
    case _ => ???
  }
}
