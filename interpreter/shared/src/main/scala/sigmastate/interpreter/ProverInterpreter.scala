package sigmastate.interpreter

import sigma.kiama.rewriting.Rewriter.{everywherebu, everywheretd, rule}
import sigma.util.CollectionUtil._
import sigma.VersionContext
import sigma.kiama.rewriting.Strategy
import sigma.data.TrivialProp.{FalseProp, TrueProp}
import sigma.ast._
import sigma.VersionContext.MaxSupportedScriptVersion
import sigmastate._
import sigmastate.crypto.DLogProtocol._
import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto._
import sigmastate.crypto.{GF2_192, GF2_192_Poly}
import sigmastate.utils.Helpers
import sigma.Coll
import sigma.Extensions.ArrayOps
import sigma.crypto.CryptoConstants
import sigma.data.{CAND, COR, CTHRESHOLD, ProveDHTuple, ProveDlog, SigmaBoolean}
import sigma.exceptions.InterpreterException
import sigma.interpreter.CostedProverResult
import sigma.serialization.SigSerializer
import sigma.util.CollectionUtil

import java.math.BigInteger
import scala.util.Try

// TODO ProverResult was moved from here, compare with new-eval after merge
/**
  * Interpreter with enhanced functionality to prove statements.
  */
trait ProverInterpreter extends Interpreter with ProverUtils {

  import sigma.crypto.CryptoConstants.secureRandomBytes
  import Interpreter._

  override type ProofT = UncheckedTree

  /** All secrets available for this prover. */
  def secrets: Seq[SigmaProtocolPrivateInput[_]]

  /**
    * Public keys of prover's secrets. This operation can be costly if there are many
    * secrets the prover knows, consider re-implementation of this field then.
    */
  def publicKeys: Seq[SigmaBoolean] = secrets.map(_.publicImage.asInstanceOf[SigmaBoolean])

  /**
    * Generate commitments for given ergo tree for prover's secrets.
    * The prover is reducing the given tree to crypto-tree by using the given context,
    *   and then generates commitments.
    */
  def generateCommitments(ergoTree: ErgoTree, ctx: CTX): HintsBag = {
    generateCommitmentsFor(ergoTree, ctx, publicKeys)
  }

  /**
    * Generate commitments for given crypto-tree (sigma-tree) for prover's secrets.
    */
  def generateCommitments(sigmaTree: SigmaBoolean): HintsBag = {
    generateCommitmentsFor(sigmaTree, publicKeys)
  }

  /**
    * The comments in this section are taken from the algorithm for the
    * Sigma-protocol prover as described in the ErgoScript white-paper
    * https://ergoplatform.org/docs/ErgoScript.pdf , Appendix A
    *
    */
  // TODO: if we are concerned about timing attacks against the prover, we should make sure that this code
  //  takes the same amount of time regardless of which nodes are real and which nodes are simulated
  //  In particular, we should avoid the use of exists and forall, because they short-circuit the evaluation
  //  once the right value is (or is not) found. We should also make all loops look similar, the same
  //  amount of copying is done regardless of what's real or simulated,
  //  real vs. simulated computations take the same time, etc.
  protected def prove(unprovenTree: UnprovenTree, message: Array[Byte], hintsBag: HintsBag): ProofT = {

    // Prover Step 1: Mark as real everything the prover can prove
    val step1 = markReal(hintsBag)(unprovenTree).get.asInstanceOf[UnprovenTree]

    // Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
    // to perform the proof. Abort.
    assert(step1.real, s"Tree root should be real but was $step1")

    // Prover Step 3: Change some "real" nodes to "simulated" to make sure each node
    // has the right number of simulated children.
    val step3 = polishSimulated(step1).get.asInstanceOf[UnprovenTree]

    // Prover Steps 4, 5, and 6 together: find challenges for simulated nodes; simulate simulated leaves;
    // compute commitments for real leaves
    val step6 = simulateAndCommit(hintsBag)(step3).get.asInstanceOf[UnprovenTree]

    // Prover Steps 7: convert the relevant information in the tree (namely, tree structure, node types,
    // the statements being proven and commitments at the leaves) to a bitstring.
    // This bitstring corresponding to a proposition to prove is needed for Strong Fiat-Shamir transformation.
    // See [BPW12] paper on Strong vs Weak Fiat-Shamir,
    // (https://link.springer.com/content/pdf/10.1007/978-3-642-34961-4_38.pdf)
    val propBytes = FiatShamirTree.toBytes(step6)(null/* prove is not profiled */)

    // Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of propBytes
    // and the message being signed.
    val rootChallenge = Challenge @@ CryptoFunctions.hashFn(CollectionUtil.concatArrays(propBytes, message)).toColl
    val step8 = step6.withChallenge(rootChallenge)

    // Prover Step 9: complete the proof by computing challenges at real nodes and additionally responses at real leaves
    val step9 = proving(hintsBag)(step8).get.asInstanceOf[ProofTree]

    // Syntactic step that performs a type conversion only
    convertToUnchecked(step9)
  }

  def prove(ergoTree: ErgoTree,
            context: CTX,
            message: Array[Byte],
            hintsBag: HintsBag): Try[CostedProverResult] =
    prove(emptyEnv, ergoTree, context, message, hintsBag)

  def prove(ergoTree: ErgoTree,
            context: CTX,
            message: Array[Byte]): Try[CostedProverResult] =
    prove(emptyEnv, ergoTree, context, message, HintsBag.empty)

  def prove(env: ScriptEnv,
            ergoTree: ErgoTree,
            context: CTX,
            message: Array[Byte],
            hintsBag: HintsBag = HintsBag.empty): Try[CostedProverResult] = Try {
    checkSoftForkCondition(ergoTree, context) match {
      case Some(_) =>
        throw new InterpreterException(
          s"Both ErgoTree version ${ergoTree.version} and activated version " +
            s"${context.activatedScriptVersion} is greater than MaxSupportedScriptVersion $MaxSupportedScriptVersion")

      case None => // proceed normally
    }

    VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
      val (resValue, resCost) = {
        val reduced = fullReduction(ergoTree, context, env)
        val fullCost = addCryptoCost(reduced.value, reduced.cost, context.costLimit)
        (reduced.value, fullCost)
      }

      val proof = generateProof(resValue, message, hintsBag)
      CostedProverResult(proof, context.extension, resCost)
    }
  }

  def generateProof(sb: SigmaBoolean,
                    message: Array[Byte],
                    hintsBag: HintsBag): Array[Byte] = {
    val proofTree = sb match {
      case TrueProp => NoProof
      case FalseProp => syntax.error("Script reduced to false")
      case sigmaTree =>
        val unprovenTree = convertToUnproven(sigmaTree)
        prove(unprovenTree, message, hintsBag)
    }
    // Prover Step 10: output the right information into the proof
    val proof = SigSerializer.toProofBytes(proofTree)
    proof
  }
  /**
    * Prover Step 1: This step will mark as "real" every node for which the prover can produce a real proof.
    * This step may mark as "real" more nodes than necessary if the prover has more than the minimal
    * necessary number of witnesses (for example, more than one child of an OR).
    * This will be corrected in the next step.
    * In a bottom-up traversal of the tree, do the following for each node:
    *
    */
  def markReal(hintsBag: HintsBag): Strategy = everywherebu(rule[Any] {
    case and: CAndUnproven =>
      // If the node is AND, mark it "real" if all of its children are marked real; else mark it "simulated"
      val simulated = and.children.exists(_.asInstanceOf[UnprovenTree].simulated)
      and.copy(simulated = simulated)
    case or: COrUnproven =>
      // If the node is OR, mark it "real" if at least one child is marked real; else mark it "simulated"
      val simulated = or.children.forall(_.asInstanceOf[UnprovenTree].simulated)
      or.copy(simulated = simulated)
    case t: CThresholdUnproven =>
      // If the node is THRESHOLD(k), mark it "real" if at least k of its children are marked real; else mark it "simulated"
      val realCount = t.children.foldLeft(0) { (count, child) =>
        count + (if (child.asInstanceOf[UnprovenTree].simulated) 0 else 1)
      }
      t.copy(simulated = realCount < t.k)
    // UnprovenSchnorr | UnprovenDiffieHellmanTuple case
    case ul: UnprovenLeaf =>
      // If the node is a leaf, mark it "real'' if either the witness for it is available or a hint shows the secret
      // is known to an external participant in multi-signing;
      // else mark it "simulated"
      val isReal = hintsBag.realImages.contains(ul.proposition) || secrets.exists {
        case in: SigmaProtocolPrivateInput[_] => in.publicImage == ul.proposition
      }
      ul.withSimulated(!isReal)
    case t: UnprovenTree =>
      syntax.error(s"Don't know how to markReal($t)")
  })

  /**
    * Set positions for children of a unproven inner node (conjecture, so AND/OR/THRESHOLD)
    */
  protected def setPositions(uc: UnprovenConjecture): UnprovenConjecture = {
    val updChildren = uc.children.zipWithIndex.map { case (pt, idx) =>
        pt.asInstanceOf[UnprovenTree].withPosition(uc.position.child(idx))
    }
    uc match {
      case and: CAndUnproven => and.copy(children = updChildren)
      case or: COrUnproven => or.copy(children = updChildren)
      case threshold: CThresholdUnproven => threshold.copy(children = updChildren)
    }
  }

  /**
    * Prover Step 3: This step will change some "real" nodes to "simulated" to make sure each node has
    * the right number of simulated children. Also, children will get proper position set during this step.
    * In a top-down traversal of the tree, do the following for each node:
    */
  val polishSimulated: Strategy = everywheretd(rule[Any] {
    case and: CAndUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      val a = if (and.simulated) {
        and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      } else {
        and
      }
      setPositions(a)

    case or: COrUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      val o = if (or.simulated) {
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
      setPositions(o)
    case t: CThresholdUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      val th = if (t.simulated) {
        t.copy(children = t.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      } else {
        // If the node is THRESHOLD(k) marked "real", mark all but k of its children "simulated"
        // (the node is guaranteed, by the previous step, to have at least k "real" children).
        // Which particular ones are left "real" is not important for security;
        // the choice can be guided by efficiency or convenience considerations.
        //
        // We'll mark the first k real ones real
        val newChildren = t.children.foldLeft((Seq[UnprovenTree](), 0)) { case ((children, countOfReal), child) =>
          val kid = child.asInstanceOf[UnprovenTree]
          val (newKid, newCountOfReal) = if (kid.real) {
            ( { if (countOfReal >= t.k) kid.withSimulated(true) else kid }, countOfReal + 1)
          } else {
            (kid, countOfReal)
          }
          (children :+ newKid, newCountOfReal)
        }._1
        t.copy(children = newChildren)
      }
      setPositions(th)
    case su: UnprovenSchnorr => su
    case dhu: UnprovenDiffieHellmanTuple => dhu
    case _: UnprovenTree => ???
  })

  /**
    * Prover Step 4: In a top-down traversal of the tree, compute the challenges e for simulated children of every node
    * Prover Step 5: For every leaf marked "simulated", use the simulator of the Sigma-protocol for that leaf
    * to compute the commitment $a$ and the response z, given the challenge e that is already stored in the leaf.
    * Prover Step 6: For every leaf marked "real", use the first prover step of the Sigma-protocol for that leaf to
    * compute the commitment a.
    */
  def simulateAndCommit(hintsBag: HintsBag): Strategy = everywheretd(rule[Any] {
    // Step 4 part 1: If the node is marked "real", then each of its simulated children gets a fresh uniformly
    // random challenge in {0,1}^t.
    case and: CAndUnproven if and.real => and // A real AND node has no simulated children

    //real OR or Threshold case
    case uc: UnprovenConjecture if uc.real =>
      val newChildren = uc.children.cast[UnprovenTree].map(c =>
        if (c.real) {
          c
        } else {
          // take challenge from previously done proof stored in the hints bag,
          // or generate random challenge for simulated child
          val newChallenge = hintsBag.proofs.find(_.position == c.position).map(_.challenge).getOrElse(
            Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes).toColl
          )
          c.withChallenge(newChallenge)
        }
      )
      uc match {
        case or: COrUnproven => or.copy(children = newChildren)
        case t: CThresholdUnproven => t.copy(children = newChildren)
        case _ => ???
      }

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
      val t = unprovenChildren.tail.map(
        _.withChallenge(Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes).toColl)
      )
      val toXor: Seq[Coll[Byte]] = or.challengeOpt.get +: t.map(_.challengeOpt.get)
      val xoredChallenge = Challenge @@ Helpers.xor(toXor: _*)
      val h = unprovenChildren.head.withChallenge(xoredChallenge)
      or.copy(children = h +: t)

    case t: CThresholdUnproven if t.simulated =>
      // The faster algorithm is as follows. Pick n-k fresh uniformly random values
      // q_1, ..., q_{n-k} from {0,1}^t and let q_0=e_0.
      // Viewing 1, 2, ..., n and q_0, ..., q_{n-k} as elements of GF(2^t),
      // evaluate the polynomial Q(x) = sum {q_i x^i} over GF(2^t) at points 1, 2, ..., n
      // to get challenges for child 1, 2, ..., n, respectively.
      assert(t.challengeOpt.isDefined)
      val n = t.children.length
      val unprovenChildren = t.children.cast[UnprovenTree]
      val q = GF2_192_Poly.fromByteArray(t.challengeOpt.get.toArray, secureRandomBytes(CryptoFunctions.soundnessBytes * (n - t.k)))

      val newChildren = unprovenChildren.foldLeft((Seq[UnprovenTree](), 1)) {
        case ((childSeq, childIndex), child) =>
          (childSeq :+ child.withChallenge(Challenge @@ q.evaluate(childIndex.toByte).toByteArray.toColl), childIndex + 1)
      }._1
      t.withPolynomial(q).copy(children = newChildren)

    // The algorithm with better resistance to timing attacks is as follows.
    // Pick n-k fresh uniformly random values e_1, ..., e_{n-k}
    // as challenges for the children number 1, ..., n-k.
    // Let i_0 = 0. Viewing 0, 1, 2, ..., n and e_0, ..., e_{n-k} as elements of GF(2^t),
    // find (via polynomial interpolation) the
    // lowest-degree polynomial Q(x)=sum_{i=0}^{n-k} a_i x^i  over GF(2^t) that is equal to e_j at j for each j
    // from 0 to n-k (this polynomial will have n-k+1 coefficients, and the lowest coefficient will be e_0).
    // Set the challenge at child j for n-k<j<= n to equal Q(j).

    /* **** Uncomment this and comment out the above algorithm if you want better resistance to timing attacks
    assert(t.challengeOpt.isDefined)
    val n = t.children.length
    val unprovenChildren = t.children.cast[UnprovenTree]
    val childrenWithRandomChallenges = unprovenChildren.slice(0, n-t.k).map(_.withChallenge(Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes)))
    val (points, values, _) = childrenWithRandomChallenges.foldLeft(((Array[Byte](), Array[GF2_192](),1))) {
      case ((p, v, count), child) =>
        val (newPoints, newValues) =
          if (count <= n - t.k) {
            (p :+ count.toByte, v :+ new GF2_192(child.challengeOpt.get))
          }
          else (p, v)
        (newPoints, newValues, count + 1)
    }
    val q = GF2_192_Poly.interpolate(points, values, new GF2_192(t.challengeOpt.get))

    val newChildren = unprovenChildren.slice(n-t.k, n).foldLeft((childrenWithRandomChallenges, n-t.k+1)) {
      case ((childSeq, childIndex), child) =>
        (childSeq :+ child.withChallenge(Challenge @@ q.evaluate(childIndex.toByte).toByteArray), childIndex + 1)
    }._1
    t.withPolynomial(q).copy(children=newChildren)
    */

    case su: UnprovenSchnorr =>
      // Steps 5 & 6: first try pulling out commitment from the hints bag. If it exists proceed with it,
      // otherwise, compute the commitment (if the node is real) or simulate it (if the node is simulated)

      // Step 6 (real leaf -- compute the commitment a or take it from the hints bag)
      hintsBag.commitments.find(_.position == su.position).map { cmtHint =>
        su.copy(commitmentOpt = Some(cmtHint.commitment.asInstanceOf[FirstDLogProverMessage]))
      }.getOrElse {
        if (su.simulated) {
          // Step 5 (simulated leaf -- complete the simulation)
          assert(su.challengeOpt.isDefined)
          val (fm, sm) = DLogProver.simulate(su.proposition, su.challengeOpt.get)
          UncheckedSchnorr(su.proposition, Some(fm), su.challengeOpt.get, sm)
        } else {
          // Step 6 -- compute the commitment
          val (r, commitment) = DLogProver.firstMessage()
          su.copy(commitmentOpt = Some(commitment), randomnessOpt = Some(r))
        }
      }

    case dhu: UnprovenDiffieHellmanTuple =>
       //Steps 5 & 6: pull out commitment from the hints bag, otherwise, compute the commitment(if the node is real),
       // or simulate it (if the node is simulated)

        // Step 6 (real leaf -- compute the commitment a or take it from the hints bag)
        hintsBag.commitments.find(_.position == dhu.position).map { cmtHint =>
          dhu.copy(commitmentOpt = Some(cmtHint.commitment.asInstanceOf[FirstDHTupleProverMessage]))
        }.getOrElse {
          if (dhu.simulated) {
            // Step 5 (simulated leaf -- complete the simulation)
            assert(dhu.challengeOpt.isDefined)
            val (fm, sm) = DiffieHellmanTupleProver.simulate(dhu.proposition, dhu.challengeOpt.get)
            UncheckedDiffieHellmanTuple(dhu.proposition, Some(fm), dhu.challengeOpt.get, sm)
          } else {
            // Step 6 -- compute the commitment
            val (r, fm) = DiffieHellmanTupleProver.firstMessage(dhu.proposition)
            dhu.copy(commitmentOpt = Some(fm), randomnessOpt = Some(r))
          }
        }

    case t: ProofTree => syntax.error(s"Don't know how to challengeSimulated($t)")
  })

  private def extractChallenge(pt: ProofTree): Option[Challenge] = pt match {
    case upt: UnprovenTree => upt.challengeOpt
    case sn: UncheckedSchnorr => Some(sn.challenge)
    case dh: UncheckedDiffieHellmanTuple => Some(dh.challenge)
    case _ => syntax.error(s"Cannot extractChallenge($pt)")
  }

  /**
    * Prover Step 9: Perform a top-down traversal of only the portion of the tree marked "real" in order to compute
    * the challenge e for every node marked "real" below the root and, additionally, the response z for every leaf
    * marked "real"
    */
  def proving(hintsBag: HintsBag): Strategy = everywheretd(rule[Any] {
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

    case t: CThresholdUnproven if t.real =>
      // If the node is THRESHOLD(k), number its children from 1 to no. Let i_1,..., i_{n-k}
      // be the indices of the children marked `"simulated" and e_1, ...,  e_{n-k} be their corresponding challenges.
      // Let i_0 = 0. Viewing 0, 1, 2, ..., n and e_0, ..., e_{n-k} as elements of GF(2^t),
      // find (via polynomial interpolation) the lowest-degree polynomial
      // Q(x)=sum_{i=0}^{n-k} a_i x^i  over GF(2^t) that is equal to e_j at i_j for each f from 0 to n-k
      // (this polynomial will have n-k+1 coefficients, and the lowest coefficient will be e_0). For child number
      // i of the node, if the child is marked "real", compute its challenge as Q(i) (if the child is marked
      // "simulated", its challenge is already Q(i), by construction of Q).
      assert(t.challengeOpt.isDefined)
      val (points, values, _) = t.children.foldLeft(Array[Byte](), Array[GF2_192](), 1) {
        case ((p, v, count), child) =>
          val (newPoints, newValues) = {
            // This is the easiest way to find out whether a child is simulated -- just to check if it alread
            // has a challenge. Other ways are more of a pain because the children can be of different types
            val challengeOpt = extractChallenge(child)
            if (challengeOpt.isEmpty) (p, v)
            else (p :+ count.toByte, v :+ new GF2_192(challengeOpt.get.toArray))

          }
          (newPoints, newValues, count + 1)
      }
      val q = GF2_192_Poly.interpolate(points, values, new GF2_192(t.challengeOpt.get.toArray))
      val newChildren = t.children.foldLeft(Seq[ProofTree](), 1) {
        case ((s, count), child) =>
          val newChild = child match {
            case r: UnprovenTree if r.real => r.withChallenge(Challenge @@ q.evaluate(count.toByte).toByteArray.toColl)
            case p: ProofTree => p
          }
          (s :+ newChild, count + 1)
      }._1
      t.withPolynomial(q).copy(children = newChildren)

    // If the node is a leaf marked "real", compute its response according to the second prover step
    // of the Sigma-protocol given the commitment, challenge, and witness, or pull response from the hints bag
    case su: UnprovenSchnorr if su.real =>
      assert(su.challengeOpt.isDefined, s"Real UnprovenSchnorr $su should have challenge defined")
      val privKeyOpt = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)

      val z = privKeyOpt match {
        case Some(privKey: DLogProverInput) =>
          hintsBag.ownCommitments.find(_.position == su.position).map { oc =>
            DLogProver.secondMessage(
              privKey,
              oc.secretRandomness,
              su.challengeOpt.get)
          }.getOrElse {
            DLogProver.secondMessage(
              privKey,
              su.randomnessOpt.get,
              su.challengeOpt.get)
          }

        case _ =>
          hintsBag.realProofs.find(_.position == su.position).map { proof =>
            val provenSchnorr = proof.uncheckedTree.asInstanceOf[UncheckedSchnorr]
            provenSchnorr.secondMessage
          }.getOrElse {
            val bs = secureRandomBytes(32)
            SecondDLogProverMessage(new BigInteger(1, bs).mod(CryptoConstants.groupOrder))
          }
      }
      UncheckedSchnorr(su.proposition, None, su.challengeOpt.get, z)

    // If the node is a leaf marked "real", compute its response according to the second prover step
    // of the Sigma-protocol given the commitment, challenge, and witness, or pull response from the hints bag
    case dhu: UnprovenDiffieHellmanTuple if dhu.real =>
      assert(dhu.challengeOpt.isDefined, s"Real UnprovenDiffieHellmanTuple $dhu should have challenge defined")
      val privKeyOpt = secrets
        .filter(_.isInstanceOf[DiffieHellmanTupleProverInput])
        .find(_.asInstanceOf[DiffieHellmanTupleProverInput].publicImage == dhu.proposition)

      val z = privKeyOpt match {
        case Some(privKey) =>
          hintsBag.ownCommitments.find(_.position == dhu.position).map { oc =>
            DiffieHellmanTupleProver.secondMessage(
              privKey.asInstanceOf[DiffieHellmanTupleProverInput],
              oc.secretRandomness,
              dhu.challengeOpt.get)
          }.getOrElse {
            DiffieHellmanTupleProver.secondMessage(
              privKey.asInstanceOf[DiffieHellmanTupleProverInput],
              dhu.randomnessOpt.get,
              dhu.challengeOpt.get)
          }

        case None =>
          hintsBag.realProofs.find(_.position == dhu.position).map { proof =>
            val provenSchnorr = proof.uncheckedTree.asInstanceOf[UncheckedDiffieHellmanTuple]
            provenSchnorr.secondMessage
          }.getOrElse {
            val bs = secureRandomBytes(32)
            SecondDHTupleProverMessage(new BigInteger(1, bs).mod(CryptoConstants.groupOrder))
          }
      }
      UncheckedDiffieHellmanTuple(dhu.proposition, None, dhu.challengeOpt.get, z)

    // if the simulated node is proven by someone else, take it from hints bag
    case su: UnprovenLeaf if su.simulated =>
      hintsBag.simulatedProofs.find(_.image == su.proposition).map { proof =>
        proof.uncheckedTree
      }.getOrElse(su)

    case sn: UncheckedSchnorr => sn

    case dh: UncheckedDiffieHellmanTuple => dh

    case ut: UnprovenTree => ut

    case t: ProofTree =>
      logMessage(s"Wrong input in prove(): $t");
      ???
  })


  //converts SigmaTree => UnprovenTree
  def convertToUnproven(sigmaTree: SigmaBoolean): UnprovenTree = sigmaTree match {
    case and@CAND(sigmaTrees) =>
      CAndUnproven(and, None, simulated = false, sigmaTrees.map(convertToUnproven))
    case or@COR(children) =>
      COrUnproven(or, None, simulated = false, children.map(convertToUnproven))
    case threshold@CTHRESHOLD(k, children) =>
      CThresholdUnproven(threshold, None, simulated = false, k, children.map(convertToUnproven), None)
    case ci: ProveDlog =>
      UnprovenSchnorr(ci, None, None, None, simulated = false)
    case dh: ProveDHTuple =>
      UnprovenDiffieHellmanTuple(dh, None, None, None, simulated = false)
    case _ =>
      syntax.error(s"Cannot convertToUnproven($sigmaTree)")
  }

  //converts ProofTree => UncheckedSigmaTree
  def convertToUnchecked(proofTree: ProofTree): UncheckedSigmaTree = proofTree match {
    case and: CAndUnproven =>
      CAndUncheckedNode(and.challengeOpt.get, and.children.map(convertToUnchecked))
    case or: COrUnproven =>
      COrUncheckedNode(or.challengeOpt.get, or.children.map(convertToUnchecked))
    case t: CThresholdUnproven =>
      CThresholdUncheckedNode(t.challengeOpt.get, t.children.map(convertToUnchecked), t.k, t.polynomialOpt)
    case s: UncheckedSchnorr => s
    case d: UncheckedDiffieHellmanTuple => d
    case a: Any =>
      syntax.error(s"Cannot convertToUnproven($a)")
  }

  /**
    *
    * Sign arbitrary message under a key representing a statement provable via a sigma-protocol.
    *
    * @param sigmaTree - public key
    * @param message - message to sign
    * @param hintsBag - additional hints for a signer (useful for distributed signing)
    * @return - signature or error
    */
  def signMessage(sigmaTree: SigmaBoolean,
                  message: Array[Byte],
                  hintsBag: HintsBag): Try[Array[Byte]] = Try {
    val unprovenTree = convertToUnproven(sigmaTree)
    val proofTree = prove(unprovenTree, message, hintsBag)
    SigSerializer.toProofBytes(proofTree)
  }

}
