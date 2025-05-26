package sigmastate.interpreter

import sigma.ast.ErgoTree
import sigma.data.{ProveDHTuple, ProveDlog, SigmaBoolean, SigmaConjecture, SigmaLeaf}
import sigma.serialization.SigSerializer
import sigmastate._
import sigmastate.crypto.DLogProtocol.DLogProver
import sigmastate.crypto.DiffieHellmanTupleProver


trait ProverUtils extends Interpreter {

  /**
    * Generate commitments for a given ergoTree (mixed-tree) and public keys.
    *
    * First, the given tree is to be reduced to crypto-tree (sigma-tree) by using context provided.
    */
  def generateCommitmentsFor(ergoTree: ErgoTree,
                             context: CTX,
                             generateFor: Seq[SigmaBoolean]): HintsBag = {
    val reduced = fullReduction(ergoTree, context, Interpreter.emptyEnv)
    generateCommitmentsFor(reduced.value, generateFor)
  }

  /**
    * A method which is is generating commitments for all the public keys provided.
    *
    * Currently only keys in form of ProveDlog and ProveDiffieHellman are supported, not more complex subtrees.
    *
    * @param sigmaTree - crypto-tree
    * @param generateFor - public keys for which commitments should be generated
    * @return generated commitments (private, containing secret randomness, and public, containing only commitments)
    */
  def generateCommitmentsFor(sigmaTree: SigmaBoolean,
                             generateFor: Seq[SigmaBoolean]): HintsBag = {

    def traverseNode(sb: SigmaBoolean,
                     bag: HintsBag,
                     position: NodePosition): HintsBag = {
      sb match {
        case sc: SigmaConjecture =>
          sc.children.zipWithIndex.foldLeft(bag) { case (b, (child, idx)) =>
            traverseNode(child, b, position.child(idx))
          }
        case leaf: SigmaLeaf =>
          if (generateFor.contains(leaf)) {
            val (r, a) = leaf match {
              case _: ProveDlog =>
                DLogProver.firstMessage()
              case pdh: ProveDHTuple =>
                DiffieHellmanTupleProver.firstMessage(pdh)
              case _ => ???
            }
            val hints = Seq(OwnCommitment(leaf, r, a, position), RealCommitment(leaf, a, position))
            bag.addHints(hints: _*)
          } else {
            bag
          }
      }
    }

    traverseNode(sigmaTree, HintsBag.empty, position = NodePosition.CryptoTreePrefix)
  }

  /**
    * A method which is extracting partial proofs of secret knowledge for particular secrets with their
    * respective public images given. Useful for distributed signature applications.
    *
    * See DistributedSigSpecification for examples of usage.
    *
    * @param context                   - context used to reduce the proposition
    * @param ergoTree                 - proposition to reduce
    * @param proof                     - proof for reduced proposition
    * @param realSecretsToExtract      - public keys of secrets with real proofs
    * @param simulatedSecretsToExtract - public keys of secrets with simulated proofs
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def bagForMultisig(context: CTX,
                     ergoTree: ErgoTree,
                     proof: Array[Byte],
                     realSecretsToExtract: Seq[SigmaBoolean],
                     simulatedSecretsToExtract: Seq[SigmaBoolean] = Seq.empty): HintsBag = {
    val reduced = fullReduction(ergoTree, context, Interpreter.emptyEnv)
    bagForMultisig(reduced.value, proof, realSecretsToExtract, simulatedSecretsToExtract)
  }

  /**
    * A method which is extracting partial proofs of secret knowledge for particular secrets with their
    * respective public images given. Useful for distributed signature applications.
    *
    * See DistributedSigSpecification for examples of usage.
    *
    * @param sigmaTree                 - public key (in form of a sigma-tree)
    * @param proof                     - signature for the key
    * @param realSecretsToExtract      - public keys of secrets with real proofs
    * @param simulatedSecretsToExtract - public keys of secrets with simulated proofs
    * @return - bag of OtherSecretProven and OtherCommitment hints
    */
  def bagForMultisig(sigmaTree: SigmaBoolean,
                     proof: Array[Byte],
                     realSecretsToExtract: Seq[SigmaBoolean],
                     simulatedSecretsToExtract: Seq[SigmaBoolean]): HintsBag = {

    val ut = SigSerializer.parseAndComputeChallenges(sigmaTree, proof)(null)
    val proofTree = computeCommitments(ut).get.asInstanceOf[UncheckedSigmaTree]

    def traverseNode(tree: ProofTree,
                     realPropositions: Seq[SigmaBoolean],
                     simulatedPropositions: Seq[SigmaBoolean],
                     hintsBag: HintsBag,
                     position: NodePosition): HintsBag = {
      tree match {
        case inner: UncheckedConjecture =>
          inner.children.zipWithIndex.foldLeft(hintsBag) { case (hb, (c, idx)) =>
            traverseNode(c, realPropositions, simulatedPropositions, hb, position.child(idx))
          }
        case leaf: UncheckedLeaf =>
          val realFound = realPropositions.contains(leaf.proposition)
          val simulatedFound = simulatedPropositions.contains(leaf.proposition)
          if (realFound || simulatedFound) {
            val hints = if (realFound) {
              Seq(
                RealCommitment(leaf.proposition, leaf.commitmentOpt.get, position),
                RealSecretProof(leaf.proposition, leaf.challenge, leaf, position)
              )
            } else {
              Seq(
                SimulatedCommitment(leaf.proposition, leaf.commitmentOpt.get, position),
                SimulatedSecretProof(leaf.proposition, leaf.challenge, leaf, position)
              )
            }
            hintsBag.addHints(hints: _*)
          } else hintsBag
      }
    }

    traverseNode(proofTree, realSecretsToExtract, simulatedSecretsToExtract, HintsBag.empty, NodePosition.CryptoTreePrefix)
  }

}
