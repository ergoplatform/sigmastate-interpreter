package sigma.serialization

import debox.cfor
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.ast.{FixedCost, JitCost, NamedDesc, OperationCostInfo, PerItemCost}
import sigma.crypto.{BigIntegers, CryptoConstants}
import sigma.data.{CAND, COR, CTHRESHOLD, ProveDHTuple, ProveDlog, SigmaBoolean}
import sigma.util.safeNewArray
import sigmastate.{CAndUncheckedNode, COrUncheckedNode, CThresholdUncheckedNode, NoProof, UncheckedDiffieHellmanTuple, UncheckedSchnorr, UncheckedSigmaTree, UncheckedTree}
import sigmastate.crypto.DLogProtocol.SecondDLogProverMessage
import sigmastate.crypto.VerifierMessage.Challenge
import sigmastate.crypto.{GF2_192_Poly, SecondDHTupleProverMessage}
import sigmastate.interpreter.CErgoTreeEvaluator.{fixedCostOp, perItemCostOp}
import sigmastate.interpreter.CErgoTreeEvaluator
import sigmastate.utils.Helpers

/** Contains implementation of signature (aka proof) serialization.
 *
  * @see toProofBytes, parseAndComputeChallenges
  */
class SigSerializer {
  /** Log warning message using this class's logger. */
  def warn(msg: String) = println(msg)

  /** A size of challenge in Sigma protocols, in bits. */
  val hashSize = CryptoConstants.soundnessBits / 8

  /** Number of bytes to represent any group element as byte array */
  val order = sigma.crypto.groupSize

  /** Recursively traverses the given node and serializes challenges and prover messages
    * to the given writer.
    * Note, sigma propositions and commitments are not serialized.
    *
    * @param tree tree to traverse and serialize
    * @return the proof bytes containing all the serialized challenges and prover messages
    *         (aka `z` values)
    */
  def toProofBytes(tree: UncheckedTree): Array[Byte] = {
    tree match {
      case NoProof =>
        Array.emptyByteArray
      case t: UncheckedSigmaTree =>
        val w = SigmaSerializer.startWriter()
        toProofBytes(t, w, writeChallenge = true)
        val res = w.toBytes
        res
    }
  }

  /** Recursively traverses the given node and serializes challenges and prover messages
    * to the given writer.
    * Note, sigma propositions and commitments are not serialized.
    *
    * @param node           subtree to traverse
    * @param w              writer to put the bytes
    * @param writeChallenge if true, than node.challenge is serialized, and omitted
    *                       otherwise.
    */
  def toProofBytes(node: UncheckedSigmaTree,
                   w: SigmaByteWriter,
                   writeChallenge: Boolean): Unit = {
    if (writeChallenge) {
      w.putBytes(node.challenge.toArray)
    }
    node match {
      case dl: UncheckedSchnorr =>
        val z = BigIntegers.asUnsignedByteArray(order, dl.secondMessage.z.bigInteger)
        w.putBytes(z)

      case dh: UncheckedDiffieHellmanTuple =>
        val z = BigIntegers.asUnsignedByteArray(order, dh.secondMessage.z)
        w.putBytes(z)

      case and: CAndUncheckedNode =>
        // don't write children's challenges -- they are equal to the challenge of this node
        val cs = and.children.toArray
        cfor(0)(_ < cs.length, _ + 1) { i =>
          val child = cs(i)
          toProofBytes(child, w, writeChallenge = false)
        }

      case or: COrUncheckedNode =>
        // don't write last child's challenge -- it's computed by the verifier via XOR
        val cs = or.children.toArray
        val iLastChild = cs.length - 1
        cfor(0)(_ < iLastChild, _ + 1) { i =>
          val child = cs(i)
          toProofBytes(child, w, writeChallenge = true)
        }
        toProofBytes(cs(iLastChild), w, writeChallenge = false)

      case t: CThresholdUncheckedNode =>
        // write the polynomial, except the zero coefficient
        val poly = t.polynomialOpt.get.toByteArray(false)
        w.putBytes(poly)

        // don't write children's challenges
        val cs = t.children.toArray
        cfor(0)(_ < cs.length, _ + 1) { i =>
          val child = cs(i)
          toProofBytes(child, w, writeChallenge = false)
        }

      case _ =>
        throw new SerializerException(s"Don't know how to execute toBytes($node)")
    }
  }

  /** Verifier Step 2: In a top-down traversal of the tree, obtain the challenges for the
    * children of every non-leaf node by reading them from the proof or computing them.
    * Verifier Step 3: For every leaf node, read the response z provided in the proof.
    *
    * @param exp   sigma proposition which defines the structure of bytes from the reader
    * @param proof proof to extract challenges from
    * @param E     optional evaluator (can be null) which is used for profiling of operations.
    *              When `E` is `null`, then profiling is turned-off and has no effect on
    *              the execution.
    * @return An instance of [[UncheckedTree]] i.e. either [[NoProof]] or [[UncheckedSigmaTree]]
    */
  def parseAndComputeChallenges(exp: SigmaBoolean, proof: Array[Byte])(implicit E: CErgoTreeEvaluator): UncheckedTree = {
    if (proof.isEmpty)
      NoProof
    else {
      // Verifier step 1: Read the root challenge from the proof.
      val r = SigmaSerializer.startReader(proof)
      val res = parseAndComputeChallenges(exp, r, null)
      res
    }
  }

  /** Represents cost of parsing UncheckedSchnorr node from proof bytes. */
  final val ParseChallenge_ProveDlog = OperationCostInfo(
    FixedCost(JitCost(10)), NamedDesc("ParseChallenge_ProveDlog"))

  /** Represents cost of parsing UncheckedDiffieHellmanTuple node from proof bytes. */
  final val ParseChallenge_ProveDHT = OperationCostInfo(
    FixedCost(JitCost(10)), NamedDesc("ParseChallenge_ProveDHT"))

  /** Represents cost of parsing GF2_192_Poly from proof bytes. */
  final val ParsePolynomial = OperationCostInfo(
    PerItemCost(baseCost = JitCost(10), perChunkCost = JitCost(10), chunkSize = 1),
    NamedDesc("ParsePolynomial"))

  /** Represents cost of:
    * 1) evaluating a polynomial
    * 2) obtaining GF2_192 instance
    * 3) converting it to array of bytes
    */
  final val EvaluatePolynomial = OperationCostInfo(
    PerItemCost(baseCost = JitCost(3), perChunkCost = JitCost(3), chunkSize = 1),
    NamedDesc("EvaluatePolynomial"))

  /** Helper method to read requested or remaining bytes from the reader. */
  def readBytesChecked(r: SigmaByteReader, numRequestedBytes: Int, onError: String => Unit): Array[Byte] = {
    val bytes = r.getBytesUnsafe(numRequestedBytes)
    if (bytes.length != numRequestedBytes) {
      val hex = Base16.encode(r.getAllBufferBytes)
      onError(hex)
    }
    bytes
  }

  /** Verifier Step 2: In a top-down traversal of the tree, obtain the challenges for the
    * children of every non-leaf node by reading them from the proof or computing them.
    * Verifier Step 3: For every leaf node, read the response z provided in the proof.
    *
    * @param exp          sigma proposition which defines the structure of bytes from the reader
    * @param r            reader to extract challenges from
    * @param challengeOpt if non-empty, then the challenge has been computed for this node
    *                     by its parent; else it needs to be read from the proof (via reader)
    * @param E            optional evaluator (can be null) which is used for profiling of operations.
    *                     When `E` is `null`, then profiling is turned-off and has no effect on
    *                     the execution.
    * @return An instance of [[UncheckedSigmaTree]]
    *
    * HOTSPOT: don't beautify the code
    * Note, null` is used instead of Option to avoid allocations.
    */
  def parseAndComputeChallenges(
        exp: SigmaBoolean,
        r: SigmaByteReader,
        challengeOpt: Challenge = null)(implicit E: CErgoTreeEvaluator): UncheckedSigmaTree = {
    // Verifier Step 2: Let e_0 be the challenge in the node here (e_0 is called "challenge" in the code)
    val challenge = if (challengeOpt == null) {
      Challenge @@ readBytesChecked(r, hashSize,
        hex => warn(s"Invalid challenge in: $hex")).toColl
    } else {
      challengeOpt
    }

    exp match {
      case dl: ProveDlog =>
        // Verifier Step 3: For every leaf node, read the response z provided in the proof.
        fixedCostOp(ParseChallenge_ProveDlog) {
          val z_bytes = readBytesChecked(r, order, hex => warn(s"Invalid z bytes for $dl: $hex"))
          val z = BigIntegers.fromUnsignedByteArray(z_bytes)
          UncheckedSchnorr(dl, None, challenge, SecondDLogProverMessage(z))
        }

      case dh: ProveDHTuple =>
        // Verifier Step 3: For every leaf node, read the response z provided in the proof.
        fixedCostOp(ParseChallenge_ProveDHT) {
          val z_bytes = readBytesChecked(r, order, hex => warn(s"Invalid z bytes for $dh: $hex"))
          val z = BigIntegers.fromUnsignedByteArray(z_bytes)
          UncheckedDiffieHellmanTuple(dh, None, challenge, SecondDHTupleProverMessage(z))
        }

      case and: CAND =>
        // Verifier Step 2: If the node is AND, then all of its children get e_0 as the challenge
        val nChildren = and.children.length
        val children = safeNewArray[UncheckedSigmaTree](nChildren)
        cfor(0)(_ < nChildren, _ + 1) { i =>
          children(i) = parseAndComputeChallenges(and.children(i), r, challenge)
        }
        CAndUncheckedNode(challenge, children)

      case or: COR =>
        // Verifier Step 2: If the node is OR, then each of its children except rightmost
        // one gets the challenge given in the proof for that node.
        // The rightmost child gets a challenge computed as an XOR of the challenges of all the other children and e_0.

        // Read all the children but the last and compute the XOR of all the challenges including e_0
        val nChildren = or.children.length
        val children = safeNewArray[UncheckedSigmaTree](nChildren)
        val xorBuf = challenge.toArray.clone()
        val iLastChild = nChildren - 1
        cfor(0)(_ < iLastChild, _ + 1) { i =>
          val parsedChild = parseAndComputeChallenges(or.children(i), r, null)
          children(i) = parsedChild
          Helpers.xorU(xorBuf, parsedChild.challenge.toArray) // xor it into buffer
        }
        val lastChild = or.children(iLastChild)

        // use the computed XOR for last child's challenge
        children(iLastChild) = parseAndComputeChallenges(
          lastChild, r, challengeOpt = Challenge @@ xorBuf.toColl)

        COrUncheckedNode(challenge, children)

      case th: CTHRESHOLD =>
        // Verifier Step 2: If the node is THRESHOLD,
        // evaluate the polynomial Q(x) at points 1, 2, ..., n to get challenges for child 1, 2, ..., n, respectively.

        // Read the polynomial -- it has n-k coefficients
        val nChildren = th.children.length
        val nCoefs = nChildren - th.k
        val polynomial = perItemCostOp(ParsePolynomial, nCoefs) { () =>
          val coeffBytes = readBytesChecked(r, hashSize * nCoefs,
            hex => warn(s"Invalid coeffBytes for $th: $hex"))
          GF2_192_Poly.fromByteArray(challenge.toArray, coeffBytes)
        }

        val children = safeNewArray[UncheckedSigmaTree](nChildren)
        cfor(0)(_ < nChildren, _ + 1) { i =>
          val c = perItemCostOp(EvaluatePolynomial, nCoefs) { () =>
            Challenge @@ polynomial.evaluate((i + 1).toByte).toByteArray.toColl
          }
          children(i) = parseAndComputeChallenges(th.children(i), r, c)
        }

        CThresholdUncheckedNode(challenge, children, th.k, Some(polynomial))
    }
  }

}

object SigSerializer extends SigSerializer