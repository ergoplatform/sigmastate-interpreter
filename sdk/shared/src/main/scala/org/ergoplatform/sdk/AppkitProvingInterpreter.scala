package org.ergoplatform.sdk

import debox.cfor
import org.ergoplatform._
import org.ergoplatform.sdk.Extensions.{CollOps, PairCollOps}
import org.ergoplatform.sdk.JavaHelpers.{TokenColl, UniversalConverter}
import org.ergoplatform.sdk.utils.ArithUtils
import org.ergoplatform.sdk.wallet.protocol.context.{BlockchainStateContext, TransactionContext}
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.validation.ValidationRules
import sigma.VersionContext
import sigma.data.{AvlTreeData, ProveDlog, SigmaBoolean}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.util.Extensions.LongOps
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import sigmastate.interpreter.Interpreter.{ReductionResult, estimateCryptoVerifyCost}
import sigmastate.interpreter._
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

import java.util
import java.util.{Objects, List => JList}
import scala.collection.mutable
import scala.util.Try

/**
 * A class which holds secrets and can sign transactions (aka generate proofs).
 *
 * @param secretKeys secrets in extended form to be used by prover
 * @param dLogInputs  prover inputs containing secrets for generating proofs for [[ProveDlog]] nodes.
 * @param dhtInputs  prover inputs containing secrets for generating proofs for [[ProveDHTuple]] nodes.
 * @param params     ergo blockchain parameters
 */
class AppkitProvingInterpreter(
      val secretKeys: IndexedSeq[ExtendedSecretKey],
      val dLogInputs: IndexedSeq[DLogProverInput],
      val dhtInputs: IndexedSeq[DiffieHellmanTupleProverInput],
      params: BlockchainParameters)
  extends ReducingInterpreter(params) with ProverInterpreter {

  override type CTX = ErgoLikeContext
  import org.ergoplatform.sdk.SdkIsos._

  /** All secrets available to this interpreter including [[ExtendedSecretKey]], dlog and
    * dht secrets.
    */
  override val secrets: Seq[SigmaProtocolPrivateInput[_]] = {
    val dlogs: IndexedSeq[DLogProverInput] = secretKeys.map(_.privateInput)
    dlogs ++ dLogInputs ++ dhtInputs
  }

  /** Public keys corresponding to dlog secrets (aka publicImage). */
  val pubKeys: Seq[ProveDlog] = secrets
      .filter { case _: DLogProverInput => true case _ => false }
      .map(_.asInstanceOf[DLogProverInput].publicImage)

  /** Helper method to accumulate cost while checking limit.
    *
    * @param currentCost current cost value
    * @param delta       additional cost to add to the current value
    * @param limit       total cost limit
    * @param msgSuffix   suffix added to the exception message
    * @return new increased cost when it doesn't exceed the limit
    * @throws Exception
    */
  def addCostLimited(currentCost: Long, delta: Long, limit: Long, msgSuffix: => String): Long = {
    val newCost = java7.compat.Math.addExact(currentCost, delta)
    if (newCost > limit)
      throw new Exception(s"Cost of transaction $newCost exceeds limit $limit: $msgSuffix")
    newCost
  }

  /** Reduces and signs the given transaction.
   *
   * @param unreducedTx  unreduced transaction data to be reduced (contains unsigned transaction)
   * @param stateContext state context of the blockchain in which the transaction should be signed
   * @param baseCost     the cost accumulated before this transaction
   * @return a new signed transaction with all inputs signed and the cost of this transaction
   *         The returned cost doesn't include `baseCost`.
   */
  def sign(unreducedTx: UnreducedTransaction,
           stateContext: BlockchainStateContext,
           baseCost: Int): Try[SignedTransaction] = Try {
    val maxCost = params.maxBlockCost
    var currentCost: Long = baseCost

    val reducedTx = reduceTransaction(unreducedTx, stateContext, baseCost)
    currentCost = addCostLimited(currentCost, reducedTx.ergoTx.cost, maxCost, msgSuffix = reducedTx.toString())

    val signedTx = signReduced(reducedTx, currentCost.toInt)
    currentCost += signedTx.cost // this never overflows if signReduced is successful

    val reductionAndVerificationCost = (currentCost - baseCost).toIntExact
    signedTx.copy(cost = reductionAndVerificationCost)
  }

  /** Reduce inputs of the given unsigned transaction to provable sigma propositions using
    * the given context. See [[ReducedErgoLikeTransaction]] for details.
    *
    * @note requires `unsignedTx` and `boxesToSpend` have the same boxIds in the same order.
    * @param boxesToSpend input boxes of the transaction
    * @param dataBoxes    data inputs of the transaction
    * @param stateContext state context of the blockchain in which the transaction should be signed
    * @param baseCost     the cost accumulated so far and before this operation
    * @param tokensToBurn requested tokens to be burnt in the transaction, if empty no burning allowed
    * @return a new reduced transaction with all inputs reduced and the cost of this transaction
    *         The returned cost doesn't include (so they need to be added back to get the total cost):
    *         1) `baseCost`
    *         2) reduction cost for each input.
    */
  def reduceTransaction(
      unsignedTx: UnsignedErgoLikeTransaction,
      boxesToSpend: IndexedSeq[ExtendedInputBox],
      dataBoxes: IndexedSeq[ErgoBox],
      stateContext: BlockchainStateContext,
      baseCost: Int,
      tokensToBurn: IndexedSeq[ErgoToken]): ReducedErgoLikeTransaction = {
    if (unsignedTx.inputs.length != boxesToSpend.length) throw new Exception("Not enough boxes to spend")
    if (unsignedTx.dataInputs.length != dataBoxes.length) throw new Exception("Not enough data boxes")

    val inputTokens = boxesToSpend.flatMap(_.box.additionalTokens.toArray)
    val outputTokens = unsignedTx.outputCandidates.flatMap(_.additionalTokens.toArray)
    val tokenDiff = JavaHelpers.subtractTokens(outputTokens, inputTokens)
    if (tokenDiff.nonEmpty) {
      val (toBurn, toMint) = tokenDiff.partition(_._2 < 0) // those with negative diff are to be burnt
      if (toBurn.nonEmpty) {
        if (tokensToBurn.nonEmpty) {
          val requestedToBurn = isoTokensListToTokenColl.to(tokensToBurn.convertTo[JList[ErgoToken]])
          val diff = JavaHelpers.subtractTokenColls(
            reducedTokens = toBurn.mapSecond(v => -v), // make positive amounts
            subtractedTokens = requestedToBurn
          )
          if (diff.nonEmpty) { // empty diff would mean equality
            throw TokenBalanceException(
              "Transaction tries to burn tokens, but not how it was requested", diff)
          }
        } else {
          throw TokenBalanceException(
            "Transaction tries to burn tokens when no burning was requested", tokenDiff)
        }
      }
      if (toMint.nonEmpty) {
        if (toMint.length > 1) {
          throw TokenBalanceException("Only one token can be minted in a transaction", toMint)
        }
        val isCorrectMintedTokenId = Objects.deepEquals(toMint(0)._1.toArray, boxesToSpend.head.box.id)
        if (!isCorrectMintedTokenId) {
          throw TokenBalanceException("Cannot mint a token with invalid id", toMint)
        }
      }
    }

    // Cost of transaction initialization: we should read and parse all inputs and data inputs,
    // and also iterate through all outputs to check rules
    val initialCost = ArithUtils.addExact(
      Interpreter.interpreterInitCost,
      java7.compat.Math.multiplyExact(boxesToSpend.size, params.inputCost),
      java7.compat.Math.multiplyExact(dataBoxes.size, params.dataInputCost),
      java7.compat.Math.multiplyExact(unsignedTx.outputCandidates.size, params.outputCost)
    )
    val maxCost = params.maxBlockCost
    val startCost = addCostLimited(baseCost, initialCost, maxCost, msgSuffix = unsignedTx.toString())

    val transactionContext = TransactionContext(boxesToSpend.map(_.box), dataBoxes, unsignedTx)

    val (outAssets, outAssetsNum) = JavaHelpers.extractAssets(unsignedTx.outputCandidates)
    val (inAssets, inAssetsNum) = JavaHelpers.extractAssets(boxesToSpend.map(_.box))

    val tokenAccessCost = params.tokenAccessCost
    val totalAssetsAccessCost =
      java7.compat.Math.addExact(
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(outAssetsNum, inAssetsNum), tokenAccessCost),
        java7.compat.Math.multiplyExact(java7.compat.Math.addExact(inAssets.size, outAssets.size), tokenAccessCost))

    val txCost = addCostLimited(startCost,
      delta = totalAssetsAccessCost,
      limit = maxCost, msgSuffix = s"when adding assets cost of $totalAssetsAccessCost")

    var currentCost = txCost
    val reducedInputs = mutable.ArrayBuilder.make[ReducedInputData]

    for ((inputBox, boxIdx) <- boxesToSpend.zipWithIndex) {
      val unsignedInput = unsignedTx.inputs(boxIdx)
      require(util.Arrays.equals(unsignedInput.boxId, inputBox.box.id))

      val context = new ErgoLikeContext(
        AvlTreeData.avlTreeFromDigest(stateContext.previousStateDigest),
        stateContext.sigmaLastHeaders,
        stateContext.sigmaPreHeader,
        transactionContext.dataBoxes,
        transactionContext.boxesToSpend,
        transactionContext.spendingTransaction,
        boxIdx.toShort,
        inputBox.extension,
        ValidationRules.currentSettings,
        costLimit = maxCost,
        initCost = currentCost,
        activatedScriptVersion = (params.blockVersion - 1).toByte
      )

      val reducedInput = reduce(Interpreter.emptyEnv, inputBox.box.ergoTree, context)
      currentCost = reducedInput.reductionResult.cost // Note, this value includes context.initCost
      reducedInputs += reducedInput
    }
    val reducedTx = ReducedErgoLikeTransaction(
      unsignedTx, reducedInputs.result(), cost = (currentCost - baseCost).toIntExact)
    reducedTx
  }

  /** Signs the given transaction (i.e. providing spending proofs) for each input so that
    * the resulting transaction can be submitted to the blockchain.
    * Note, this method doesn't require context to generate proofs (aka signatures).
    *
    * @param reducedTx unsigend transaction augmented with reduced
    * @return a new signed transaction with all inputs signed and the cost of this transaction
    *         The returned cost includes:
    *         - the costs of obtaining reduced transaction
    *         - the cost of verification of each signed input
    */
  def signReduced(reducedTx: ReducedTransaction, baseCost: Int): SignedTransaction = {
    val provedInputs = mutable.ArrayBuilder.make[Input]
    val unsignedTx = reducedTx.ergoTx.unsignedTx

    val maxCost = params.maxBlockCost
    var currentCost: Long = baseCost

    for ((reducedInput, boxIdx) <- reducedTx.ergoTx.reducedInputs.zipWithIndex ) {
      val unsignedInput = unsignedTx.inputs(boxIdx)
      val proverResult = proveReduced(reducedInput, unsignedTx.messageToSign)
      val signedInput = Input(unsignedInput.boxId, proverResult)

      val verificationCost = estimateCryptoVerifyCost(reducedInput.reductionResult.value).toBlockCost
      currentCost = addCostLimited(currentCost, verificationCost, maxCost, msgSuffix = signedInput.toString())

      provedInputs += signedInput
    }

    val signedTx = new ErgoLikeTransaction(
      provedInputs.result(), unsignedTx.dataInputs, unsignedTx.outputCandidates)

    // compute accumulated crypto verification cost of all inputs
    val txVerificationCost = (currentCost - baseCost).toIntExact
    SignedTransaction(signedTx, txVerificationCost)
  }

  // TODO pull this method up to the base class and reuse in `prove`
  /** Generates proof (aka signature) for the given message using secrets of this prover.
    * All the necessary secrets should be configured in this prover to satisfy the given
    * sigma proposition in the reducedInput.
    */
  def proveReduced(
        reducedInput: ReducedInputData,
        message: Array[Byte],
        hintsBag: HintsBag = HintsBag.empty): ProverResult = {
    val proof = generateProof(reducedInput.reductionResult.value, message, hintsBag)
    new ProverResult(proof, reducedInput.extension)
  }

}

/** Thrown during transaction signing when inputs token are not balanced with output tokens.
  * @param tokensDiff balance difference which caused the error
  */
case class TokenBalanceException(
  message: String,
  tokensDiff: TokenColl
) extends Exception(s"Input and output tokens are not balanced: $message")

/** Represents data necessary to sign an input of an unsigned transaction.
  * @param reductionResult result of reducing input script to a sigma proposition
  * @param extension context extensions (aka context variables) used by script and which
  *                  are also necessary to verify the transaction on-chain. Extensions are
  *                  included in tx bytes, which are signed.
  */
case class ReducedInputData(reductionResult: ReductionResult, extension: ContextExtension)

/** Represent `reduced` transaction, i.e. unsigned transaction where each unsigned input
  * is augmented with [[ReducedInputData]] which contains a script reduction result.
  * After an unsigned transaction is reduced it can be signed without context.
  * Thus, it can be serialized and transferred for example to Cold Wallet and signed
  * in an environment where secrets are known.
  */
case class ReducedErgoLikeTransaction(
  unsignedTx: UnsignedErgoLikeTransaction,
  reducedInputs: Seq[ReducedInputData],
  cost: Int
) {
  require(unsignedTx.inputs.length == reducedInputs.length)
}

/** HOTSPOT: don't beautify the code */
object ReducedErgoLikeTransactionSerializer extends SigmaSerializer[ReducedErgoLikeTransaction, ReducedErgoLikeTransaction] {

  override def serialize(tx: ReducedErgoLikeTransaction, w: SigmaByteWriter): Unit = {
    val msg = tx.unsignedTx.messageToSign
    w.putUInt(msg.length)  // size of the tx bytes to restore tx reliably
    w.putBytes(msg)

    // serialize sigma propositions for each input
    val nInputs = tx.reducedInputs.length
    // no need to save nInputs because it is known from unsignedTx.inputs
    cfor(0)(_ < nInputs, _ + 1) { i =>
      val input = tx.reducedInputs(i)
      SigmaBoolean.serializer.serialize(input.reductionResult.value, w)
      w.putULong(input.reductionResult.cost)
      // Note, we don't need to save `extension` field because it has already
      // been saved in msg
    }
    w.putUInt(tx.cost)
  }

  override def parse(r: SigmaByteReader): ReducedErgoLikeTransaction = {
    val nBytes = r.getUInt()
    val msg = r.getBytes(nBytes.toIntExact)

    // here we read ErgoLikeTransaction which is used below as raw data for
    // the new UnsignedErgoLikeTransaction
    val tx = ErgoLikeTransactionSerializer.parse(SigmaSerializer.startReader(msg))

    // serialize sigma propositions for each input
    val nInputs = tx.inputs.length
    val reducedInputs = new Array[ReducedInputData](nInputs)
    val unsignedInputs = new Array[UnsignedInput](nInputs)
    cfor(0)(_ < nInputs, _ + 1) { i =>
      val sb = SigmaBoolean.serializer.parse(r)
      val cost = r.getULong()
      val input = tx.inputs(i)
      val extension = input.extension
      val reductionResult = ReductionResult(sb, cost)
      reducedInputs(i) = ReducedInputData(reductionResult, extension)
      unsignedInputs(i) = new UnsignedInput(input.boxId, extension)
    }
    val cost = r.getUIntExact
    val unsignedTx = UnsignedErgoLikeTransaction(unsignedInputs, tx.dataInputs, tx.outputCandidates)
    ReducedErgoLikeTransaction(unsignedTx, reducedInputs, cost)
  }

  /** Parses the [[ReducedErgoLikeTransaction]] using the given blockVersion.
    * @param blockVersion version of Ergo protocol to use during parsing.
    */
  def parse(r: SigmaByteReader, blockVersion: Byte): ReducedErgoLikeTransaction = {
    val scriptVersion = (blockVersion - 1).toByte
    VersionContext.withVersions(scriptVersion, scriptVersion) {
      parse(r)
    }
  }
}

