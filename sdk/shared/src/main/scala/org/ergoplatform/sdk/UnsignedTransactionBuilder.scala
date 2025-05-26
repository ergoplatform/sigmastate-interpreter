package org.ergoplatform.sdk

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.ergoplatform.sdk.BlockchainParameters.MinChangeValue
import org.ergoplatform.sdk.BoxSelection.InputBoxesValidator
import org.ergoplatform.sdk.Extensions.HeaderOps
import org.ergoplatform.sdk.wallet.{AssetUtils, TokensMap}
import scorex.util.{ModifierId, bytesToId}
import sigma.Extensions.ArrayOps
import sigmastate.utils.Extensions.ModifierIdOps
import sigma.Coll
import sigma.Extensions.CollBytesOps
import sigma.PreHeader
import sigma.data.Digest32CollRType

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class UnsignedTransactionBuilder(val ctx: BlockchainContext) {
  private[sdk] val _inputs: ArrayBuffer[ExtendedInputBox] = ArrayBuffer.empty[ExtendedInputBox]
  private[sdk] val _outputs: ArrayBuffer[OutBox] = ArrayBuffer.empty[OutBox]
  private[sdk] val _dataInputs: ArrayBuffer[ErgoBox] = ArrayBuffer.empty[ErgoBox]

  private var _tokensToBurn: Option[ArrayBuffer[ErgoToken]] = None
  private var _feeAmount: Option[Long] = None
  private var _changeAddress: Option[ErgoAddress] = None
  private var _ph: Option[PreHeader] = None

  def preHeader(ph: PreHeader): this.type = {
    require(_ph.isEmpty, "PreHeader is already specified")
    _ph = Some(ph)
    this
  }

  def addInputs(boxes: ExtendedInputBox*): this.type = {
    _inputs ++= boxes
    this
  }

  def addDataInputs(boxes: ErgoBox*): this.type = {
    _dataInputs ++= boxes
    this
  }

  def addOutputs(outBoxes: OutBox*): this.type = {
    _outputs ++= outBoxes
    this
  }

  def fee(feeAmount: Long): this.type = {
    require(_feeAmount.isEmpty, "Fee already defined")
    _feeAmount = Some(feeAmount)
    this
  }

  def addTokensToBurn(tokens: ErgoToken*): this.type = {
    if (_tokensToBurn.isEmpty)
      _tokensToBurn = Some(ArrayBuffer.empty[ErgoToken])

    _tokensToBurn.get ++= tokens
    this
  }

  def sendChangeTo(changeAddress: ErgoAddress): this.type = {
    require(_changeAddress.isEmpty, "Change address is already specified")
    _changeAddress = Some(changeAddress)
    this
  }

  private def getDefined[T](opt: Option[T], msg: => String): T = {
    opt match {
      case Some(x) => x
      case _ =>
        throw new IllegalArgumentException("requirement failed: " + msg)
    }
  }

  def build(): UnreducedTransaction = {
    val boxesToSpend = _inputs.toIndexedSeq
    val outputCandidates = _outputs.map(c => c.candidate).toIndexedSeq
    require(!outputCandidates.isEmpty, "Output boxes are not specified")

    val dataInputBoxes = _dataInputs.toIndexedSeq
    val dataInputs = _dataInputs.map(box => DataInput(box.id)).toIndexedSeq
    require(_feeAmount.isEmpty || _feeAmount.get >= BlockchainParameters.MinFee,
      s"When fee amount is defined it should be >= ${BlockchainParameters.MinFee}, got ${_feeAmount.get}")
    val changeAddress = getDefined(_changeAddress, "Change address is not defined")
    val inputBoxesSeq = boxesToSpend.map(eb => eb.box)
    val requestedToBurn = _tokensToBurn.fold(IndexedSeq.empty[ErgoToken])(_.toIndexedSeq)
    val burnTokens = SdkIsos.isoErgoTokenSeqToLinkedMap.to(requestedToBurn).toMap
    val rewardDelay = ctx.networkType match {
      case NetworkType.Mainnet => BlockchainParameters.MinerRewardDelay_Mainnet
      case NetworkType.Testnet => BlockchainParameters.MinerRewardDelay_Testnet
    }
    val tx = UnsignedTransactionBuilder.buildUnsignedTx(
      inputs = inputBoxesSeq, dataInputs = dataInputs, outputCandidates = outputCandidates,
      currentHeight = ctx.height, createFeeOutput = _feeAmount,
      changeAddress = changeAddress, minChangeValue = MinChangeValue,
      minerRewardDelay = rewardDelay,
      burnTokens = burnTokens).get

    // the method above don't accept ContextExtension along with inputs, thus, after the
    // transaction has been built we need to zip with the extensions that have been
    // attached to the inputBoxes
    val txWithExtensions = new UnsignedErgoLikeTransaction(
      inputs = boxesToSpend.map(_.toUnsignedInput),
      tx.dataInputs, tx.outputCandidates
    )
    UnreducedTransaction(txWithExtensions, boxesToSpend, dataInputBoxes, requestedToBurn)
  }

  def preHeader: PreHeader = _ph.getOrElse(ctx.headers(0).toPreHeader)

  def outBoxBuilder: OutBoxBuilder = OutBoxBuilder(this)

  def networkType: NetworkType = ctx.networkType

  def inputBoxes: IndexedSeq[ExtendedInputBox] = _inputs.toIndexedSeq

  def outputBoxes: IndexedSeq[OutBox] = _outputs.toIndexedSeq
}

object UnsignedTransactionBuilder {
  def apply(ctx: BlockchainContext): UnsignedTransactionBuilder = new UnsignedTransactionBuilder(ctx)

  private def validateStatelessChecks(
      inputs: IndexedSeq[ErgoBox], dataInputs: IndexedSeq[DataInput],
      outputCandidates: Seq[ErgoBoxCandidate]): Unit = {
    // checks from ErgoTransaction.validateStateless
    require(inputs.nonEmpty, "inputs cannot be empty")
    require(outputCandidates.nonEmpty, "outputCandidates cannot be empty")
    require(inputs.size <= Short.MaxValue, s"too many inputs - ${inputs.size} (max ${Short.MaxValue})")
    require(dataInputs.size <= Short.MaxValue, s"too many dataInputs - ${dataInputs.size} (max ${Short.MaxValue})")
    require(outputCandidates.size <= Short.MaxValue,
      s"too many outputCandidates - ${outputCandidates.size} (max ${Short.MaxValue})")
    require(outputCandidates.forall(_.value >= 0), s"outputCandidate.value must be >= 0")
    val outputSumTry = Try(outputCandidates.map(_.value).reduce(java7.compat.Math.addExact(_, _)))
    require(outputSumTry.isSuccess, s"Sum of transaction output values should not exceed ${Long.MaxValue}")
    require(inputs.distinct.size == inputs.size, s"There should be no duplicate inputs")
  }

  def collectOutputTokens(outputCandidates: Seq[ErgoBoxCandidate]): TokensMap = {
    AssetUtils.mergeAssets(
      initialMap = Map.empty[ModifierId, Long],
      maps = outputCandidates.map(b => collTokensToMap(b.additionalTokens)): _*)
  }

  def collTokensToMap(tokens: Coll[(TokenId, Long)]): TokensMap =
    tokens.toArray.map(t => t._1.toModifierId -> t._2).toMap

  def tokensMapToColl(tokens: TokensMap): Coll[(TokenId, Long)] =
    tokens.toArray.map { t => t._1.toTokenId -> t._2 }.toColl

  /** Creates unsigned transaction from given inputs and outputs adding outputs with miner's fee and change
    * Runs required checks ensuring that resulted transaction will be successfully validated by a node.
    *
    * @param inputs           - input boxes
    * @param dataInputs       - data inputs
    * @param outputCandidates - output candidate boxes
    * @param currentHeight    - current height (used in miner's fee box and change box)
    * @param createFeeOutput  - optional fee amount to put in a new miner's fee box, which will be
    *                         created by this method. If None, then feeOut is not created.
    * @param changeAddress    - address where to send change from the input boxes
    * @param minChangeValue   - minimum change value to send, otherwise add to miner's fee
    * @param minerRewardDelay - reward delay to encode in miner's fee box
    * @return unsigned transaction
    */
  def buildUnsignedTx(
      inputs: IndexedSeq[ErgoBox],
      dataInputs: IndexedSeq[DataInput],
      outputCandidates: Seq[ErgoBoxCandidate],
      currentHeight: Int,
      createFeeOutput: Option[Long],
      changeAddress: ErgoAddress,
      minChangeValue: Long,
      minerRewardDelay: Int,
      burnTokens: TokensMap = Map.empty
  ): Try[UnsignedErgoLikeTransaction] = Try {
    validateStatelessChecks(inputs, dataInputs, outputCandidates)

    // TODO: implement all appropriate checks from ErgoTransaction.validateStatefull
    val feeAmount = createFeeOutput.getOrElse(0L)
    require(createFeeOutput.fold(true)(_ > 0), s"expected fee amount > 0, got $feeAmount")
    val inputTotal = inputs.map(_.value).sum
    val outputSum = outputCandidates.map(_.value).sum
    val outputTotal = outputSum + feeAmount
    val changeAmt = inputTotal - outputTotal
    require(changeAmt >= 0, s"total inputs $inputTotal is less then total outputs $outputTotal")
    val firstInputBoxId = bytesToId(inputs(0).id)
    val tokensOut = collectOutputTokens(outputCandidates)
    // remove minted tokens if any
    val tokensOutNoMinted = tokensOut.filterKeys(_ != firstInputBoxId)
    val mintedTokensNum = tokensOut.size - tokensOutNoMinted.size
    require(mintedTokensNum <= 1, s"Only one token can be minted, but found $mintedTokensNum")
    require(burnTokens.values.forall(_ > 0),
      s"Incorrect burnTokens specification, positive values are expected: $burnTokens")
    // add burnTokens to target assets so that they are excluded from the change outputs
    // thus total outputs assets will be reduced which is interpreted as _token burning_
    val tokensOutWithBurned = AssetUtils.mergeAssets(tokensOutNoMinted.toMap, burnTokens)
    val boxSelector = new InputBoxesValidator
    val selection = boxSelector.select[ErgoBox](inputs.iterator, _ => true, outputTotal, tokensOutWithBurned) match {
      case Left(err) => throw new IllegalArgumentException(
        s"failed to calculate change for outputTotal: $outputTotal, \ntokens: $tokensOut, \nburnTokens: $burnTokens, \ninputs: $inputs, \nreason: $err")
      case Right(v) => v
    }
    // although we're only interested in change boxes, make sure selection contains exact inputs
    assert(selection.inputBoxes == inputs, s"unexpected selected boxes, expected: $inputs, got ${selection.inputBoxes}")
    val changeBoxes = selection.changeBoxes
    val changeBoxesHaveTokens = changeBoxes.exists(_.tokens.nonEmpty)
    val changeGoesToFee = changeAmt < minChangeValue && !changeBoxesHaveTokens
    require(!changeGoesToFee || (changeAmt == 0 || createFeeOutput.isDefined),
      s"""When change=$changeAmt < minChangeValue=$minChangeValue it is added to miner's fee,
        |in this case createFeeOutput should be defined
        |""".stripMargin)
    val feeOutOpt = createFeeOutput.map { fee =>
      // if computed changeAmt is too small give it to miner as tips
      val actualFee = if (changeGoesToFee) fee + changeAmt else fee
      new ErgoBoxCandidate(
        actualFee,
        ErgoTreePredef.feeProposition(minerRewardDelay),
        currentHeight
      )
    }
    val addedChangeOut = if (!changeGoesToFee) {
      val script = changeAddress.script
      changeBoxes.map { cb =>
        new ErgoBoxCandidate(cb.value, script, currentHeight, tokensMapToColl(cb.tokens))
      }
    } else {
      Seq()
    }
    val finalOutputCandidates = outputCandidates ++ feeOutOpt ++ addedChangeOut
    new UnsignedErgoLikeTransaction(
      inputs.map(b => new UnsignedInput(b.id)),
      dataInputs,
      finalOutputCandidates.toIndexedSeq
    )
  }
}

