package org.ergoplatform.sdk

import sigma.data.SigmaConstants.MaxBoxSize
import org.ergoplatform.sdk.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.sdk.wallet.{AssetUtils, TokensMap}
import org.ergoplatform.{ErgoBoxAssets, ErgoBoxAssetsHolder}
import scorex.util.ModifierId

import scala.collection.mutable

object BoxSelection {
  // from https://github.com/ergoplatform/ergo/blob/2ce78a0380977b8ca354518edca93a5269ac9f53/src/main/scala/org/ergoplatform/settings/Parameters.scala#L258-L258
  private val MinValuePerByteDefault = 30 * 12

  val MinBoxValue: Long = (MaxBoxSize.value / 2L) * MinValuePerByteDefault

  trait Error {
    def message: String
  }

  final case class NotEnoughErgsError(
      message: String,
      balanceFound: Long) extends Error

  final case class NotEnoughTokensError(
      message: String,
      tokensFound: Map[ModifierId, Long]) extends Error

  final case class NotEnoughCoinsForChangeBoxesError(message: String) extends Error

  /**
    * Pass through implementation of the box selector. Unlike DefaultBoxSelector from ergo-wallet,
    * it does not select input boxes. We do this in SDK ourselves and do not need the selector
    * to interfere with how we built our transaction. Instead, this selector performs validation
    * and calculates the necessary change box
    */
  class InputBoxesValidator {

    def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
        externalFilter: T => Boolean,
        targetBalance: Long,
        targetAssets: TokensMap): Either[Error, BoxSelectionResult[T]] = {
      //mutable structures to collect results
      val res = mutable.Buffer[T]()
      var currentBalance = 0L
      val currentAssets = mutable.Map[ModifierId, Long]()

      // select all input boxes - we only validate here
      inputBoxes.foreach { box: T =>
        currentBalance = currentBalance + box.value
        AssetUtils.mergeAssetsMut(currentAssets, box.tokens)
        res += box
      }

      if (currentBalance - targetBalance >= 0) {
        //now check if we found all tokens
        if (targetAssets.forall {
          case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt
        }) {
          formChangeBoxes(currentBalance, targetBalance, currentAssets, targetAssets) match {
            case Right(changeBoxes) => Right(new BoxSelectionResult(res.toSeq, changeBoxes, None))
            case Left(error) => Left(error)
          }
        } else {
          Left(NotEnoughTokensError(
            s"Not enough tokens in input boxes to send $targetAssets (found only $currentAssets)", currentAssets.toMap)
          )
        }
      } else {
        Left(NotEnoughErgsError(
          s"not enough boxes to meet ERG needs $targetBalance (found only $currentBalance)", currentBalance)
        )
      }
    }

    /**
      * Helper method to construct change outputs
      *
      * @param foundBalance    - ERG balance of boxes collected
      *                        (spendable only, so after possibly deducting re-emission tokens)
      * @param targetBalance   - ERG amount to be transferred to recipients
      * @param foundBoxAssets  - assets balances of boxes
      * @param targetBoxAssets - assets amounts to be transferred to recipients
      * @return
      */
    def formChangeBoxes(foundBalance: Long,
        targetBalance: Long,
        foundBoxAssets: mutable.Map[ModifierId, Long],
        targetBoxAssets: TokensMap): Either[Error, Seq[ErgoBoxAssets]] = {
      AssetUtils.subtractAssetsMut(foundBoxAssets, targetBoxAssets)
      val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = foundBoxAssets.grouped(MaxAssetsPerBox).toSeq
      val changeBalance = foundBalance - targetBalance
      //at least a minimum amount of ERG should be assigned per a created box
      if (changeBoxesAssets.size * MinBoxValue > changeBalance) {
        Left(NotEnoughCoinsForChangeBoxesError(
          s"Not enough nanoERGs ($changeBalance nanoERG) to create ${changeBoxesAssets.size} change boxes, \nfor $changeBoxesAssets"
        ))
      } else {
        val changeBoxes = if (changeBoxesAssets.nonEmpty) {
          val baseChangeBalance = changeBalance / changeBoxesAssets.size

          val changeBoxesNoBalanceAdjusted = changeBoxesAssets.map { a =>
            ErgoBoxAssetsHolder(baseChangeBalance, a.toMap)
          }

          val modifiedBoxOpt = changeBoxesNoBalanceAdjusted.headOption.map { firstBox =>
            ErgoBoxAssetsHolder(
              changeBalance - baseChangeBalance * (changeBoxesAssets.size - 1),
              firstBox.tokens
            )
          }

          modifiedBoxOpt.toSeq ++ changeBoxesNoBalanceAdjusted.tail
        } else if (changeBalance > 0) {
          Seq(ErgoBoxAssetsHolder(changeBalance))
        } else {
          Seq.empty
        }

        Right(changeBoxes)
      }
    }

  }

}

