package org.ergoplatform.dsl

import org.ergoplatform.dsl.ContractSyntax.Token

trait StdContracts { self: ContractSyntax =>
  import spec._
  def transferErgWithChange(tx: TransactionCandidate, from: OutBox, to: PropositionSpec, ergAmt: Long): (OutBox, Option[OutBox]) = {
    val ergChange = from.value - ergAmt
    if (ergChange < 0)
      error(s"Cannot transfer $ergAmt Ergs from $from to $to: not enough Ergs")
    val destBox = tx.outBox(ergAmt, to)
    val changeBox = if (ergChange > 0) Some(tx.outBox(ergChange, from.propSpec))
    else None
    (destBox, changeBox)
  }

  def transferTokenWithChange(tx: TransactionCandidate, from: OutBox, to: PropositionSpec, tokenAmt: Token): (OutBox, Option[OutBox]) = {
    val tokenChange = from.token(tokenAmt.id).value - tokenAmt.value
    if (tokenChange < 0)
      error(s"Cannot transfer $tokenAmt from $from to $to: not enough amount of token")

    val ergChange = from.value - MinErgValue
    if (ergChange < MinErgValue)
      error(s"Cannot transfer $tokenAmt from $from to $to: not enough amount of Erg for two boxes")

    val destBox = tx.outBox(MinErgValue, to)
        .withTokens(tokenAmt)
    val changeBox =
      if (ergChange > 0) {
        val box = tx.outBox(ergChange, from.propSpec)
        Some(box)
      }
      else None
    (destBox, changeBox)
  }

}
