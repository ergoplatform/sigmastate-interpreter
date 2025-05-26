package org.ergoplatform.sdk

import org.ergoplatform.ErgoBoxAssets

/**
  * Containter for box selector output
  *
  * @param inputBoxes         - transaction inputs chosen by a selector
  * @param changeBoxes        - change outputs
  * @param payToReemissionBox - pay-to-reemission output mde according to EIP-27, if needed
  */
class BoxSelectionResult[T <: ErgoBoxAssets](
    val inputBoxes: Seq[T],
    val changeBoxes: Seq[ErgoBoxAssets],
    val payToReemissionBox: Option[ErgoBoxAssets])
