package org.ergoplatform

import scorex.util.ModifierId

trait ErgoBoxAssets {
  def value: Long
  def tokens: Map[ModifierId, Long]
}

final case class ErgoBoxAssetsHolder(
  val value: Long,
  val tokens: Map[ModifierId, Long]
) extends ErgoBoxAssets

object ErgoBoxAssetsHolder {
  def apply(value: Long): ErgoBoxAssetsHolder = ErgoBoxAssetsHolder(value, Map())
}