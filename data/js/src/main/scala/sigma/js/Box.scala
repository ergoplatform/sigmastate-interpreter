package sigma.js

import org.ergoplatform.ErgoBox
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.data.Iso
import sigma.data.js.Isos.{isoAmount, isoNonMandatoryRegisters, isoTokenArray}
import sigma.serialization.ErgoTreeSerializer
import sigmastate.fleetSdkCommon.distEsmTypesBoxesMod.{Box => FBox}
import sigmastate.fleetSdkCommon.distEsmTypesCommonMod.Amount
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.{distEsmTypesCommonMod => commonMod}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigma.Box]] available from JS. */
@JSExportTopLevel("Box")
class Box(val box: FBox[Amount, NonMandatoryRegisters]) extends js.Object

object Box extends js.Object {
  /** Represents a box in Fleet SDK. */
  type FleetBox = FBox[commonMod.Amount, NonMandatoryRegisters]

  /** Converts Fleet box to ErgoBox and back. */
  val isoBox: Iso[FleetBox, ErgoBox] = new Iso[FleetBox, ErgoBox] {
    override def to(x: FleetBox): ErgoBox = {
      val ergoBox = new ErgoBox(
        value = isoAmount.to(x.value),
        ergoTree = {
          val bytes = Base16.decode(x.ergoTree).get
          ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        },
        creationHeight = x.creationHeight.toInt,
        additionalTokens = isoTokenArray.to(x.assets),
        additionalRegisters = isoNonMandatoryRegisters.to(x.additionalRegisters),
        transactionId = ModifierId @@ x.transactionId,
        index = x.index.toShort
      )
      ergoBox
    }

    override def from(x: ErgoBox): FleetBox = {
      val ergoTree = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(x.ergoTree)
      val ergoTreeStr = Base16.encode(ergoTree)
      val assets = isoTokenArray.from(x.additionalTokens)
      FBox[commonMod.Amount, NonMandatoryRegisters](
        boxId = Base16.encode(x.id),
        ergoTree = ergoTreeStr,
        value = isoAmount.from(x.value),
        assets = assets,
        creationHeight = x.creationHeight,
        additionalRegisters = isoNonMandatoryRegisters.from(x.additionalRegisters),
        transactionId = x.transactionId,
        index = x.index
      )
    }
  }


}