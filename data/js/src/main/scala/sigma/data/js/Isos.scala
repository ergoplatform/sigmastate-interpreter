package sigma.data.js

import org.ergoplatform.ErgoBox._
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import sigma.Extensions._
import sigma.ast.syntax.GroupElementConstant
import sigma.ast.{Constant, GroupElementConstant, SType}
import sigma.data.Iso.isoStringToArray
import sigma.data.{CGroupElement, Digest32Coll, Iso}
import sigma.js.GroupElement
import sigma.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigma.{Coll, Colls}
import sigmastate.fleetSdkCommon.distEsmTypesCommonMod.HexString
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.{distEsmTypesBoxesMod => boxesMod, distEsmTypesCommonMod => commonMod, distEsmTypesRegistersMod => registersMod, distEsmTypesTokenMod => tokenMod}

import scala.scalajs.js

/** Definitions of isomorphisms for sigma-data module.
  * @see sigma.data.Iso
  */
object Isos {

  val isoStringToGroupElement: Iso[String, sigma.GroupElement] = new Iso[String, sigma.GroupElement] {
    override def to(x: String): sigma.GroupElement = {
      val bytes = Base16.decode(x).get
      ValueSerializer.deserialize(bytes).asInstanceOf[GroupElementConstant].value
    }
    override def from(x: sigma.GroupElement): String = {
      val bytes = ValueSerializer.serialize(GroupElementConstant(x))
      Base16.encode(bytes)
    }
  }

  val isoGroupElement: Iso[GroupElement, sigma.GroupElement] = new Iso[GroupElement, sigma.GroupElement] {
    override def to(x: GroupElement): sigma.GroupElement = {
      CGroupElement(x.point)
    }
    override def from(x: sigma.GroupElement): GroupElement = {
      new GroupElement(x.asInstanceOf[CGroupElement].wrappedValue)
    }
  }

  implicit val isoBoxId: Iso[boxesMod.BoxId, ErgoBox.BoxId] = new Iso[boxesMod.BoxId, ErgoBox.BoxId] {
    override def to(x: boxesMod.BoxId): ErgoBox.BoxId = ADKey @@@ isoStringToArray.to(x)

    override def from(x: ErgoBox.BoxId): boxesMod.BoxId = isoStringToArray.from(x)
  }

  implicit val isoHexStringToConstant: Iso[HexString, Constant[SType]] = new Iso[HexString, Constant[SType]] {
    override def to(x: HexString): Constant[SType] = {
      val bytes = isoStringToArray.to(x)
      val value = ValueSerializer.deserialize(bytes)
      value.asInstanceOf[Constant[SType]]
    }
    override def from(x: Constant[SType]): HexString = {
      val bytes = ValueSerializer.serialize(x)
      isoStringToArray.from(bytes)
    }
  }


  implicit val isoAmount: Iso[commonMod.Amount, Long] = new Iso[commonMod.Amount, Long] {
    override def to(x: commonMod.Amount): Long = x.asInstanceOf[Any] match {
      case s: String => BigInt(s).toLong
      case _ => java.lang.Long.parseLong(x.asInstanceOf[js.BigInt].toString(10))
    }
    override def from(x: Long): commonMod.Amount = x.toString
  }

  implicit val isoToken: Iso[tokenMod.TokenAmount[commonMod.Amount], Token] =
    new Iso[tokenMod.TokenAmount[commonMod.Amount], Token] {
      override def to(x: tokenMod.TokenAmount[commonMod.Amount]): Token =
        (Digest32Coll @@@ Colls.fromArray(Base16.decode(x.tokenId).get), isoAmount.to(x.amount))

      override def from(x: Token): tokenMod.TokenAmount[commonMod.Amount] =
        tokenMod.TokenAmount[commonMod.Amount](isoAmount.from(x._2), x._1.toHex)
    }

  val isoTokenArray: Iso[js.Array[tokenMod.TokenAmount[commonMod.Amount]], Coll[Token]] =
    new Iso[js.Array[tokenMod.TokenAmount[commonMod.Amount]], Coll[Token]] {
      override def to(x: js.Array[tokenMod.TokenAmount[commonMod.Amount]]): Coll[Token] = {
        sigma.js.Isos.isoArrayToColl(isoToken).to(x)
      }
      override def from(x: Coll[Token]): js.Array[tokenMod.TokenAmount[commonMod.Amount]] = {
        sigma.js.Isos.isoArrayToColl(isoToken).from(x)
      }
    }

  val isoNonMandatoryRegisters: Iso[registersMod.NonMandatoryRegisters, AdditionalRegisters] =
    new Iso[registersMod.NonMandatoryRegisters, AdditionalRegisters] {
      override def to(x: registersMod.NonMandatoryRegisters): AdditionalRegisters = {
        val regs = Seq(
          x.R4 -> R4,
          x.R5 -> R5,
          x.R6 -> R6,
          x.R7 -> R7,
          x.R8 -> R8,
          x.R9 -> R9
        ).collect {
          case (regOpt, id) if regOpt.isDefined => id -> isoHexStringToConstant.to(regOpt.get)
        }
        Map(regs:_*)
      }
      override def from(regs: AdditionalRegisters): registersMod.NonMandatoryRegisters = {
        def regHexOpt(t: NonMandatoryRegisterId): Option[HexString] =
          regs.get(t).map(v => isoHexStringToConstant.from(v.asInstanceOf[Constant[SType]]))

        val resRegs = NonMandatoryRegisters()
        regHexOpt(R4).foreach(resRegs.setR4(_))
        regHexOpt(R5).foreach(resRegs.setR5(_))
        regHexOpt(R6).foreach(resRegs.setR6(_))
        regHexOpt(R7).foreach(resRegs.setR7(_))
        regHexOpt(R8).foreach(resRegs.setR8(_))
        regHexOpt(R9).foreach(resRegs.setR9(_))
        resRegs
      }
    }

  implicit val isoBoxCandidate: Iso[boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters], ErgoBoxCandidate] = new Iso[boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters], ErgoBoxCandidate] {
    override def to(x: boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters]): ErgoBoxCandidate = {
      val ergoBoxCandidate = new ErgoBoxCandidate(
        value = isoAmount.to(x.value),
        ergoTree = {
          val bytes = Base16.decode(x.ergoTree).get
          ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
        },
        x.creationHeight.toInt,
        additionalTokens = isoTokenArray.to(x.assets),
        additionalRegisters = isoNonMandatoryRegisters.to(x.additionalRegisters)
      )
      ergoBoxCandidate
    }

    override def from(x: ErgoBoxCandidate): boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters] = {
      val ergoTree = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(x.ergoTree)
      val ergoTreeStr = Base16.encode(ergoTree)
      val assets = isoTokenArray.from(x.additionalTokens)
      boxesMod.BoxCandidate[commonMod.Amount, NonMandatoryRegisters](
        ergoTree = ergoTreeStr,
        value = isoAmount.from(x.value),
        assets = assets,
        creationHeight = x.creationHeight,
        additionalRegisters = isoNonMandatoryRegisters.from(x.additionalRegisters)
      )
    }
  }


}
