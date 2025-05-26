package org.ergoplatform.sdk.js

import org.ergoplatform._
import org.ergoplatform.sdk.ExtendedInputBox
import org.ergoplatform.sdk.wallet.protocol.context
import sigma.ast.{Constant, SType}
import sigma.data.Iso
import sigma.data.Iso.{isoStringToArray, isoStringToColl}
import sigma.data.js.{Isos => DataIsos}
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.js.AvlTree
import sigmastate.eval.{CHeader, CPreHeader}
import sigmastate.fleetSdkCommon.distEsmTypesBoxesMod.Box
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.distEsmTypesTokenMod.TokenAmount
import sigmastate.fleetSdkCommon.distEsmTypesTransactionsMod.{SignedTransaction, UnsignedTransaction}
import sigmastate.fleetSdkCommon.{distEsmTypesCommonMod => commonMod, distEsmTypesContextExtensionMod => contextExtensionMod, distEsmTypesInputsMod => inputsMod, distEsmTypesProverResultMod => proverResultMod}

import scala.collection.immutable.ListMap
import scala.scalajs.js
import scala.scalajs.js.Object

/** Definitions of isomorphisms for sigma-sdk module.
  * @see sigma.data.Iso
  */
object Isos {
  implicit val isoHeader: Iso[Header, sigma.Header] = new Iso[Header, sigma.Header] {
    override def to(a: Header): sigma.Header = {
      CHeader(
        id = isoStringToColl.to(a.id),
        version = a.version,
        parentId = isoStringToColl.to(a.parentId),
        ADProofsRoot = isoStringToColl.to(a.ADProofsRoot),
        stateRoot = AvlTree.isoAvlTree.to(a.stateRoot),
        transactionsRoot = isoStringToColl.to(a.transactionsRoot),
        timestamp = sigma.js.Isos.isoBigIntToLong.to(a.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.to(a.nBits),
        height = a.height,
        extensionRoot = isoStringToColl.to(a.extensionRoot),
        minerPk = DataIsos.isoGroupElement.to(a.minerPk),
        powOnetimePk = DataIsos.isoGroupElement.to(a.powOnetimePk),
        powNonce = isoStringToColl.to(a.powNonce),
        powDistance = sigma.js.Isos.isoBigInt.to(a.powDistance),
        votes = isoStringToColl.to(a.votes)
      )
    }
    override def from(b: sigma.Header): Header = {
      val header = b.asInstanceOf[CHeader]
      new Header(
        id = isoStringToColl.from(header.id),
        version = header.version,
        parentId = isoStringToColl.from(header.parentId),
        ADProofsRoot = isoStringToColl.from(header.ADProofsRoot),
        stateRoot = AvlTree.isoAvlTree.from(header.stateRoot),
        transactionsRoot = isoStringToColl.from(header.transactionsRoot),
        timestamp = sigma.js.Isos.isoBigIntToLong.from(header.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.from(header.nBits),
        height = header.height,
        extensionRoot = isoStringToColl.from(header.extensionRoot),
        minerPk = DataIsos.isoGroupElement.from(header.minerPk),
        powOnetimePk = DataIsos.isoGroupElement.from(header.powOnetimePk),
        powNonce = isoStringToColl.from(header.powNonce),
        powDistance = sigma.js.Isos.isoBigInt.from(header.powDistance),
        votes = isoStringToColl.from(header.votes)
      )
    }
  }

  implicit val isoPreHeader: Iso[PreHeader, sigma.PreHeader] = new Iso[PreHeader, sigma.PreHeader] {
    override def to(a: PreHeader): sigma.PreHeader = {
      CPreHeader(
        version = a.version,
        parentId = isoStringToColl.to(a.parentId),
        timestamp = sigma.js.Isos.isoBigIntToLong.to(a.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.to(a.nBits),
        height = a.height,
        minerPk = DataIsos.isoGroupElement.to(a.minerPk),
        votes = isoStringToColl.to(a.votes)
      )
    }
    override def from(b: sigma.PreHeader): PreHeader = {
      val header = b.asInstanceOf[CPreHeader]
      new PreHeader(
        version = header.version,
        parentId = isoStringToColl.from(header.parentId),
        timestamp = sigma.js.Isos.isoBigIntToLong.from(header.timestamp),
        nBits = sigma.js.Isos.isoBigIntToLong.from(header.nBits),
        height = header.height,
        minerPk = DataIsos.isoGroupElement.from(header.minerPk),
        votes = isoStringToColl.from(header.votes)
      )
    }
  }

  val isoBlockchainParameters: Iso[BlockchainParameters, sdk.BlockchainParameters] = new Iso[BlockchainParameters, sdk.BlockchainParameters] {
    override def to(a: BlockchainParameters): sdk.BlockchainParameters = {
      sdk.CBlockchainParameters(
        storageFeeFactor = a.storageFeeFactor,
        minValuePerByte = a.minValuePerByte,
        maxBlockSize = a.maxBlockSize,
        tokenAccessCost = a.tokenAccessCost,
        inputCost = a.inputCost,
        dataInputCost = a.dataInputCost,
        outputCost = a.outputCost,
        maxBlockCost = a.maxBlockCost,
        softForkStartingHeight = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).to(a.softForkStartingHeight),
        softForkVotesCollected = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).to(a.softForkVotesCollected),
        blockVersion = a.blockVersion
      )
    }
    override def from(b: sdk.BlockchainParameters): BlockchainParameters = {
      new BlockchainParameters(
        storageFeeFactor = b.storageFeeFactor,
        minValuePerByte = b.minValuePerByte,
        maxBlockSize = b.maxBlockSize,
        tokenAccessCost = b.tokenAccessCost,
        inputCost = b.inputCost,
        dataInputCost = b.dataInputCost,
        outputCost = b.outputCost,
        maxBlockCost = b.maxBlockCost,
        softForkStartingHeight = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).from(b.softForkStartingHeight),
        softForkVotesCollected = sigma.js.Isos.isoUndefOr[Int, Int](Iso.identityIso).from(b.softForkVotesCollected),
        blockVersion = b.blockVersion
      )
    }
  }

  implicit val isoBlockchainStateContext: Iso[BlockchainStateContext, context.BlockchainStateContext] = new Iso[BlockchainStateContext, context.BlockchainStateContext] {
    override def to(a: BlockchainStateContext): context.BlockchainStateContext = {
      context.CBlockchainStateContext(
        sigmaLastHeaders = sigma.js.Isos.isoArrayToColl(isoHeader).to(a.sigmaLastHeaders),
        previousStateDigest = isoStringToColl.to(a.previousStateDigest),
        sigmaPreHeader = isoPreHeader.to(a.sigmaPreHeader)
      )
    }

    override def from(b: context.BlockchainStateContext): BlockchainStateContext = {
      new BlockchainStateContext(
        sigmaLastHeaders = sigma.js.Isos.isoArrayToColl(isoHeader).from(b.sigmaLastHeaders),
        previousStateDigest = isoStringToColl.from(b.previousStateDigest),
        sigmaPreHeader = isoPreHeader.from(b.sigmaPreHeader)
      )
    }
  }

  implicit val isoContextExtension: Iso[contextExtensionMod.ContextExtension, ContextExtension] = new Iso[contextExtensionMod.ContextExtension, ContextExtension] {
    override def to(x: contextExtensionMod.ContextExtension): ContextExtension = {
      var map = new ListMap[Byte, Constant[SType]]()
      val keys = js.Object.keys(x).sorted
      for ( k <- keys ) {
        val id = k.toInt.toByte
        val c = DataIsos.isoHexStringToConstant.to(x.apply(id).get.get)
        map = map + (id -> c)
      }
      ContextExtension(map)
    }

    override def from(x: ContextExtension): contextExtensionMod.ContextExtension = {
      val res = new Object().asInstanceOf[contextExtensionMod.ContextExtension]
      x.values.foreach { case (k, v: Constant[_]) =>
        val hex = DataIsos.isoHexStringToConstant.from(v)
        res.update(k, hex)
      }
      res
    }
  }

  implicit val isoUnsignedInput: Iso[inputsMod.UnsignedInput, UnsignedInput] = new Iso[inputsMod.UnsignedInput, UnsignedInput] {
    override def to(x: inputsMod.UnsignedInput): UnsignedInput =
      new UnsignedInput(DataIsos.isoBoxId.to(x.boxId), isoContextExtension.to(x.extension))

    override def from(x: UnsignedInput): inputsMod.UnsignedInput =
      inputsMod.UnsignedInput(DataIsos.isoBoxId.from(x.boxId), isoContextExtension.from(x.extension))
  }

  implicit val isoProverResult: Iso[proverResultMod.ProverResult, ProverResult] = new Iso[proverResultMod.ProverResult, ProverResult] {
    override def to(a: proverResultMod.ProverResult): ProverResult = {
      ProverResult(
        proof = isoStringToArray.to(a.proofBytes),
        extension = isoContextExtension.to(a.extension)
      )
    }
    override def from(b: ProverResult): proverResultMod.ProverResult = {
      proverResultMod.ProverResult(
        isoContextExtension.from(b.extension),
        isoStringToArray.from(b.proof)
      )
    }
  }

  implicit val isoSignedInput: Iso[inputsMod.SignedInput, Input] = new Iso[inputsMod.SignedInput, Input] {
    override def to(x: inputsMod.SignedInput): Input =
      Input(DataIsos.isoBoxId.to(x.boxId), isoProverResult.to(x.spendingProof))
    override def from(x: Input): inputsMod.SignedInput =
      inputsMod.SignedInput(DataIsos.isoBoxId.from(x.boxId), isoProverResult.from(x.spendingProof))
  }

  implicit val isoDataInput: Iso[inputsMod.DataInput, DataInput] = new Iso[inputsMod.DataInput, DataInput] {
    override def to(x: inputsMod.DataInput): DataInput =
      DataInput(DataIsos.isoBoxId.to(x.boxId))

    override def from(x: DataInput): inputsMod.DataInput =
      inputsMod.DataInput(DataIsos.isoBoxId.from(x.boxId))
  }

  val isoEIP12UnsignedInput: Iso[inputsMod.EIP12UnsignedInput, ExtendedInputBox] =
    new Iso[inputsMod.EIP12UnsignedInput, ExtendedInputBox] {
      override def to(x: inputsMod.EIP12UnsignedInput): ExtendedInputBox = {
        val box = Box[commonMod.Amount, NonMandatoryRegisters](
          boxId = x.boxId,
          ergoTree = x.ergoTree,
          value = x.value,
          assets = x.assets.asInstanceOf[js.Array[TokenAmount[commonMod.Amount]]],
          creationHeight = x.creationHeight,
          additionalRegisters = x.additionalRegisters,
          transactionId = x.transactionId,
          index = x.index
        )
        val ergoBox = sigma.js.Box.isoBox.to(box)
        val extendedInputBox = ExtendedInputBox(ergoBox, isoContextExtension.to(x.extension))
        extendedInputBox
      }
      override def from(x: ExtendedInputBox): inputsMod.EIP12UnsignedInput = {
        val box = sigma.js.Box.isoBox.from(x.box)
        val ext = isoContextExtension.from(x.extension)
        inputsMod.EIP12UnsignedInput(
          boxId = box.boxId,
          ergoTree = box.ergoTree,
          value = box.value.toString,
          assets = box.assets.asInstanceOf[js.Array[TokenAmount[String]]],
          creationHeight = box.creationHeight,
          additionalRegisters = box.additionalRegisters,
          transactionId = box.transactionId,
          index = box.index,
          extension = ext
        )
      }
    }

  val isoUnsignedTransaction: Iso[UnsignedTransaction, UnsignedErgoLikeTransaction] =
    new Iso[UnsignedTransaction, UnsignedErgoLikeTransaction] {
      override def to(a: UnsignedTransaction): UnsignedErgoLikeTransaction = {
        new UnsignedErgoLikeTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoUnsignedInput).to(a.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).to(a.dataInputs),
          outputCandidates = sigma.js.Isos.isoArrayToIndexed(DataIsos.isoBoxCandidate).to(a.outputs),
        )
      }

      override def from(b: UnsignedErgoLikeTransaction): UnsignedTransaction = {
        UnsignedTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoUnsignedInput).from(b.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).from(b.dataInputs),
          outputs = sigma.js.Isos.isoArrayToIndexed(DataIsos.isoBoxCandidate).from(b.outputCandidates)
        )
      }
    }

  val isoSignedTransaction: Iso[SignedTransaction, ErgoLikeTransaction] =
    new Iso[SignedTransaction, ErgoLikeTransaction] {
      override def to(a: SignedTransaction): ErgoLikeTransaction = {
        new ErgoLikeTransaction(
          inputs = sigma.js.Isos.isoArrayToIndexed(isoSignedInput).to(a.inputs),
          dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).to(a.dataInputs),
          outputCandidates = sigma.js.Isos.isoArrayToIndexed(sigma.js.Box.isoBox).to(a.outputs),
        )
      }

      override def from(tx: ErgoLikeTransaction): SignedTransaction = {
        val inputs = sigma.js.Isos.isoArrayToIndexed(isoSignedInput).from(tx.inputs)
        val dataInputs = sigma.js.Isos.isoArrayToIndexed(isoDataInput).from(tx.dataInputs)
        val outputs = sigma.js.Isos.isoArrayToIndexed(sigma.js.Box.isoBox).from(tx.outputs)
        SignedTransaction(dataInputs, tx.id, inputs, outputs)
      }
    }

}