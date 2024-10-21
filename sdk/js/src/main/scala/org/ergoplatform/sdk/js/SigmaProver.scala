package org.ergoplatform.sdk.js

import org.ergoplatform.sdk
import sigmastate.fleetSdkCommon.distEsmTypesBoxesMod.Box
import sigmastate.fleetSdkCommon.distEsmTypesRegistersMod.NonMandatoryRegisters
import sigmastate.fleetSdkCommon.{distEsmTypesCommonMod => commonMod, distEsmTypesInputsMod => inputsMod, distEsmTypesTokenMod => tokenMod, distEsmTypesTransactionsMod => transactionsMod}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sdk.SigmaProver]] available from JS. */
@JSExportTopLevel("SigmaProver")
class SigmaProver(_prover: sdk.SigmaProver) extends js.Object {
  import Isos._

  /** Returns the Pay-to-Public-Key (P2PK) address associated with the prover's public key.
    * The returned address corresponds to the master secret derived from the mnemonic
    * phrase configured in the [[ProverBuilder]].
    */
  def getP2PKAddress(): String = {
    val addr = _prover.getP2PKAddress
    addr.toString
  }

  /** Returns the prover's secret key. */
  def getSecretKey(): js.BigInt =
    sigma.js.Isos.isoBigInt.from(_prover.getSecretKey)

  /** Returns an array of EIP-3 addresses associated with the prover's secret keys. */
  def getEip3Addresses(): js.Array[String] = {
    val addresses = _prover.getEip3Addresses
    js.Array(addresses.map(_.toString): _*)
  }

  /** Reduces the transaction to the reduced form, which is ready to be signed.
    * @param stateCtx blockchain state context
    * @param unsignedTx unsigned transaction to be reduced (created by Fleet builders)
    * @param boxesToSpend boxes to be spent by the transaction
    * @param dataInputs data inputs to be used by the transaction
    * @param tokensToBurn tokens to be burned by the transaction
    * @param baseCost base cost of the transaction
    * @return reduced transaction
    */
  def reduce(
      stateCtx: BlockchainStateContext,
      unsignedTx: transactionsMod.UnsignedTransaction,
      boxesToSpend: js.Array[inputsMod.EIP12UnsignedInput],
      dataInputs: js.Array[Box[commonMod.Amount, NonMandatoryRegisters]],
      tokensToBurn: js.Array[tokenMod.TokenAmount[commonMod.Amount]],
      baseCost: Int): ReducedTransaction = {
    val unreducedTx = sdk.UnreducedTransaction(
      unsignedTx = isoUnsignedTransaction.to(unsignedTx),
      boxesToSpend = sigma.js.Isos.isoArrayToIndexed(isoEIP12UnsignedInput).to(boxesToSpend),
      dataInputs = sigma.js.Isos.isoArrayToIndexed(sigma.js.Box.isoBox).to(dataInputs),
      tokensToBurn = sigma.js.Isos.isoArrayToIndexed(sigma.data.js.Isos.isoToken.andThen(sdk.SdkIsos.isoErgoTokenToPair.inverse)).to(tokensToBurn)
    )
    val ctx = isoBlockchainStateContext.to(stateCtx)
    val reducedTx = _prover.reduce(ctx, unreducedTx, baseCost)
    new ReducedTransaction(reducedTx)
  }

  /** Signs the reduced transaction.
    * @param reducedTx reduced transaction to be signed
    * @return signed transaction containting all the required proofs (signatures)
    */
  def signReduced(reducedTx: ReducedTransaction): transactionsMod.SignedTransaction = {
    val signed = _prover.signReduced(reducedTx._tx)
    isoSignedTransaction.from(signed.ergoTx)
  }
}
