package org.ergoplatform.sdk

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.sdk.JavaHelpers.collRType
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import sigma.Colls
import sigma.ast.{ErgoTree, SType}
import sigma.data.{RType, SigmaConstants}
import sigma.ast.{Constant, EvaluatedValue}

import scala.collection.mutable.ArrayBuffer

class OutBoxBuilder(val _txB: UnsignedTransactionBuilder) {
  private val _ctx = _txB.ctx
  private var _value: Long = 0
  private var _contract: ErgoTree = _
  private val _tokens = ArrayBuffer.empty[ErgoToken]
  private val _registers = ArrayBuffer.empty[Constant[_]]
  private var _creationHeightOpt: Option[Int] = None

  def value(value: Long): this.type = {
    _value = value
    this
  }

  def contract(contract: ErgoTree): this.type = {
    _contract = contract
    this
  }

  def tokens(tokens: ErgoToken*): this.type = {
    require(tokens.nonEmpty, "At least one token should be specified")
    val maxTokens = SigmaConstants.MaxTokens.value
    require(tokens.size <= maxTokens, SigmaConstants.MaxTokens.description + s": $maxTokens")
    _tokens ++= tokens
    this
  }

  def registers(registers: Constant[_]*): this.type = {
    require(registers.nonEmpty, "At least one register should be specified")
    _registers.clear()
    _registers ++= registers
    this
  }

  def creationHeight(height: Int): OutBoxBuilder = {
    _creationHeightOpt = Some(height)
    this
  }

  def build(): OutBox = {
    require(_contract != null, "Contract is not defined")
    val ergoBoxCandidate = OutBoxBuilder.createBoxCandidate(
      _value, _contract, _tokens.toSeq, _registers.toSeq,
      creationHeight = _creationHeightOpt.getOrElse(_txB.ctx.height))
    OutBox(ergoBoxCandidate)
  }
}

object OutBoxBuilder {
  def apply(txB: UnsignedTransactionBuilder): OutBoxBuilder = new OutBoxBuilder(txB)

  private[sdk] def createBoxCandidate(
      value: Long, tree: ErgoTree,
      tokens: Seq[ErgoToken],
      registers: Seq[Constant[_]], creationHeight: Int): ErgoBoxCandidate = {
    import org.ergoplatform.ErgoBox.nonMandatoryRegisters
    val nRegs = registers.length
    require(nRegs <= nonMandatoryRegisters.length,
      s"Too many additional registers $nRegs. Max allowed ${nonMandatoryRegisters.length}")
    implicit val TokenIdRType: RType[TokenId] = collRType(sigma.ByteType).asInstanceOf[RType[TokenId]]
    val ts = Colls.fromItems(tokens.map(SdkIsos.isoErgoTokenToPair.to(_)): _*)
    val rs = registers.zipWithIndex.map { case (c, i) =>
      val id = ErgoBox.nonMandatoryRegisters(i)
      id -> c.asInstanceOf[EvaluatedValue[_ <: SType]]
    }.toMap
    new ErgoBoxCandidate(value, tree, creationHeight, ts, rs)
  }
}

