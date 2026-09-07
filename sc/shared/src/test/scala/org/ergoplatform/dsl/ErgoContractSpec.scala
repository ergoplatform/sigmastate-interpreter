package org.ergoplatform.dsl

import sigma.Coll
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition, Token}
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import sigma.compiler.ir.IRContext
import sigma.interpreter.CostedProverResult

class ErgoContractSpec(implicit val IR: IRContext) extends ContractSpec {

  case class ErgoOutBox(tx: TransactionCandidate, boxIndex: Int, value: Long, propSpec: PropositionSpec)
    extends OutBox {
    override def id: BoxId = ???

    override def withTokens(tokens: Token*): OutBox = ???

    override def withRegs(regs: (NonMandatoryRegisterId, Any)*): OutBox = ???

    override def token(id: TokenId): Token = ???

    override private[dsl] def ergoBox = ???
  }

  trait TransactionContext {
    def block: BlockCandidate
    def attachProof(proofs: (InputBox, CostedProverResult)*): Unit
    def submit(): Unit
  }

  def getBlock(height: Int): ChainBlock = ???
  def getBoxesByParty(party: ProtocolParty): Seq[OutBox] = ???
  def getBoxById(id: Coll[Byte]): OutBox = {
//    new ErgoOutBox()
    ???
  }
  def newTransactionContext: TransactionContext = ???

  override private[dsl] def mkPropositionSpec(name: String,
      dslSpec: Proposition,
      scriptSpec: ErgoScript) = ???

  override protected def mkProvingParty(name: String): ProvingParty = ???

  override protected def mkVerifyingParty(name: String): VerifyingParty = ???
}
