package sigmastate.utxo.examples

import org.ergoplatform.dsl.ContractSyntax.Token
import sigma.Context
import org.ergoplatform.ErgoBox.{R4, TokenId}
import sigma.Coll
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, StdContracts}

/** Contract specification for assets atomic exchange transactions.
  * @param deadline    block header after which the transfer can be cancelled.
  * @param tokenId     id of the token to exchange
  * @param tokenBuyer  The party, who wants to buy some amount of token with id == `tokenId`.
  * @param tokenSeller The party, who wants to sell some amount of token with id == `tokenId`.
  * */
case class AssetsAtomicExchange[Spec <: ContractSpec]
    (val deadline: Int, val tokenId: TokenId,
     val tokenBuyer: Spec#ProvingParty,
     val tokenSeller: Spec#ProvingParty)
    (implicit val spec: Spec)
    extends SigmaContractSyntax with StdContracts
{
  def pkA = tokenBuyer.pubKey
  def pkB = tokenSeller.pubKey

  lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "deadline" -> deadline, "tokenId" -> tokenId)

  lazy val buyerProp = proposition("buyer", { ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkA) || sigmaProp({
      val tokenData = OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)
      val knownId = OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
      allOf(Coll(
        tokenData._1 == tokenId,
        tokenData._2 >= 60L,
        OUTPUTS(0).propositionBytes == pkA.propBytes,
        knownId
      ))
    })
  },
  """{
   |  (HEIGHT > deadline && pkA) || {
   |    val tokenData = OUTPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)
   |    allOf(Coll(
   |      tokenData._1 == tokenId,
   |      tokenData._2 >= 60L,
   |      OUTPUTS(0).propositionBytes == pkA.propBytes,
   |      OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
   |    ))
   |  }
   |}
  """.stripMargin)

  lazy val sellerProp = proposition("seller", {ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkB) || sigmaProp({
      val knownBoxId = OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
      allOf(Coll(
        OUTPUTS(1).value >= 100L,
        knownBoxId,
        OUTPUTS(1).propositionBytes == pkB.propBytes
      ))
    })
  },
  """{
   |  (HEIGHT > deadline && pkB) ||
   |    allOf(Coll(
   |      OUTPUTS(1).value >= 100L,
   |      OUTPUTS(1).propositionBytes == pkB.propBytes,
   |      OUTPUTS(1).R4[Coll[Byte]].get == SELF.id
   |    ))
   |}
  """.stripMargin)

  lazy val buyerSignature  = proposition("buyerSignature", _ => pkA, "pkA")
  lazy val sellerSignature = proposition("sellerSignature", _ => pkB, "pkB")

  import spec._

  /** This methods starts exchange protocol using two boxes.
    * It requires that the boxes are protected by `buyerSignature` and `sellerSignature` correspondingly.
    * It creates a transaction in the target block with two holder boxes and two change boxes.
    * @return a pair of holder boxes
    */
  def startExchange(targetBlock: BlockCandidate, buyerErgBox: OutBox, sellerTokenBox: OutBox, ergAmt: Long, tokenAmt: Token): (OutBox, OutBox) = {
    require(buyerErgBox.propSpec == buyerSignature && sellerTokenBox.propSpec == sellerSignature)

    val tx = targetBlock.newTransaction().spending(buyerErgBox, sellerTokenBox)
    val (buyerHolder, _) = transferErgWithChange(tx, buyerErgBox, buyerProp, ergAmt)
    val (sellerHolder, _) = transferTokenWithChange(tx, sellerTokenBox, sellerProp, tokenAmt)
    (buyerHolder, sellerHolder)
  }

  /** Having two boxes prepared for token exchange, this method creates spending transaction which
    * completes the exchange protocol.
    * @param  targetBlock   block in which the spending transaction will be created
    * @param  buyerHolder   holder box with buyer's Ergs
    * @param  sellerHolder  holder box with seller's tokens
    * @return               a pair of boxes with buyers's tokens and seller's Ergs
    * */
  def finishExchange(targetBlock: BlockCandidate, buyerHolder: OutBox, sellerHolder: OutBox): (OutBox, OutBox) = {
    require(buyerHolder.propSpec == buyerProp && sellerHolder.propSpec == sellerProp)
    val spendingTx = targetBlock.newTransaction().spending(buyerHolder, sellerHolder)
    val buyerTokens = spendingTx
        .outBox(sellerHolder.value, buyerSignature)
        .withTokens(Token(tokenId, sellerHolder.token(tokenId).value))
        .withRegs(R4 -> buyerHolder.id)
    val sellerErgs = spendingTx
        .outBox(buyerHolder.value, sellerSignature)
        .withRegs(R4 -> sellerHolder.id)
    (buyerTokens, sellerErgs)
  }
}
