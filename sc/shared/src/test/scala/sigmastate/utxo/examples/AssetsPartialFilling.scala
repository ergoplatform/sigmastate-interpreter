package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.dsl.ContractSyntax.Token
import sigma.Context
import sigma.Coll
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, StdContracts}

/**
  * @param tokenBuyer The party, who wants to buy some amount of token with id == `tokenId`.
  * @param tokenSeller The party, who wants to sell some amount of token with id == `tokenId`. */
case class AssetsPartialFilling[Spec <: ContractSpec]
    (deadline: Int, token1: TokenId,
      tokenBuyer: Spec#ProvingParty,
      tokenSeller: Spec#ProvingParty)
    (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
{
  def pkA = tokenBuyer.pubKey
  def pkB = tokenSeller.pubKey
  
  lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "deadline" -> deadline, "token1" -> token1)

  lazy val buyerProp = proposition("buyer", { ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkA) || sigmaProp({

      val outIdx = getVar[Short](127).get
      val out = OUTPUTS(outIdx)
      val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
      val tokenId = tokenData._1
      val tokenValue = tokenData._2
      val outValue = out.value
      val price = 500

      allOf(Coll(
        tokenId == token1,
        tokenValue >= 1,
        (SELF.value - outValue) <= tokenValue * price,
        out.propositionBytes == pkA.propBytes,
        out.R4[Coll[Byte]].get == SELF.id
      ))
    })
  },
  """(HEIGHT > deadline && pkA) || {
   |
   |  val outIdx = getVar[Short](127).get
   |  val out = OUTPUTS(outIdx)
   |  val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
   |  val tokenId = tokenData._1
   |  val tokenValue = tokenData._2
   |  val outValue = out.value
   |  val price = 500
   |
   |  allOf(Coll(
   |      tokenId == token1,
   |      tokenValue >= 1,
   |      (SELF.value - outValue) <= tokenValue * price,
   |      out.propositionBytes == pkA.propBytes,
   |      out.R4[Coll[Byte]].get == SELF.id
   |  ))
   |}
  """.stripMargin)

  lazy val sellerProp = proposition("seller", {ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkB) || sigmaProp({
      val outIdx = getVar[Short](127).get
      val out = OUTPUTS(outIdx)

      val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
      val tokenId = tokenData._1
      val tokenValue = tokenData._2

      val selfTokenData = SELF.R2[Coll[(Coll[Byte], Long)]].get(0)
      val selfTokenId = selfTokenData._1
      val selfTokenValue = selfTokenData._2

      val selfValue = SELF.value
      val outValue = out.value

      val sold = selfTokenValue - tokenValue

      val price = 495
      val outR4 = out.R4[Coll[Byte]].get

      allOf(Coll(
        sold >= 1,
        (outValue - selfValue) >= sold*price,
        outR4 == SELF.id,
        out.propositionBytes == pkB.propBytes
      ))
    })
  },
  """ (HEIGHT > deadline && pkB) || {
   |   val outIdx = getVar[Short](127).get
   |   val out = OUTPUTS(outIdx)
   |
   |   val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
   |   val tokenId = tokenData._1
   |   val tokenValue = tokenData._2
   |
   |   val selfTokenData = SELF.R2[Coll[(Coll[Byte], Long)]].get(0)
   |   val selfTokenId = selfTokenData._1
   |   val selfTokenValue = selfTokenData._2
   |
   |   val selfValue = SELF.value
   |   val outValue = out.value
   |
   |   val sold = selfTokenValue - tokenValue
   |
   |   val price = 495
   |
   |   allOf(Coll(
   |        sold >= 1,
   |        (outValue - selfValue) >= sold*price,
   |        out.R4[Coll[Byte]].get == SELF.id,
   |        out.propositionBytes == pkB.propBytes
   |   ))
   | }
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

}
