/**
 * Token Sale Contract Example
 * 
 * Demonstrates token sales with:
 * - Fixed price per token
 * - Buyer receives tokens
 * - Seller receives payment
 */

{
  val tokenId = SELF.R4[Coll[Byte]].get
  val pricePerToken = SELF.R5[Long].get
  val seller = SELF.R6[GroupElement].get
  
  // Calculate tokens to send based on payment
  val payment = OUTPUTS(0).value
  val tokenAmount = payment / pricePerToken
  
  // Buyer gets tokens
  val buyerGetsTokens = {
    val buyerOutput = OUTPUTS(1)
    buyerOutput.tokens.size == 1 &&
    buyerOutput.tokens(0)._1 == tokenId &&
    buyerOutput.tokens(0)._2 == tokenAmount
  }
  
  // Seller gets payment
  val sellerGetsPayment = {
    val sellerOutput = OUTPUTS(0)
    sellerOutput.value == payment &&
    sellerOutput.propositionBytes == seller.getEncoded
  }
  
  sigmaProp(buyerGetsTokens && sellerGetsPayment)
}
