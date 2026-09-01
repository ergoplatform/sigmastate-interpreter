/**
 * Token Sale Contract
 * 
 * Secure implementation of a token sale with proper validation.
 * 
 * Registers:
 * R4: (Coll[Byte], Long) - (TokenId, Price per token in nanoERG)
 * R5: GroupElement - Seller's public key
 * R6: Long - Minimum purchase amount
 * R7: Long - Maximum purchase amount
 * 
 * Actions:
 * 1. Buy - Purchase tokens at fixed price
 * 2. Cancel - Seller cancels sale and retrieves tokens
 */

{
  val tokenInfo = SELF.R4[(Coll[Byte], Long)].get
  val tokenId = tokenInfo._1
  val pricePerToken = tokenInfo._2
  
  val sellerPubKey = SELF.R5[GroupElement].get
  val minPurchase = SELF.R6[Long].get
  val maxPurchase = SELF.R7[Long].get
  
  // BUY ACTION
  val isBuy = {
    // Get buyer's payment
    val payment = OUTPUTS(0).value
    
    // Calculate tokens to send (with overflow protection)
    val tokenAmountBig = payment.toBigInt / pricePerToken.toBigInt
    require(tokenAmountBig <= Long.MaxValue.toBigInt, "Token amount overflow")
    val tokenAmount = tokenAmountBig.toLong
    
    // Validate purchase limits
    val withinLimits = tokenAmount >= minPurchase && tokenAmount <= maxPurchase
    
    // Validate buyer receives correct tokens
    val buyerOutput = OUTPUTS(1)
    val correctTokens = 
      buyerOutput.tokens.size == 1 &&
      buyerOutput.tokens(0)._1 == tokenId &&
      buyerOutput.tokens(0)._2 == tokenAmount
    
    // Validate seller receives payment
    val sellerOutput = OUTPUTS(0)
    val sellerGetsPayment = 
      sellerOutput.value == payment &&
      sellerOutput.propositionBytes == sellerPubKey.getEncoded
    
    // Validate remaining tokens stay in contract
    val remainingTokens = SELF.tokens(0)._2 - tokenAmount
    val contractContinues = if (remainingTokens > 0) {
      val continuationBox = OUTPUTS(2)
      continuationBox.propositionBytes == SELF.propositionBytes &&
      continuationBox.tokens(0)._1 == tokenId &&
      continuationBox.tokens(0)._2 == remainingTokens &&
      continuationBox.R4[(Coll[Byte], Long)].get == tokenInfo &&
      continuationBox.R5[GroupElement].get == sellerPubKey
    } else {
      true  // All tokens sold
    }
    
    withinLimits &&
    correctTokens &&
    sellerGetsPayment &&
    contractContinues
  }
  
  // CANCEL ACTION
  val isCancel = {
    // Only seller can cancel
    val sellerSigned = proveDlog(sellerPubKey)
    
    // Seller gets all tokens back
    val sellerGetsTokens = 
      OUTPUTS(0).tokens(0)._1 == tokenId &&
      OUTPUTS(0).tokens(0)._2 == SELF.tokens(0)._2 &&
      OUTPUTS(0).propositionBytes == sellerPubKey.getEncoded
    
    sellerSigned && sellerGetsTokens
  }
  
  sigmaProp(isBuy || isCancel)
}
