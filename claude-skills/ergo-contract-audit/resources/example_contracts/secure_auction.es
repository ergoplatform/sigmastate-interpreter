/**
 * Secure Auction Contract
 * 
 * This contract implements a secure auction with proper access control,
 * input validation, and protection against common vulnerabilities.
 * 
 * Registers:
 * R4: GroupElement - Seller's public key
 * R5: Long - Minimum bid amount (in nanoERG)
 * R6: Int - Auction end height
 * R7: Option[GroupElement] - Current highest bidder (None initially)
 * 
 * Actions:
 * 1. Bid - Anyone can place a bid >= minBid before end height
 * 2. Close - Seller can close auction after end height
 */

{
  // Extract contract parameters
  val sellerPubKey = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  val endHeight = SELF.R6[Int].get
  val currentBidder = SELF.R7[Option[GroupElement]]
  
  // Validate auction is still active
  val auctionActive = HEIGHT < endHeight
  
  // BID ACTION
  val isBid = {
    // Validate bid amount
    val bidAmount = OUTPUTS(0).value
    val validBidAmount = bidAmount >= minBid
    
    // Ensure bid is higher than current (if exists)
    val higherThanCurrent = currentBidder.fold(true)({
      (prevBidder: GroupElement) => bidAmount > SELF.value
    })
    
    // Validate output preserves contract
    val correctScript = OUTPUTS(0).propositionBytes == SELF.propositionBytes
    
    // Update highest bidder in R7
    val bidderPubKey = getVar[GroupElement](0).get
    val updatedBidder = OUTPUTS(0).R7[GroupElement].get == bidderPubKey
    
    // Refund previous bidder if exists
    val correctRefund = currentBidder.fold(true)({
      (prevBidder: GroupElement) =>
        OUTPUTS(1).value == SELF.value &&
        OUTPUTS(1).propositionBytes == prevBidder.getEncoded
    })
    
    auctionActive &&
    validBidAmount &&
    higherThanCurrent &&
    correctScript &&
    updatedBidder &&
    correctRefund
  }
  
  // CLOSE ACTION
  val isClose = {
    // Only seller can close
    val sellerSigned = proveDlog(sellerPubKey)
    
    // Can only close after auction ends
    val auctionEnded = HEIGHT >= endHeight
    
    // Send funds to seller
    val sellerGetsProceeds = OUTPUTS(0).value == SELF.value &&
                             OUTPUTS(0).propositionBytes == sellerPubKey.getEncoded
    
    sellerSigned &&
    auctionEnded &&
    sellerGetsProceeds
  }
  
  sigmaProp(isBid || isClose)
}
