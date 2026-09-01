/**
 * Auction Contract Example
 * 
 * Demonstrates a simple auction where:
 * - Anyone can bid if amount >= minBid and before endHeight
 * - Seller can close auction after endHeight
 */

{
  val seller = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  val endHeight = SELF.R6[Int].get
  
  // BID: Before deadline, bid must be >= minBid
  val isBid = {
    val bidAmount = OUTPUTS(0).value
    val beforeDeadline = HEIGHT < endHeight
    val validBid = bidAmount >= minBid
    beforeDeadline && validBid
  }
  
  // CLOSE: After deadline, seller can claim funds
  val isClose = {
    val afterDeadline = HEIGHT >= endHeight
    val sellerSigned = proveDlog(seller)
    afterDeadline && sellerSigned
  }
  
  sigmaProp(isBid || isClose)
}
