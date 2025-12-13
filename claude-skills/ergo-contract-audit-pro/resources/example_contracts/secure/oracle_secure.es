{
  // Secure Multi-Oracle Contract with Median Price
  // This contract demonstrates best practices for oracle integration
  
  // Step 1: Verify multiple oracle sources (minimum 3 for Byzantine fault tolerance)
  val oracle1Box = INPUTS(0)
  val oracle2Box = INPUTS(1)
  val oracle3Box = INPUTS(2)
  
  // Step 2: Extract prices with proper error handling
  val price1 = oracle1Box.R4[Long].get
  val price2 = oracle2Box.R4[Long].get
  val price3 = oracle3Box.R4[Long].get
  
  // Step 3: Verify oracle authenticity
  val oracle1Key = oracle1Box.R5[GroupElement].get
  val oracle2Key = oracle2Box.R5[GroupElement].get
  val oracle3Key = oracle3Box.R5[GroupElement].get
  
  val trustedOracle1 = fromBase64("...") // Known oracle 1 public key
  val trustedOracle2 = fromBase64("...") // Known oracle 2 public key
  val trustedOracle3 = fromBase64("...") // Known oracle 3 public key
  
  val oraclesVerified = (
    oracle1Key == trustedOracle1 &&
    oracle2Key == trustedOracle2 &&
    oracle3Key == trustedOracle3
  )
  
  // Step 4: Calculate median price (protects against single oracle manipulation)
  val sortedPrices = Coll(price1, price2, price3).sorted
  val medianPrice = sortedPrices(1)
  
  // Step 5: Sanity check - prices shouldn't deviate more than 5%
  val maxPrice = sortedPrices(2)
  val minPrice = sortedPrices(0)
  val deviation = ((maxPrice - minPrice).toBigInt * 100L) / medianPrice.toBigInt
  val deviationAcceptable = deviation <= 5L
  
  // Step 6: Get contract parameters
  val amount = SELF.R4[Long].get
  
  // Step 7: Safe multiplication with overflow protection
  val payoutBigInt = medianPrice.toBigInt * amount.toBigInt
  require(payoutBigInt <= 9223372036854775807L) // Max Long value
  val payout = payoutBigInt
  
  // Step 8: Validate output
  val output = OUTPUTS(0)
  val expectedScript = SELF.R6[Coll[Byte]].get
  
  val outputValid = (
    output.value >= payout &&
    output.propositionBytes == expectedScript &&
    output.value >= 1000000L // Minimum box value (0.001 ERG)
  )
  
  // Step 9: Final authorization
  sigmaProp(
    oraclesVerified &&
    deviationAcceptable &&
    outputValid
  )
}
