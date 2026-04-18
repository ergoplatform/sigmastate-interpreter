/**
 * VULNERABLE Oracle Contract
 * 
 * WARNING: This contract contains intentional vulnerabilities for educational purposes.
 * DO NOT use in production!
 * 
 * Vulnerabilities:
 * 1. Single oracle dependency (oracle manipulation)
 * 2. No signature verification (access control)
 * 3. Integer overflow risk (arithmetic safety)
 * 4. Missing output validation (context validation)
 */

{
  // VULNERABILITY #1: Single oracle dependency
  // An attacker could manipulate the oracle or create a fake oracle box
  val oracleBox = INPUTS(0)
  val price = oracleBox.R4[Long].get  // No validation of oracle authenticity!
  
  // VULNERABILITY #2: No signature verification
  // Anyone can trigger this contract without authentication
  val amount = SELF.R4[Long].get
  
  // VULNERABILITY #3: Integer overflow
  // If price * amount exceeds Long.MaxValue, it will wrap around
  val payout = price * amount  // Should use .toBigInt!
  
  // VULNERABILITY #4: Missing output validation
  // Output script not validated - funds could go anywhere!
  val correctPayout = OUTPUTS(0).value == payout
  // Missing: OUTPUTS(0).propositionBytes validation
  
  // VULNERABILITY #5: No height/timestamp checks
  // Oracle data could be stale
  
  sigmaProp(correctPayout)
}

/**
 * SECURE VERSION:
 * 
 * {
 *   // Use multiple oracles
 *   val oracle1 = INPUTS(0)
 *   val oracle2 = INPUTS(1)
 *   val oracle3 = INPUTS(2)
 *   
 *   // Verify oracle authenticity
 *   val validOracles = 
 *     oracle1.tokens(0)._1 == oracleNFT1 &&
 *     oracle2.tokens(0)._1 == oracleNFT2 &&
 *     oracle3.tokens(0)._1 == oracleNFT3
 *   
 *   // Get median price
 *   val prices = Coll(
 *     oracle1.R4[Long].get,
 *     oracle2.R4[Long].get,
 *     oracle3.R4[Long].get
 *   ).sorted
 *   val medianPrice = prices(1)
 *   
 *   // Check oracle freshness
 *   val maxAge = 10  // blocks
 *   val fresh = 
 *     HEIGHT - oracle1.R5[Int].get <= maxAge &&
 *     HEIGHT - oracle2.R5[Int].get <= maxAge &&
 *     HEIGHT - oracle3.R5[Int].get <= maxAge
 *   
 *   // Safe arithmetic with overflow protection
 *   val amount = SELF.R4[Long].get
 *   val payoutBig = medianPrice.toBigInt * amount.toBigInt
 *   require(payoutBig <= Long.MaxValue.toBigInt, "Overflow detected")
 *   val payout = payoutBig.toLong
 *   
 *   // Verify owner signature
 *   val ownerPubKey = SELF.R5[GroupElement].get
 *   val ownerSigned = proveDlog(ownerPubKey)
 *   
 *   // Validate output
 *   val recipientPubKey = SELF.R6[GroupElement].get
 *   val correctOutput = 
 *     OUTPUTS(0).value == payout &&
 *     OUTPUTS(0).propositionBytes == recipientPubKey.getEncoded
 *   
 *   sigmaProp(
 *     validOracles &&
 *     fresh &&
 *     ownerSigned &&
 *     correctOutput
 *   )
 * }
 */
