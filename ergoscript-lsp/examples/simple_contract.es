/**
 * Simple ErgoScript Contract Example
 * 
 * A basic contract that checks if the current height is greater than a deadline
 * and requires the owner's signature.
 */

{
  // Extract parameters from registers
  val owner = SELF.R4[GroupElement].get
  val deadline = SELF.R5[Int].get
  
  // Check if deadline has passed
  val deadlinePassed = HEIGHT > deadline
  
  // Require both conditions: deadline passed AND owner signature
  sigmaProp(deadlinePassed && proveDlog(owner))
}
