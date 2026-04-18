// Hash Preimage Contract (HTLC-style)
// Can be spent by revealing the preimage of a hash, or refunded after deadline

{
  // Expected hash stored in R4
  val expectedHash = SELF.R4[Coll[Byte]].get
  
  // Refund address and deadline
  val refundPk = SELF.R5[SigmaProp].get
  val deadline = SELF.R6[Int].get
  
  // Get preimage from context extension
  val preimage = getVar[Coll[Byte]](0).get
  
  // Hash of the preimage
  val hash = blake2b256(preimage)
  
  // Either:
  // 1. Correct preimage is revealed
  // 2. Deadline passed and refund key signs
  
  sigmaProp(hash == expectedHash) || 
    (refundPk && sigmaProp(HEIGHT > deadline))
}
