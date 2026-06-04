// Self-Replicating Contract (State Machine)
// Contract that must create a copy of itself in the output

{
  // Owner public key
  val ownerPk = SELF.R4[SigmaProp].get
  
  // Counter stored in R5 (example state)
  val currentCounter = SELF.R5[Long].get
  
  // Find output with same script
  val selfOutput = OUTPUTS.filter({ (box: Box) =>
    box.propositionBytes == SELF.propositionBytes
  })
  
  // Must have exactly one self-replicating output
  val hasOneOutput = selfOutput.size == 1
  
  // Output counter must be incremented
  val outputCounter = selfOutput(0).R5[Long].get
  val counterIncremented = outputCounter == currentCounter + 1L
  
  // Value must be preserved
  val valuePreserved = selfOutput(0).value >= SELF.value
  
  // Owner must sign
  ownerPk && sigmaProp(hasOneOutput && counterIncremented && valuePreserved)
}
