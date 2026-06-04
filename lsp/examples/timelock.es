// Time-Locked Vault Contract
// Owner can spend anytime, or anyone can spend after deadline

{
  // Configuration from registers
  val ownerPk = SELF.R4[SigmaProp].get
  val unlockHeight = SELF.R5[Int].get
  
  // Either owner signature OR deadline has passed
  ownerPk || sigmaProp(HEIGHT > unlockHeight)
}
