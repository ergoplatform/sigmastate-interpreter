// 2-of-3 Multisig Contract
// Requires at least 2 out of 3 signatures to spend

{
  // Public keys stored in registers
  val pk1 = SELF.R4[SigmaProp].get
  val pk2 = SELF.R5[SigmaProp].get
  val pk3 = SELF.R6[SigmaProp].get
  
  // Threshold signature: 2 of 3 required
  atLeast(2, Coll(pk1, pk2, pk3))
}
