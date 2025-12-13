// Token Transfer Contract
// Demonstrates working with tokens

{
  // The token we're protecting
  val tokenId = SELF.tokens(0)._1
  val tokenAmount = SELF.tokens(0)._2
  
  // Owner public key
  val ownerPk = SELF.R4[SigmaProp].get
  
  // Verify that:
  // 1. Owner signs the transaction
  // 2. Output preserves the same token
  
  val outputBox = OUTPUTS(0)
  val outputHasToken = outputBox.tokens.exists({ (t: (Coll[Byte], Long)) =>
    t._1 == tokenId && t._2 == tokenAmount
  })
  
  ownerPk && sigmaProp(outputHasToken)
}
