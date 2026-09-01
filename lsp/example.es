{
  // Simple ErgoScript example: a basic box guard script
  // This validates that the output box has a specific value
  
  val minValue = 1000000000L  // 1 ERG in nanoERG
  
  val outputOK = {
    val out = OUTPUTS(0)
    out.value >= minValue && 
    out.propositionBytes == SELF.propositionBytes
  }
  
  sigmaProp(outputOK)
}
