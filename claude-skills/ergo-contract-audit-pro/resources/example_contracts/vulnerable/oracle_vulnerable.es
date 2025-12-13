{
  // Vulnerable Oracle Contract - DO NOT USE IN PRODUCTION
  // This contract demonstrates multiple critical vulnerabilities
  
  val oracleBox = INPUTS(0)
  val price = oracleBox.R4[Long].get
  val amount = SELF.R4[Long].get
  val payout = price * amount
  
  sigmaProp(OUTPUTS(0).value == payout)
}
