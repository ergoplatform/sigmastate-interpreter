{
  // Simple time-lock contract
  // Funds locked until block height 1000000
  val lockDeadline = 1000000L
  val recipient = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9rRcWBTpYf")
  
  sigmaProp(HEIGHT > lockDeadline) && recipient
}