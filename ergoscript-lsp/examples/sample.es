/**
 * Sample ErgoScript Contract
 * Demonstrates time-locked multi-signature with token validation
 * 
 * This contract allows spending if:
 * 1. Current height is above 1,000,000 AND
 * 2. At least 2 of 3 signatures are provided AND
 * 3. Output preserves the token
 */

{
  // Define public keys for multisig
  val owner1 = proveDlog(PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9rn"))
  val owner2 = proveDlog(PK("3WwbzW6u8hKWBcL1W7kNVMr25qWRG"))
  val owner3 = proveDlog(PK("JryiGJaw1cvdoSfpbvFcsVj9XhZG"))

  // Time lock condition - must be after block height 1,000,000
  val timeLockPassed = HEIGHT > 1000000L

  // Multi-signature requirement - 2 of 3 signatures needed
  val multiSig = atLeast(2, Coll(owner1, owner2, owner3))

  // Token validation - ensure first token is preserved in output
  val hasToken = SELF.tokens.size > 0
  val tokenId = SELF.tokens(0)._1
  val tokenAmount = SELF.tokens(0)._2
  
  val outputHasToken = OUTPUTS(0).tokens.size > 0 &&
                       OUTPUTS(0).tokens(0)._1 == tokenId &&
                       OUTPUTS(0).tokens(0)._2 >= tokenAmount

  // Value preservation check
  val valuePreserved = OUTPUTS(0).value >= SELF.value

  // Final condition: all requirements must be satisfied
  sigmaProp(
    timeLockPassed &&
    multiSig &&
    hasToken &&
    outputHasToken &&
    valuePreserved
  )
}


// --- EXAMPLES WITH INTENTIONAL ERRORS (for testing diagnostics) ---

// Uncomment these to test error detection:

/*
// Example 1: Unmatched opening brace
{
  val x = HEIGHT > 100

// Example 2: Unmatched closing parenthesis
val result = (OUTPUTS.size + INPUTS.size))

// Example 3: Unmatched bracket
val tokens = SELF.tokens[0

// Example 4: Mismatched braces
{
  val condition = HEIGHT > 500000
]
*/


// --- HOVER EXAMPLES ---
// Hover over these keywords to see documentation:
// - SigmaProp
// - HEIGHT
// - OUTPUTS
// - INPUTS
// - SELF
// - tokens
// - sigmaProp
// - proveDlog
// - atLeast


// --- SIMPLE EXAMPLES ---

// Example: Simple public key check
// { proveDlog(ownerPubKey) }

// Example: Height-based time lock
// { sigmaProp(HEIGHT > 500000) }

// Example: Output value check
// { sigmaProp(OUTPUTS(0).value >= 1000000000L) }

// Example: Token existence check
// { sigmaProp(SELF.tokens.size > 0) }
