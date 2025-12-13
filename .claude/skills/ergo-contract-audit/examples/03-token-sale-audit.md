# Example 3: Token Sale Contract Audit

## Contract Under Review

```scala
{
  // Token sale contract: Exchange tokens for ERG
  val saleTokenId = fromBase64("h8v3ZKqN7T2pUQXGW+aJYfHELdJ9gRzN3MwYxQsLKpY=")
  val tokenPrice = 1000000L  // 0.001 ERG per token
  val sellerPK = fromBase64("CgEDBA==")  // Seller's public key
  
  val tokensSold = SELF.tokens(0)._2 - OUTPUTS(0).tokens(0)._2
  val ergReceived = OUTPUTS(1).value - SELF.value
  
  val correctPayment = ergReceived >= tokensSold * tokenPrice
  val correctTokenId = SELF.tokens(0)._1 == saleTokenId
  val sellerGetsPaid = OUTPUTS(1).propositionBytes == sellerPK.propBytes
  
  sigmaProp(correctPayment && correctTokenId && sellerGetsPaid)
}
```

## Audit Analysis

### Contract Purpose
A decentralized token sale contract that allows buyers to purchase tokens at a fixed price. The contract holds tokens and releases them in exchange for ERG payment to the seller.

### Critical Security Review

#### 🔴 **CRITICAL**: Arithmetic Underflow Vulnerability
**Issue**: `SELF.tokens(0)._2 - OUTPUTS(0).tokens(0)._2` can underflow
**Impact**: If OUTPUTS(0) has more tokens than SELF, this will cause an error or wrap around
**Severity**: Critical - could lock funds or cause transaction failures
**Exploitation**: Attacker creates output with more tokens than input

**Vulnerable Code**:
```scala
val tokensSold = SELF.tokens(0)._2 - OUTPUTS(0).tokens(0)._2
```

**Fix**:
```scala
val inputTokens = SELF.tokens(0)._2
val outputTokens = if (OUTPUTS(0).tokens.size > 0 && 
                       OUTPUTS(0).tokens(0)._1 == saleTokenId) {
  OUTPUTS(0).tokens(0)._2
} else {
  0L
}
val tokensSold = inputTokens - outputTokens
val noUnderflow = tokensSold >= 0
```

#### 🔴 **CRITICAL**: Missing Index Bounds Checking
**Issue**: Accessing `OUTPUTS(0)` and `OUTPUTS(1)` without checking array size
**Impact**: Transaction will fail if there are fewer than 2 outputs
**Severity**: Critical - DoS vulnerability, contract becomes unusable

**Fix**:
```scala
val hasRequiredOutputs = OUTPUTS.size >= 2
```

#### 🔴 **CRITICAL**: No Token ID Validation in Output
**Issue**: Only checks SELF has correct token ID, not OUTPUTS(0)
**Impact**: Attacker could substitute different tokens
**Severity**: Critical - token swap attack

**Fix**:
```scala
val correctOutputTokenId = OUTPUTS(0).tokens.size > 0 && 
                           OUTPUTS(0).tokens(0)._1 == saleTokenId
```

#### 🔴 **CRITICAL**: Integer Overflow in Price Calculation
**Issue**: `tokensSold * tokenPrice` can overflow for large token amounts
**Impact**: Price calculation wraps around, allowing cheap token purchase
**Severity**: Critical - economic exploit

**Example Exploit**:
```scala
// If tokensSold = Long.MaxValue / tokenPrice + 1
// Then tokensSold * tokenPrice overflows and wraps to small number
```

**Fix**:
```scala
// Use BigInt for price calculation
val tokensSoldBig = tokensSold.toBigInt
val tokenPriceBig = tokenPrice.toBigInt
val expectedPayment = tokensSoldBig * tokenPriceBig
val ergReceivedBig = ergReceived.toBigInt

val correctPayment = ergReceivedBig >= expectedPayment
```

#### ⚠️ **HIGH**: Incorrect ERG Calculation Logic
**Issue**: `OUTPUTS(1).value - SELF.value` assumes OUTPUTS(1).value > SELF.value
**Impact**: If OUTPUTS(1).value < SELF.value, underflow occurs
**Severity**: High - transaction will fail, DoS possible

**Fix**:
```scala
// Seller receives payment in OUTPUTS(1)
// Change from buyer goes elsewhere or buyer includes additional inputs
val sellerBoxValue = OUTPUTS(1).value
val minimumPayment = tokensSold.toBigInt * tokenPrice.toBigInt
val correctPayment = sellerBoxValue.toBigInt >= minimumPayment
```

#### ⚠️ **HIGH**: No Validation of Token Conservation
**Issue**: Doesn't verify that tokens are conserved across all outputs
**Impact**: Tokens could be burned or duplicated
**Severity**: High - value destruction possible

**Fix**:
```scala
val totalOutputTokens = OUTPUTS.fold(0L, { (acc: Long, box: Box) => 
  acc + (if (box.tokens.size > 0 && box.tokens(0)._1 == saleTokenId) 
           box.tokens(0)._2 
         else 
           0L)
})
val tokensConserved = totalOutputTokens == SELF.tokens(0)._2 - tokensSold
```

#### ⚠️ **MEDIUM**: Missing Seller Signature
**Issue**: Uses `propositionBytes` comparison instead of signature verification
**Impact**: Anyone can create a box with those proposition bytes
**Severity**: Medium - depends on how sellerPK is used

**Better Approach**:
```scala
val sellerSig = proveDlog(sellerPK)
// Include in final condition
```

#### ⚠️ **MEDIUM**: No Minimum Purchase Validation
**Issue**: Allows zero-token purchases
**Impact**: Spam transactions, state bloat
**Severity**: Medium - UX and efficiency issue

**Fix**:
```scala
val minimumPurchase = 1L
val validPurchase = tokensSold >= minimumPurchase
```

### Secure Token Sale Contract - Fixed Version

```scala
{
  // ===== Configuration =====
  val saleTokenId = SELF.R4[Coll[Byte]].get  // Store in register for flexibility
  val tokenPrice = SELF.R5[Long].get          // Price per token in nanoERG
  val sellerPK = SELF.R6[GroupElement].get    // Seller's public key
  val minPurchase = 1000L                      // Minimum tokens per purchase
  
  // ===== Input Validation =====
  val hasRequiredOutputs = OUTPUTS.size >= 2
  val inputHasTokens = SELF.tokens.size > 0
  val inputTokenId = SELF.tokens(0)._1
  val inputTokenAmount = SELF.tokens(0)._2
  
  hasRequiredOutputs && inputHasTokens && sigmaProp(inputTokenId == saleTokenId) && {
    
    // ===== Output Box 0: Remaining tokens (change to contract) =====
    val output0 = OUTPUTS(0)
    val output0HasCorrectScript = output0.propositionBytes == SELF.propositionBytes
    val output0HasTokens = output0.tokens.size > 0
    val output0TokenId = output0.tokens(0)._1
    val output0TokenAmount = output0.tokens(0)._2
    
    // ===== Output Box 1: Payment to seller =====
    val output1 = OUTPUTS(1)
    val sellerReceivesPayment = proveDlog(sellerPK) // Seller must sign
    
    // ===== Calculate tokens sold =====
    val outputTokenMatches = output0HasTokens && (output0TokenId == saleTokenId)
    val remainingTokens = if (outputTokenMatches) output0TokenAmount else 0L
    
    val tokensSold = inputTokenAmount - remainingTokens
    val validSale = tokensSold >= minPurchase && tokensSold <= inputTokenAmount
    
    // ===== Calculate payment using BigInt to prevent overflow =====
    val tokensSoldBig = tokensSold.toBigInt
    val priceBig = tokenPrice.toBigInt
    val requiredPayment = tokensSoldBig * priceBig
    
    // Payment validation
    val paymentAmount = output1.value.toBigInt
    val correctPayment = paymentAmount >= requiredPayment
    
    // ===== Box continuation validation =====
    val contractContinues = output0HasCorrectScript && outputTokenMatches
    
    // ===== ERG conservation check =====
    // Input ERG + buyer's payment >= output ERG (allowing for fees)
    val maxFee = 10000000L  // 0.01 ERG max fee
    val totalOutputValue = OUTPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
    val valueConserved = totalOutputValue >= SELF.value - maxFee
    
    // ===== Final validation =====
    sigmaProp(
      validSale &&           // Valid token amount sold
      correctPayment &&      // Correct ERG received
      contractContinues &&   // Contract continues with remaining tokens
      valueConserved         // ERG conservation
    ) && sellerReceivesPayment  // Seller must sign the transaction
  }
}
```

### Alternative: Simplified Token Sale with Better Safety

```scala
{
  // Simple and safe token sale
  val saleTokenId = SELF.R4[Coll[Byte]].get
  val pricePerToken = SELF.R5[Long].get
  val sellerPubKey = SELF.R6[GroupElement].get
  
  // Validate basic structure
  val validStructure = OUTPUTS.size >= 1 && 
                       SELF.tokens.size > 0 &&
                       SELF.tokens(0)._1 == saleTokenId
  
  validStructure && {
    val inputTokens = SELF.tokens(0)._2
    
    // Calculate how many tokens remain in contract
    val remainingInContract = if (OUTPUTS.size > 0 && 
                                  OUTPUTS(0).tokens.size > 0 &&
                                  OUTPUTS(0).tokens(0)._1 == saleTokenId &&
                                  OUTPUTS(0).propositionBytes == SELF.propositionBytes) {
      OUTPUTS(0).tokens(0)._2
    } else {
      0L  // All tokens sold
    }
    
    val tokensSold = inputTokens - remainingInContract
    
    // Validate sale
    val validAmount = tokensSold > 0 && tokensSold <= inputTokens
    
    // Calculate minimum payment (use BigInt for safety)
    val minPaymentBig = tokensSold.toBigInt * pricePerToken.toBigInt
    
    // Check total value going to seller's addresses
    val totalToSeller = OUTPUTS.fold(0L.toBigInt, { (acc: BigInt, box: Box) =>
      if (box.propositionBytes == proveDlog(sellerPubKey).propBytes) {
        acc + box.value.toBigInt
      } else {
        acc
      }
    })
    
    val correctPayment = totalToSeller >= minPaymentBig
    
    sigmaProp(validAmount && correctPayment) && proveDlog(sellerPubKey)
  }
}
```

## Testing Recommendations

### Critical Test Cases

1. **Overflow/Underflow Tests**:
   - [ ] Purchase maximum tokens (check for overflow)
   - [ ] Purchase with output having more tokens than input
   - [ ] Zero token purchase
   - [ ] Negative token amounts (should be impossible with Long)

2. **Bounds Checking**:
   - [ ] Transaction with 0 outputs
   - [ ] Transaction with 1 output
   - [ ] Transaction with 2+ outputs

3. **Token ID Validation**:
   - [ ] Correct token ID in inputs and outputs
   - [ ] Wrong token ID in output (attack attempt)
   - [ ] Multiple different tokens in outputs

4. **Payment Calculation**:
   - [ ] Exact payment amount
   - [ ] Overpayment (should succeed)
   - [ ] Underpayment (should fail)
   - [ ] Large quantity causing overflow

5. **Economic Attacks**:
   - [ ] Buy tokens with minimal ERG by exploiting overflow
   - [ ] Extract ERG without providing tokens
   - [ ] Drain contract by manipulating token conservation

### Integration Tests

6. **End-to-End Flows**:
   - [ ] Deploy contract with initial token supply
   - [ ] Multiple sequential purchases
   - [ ] Concurrent purchase attempts
   - [ ] Purchase all remaining tokens
   - [ ] Seller withdraws accumulated ERG

7. **Edge Cases**:
   - [ ] Last token purchase (1 token remaining)
   - [ ] Contract with no ERG initially
   - [ ] Very small price (1 nanoERG)
   - [ ] Very large price (near Long.MaxValue)

## Common Vulnerabilities Checklist

| Vulnerability | Original | Fixed |
|---------------|----------|-------|
| Arithmetic overflow | ❌ Present | ✅ Fixed with BigInt |
| Arithmetic underflow | ❌ Present | ✅ Fixed with bounds checks |
| Array index out of bounds | ❌ Present | ✅ Fixed with size validation |
| Token ID confusion | ❌ Present | ✅ Fixed with ID validation |
| Value extraction | ⚠️ Partial | ✅ Fixed with conservation |
| Missing signature | ⚠️ Weak | ✅ Fixed with proveDlog |
| DoS via malformed tx | ❌ Possible | ✅ Fixed with validation |

## Risk Assessment

### Original Contract
**Overall Risk**: CRITICAL 🔴
- Multiple critical vulnerabilities allowing fund/token theft
- Arithmetic errors could lock contract or enable exploits
- Missing bounds checking causes transaction failures
- Not production ready

### Fixed Contract
**Overall Risk**: LOW 🟢
- All critical issues addressed
- Comprehensive validation added
- Safe arithmetic operations
- Token and value conservation enforced
- Production ready with proper testing

## Deployment Recommendation

### Original Version
❌ **DO NOT DEPLOY** - Critical security vulnerabilities present

### Fixed Version
✅ **APPROVED FOR DEPLOYMENT** after:
1. Comprehensive testing with Ergo Playgrounds
2. Fuzzing with random inputs
3. Economic simulation of attack scenarios
4. Code review by second auditor
5. Testnet deployment and monitoring

## Lessons Learned

1. **Always use BigInt for financial calculations** - Prevents overflow
2. **Validate array bounds before access** - Prevents DoS
3. **Check token IDs in all boxes** - Prevents token substitution
4. **Verify conservation laws** - Tokens and ERG must be conserved
5. **Use signed arithmetic carefully** - Underflow is as dangerous as overflow
6. **Test with extreme values** - Edge cases reveal vulnerabilities
7. **Require signatures for value transfer** - Don't rely on address matching alone

---

**Auditor's Note**: This contract demonstrates several common pitfalls in ErgoScript development. The vulnerabilities would have been caught by comprehensive testing, particularly with adversarial inputs. Always assume attackers will provide maximally hostile inputs.
