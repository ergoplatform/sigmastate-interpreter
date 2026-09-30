# ErgoScript Best Practices

**Security and quality guidelines for ErgoScript smart contract development**

---

## 1. Access Control

### Always Verify Signatures

**✅ GOOD:**
```ergoscript
val ownerPubKey = SELF.R4[GroupElement].get
val ownerSigned = proveDlog(ownerPubKey)
sigmaProp(ownerSigned && otherConditions)
```

**❌ BAD:**
```ergoscript
val ownerPubKey = SELF.R4[GroupElement].get
// ownerPubKey extracted but never verified!
sigmaProp(otherConditions)
```

### Multi-Signature Patterns

```ergoscript
// 2-of-3 multi-sig
val owner1 = SELF.R4[GroupElement].get
val owner2 = SELF.R5[GroupElement].get
val owner3 = SELF.R6[GroupElement].get

val sig1 = proveDlog(owner1)
val sig2 = proveDlog(owner2)
val sig3 = proveDlog(owner3)

// Require any 2 signatures
sigmaProp(atLeast(2, Coll(sig1, sig2, sig3)))
```

---

## 2. Arithmetic Safety

### Use BigInt for Overflow Protection

**✅ GOOD:**
```ergoscript
val sum = value1.toBigInt + value2.toBigInt
require(sum <= Long.MaxValue.toBigInt, "Overflow detected")
val result = sum.toLong
```

**❌ BAD:**
```ergoscript
val sum = value1 + value2  // Can overflow!
```

### Division by Zero Protection

**✅ GOOD:**
```ergoscript
require(divisor != 0, "Division by zero")
val result = numerator / divisor
```

**❌ BAD:**
```ergoscript
val result = numerator / divisor  // Crashes if divisor == 0
```

---

## 3. Input/Output Validation

### Validate All Outputs

**✅ GOOD:**
```ergoscript
val correctValue = OUTPUTS(0).value == expectedValue
val correctScript = OUTPUTS(0).propositionBytes == expectedScript
val correctTokens = OUTPUTS(0).tokens(0)._1 == expectedTokenId

sigmaProp(correctValue && correctScript && correctTokens)
```

**❌ BAD:**
```ergoscript
val correctValue = OUTPUTS(0).value == expectedValue
// Missing script and token validation!
sigmaProp(correctValue)
```

### Box Value Checks

```ergoscript
// Validate value ranges
require(OUTPUTS(0).value >= minValue, "Output too small")
require(OUTPUTS(0).value <= maxValue, "Output too large")

// Validate total value conservation
val inputSum = INPUTS.map(_.value).sum
val outputSum = OUTPUTS.map(_.value).sum
require(inputSum == outputSum, "Value not conserved")
```

---

## 4. Cryptographic Security

### Hash Functions

**✅ GOOD:**
```ergoscript
// Use BLAKE2b256 for all hashing
val hash = blake2b256(data)
```

**❌ BAD:**
```ergoscript
// Don't use deprecated or weak hash functions
val hash = sha256(data)  // Prefer blake2b256
```

### Randomness

**✅ GOOD:**
```ergoscript
// For non-critical randomness
val seed = blake2b256(SELF.id ++ HEIGHT.toByteArray)

// For critical randomness, use commit-reveal:
// 1. Commit phase: hash(secret)
// 2. Reveal phase: verify hash and use secret
```

**❌ BAD:**
```ergoscript
val random = HEIGHT % 100  // Predictable!
```

---

## 5. Oracle Usage

### Multiple Oracle Aggregation

**✅ GOOD:**
```ergoscript
// Use 3+ oracles
val oracle1Price = INPUTS(0).R4[Long].get
val oracle2Price = INPUTS(1).R4[Long].get
val oracle3Price = INPUTS(2).R4[Long].get

// Verify oracle authenticity
val validOracles = 
  INPUTS(0).tokens(0)._1 == oracleNFT1 &&
  INPUTS(1).tokens(0)._1 == oracleNFT2 &&
  INPUTS(2).tokens(0)._1 == oracleNFT3

// Use median price
val prices = Coll(oracle1Price, oracle2Price, oracle3Price).sorted
val medianPrice = prices(1)

// Check freshness
val maxAge = 10  // blocks
val fresh = 
  HEIGHT - INPUTS(0).R5[Int].get <= maxAge &&
  HEIGHT - INPUTS(1).R5[Int].get <= maxAge &&
  HEIGHT - INPUTS(2).R5[Int].get <= maxAge

sigmaProp(validOracles && fresh && otherConditions)
```

**❌ BAD:**
```ergoscript
// Single oracle - can be manipulated!
val price = INPUTS(0).R4[Long].get
```

---

## 6. Height and Timestamp Usage

### Use Height for Time Checks

**✅ GOOD:**
```ergoscript
val expiryHeight = SELF.R4[Int].get
val isExpired = HEIGHT >= expiryHeight  // Use >=, not ==
```

**❌ BAD:**
```ergoscript
val expiryHeight = SELF.R4[Int].get
val isExpired = HEIGHT == expiryHeight  // Can be skipped!
```

### Time Windows

```ergoscript
// Define time windows with ranges
val startHeight = SELF.R4[Int].get
val endHeight = SELF.R5[Int].get

val isActive = HEIGHT >= startHeight && HEIGHT < endHeight
```

---

## 7. Error Handling

### Descriptive Error Messages

**✅ GOOD:**
```ergoscript
require(amount >= minAmount, "Amount below minimum")
require(HEIGHT < deadline, "Deadline passed")
require(validSignature, "Invalid signature")
```

**❌ BAD:**
```ergoscript
require(amount >= minAmount)  // No error message!
```

---

## 8. Gas Optimization

### Minimize Complexity

**✅ GOOD:**
```ergoscript
// Single pass through collection
val (valueSum, tokenSum) = boxes.fold((0L, 0), {
  (acc, box) => (acc._1 + box.value, acc._2 + box.tokens.size)
})
```

**❌ BAD:**
```ergoscript
// Multiple passes - inefficient!
val valueSum = boxes.map(_.value).sum
val tokenSum = boxes.map(_.tokens.size).sum
```

### Avoid Redundant Checks

```ergoscript
// Cache expensive operations
val ownerPubKey = SELF.R4[GroupElement].get
val ownerSigned = proveDlog(ownerPubKey)

// Reuse ownerSigned instead of calling proveDlog multiple times
sigmaProp(ownerSigned && condition1 || ownerSigned && condition2)
```

---

## 9. Documentation

### Contract Header

```ergoscript
/**
 * Token Swap Contract
 * 
 * Allows atomic swap of Token A for Token B at fixed rate.
 * 
 * Registers:
 * R4: (Coll[Byte], Coll[Byte]) - (TokenA ID, TokenB ID)
 * R5: (Long, Long) - (TokenA amount, TokenB amount) - swap ratio
 * R6: GroupElement - Owner's public key
 * 
 * Actions:
 * 1. Swap - Exchange TokenA for TokenB at fixed rate
 * 2. Cancel - Owner cancels and retrieves tokens
 * 
 * Security:
 * - Owner signature required for cancel
 * - Atomic swap prevents partial execution
 * - Overflow protection on amount calculations
 */
```

### Inline Comments

```ergoscript
// Verify swap ratio is maintained
val tokenAIn = INPUTS(0).tokens(0)._2
val tokenBOut = OUTPUTS(0).tokens(0)._2
val correctRatio = tokenAIn * ratioB == tokenBOut * ratioA
```

---

## 10. Testing Guidelines

### Test All Paths

```
✓ Happy path - normal execution
✓ Edge cases - boundary values
✓ Error cases - invalid inputs
✓ Attack scenarios - malicious inputs
✓ Upgrade paths - contract evolution
```

### Test Checklist

- [ ] All require() statements can fail
- [ ] All require() statements can pass
- [ ] Overflow conditions tested
- [ ] Underflow conditions tested
- [ ] Zero values tested
- [ ] Maximum values tested
- [ ] Invalid signatures rejected
- [ ] Valid signatures accepted
- [ ] All output validations work
- [ ] Time-based conditions tested

---

## 11. Common Patterns

### Auction Pattern

```ergoscript
{
  val minBid = SELF.R4[Long].get
  val endHeight = SELF.R5[Int].get
  val seller = SELF.R6[GroupElement].get
  
  val isBid = {
    val validBid = OUTPUTS(0).value >= minBid
    val beforeEnd = HEIGHT < endHeight
    val correctContinuation = OUTPUTS(0).propositionBytes == SELF.propositionBytes
    validBid && beforeEnd && correctContinuation
  }
  
  val isClose = {
    val afterEnd = HEIGHT >= endHeight
    val sellerSigned = proveDlog(seller)
    afterEnd && sellerSigned
  }
  
  sigmaProp(isBid || isClose)
}
```

### Escrow Pattern

```ergoscript
{
  val buyer = SELF.R4[GroupElement].get
  val seller = SELF.R5[GroupElement].get
  val arbiter = SELF.R6[GroupElement].get
  
  val buyerApproves = proveDlog(buyer)
  val sellerApproves = proveDlog(seller)
  val arbiterDecides = proveDlog(arbiter)
  
  // Release to seller if buyer approves OR arbiter decides
  val releaseToSeller = (buyerApproves || arbiterDecides) &&
                        OUTPUTS(0).propositionBytes == seller.getEncoded
  
  // Refund to buyer if seller approves OR arbiter decides
  val refundToBuyer = (sellerApproves || arbiterDecides) &&
                      OUTPUTS(0).propositionBytes == buyer.getEncoded
  
  sigmaProp(releaseToSeller || refundToBuyer)
}
```

---

## 12. Deployment Checklist

### Pre-Deployment

- [ ] All critical issues fixed
- [ ] All high issues fixed
- [ ] Medium issues reviewed
- [ ] Code documented
- [ ] Tests written and passing
- [ ] Testnet deployment successful
- [ ] External audit (for high-value contracts)

### Post-Deployment

- [ ] Monitor for unusual activity
- [ ] Have upgrade/pause mechanism
- [ ] Document all assumptions
- [ ] Maintain audit trail
- [ ] Plan for incident response

---

## Resources

- [ErgoScript Tutorial](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [ErgoTree Specification](https://ergoplatform.org/docs/ErgoTree.pdf)
- [Ergo Appkit Examples](https://github.com/ergoplatform/ergo-appkit-examples)

---

**Remember:** Security is a process, not a destination. Stay updated on new vulnerabilities and best practices!
