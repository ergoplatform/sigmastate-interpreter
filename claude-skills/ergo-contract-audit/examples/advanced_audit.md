# Advanced Audit Example

This example demonstrates a comprehensive audit of a complex DeFi contract.

---

## Contract: Decentralized Exchange (DEX) Swap

```ergoscript
/**
 * DEX Token Swap Contract
 * Atomic swap between ERG and tokens at oracle-determined price
 */
{
  val tokenId = SELF.R4[Coll[Byte]].get
  val oracleNFT = SELF.R5[Coll[Byte]].get
  val feePercent = SELF.R6[Int].get  // Basis points (100 = 1%)
  val poolOwner = SELF.R7[GroupElement].get
  
  // Get oracle price
  val oracleBox = INPUTS(0)
  val oracleValid = oracleBox.tokens(0)._1 == oracleNFT
  val price = oracleBox.R4[Long].get
  
  // Calculate swap
  val ergIn = INPUTS(1).value
  val tokensOut = (ergIn * price) / 1000000000L
  val fee = (tokensOut * feePercent) / 10000
  val netTokens = tokensOut - fee
  
  // Validate outputs
  val userGetsTokens = OUTPUTS(0).tokens(0)._2 == netTokens
  val poolGetsERG = OUTPUTS(1).value >= ergIn
  val feeToOwner = OUTPUTS(2).tokens(0)._2 == fee
  
  sigmaProp(oracleValid && userGetsTokens && poolGetsERG && feeToOwner)
}
```

---

## Comprehensive Audit

### Critical Issues

**1. Single Oracle Dependency**
- **Severity:** CRITICAL
- **Category:** Oracle Manipulation
- **Issue:** Relies on single oracle for price - can be manipulated
- **Attack Vector:** Attacker creates fake oracle box with malicious price
- **Impact:** Drain entire pool with manipulated prices
- **Fix:** Use multiple oracles with median aggregation
- **CWE:** CWE-345

**2. No Oracle Freshness Check**
- **Severity:** CRITICAL
- **Category:** Logic Vulnerability
- **Issue:** Oracle price not checked for staleness
- **Attack Vector:** Use old oracle data during price volatility
- **Impact:** Arbitrage against stale prices
- **Fix:** Add `HEIGHT - oracleBox.R5[Int].get <= maxAge`

**3. Integer Overflow in Calculation**
- **Severity:** CRITICAL
- **Category:** Arithmetic Safety
- **Issue:** `ergIn * price` can overflow
- **Attack Vector:** Large ergIn value causes overflow, wrapping to small number
- **Impact:** Get tokens for free due to overflow
- **Fix:** Use `.toBigInt` for multiplication

### High Issues

**4. Missing Output Script Validation**
- **Severity:** HIGH
- **Issue:** Output scripts not validated
- **Impact:** Tokens/ERG could go to wrong addresses
- **Fix:** Validate all `OUTPUTS[i].propositionBytes`

**5. No Slippage Protection**
- **Severity:** HIGH
- **Issue:** No minimum output amount check
- **Impact:** Front-running attacks, sandwich attacks
- **Fix:** Add user-specified minimum output validation

**6. Fee Calculation Precision Loss**
- **Severity:** HIGH
- **Issue:** Integer division loses precision
- **Impact:** Rounding errors favor pool over time
- **Fix:** Use higher precision or round in user's favor

### Medium Issues

**7. No Input Validation**
- **Severity:** MEDIUM
- **Issue:** Input boxes not validated
- **Impact:** Could use wrong inputs
- **Fix:** Validate input box scripts and values

**8. Missing Pool State Validation**
- **Severity:** MEDIUM
- **Issue:** Pool continuation box not validated
- **Impact:** Pool state could be corrupted
- **Fix:** Validate pool box maintains correct token reserves

### Low Issues

**9. No Error Messages**
- **Severity:** LOW
- **Issue:** No require() statements with messages
- **Fix:** Add descriptive error messages

**10. Insufficient Documentation**
- **Severity:** LOW
- **Issue:** Missing register documentation
- **Fix:** Document all registers and invariants

---

## Secure Implementation

```ergoscript
/**
 * Secure DEX Token Swap Contract
 * 
 * Registers:
 * R4: Coll[Byte] - Token ID to swap
 * R5: Coll[Coll[Byte]] - Oracle NFT IDs (3 oracles)
 * R6: Int - Fee in basis points (100 = 1%)
 * R7: GroupElement - Pool owner public key
 * R8: Long - Min liquidity to maintain
 */
{
  val tokenId = SELF.R4[Coll[Byte]].get
  val oracleNFTs = SELF.R5[Coll[Coll[Byte]]].get
  val feePercent = SELF.R6[Int].get
  val poolOwner = SELF.R7[GroupElement].get
  val minLiquidity = SELF.R8[Long].get
  
  // Validate we have 3 oracle inputs
  require(INPUTS.size >= 4, "Need 3 oracles + user input")
  
  // Verify oracle authenticity
  val oracle1 = INPUTS(0)
  val oracle2 = INPUTS(1)
  val oracle3 = INPUTS(2)
  
  val validOracles = 
    oracle1.tokens(0)._1 == oracleNFTs(0) &&
    oracle2.tokens(0)._1 == oracleNFTs(1) &&
    oracle3.tokens(0)._1 == oracleNFTs(2)
  
  require(validOracles, "Invalid oracle NFTs")
  
  // Check oracle freshness (within 10 blocks)
  val maxAge = 10
  val fresh = 
    HEIGHT - oracle1.R5[Int].get <= maxAge &&
    HEIGHT - oracle2.R5[Int].get <= maxAge &&
    HEIGHT - oracle3.R5[Int].get <= maxAge
  
  require(fresh, "Stale oracle data")
  
  // Get median price
  val prices = Coll(
    oracle1.R4[Long].get,
    oracle2.R4[Long].get,
    oracle3.R4[Long].get
  ).sorted
  val medianPrice = prices(1)
  
  // Get user input
  val userInput = INPUTS(3)
  val ergIn = userInput.value
  
  // Safe calculation with overflow protection
  val tokensOutBig = (ergIn.toBigInt * medianPrice.toBigInt) / 1000000000L.toBigInt
  require(tokensOutBig <= Long.MaxValue.toBigInt, "Overflow in token calculation")
  val tokensOut = tokensOutBig.toLong
  
  // Calculate fee
  val feeBig = (tokensOut.toBigInt * feePercent.toBigInt) / 10000L.toBigInt
  require(feeBig <= Long.MaxValue.toBigInt, "Overflow in fee calculation")
  val fee = feeBig.toLong
  
  val netTokens = tokensOut - fee
  
  // Get minimum output from user
  val minTokensOut = getVar[Long](0).get
  require(netTokens >= minTokensOut, "Slippage too high")
  
  // Validate outputs
  val userOutput = OUTPUTS(0)
  val poolOutput = OUTPUTS(1)
  val feeOutput = OUTPUTS(2)
  
  // User gets tokens
  val userGetsTokens = 
    userOutput.tokens.size == 1 &&
    userOutput.tokens(0)._1 == tokenId &&
    userOutput.tokens(0)._2 == netTokens
  
  // Pool gets ERG and maintains state
  val poolGetsERG = poolOutput.value >= SELF.value + ergIn
  val poolMaintainsTokens = poolOutput.tokens(0)._1 == tokenId
  val poolHasLiquidity = poolOutput.tokens(0)._2 >= minLiquidity
  val poolContinues = poolOutput.propositionBytes == SELF.propositionBytes
  
  // Fee to owner
  val feeCorrect = 
    feeOutput.tokens(0)._1 == tokenId &&
    feeOutput.tokens(0)._2 == fee &&
    feeOutput.propositionBytes == poolOwner.getEncoded
  
  sigmaProp(
    userGetsTokens &&
    poolGetsERG &&
    poolMaintainsTokens &&
    poolHasLiquidity &&
    poolContinues &&
    feeCorrect
  )
}
```

---

## Audit Summary

**Total Issues:** 10 (3 Critical, 3 High, 2 Medium, 2 Low)

**Security Rating:** ❌ **CRITICAL VULNERABILITIES** - Do not deploy

**Key Improvements:**
1. ✅ Multiple oracle aggregation (median of 3)
2. ✅ Oracle freshness validation
3. ✅ Overflow protection with BigInt
4. ✅ Output script validation
5. ✅ Slippage protection
6. ✅ Pool state validation
7. ✅ Comprehensive error messages
8. ✅ Full documentation

**Estimated Fix Time:** 4-6 hours

---

## Testing Recommendations

1. **Unit Tests:**
   - Test with various ERG amounts
   - Test overflow scenarios
   - Test with stale oracle data
   - Test with manipulated oracle prices

2. **Integration Tests:**
   - Test full swap flow
   - Test with real oracle data
   - Test slippage protection
   - Test pool state transitions

3. **Security Tests:**
   - Attempt oracle manipulation
   - Attempt overflow attacks
   - Attempt front-running
   - Attempt to drain pool

4. **Testnet Deployment:**
   - Deploy to testnet
   - Run for 1 week minimum
   - Monitor for issues
   - Gather user feedback

---

## External Audit Recommendation

**⚠️ STRONGLY RECOMMENDED**

Given the complexity and value at risk in a DEX contract, we strongly recommend:

1. Professional security audit by firm specializing in blockchain
2. Bug bounty program
3. Gradual rollout with value caps
4. Emergency pause mechanism
5. Insurance coverage

**Estimated Audit Cost:** $15,000 - $30,000  
**Recommended Firms:** Trail of Bits, ConsenSys Diligence, OpenZeppelin

---

This example demonstrates the depth of analysis needed for production DeFi contracts.
