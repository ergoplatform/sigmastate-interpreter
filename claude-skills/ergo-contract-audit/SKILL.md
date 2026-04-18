---
name: Ergo Contract Auditor
description: Comprehensive security auditing for ErgoScript smart contracts on the Ergo blockchain
version: 1.0.0
author: Team Dev Engers
tags: [ergo, smart-contracts, security, audit, ergoscript, blockchain]
---

# Ergo Contract Auditor

A Claude AI skill for performing comprehensive security audits of ErgoScript smart contracts.

## Overview

This skill enables Claude to analyze ErgoScript contracts for security vulnerabilities, best practice violations, and potential exploits. It provides detailed audit reports with actionable recommendations for improving contract security.

**Capabilities:**
- Identify common vulnerabilities (reentrancy, overflow, access control issues)
- Verify cryptographic security (signatures, hashes, randomness)
- Check arithmetic safety and type handling
- Validate context usage and box management
- Assess code quality and documentation
- Generate comprehensive audit reports

---

## How to Use This Skill

### Basic Usage

1. **Provide the contract code:**
   ```
   Please audit this ErgoScript contract:
   [paste contract code]
   ```

2. **Specify audit depth (optional):**
   - `basic` - Quick security scan
   - `comprehensive` - Full security audit (default)
   - `focused` - Specific vulnerability check

3. **Review the audit report** with findings, severity ratings, and recommendations

### Advanced Usage

- **Batch auditing:** Provide multiple contracts for comparative analysis
- **Focused checks:** Request specific vulnerability scans
- **Best practice review:** Check against Ergo coding standards

---

## Audit Process

### 1. Contract Analysis
- Parse ErgoScript syntax
- Extract contract structure (guards, conditions, outputs)
- Identify key components (boxes, tokens, context variables)
- Map data flow and control flow

### 2. Security Checks

#### **Access Control** (Critical)
- Signature verification correctness
- Owner/admin privilege validation
- Multi-signature implementation
- Permission boundaries

#### **Arithmetic Safety** (High)
- Integer overflow/underflow protection
- Division by zero checks
- Type conversion safety
- Boundary condition handling

#### **Context Validation** (High)
- Input validation completeness
- Output constraint verification
- Box value and token checks
- Height and timestamp usage

#### **Cryptographic Security** (Critical)
- Hash function usage (BLAKE2b256)
- Signature scheme correctness (Schnorr)
- Random number generation
- Key management

#### **Logic Vulnerabilities** (Critical)
- Reentrancy attack vectors
- Race condition possibilities
- Front-running vulnerabilities
- Oracle manipulation risks
- Timestamp dependence issues

#### **Resource Management** (Medium)
- Cost/complexity limits
- Box creation constraints
- Token handling safety
- Memory usage patterns

#### **Code Quality** (Low)
- Code clarity and readability
- Documentation completeness
- Test coverage indicators
- Upgrade/migration paths

### 3. Report Generation
- Categorize findings by severity
- Provide code examples
- Suggest specific fixes
- Reference best practices

---

## Common Vulnerabilities

### 1. Reentrancy Attacks

**Description:** Contract calls external code that calls back into the original contract before the first invocation completes.

**ErgoScript Context:** Less common than Ethereum due to UTXO model, but possible with complex box chaining.

**Detection:**
```ergoscript
// VULNERABLE: State change after external call
val externalBox = OUTPUTS(0)
val stateUpdate = SELF.R4[Long].get - amount
// If externalBox can trigger another transaction...
```

**Fix:** Complete all state changes before any external interactions.

### 2. Integer Overflow/Underflow

**Description:** Arithmetic operations exceed type boundaries, causing unexpected wrapping.

**Detection:**
```ergoscript
// VULNERABLE: No overflow check
val total = box1Value + box2Value

// SECURE: With overflow protection
val total = box1Value.toBigInt + box2Value.toBigInt
require(total <= Long.MaxValue, "Overflow detected")
```

**Fix:** Use BigInt for large calculations, add explicit bounds checking.

### 3. Improper Access Control

**Description:** Missing or weak authentication/authorization checks.

**Detection:**
```ergoscript
// VULNERABLE: No signature check
val withdraw = OUTPUTS(0).value == SELF.value

// SECURE: Proper signature verification
val ownerPubKey = SELF.R4[GroupElement].get
val withdraw = proveDlog(ownerPubKey) && OUTPUTS(0).value == SELF.value
```

**Fix:** Always verify signatures for privileged operations.

### 4. Weak Randomness

**Description:** Predictable random number generation.

**Detection:**
```ergoscript
// VULNERABLE: Predictable randomness
val random = HEIGHT % 100

// BETTER: Use block hash (still somewhat predictable)
val random = blake2b256(SELF.id ++ HEIGHT.toByteArray)
```

**Fix:** Use VRF or commit-reveal schemes for critical randomness.

### 5. Front-Running

**Description:** Attacker observes pending transaction and submits competing transaction with higher fee.

**Detection:**
- Public auction without commit-reveal
- DEX trades without slippage protection
- Oracle updates without delay

**Fix:** Implement commit-reveal, use time locks, add slippage limits.

### 6. Oracle Manipulation

**Description:** Reliance on external data sources that can be manipulated.

**Detection:**
```ergoscript
// VULNERABLE: Single oracle source
val price = oracleBox.R4[Long].get
val payout = amount * price

// BETTER: Multiple oracle aggregation
val prices = INPUTS.filter(isOracle).map(_.R4[Long].get)
val medianPrice = median(prices)
```

**Fix:** Use multiple oracles, implement price bounds, add time-weighted averages.

### 7. Timestamp Dependence

**Description:** Critical logic depends on block timestamps which miners can manipulate (±15 minutes).

**Detection:**
```ergoscript
// VULNERABLE: Exact timestamp check
val isExpired = HEIGHT >= expiryTimestamp

// BETTER: Use block height
val isExpired = HEIGHT >= expiryHeight
```

**Fix:** Use block height instead of timestamps when possible.

---

## Best Practices

### 1. Signature Verification

**Always verify signatures for privileged operations:**
```ergoscript
val ownerPubKey = SELF.R4[GroupElement].get
val validSignature = proveDlog(ownerPubKey)

sigmaProp(validSignature && otherConditions)
```

### 2. Safe Arithmetic

**Use BigInt for calculations that might overflow:**
```ergoscript
val sum = values.fold(0L.toBigInt, {(acc: BigInt, v: Long) => acc + v})
require(sum <= Long.MaxValue, "Sum exceeds maximum")
```

### 3. Input Validation

**Validate all inputs and context variables:**
```ergoscript
// Check box values
require(OUTPUTS(0).value >= minValue, "Output too small")
require(OUTPUTS(0).value <= maxValue, "Output too large")

// Validate tokens
require(OUTPUTS(0).tokens.size == 1, "Wrong token count")
require(OUTPUTS(0).tokens(0)._1 == expectedTokenId, "Wrong token")
```

### 4. Output Constraints

**Explicitly define all output requirements:**
```ergoscript
val correctValue = OUTPUTS(0).value == expectedValue
val correctScript = OUTPUTS(0).propositionBytes == expectedScript
val correctTokens = OUTPUTS(0).tokens == expectedTokens

sigmaProp(correctValue && correctScript && correctTokens)
```

### 5. Error Messages

**Provide clear error messages for debugging:**
```ergoscript
require(condition1, "Condition 1 failed: insufficient funds")
require(condition2, "Condition 2 failed: invalid signature")
```

### 6. Gas Optimization

**Minimize computational complexity:**
```ergoscript
// INEFFICIENT: Multiple iterations
val sum1 = boxes.map(_.value).sum
val sum2 = boxes.map(_.tokens.size).sum

// EFFICIENT: Single iteration
val (valueSum, tokenSum) = boxes.fold((0L, 0), {
  (acc, box) => (acc._1 + box.value, acc._2 + box.tokens.size)
})
```

### 7. Documentation

**Document all contract assumptions and requirements:**
```ergoscript
/**
 * Auction Contract
 * 
 * R4: GroupElement - Seller's public key
 * R5: Long - Minimum bid amount
 * R6: Int - Auction end height
 * 
 * Actions:
 * 1. Bid - Anyone can bid if amount >= minBid
 * 2. Close - Seller can close after end height
 */
```

---

## Audit Report Template

```markdown
# ErgoScript Contract Audit Report

## Contract Information
- **Name:** [Contract Name]
- **Version:** [Version]
- **Auditor:** Claude AI (Ergo Contract Auditor Skill)
- **Date:** [Date]

## Executive Summary
[Brief overview of findings]

## Severity Classification
- **Critical:** Immediate action required
- **High:** Should be fixed before deployment
- **Medium:** Should be addressed
- **Low:** Consider improving
- **Informational:** Best practice suggestions

## Findings

### Critical Issues
[List of critical vulnerabilities]

### High Severity Issues
[List of high severity issues]

### Medium Severity Issues
[List of medium severity issues]

### Low Severity Issues
[List of low severity issues]

### Informational
[Best practice suggestions]

## Recommendations
[Prioritized list of fixes]

## Conclusion
[Overall security assessment]
```

---

## Example Audit

### Input Contract
```ergoscript
{
  val ownerPubKey = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  
  val bid = OUTPUTS(0).value
  val refund = OUTPUTS(1).value
  
  val validBid = bid >= minBid
  val validRefund = refund == SELF.value - bid
  
  sigmaProp(validBid && validRefund)
}
```

### Audit Output

**Findings:**

1. **CRITICAL - Missing Signature Verification**
   - **Location:** Line 8
   - **Issue:** No verification that bidder signed the transaction
   - **Impact:** Anyone can create a bid without authorization
   - **Fix:** Add `proveDlog(bidderPubKey)` check

2. **HIGH - No Ownership Check**
   - **Location:** Line 1
   - **Issue:** `ownerPubKey` is defined but never used
   - **Impact:** Owner cannot control auction
   - **Fix:** Add owner signature requirement for auction close

3. **MEDIUM - Missing Output Script Validation**
   - **Location:** Line 5-6
   - **Issue:** Output scripts not validated
   - **Impact:** Funds could go to wrong contract
   - **Fix:** Verify `OUTPUTS(0).propositionBytes`

4. **LOW - No Auction End Check**
   - **Issue:** Auction never closes
   - **Impact:** Indefinite bidding period
   - **Fix:** Add height-based expiry

**Recommendations:**
1. Add bidder signature verification (Critical)
2. Implement auction close logic with owner check (High)
3. Validate all output constraints (Medium)
4. Add time-based auction end (Low)

---

## Integration with Development Workflow

### Pre-Deployment Checklist

- [ ] Run comprehensive audit
- [ ] Address all Critical and High severity issues
- [ ] Review and consider Medium severity issues
- [ ] Document all acknowledged risks
- [ ] Perform manual code review
- [ ] Write comprehensive tests
- [ ] Test on testnet
- [ ] Get external audit for high-value contracts

### Continuous Auditing

- Audit after every significant change
- Re-audit when dependencies update
- Monitor for new vulnerability patterns
- Keep audit reports in version control

---

## Limitations

**This skill provides automated analysis but:**
- Cannot detect all possible vulnerabilities
- May produce false positives
- Requires human review for complex logic
- Should be supplemented with manual auditing
- Does not replace professional security audits for high-value contracts

**Always:**
- Perform thorough testing
- Get external audits for critical contracts
- Follow defense-in-depth principles
- Stay updated on new vulnerability patterns

---

## Resources

- [ErgoScript Documentation](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [ErgoTree Specification](https://ergoplatform.org/docs/ErgoTree.pdf)
- [Ergo Smart Contract Examples](https://github.com/ergoplatform/ergo-appkit-examples)
- [Sigma Protocols](https://en.wikipedia.org/wiki/Proof_of_knowledge#Sigma_protocols)

---

**Version:** 1.0.0  
**Last Updated:** December 2025  
**Maintained by:** Team Dev Engers
