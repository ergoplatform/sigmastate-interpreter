# Example: Multi-Signature Wallet Audit

## Contract to Audit
```scala
{
  val owner1 = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9...")
  val owner2 = PK("9fMPy1XY3GW1CGJ45rNf4Bx8nME1zNuF5jGd24vQvR...")
  val owner3 = PK("9hU8KLzGdQp4V3mR2WYn7Fq1xJtK9pT6eH2sNc8...")
  
  anyOf(Coll(owner1, owner2, owner3))
}
```

## Audit Report

### Security Audit: Multi-Signature Wallet

## Executive Summary
**Overall Risk: CRITICAL**

This 3-of-3 multi-signature wallet has a critical vulnerability that defeats its security purpose. The contract allows ANY ONE of the three owners to spend funds alone, rather than requiring multiple signatures.

## Critical Findings

### C-1: Broken Multi-Signature Logic
**Severity**: CRITICAL
**Location**: Spending condition (line 6)
**Type**: Authentication Bypass

**Description**: 
The contract uses `anyOf()` instead of `atLeast()`, which means only ONE signature is required instead of multiple signatures. This completely defeats the purpose of a multi-signature wallet.

**Current Code**:
```scala
anyOf(Coll(owner1, owner2, owner3))
```

**Issue**: `anyOf` returns true if ANY condition is met. In this case, if owner1 OR owner2 OR owner3 signs, the funds can be spent.

**Impact**: 
- Complete security bypass
- Any single owner can unilaterally spend all funds
- No protection against compromised individual keys
- Defeats the trust model of multi-sig wallets

**Attack Scenario**:
1. Attacker compromises owner1's private key
2. Attacker creates transaction spending all funds
3. Attacker signs with owner1's key
4. Transaction succeeds (owner2 and owner3 not needed)
5. All funds stolen

**Recommendation**:
Replace `anyOf` with `atLeast` to require multiple signatures:
```scala
{
  val owner1 = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9...")
  val owner2 = PK("9fMPy1XY3GW1CGJ45rNf4Bx8nME1zNuF5jGd24vQvR...")
  val owner3 = PK("9hU8KLzGdQp4V3mR2WYn7Fq1xJtK9pT6eH2sNc8...")
  
  // Require at least 2 out of 3 signatures
  atLeast(2, Coll(owner1, owner2, owner3))
}
```

Or for maximum security (3-of-3):
```scala
atLeast(3, Coll(owner1, owner2, owner3))
// Or equivalently:
allOf(Coll(owner1, owner2, owner3))
```

## Medium Findings

### M-1: No Value Conservation Check
**Severity**: MEDIUM
**Location**: Missing validation

**Description**: Contract doesn't verify that output values match input values (minus fees).

**Recommendation**: Add value conservation:
```scala
{
  val owner1 = PK("...")
  val owner2 = PK("...")
  val owner3 = PK("...")
  
  // Require 2-of-3 signatures
  val authorized = atLeast(2, Coll(owner1, owner2, owner3))
  
  // Ensure value is conserved
  val inputVal = INPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
  val outputVal = OUTPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
  val maxFee = 2000000L  // 0.002 ERG max fee
  val valueConserved = inputVal >= outputVal && (inputVal - outputVal) <= maxFee
  
  sigmaProp(valueConserved) && authorized
}
```

### M-2: Hardcoded Signers
**Severity**: MEDIUM

**Description**: Signer public keys are hardcoded, making the wallet non-upgradeable if keys need to be rotated.

**Recommendation**: Store signers in registers:
```scala
{
  val signers = SELF.R4[Coll[SigmaProp]].get
  val threshold = SELF.R5[Int].get
  
  atLeast(threshold, signers)
}
```

## Low/Informational

### I-1: Missing Token Handling
**Severity**: INFO

**Description**: Contract doesn't handle tokens explicitly. If box contains tokens, they may be lost if not properly transferred.

**Recommendation**: Document that external logic must handle token transfers, or add explicit checks.

### I-2: No Emergency Recovery
**Severity**: INFO

**Description**: If K keys are lost (where K >= 2), funds are permanently locked.

**Recommendation**: Consider adding time-delayed emergency recovery:
```scala
{
  val normalAuth = atLeast(2, Coll(owner1, owner2, owner3))
  val emergencyDeadline = SELF.creationInfo._1 + 525600  // ~1 year
  val emergencyKey = PK("...")
  val emergency = HEIGHT > emergencyDeadline && emergencyKey
  
  normalAuth || sigmaProp(emergency)
}
```

## Secure Implementation
```scala
{
  // 2-of-3 Multi-Signature Wallet with Value Protection
  
  val owner1 = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9...")
  val owner2 = PK("9fMPy1XY3GW1CGJ45rNf4Bx8nME1zNuF5jGd24vQvR...")
  val owner3 = PK("9hU8KLzGdQp4V3mR2WYn7Fq1xJtK9pT6eH2sNc8...")
  
  // Require at least 2 signatures
  val authorized = atLeast(2, Coll(owner1, owner2, owner3))
  
  // Verify value conservation
  val inputValue = INPUTS.fold(0L, {(acc: Long, box: Box) => acc + box.value})
  val outputValue = OUTPUTS.fold(0L, {(acc: Long, box: Box) => acc + box.value})
  val reasonableFee = 2000000L  // Max 0.002 ERG fee
  val valueOk = inputValue >= outputValue && 
                (inputValue - outputValue) <= reasonableFee
  
  sigmaProp(valueOk) && authorized
}
```

## Conclusion

**CRITICAL ISSUE REQUIRES IMMEDIATE FIX**

The current implementation is completely insecure and should NOT be deployed. The use of `anyOf` instead of `atLeast` means this is functionally equivalent to a single-signature wallet, not a multi-signature wallet.

**Required Actions**:
1. ❌ DO NOT deploy current contract
2. ✅ Replace `anyOf` with `atLeast(2, ...)`
3. ✅ Add value conservation checks
4. ✅ Test thoroughly with multiple signature scenarios
5. ✅ Document signature requirements clearly

**Risk Assessment**:
- **Current**: CRITICAL - Funds can be stolen by single compromised key
- **After Fix**: LOW - Secure 2-of-3 multi-sig with value protection