# Basic Audit Example

This example demonstrates a basic security audit of a simple ErgoScript contract.

---

## Contract to Audit

```ergoscript
{
  val ownerPubKey = SELF.R4[GroupElement].get
  val minAmount = SELF.R5[Long].get
  
  val outputValue = OUTPUTS(0).value
  val validAmount = outputValue >= minAmount
  
  sigmaProp(validAmount)
}
```

---

## Audit Request

```
Please audit this ErgoScript contract for security vulnerabilities:

{
  val ownerPubKey = SELF.R4[GroupElement].get
  val minAmount = SELF.R5[Long].get
  
  val outputValue = OUTPUTS(0).value
  val validAmount = outputValue >= minAmount
  
  sigmaProp(validAmount)
}
```

---

## Expected Audit Output

### Critical Issues

**1. Missing Signature Verification**
- **Severity:** CRITICAL
- **Location:** Line 8
- **Issue:** `ownerPubKey` is extracted from R4 but never used in verification
- **Impact:** Anyone can spend funds without owner authorization
- **Fix:**
  ```ergoscript
  sigmaProp(proveDlog(ownerPubKey) && validAmount)
  ```
- **CWE:** CWE-862 (Missing Authorization)

### High Issues

**2. Missing Output Script Validation**
- **Severity:** HIGH
- **Location:** Line 6
- **Issue:** Output script (propositionBytes) not validated
- **Impact:** Funds could be sent to arbitrary address
- **Fix:**
  ```ergoscript
  val correctScript = OUTPUTS(0).propositionBytes == expectedScript
  sigmaProp(proveDlog(ownerPubKey) && validAmount && correctScript)
  ```

### Medium Issues

**3. No Output Count Validation**
- **Severity:** MEDIUM
- **Issue:** Number of outputs not checked
- **Impact:** Could create unexpected outputs
- **Fix:**
  ```ergoscript
  require(OUTPUTS.size == 1, "Expected exactly 1 output")
  ```

### Low Issues

**4. Missing Error Messages**
- **Severity:** LOW
- **Issue:** No descriptive error messages
- **Impact:** Difficult to debug failures
- **Fix:**
  ```ergoscript
  require(outputValue >= minAmount, "Output below minimum amount")
  ```

**5. No Documentation**
- **Severity:** LOW
- **Issue:** Contract lacks documentation
- **Impact:** Unclear purpose and usage
- **Fix:** Add header documentation explaining R4, R5, and contract purpose

---

## Corrected Contract

```ergoscript
/**
 * Simple Withdrawal Contract
 * 
 * Allows owner to withdraw funds above minimum amount.
 * 
 * Registers:
 * R4: GroupElement - Owner's public key
 * R5: Long - Minimum withdrawal amount (nanoERG)
 */
{
  val ownerPubKey = SELF.R4[GroupElement].get
  val minAmount = SELF.R5[Long].get
  
  // Verify owner signature
  val ownerSigned = proveDlog(ownerPubKey)
  
  // Validate output
  val outputValue = OUTPUTS(0).value
  require(outputValue >= minAmount, "Output below minimum amount")
  
  // Validate output script
  val correctScript = OUTPUTS(0).propositionBytes == ownerPubKey.getEncoded
  
  // Validate output count
  require(OUTPUTS.size == 1, "Expected exactly 1 output")
  
  sigmaProp(ownerSigned && correctScript)
}
```

---

## Summary

**Issues Found:** 5 (1 Critical, 1 High, 1 Medium, 2 Low)

**Recommendations:**
1. **CRITICAL:** Add owner signature verification immediately
2. **HIGH:** Validate output script to prevent fund redirection
3. **MEDIUM:** Add output count validation
4. **LOW:** Add error messages and documentation

**Security Rating:** ⚠️ **UNSAFE** - Do not deploy without fixes

---

## Next Steps

1. Apply all Critical and High fixes
2. Review Medium and Low recommendations
3. Add comprehensive tests
4. Re-audit after changes
5. Test on testnet before mainnet deployment
