# Example 2: Multi-Signature Wallet Audit

## Contract Under Review

```scala
{
  // 2-of-3 multisig wallet
  val pubKey1 = SELF.R4[GroupElement].get
  val pubKey2 = SELF.R5[GroupElement].get  
  val pubKey3 = SELF.R6[GroupElement].get
  
  val sig1 = proveDlog(pubKey1)
  val sig2 = proveDlog(pubKey2)
  val sig3 = proveDlog(pubKey3)
  
  atLeast(2, Coll(sig1, sig2, sig3))
}
```

## Audit Analysis

### Contract Purpose
A 2-of-3 multi-signature wallet requiring any 2 out of 3 authorized signers to approve a transaction.

### Security Review

#### ✅ **PASS**: Threshold Signature Pattern
- Correctly uses `atLeast(2, ...)` for 2-of-3 threshold
- Proper use of Coll to group sigma propositions
- Threshold signature is a native ErgoScript feature (Sigma Protocol)

#### ⚠️ **HIGH**: Register Access Without Validation
**Issue**: All three `.get` calls can fail if registers are not set
**Impact**: Transaction creation will fail, but no funds can be stolen
**Severity**: High because it affects usability and could lock funds if box is malformed
**Recommendation**:
```scala
{
  // Validate all registers exist before accessing
  val registersValid = SELF.R4[GroupElement].isDefined && 
                       SELF.R5[GroupElement].isDefined && 
                       SELF.R6[GroupElement].isDefined
  
  registersValid && {
    val pubKey1 = SELF.R4[GroupElement].get
    val pubKey2 = SELF.R5[GroupElement].get  
    val pubKey3 = SELF.R6[GroupElement].get
    
    val sig1 = proveDlog(pubKey1)
    val sig2 = proveDlog(pubKey2)
    val sig3 = proveDlog(pubKey3)
    
    atLeast(2, Coll(sig1, sig2, sig3))
  }
}
```

#### ℹ️ **INFO**: Value Conservation
**Finding**: No explicit output validation
**Recommendation**: For high-value wallets, add output constraints:
```scala
{
  // ... signature verification ...
  
  val validSignatures = atLeast(2, Coll(sig1, sig2, sig3))
  
  // Optional: ensure funds go to expected outputs
  val validOutputs = OUTPUTS.size >= 1 && 
                     OUTPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value }) >= 
                     SELF.value - 1000000L  // Allow for fees
  
  validSignatures && validOutputs
}
```

#### ✅ **PASS**: Cryptographic Security
- Sigma protocol threshold signatures are cryptographically sound
- No custom crypto that could be vulnerable
- Proper use of proveDlog for each public key

#### ⚠️ **MEDIUM**: No Key Rotation Mechanism
**Issue**: Once deployed, the 3 public keys cannot be changed
**Impact**: If a key is compromised, funds must be moved to new multisig
**Recommendation**: Consider a more advanced pattern with key rotation capability:
```scala
{
  val currentKeySet = SELF.R4[Coll[GroupElement]].get
  val threshold = SELF.R7[Int].get  // Configurable threshold
  
  val signatures = currentKeySet.map { (pk: GroupElement) => proveDlog(pk) }
  
  val validSpend = atLeast(threshold, signatures)
  
  // Allow key rotation if all current keys agree
  val isKeyRotation = OUTPUTS(0).R4[Coll[GroupElement]].isDefined && 
                      OUTPUTS(0).propositionBytes == SELF.propositionBytes
  
  val keyRotationApproved = atLeast(currentKeySet.size, signatures)  // All keys required
  
  validSpend || (isKeyRotation && keyRotationApproved)
}
```

### Advanced Multi-Sig Patterns

#### Option 1: M-of-N with Dynamic Threshold
```scala
{
  val pubKeys = SELF.R4[Coll[GroupElement]].get
  val requiredSigs = SELF.R5[Int].get  // Threshold stored in register
  
  // Ensure threshold is valid
  val validThreshold = requiredSigs > 0 && requiredSigs <= pubKeys.size
  
  validThreshold && {
    val sigs = pubKeys.map { (pk: GroupElement) => proveDlog(pk) }
    atLeast(requiredSigs, sigs)
  }
}
```

#### Option 2: Weighted Multi-Sig
```scala
{
  // Each key has a weight, need total weight >= threshold
  val pubKey1 = SELF.R4[GroupElement].get
  val pubKey2 = SELF.R5[GroupElement].get  
  val pubKey3 = SELF.R6[GroupElement].get
  
  val weight1 = 50  // Owner: 50%
  val weight2 = 30  // Admin: 30%
  val weight3 = 20  // Operator: 20%
  val threshold = 60  // Need 60% to approve
  
  val sig1 = proveDlog(pubKey1)
  val sig2 = proveDlog(pubKey2)
  val sig3 = proveDlog(pubKey3)
  
  // Check different combinations
  val twoOfThree = (sig1 && sig2) || (sig1 && sig3) || (sig2 && sig3)
  
  // Note: ErgoScript doesn't have native weighted threshold,
  // so we enumerate valid combinations
  sig1 || (sig2 && sig3)  // Owner alone OR Admin+Operator
}
```

## Testing Recommendations

### Unit Tests
1. **Basic Functionality**:
   - [ ] Test with exactly 2 signatures (should succeed)
   - [ ] Test with all 3 signatures (should succeed)
   - [ ] Test with only 1 signature (should fail)
   - [ ] Test with 0 signatures (should fail)

2. **Key Combinations**:
   - [ ] Sig1 + Sig2 ✓
   - [ ] Sig1 + Sig3 ✓
   - [ ] Sig2 + Sig3 ✓
   - [ ] Each single signature alone ✗

3. **Register Validation**:
   - [ ] All registers present and valid (should succeed)
   - [ ] Missing R4 (should fail)
   - [ ] Missing R5 (should fail)
   - [ ] Missing R6 (should fail)
   - [ ] Wrong type in register (should fail at compilation)

### Integration Tests
4. **Transaction Flow**:
   - [ ] Create multisig box
   - [ ] Collect signatures from 2 parties
   - [ ] Build and submit transaction
   - [ ] Verify funds transferred correctly

5. **Security Tests**:
   - [ ] Attempt signature forgery (should fail)
   - [ ] Attempt replay attack with old signatures (should fail)
   - [ ] Test with key not in the set (should fail)

### Performance Tests
6. **Scalability**:
   - [ ] Test with maximum number of outputs
   - [ ] Test with large value transfers
   - [ ] Measure transaction size and cost

## Common Vulnerabilities Checked

| Vulnerability | Status | Notes |
|---------------|--------|-------|
| Signature threshold bypass | ✅ Safe | atLeast() enforces threshold |
| Missing key validation | ⚠️ Needs improvement | Add register existence checks |
| Value extraction | ℹ️ Consider | Add output validation for critical wallets |
| Key compromise | ⚠️ Limited | No key rotation mechanism |
| DoS via malformed box | ⚠️ Possible | Register validation prevents this |
| Signature replay | ✅ Safe | Handled by Ergo protocol |

## Risk Assessment
**Overall Risk**: MEDIUM
- Core threshold signature logic is sound
- Missing input validation could cause usability issues
- No mechanism for key recovery or rotation
- Suitable for moderate-value wallets with trusted key holders

## Deployment Recommendation
⚠️ **CONDITIONAL APPROVAL**: 
- Add register validation before production use
- Implement key rotation for high-value wallets
- Add monitoring for unauthorized spending attempts
- Document key management procedures for signers

## Production-Ready Version

```scala
{
  // 2-of-3 multisig with full validation
  
  // Ensure all required registers exist
  val registersValid = SELF.R4[GroupElement].isDefined && 
                       SELF.R5[GroupElement].isDefined && 
                       SELF.R6[GroupElement].isDefined
  
  registersValid && {
    val pubKey1 = SELF.R4[GroupElement].get
    val pubKey2 = SELF.R5[GroupElement].get  
    val pubKey3 = SELF.R6[GroupElement].get
    
    // Create sigma propositions
    val sig1 = proveDlog(pubKey1)
    val sig2 = proveDlog(pubKey2)
    val sig3 = proveDlog(pubKey3)
    
    // Require 2 of 3 signatures
    val validSignatures = atLeast(2, Coll(sig1, sig2, sig3))
    
    // Optional: Add output validation for high-security scenarios
    val totalOutput = OUTPUTS.fold(0L, { (acc: Long, box: Box) => acc + box.value })
    val valueConserved = totalOutput >= SELF.value - 10000000L  // 0.01 ERG max fee
    
    // Combine all conditions
    validSignatures && sigmaProp(valueConserved)
  }
}
```

---

**Auditor Notes**: This is a standard multisig pattern. The main improvements needed are input validation and consideration of key rotation for long-lived wallets. The cryptographic foundation is solid.
