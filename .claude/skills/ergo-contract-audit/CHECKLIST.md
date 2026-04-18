# Ergo Contract Security Testing Checklist

Use this checklist when auditing ErgoScript contracts to ensure comprehensive security coverage.

## 📋 Pre-Audit Preparation

- [ ] Understand contract purpose and intended behavior
- [ ] Identify all state transitions (if multi-stage)
- [ ] Document assumptions and invariants
- [ ] Review similar contract patterns for known issues
- [ ] Gather test scenarios from developers

## 🔍 Static Analysis

### Type Safety
- [ ] All types properly declared and used
- [ ] No unsafe type conversions (toByte, toShort, toInt without bounds checking)
- [ ] Option types handled safely (no unchecked `.get` calls)
- [ ] Collection types used correctly

### Arithmetic Operations
- [ ] No integer overflow possibilities
  - [ ] Check all multiplications
  - [ ] Check all additions
  - [ ] Verify large value handling
- [ ] No integer underflow possibilities
  - [ ] Check all subtractions
  - [ ] Verify minimum value handling
- [ ] Use of BigInt where appropriate for financial calculations
- [ ] Proper modular arithmetic for UnsignedBigInt

### Access Patterns
- [ ] Array/Collection bounds checked before access
  - [ ] `INPUTS(i)` - verify i < INPUTS.size
  - [ ] `OUTPUTS(i)` - verify i < OUTPUTS.size
  - [ ] Collection indexing - verify bounds
- [ ] Register access validated
  - [ ] R4-R9 existence checked before `.get`
  - [ ] Correct types expected in registers
- [ ] Token access validated
  - [ ] Check tokens.size before accessing tokens(i)
  - [ ] Validate token ID before operations

## 💰 Value Conservation

### ERG Balance
- [ ] Input ERG equals output ERG (minus fees)
- [ ] Maximum fee specified and reasonable
- [ ] No unauthorized value extraction
- [ ] Box value always positive
- [ ] Total output value validated

### Token Conservation
- [ ] Input tokens equal output tokens (or intentional burn)
- [ ] Token IDs preserved correctly
- [ ] No unauthorized token creation
- [ ] Token amounts validated
- [ ] Multi-token scenarios handled

## 🔐 Spending Conditions

### SigmaProp Validation
- [ ] All required signatures enforced
- [ ] Correct use of `&&`, `||` operators
- [ ] No accidentally negated conditions
- [ ] Threshold signatures configured correctly
- [ ] proveDlog used properly for public keys

### Logic Completeness
- [ ] All execution paths lead to valid sigma prop
- [ ] No spending condition bypass possible
- [ ] Edge cases covered (zero values, exact boundaries)
- [ ] Time-lock conditions properly enforced
- [ ] State-dependent conditions validated

## 🔄 State Management (Multi-Stage Contracts)

### State Identification
- [ ] Explicit state identifier present
- [ ] State stored in consistent register
- [ ] State transitions enumerated completely
- [ ] Invalid states impossible to reach

### Transition Validation
- [ ] Only valid transitions allowed
- [ ] Transition guards mutually exclusive
- [ ] No race conditions between transitions
- [ ] State data preserved correctly
- [ ] Output script validated for new state

### Data Persistence
- [ ] Critical data preserved across transitions
- [ ] Register contents validated
- [ ] Collection data (contributors, etc.) maintained
- [ ] Configuration parameters immutable when needed

## 🎫 Token Management

### Token Operations
- [ ] Token ID verified in all operations
- [ ] Token amounts validated (no negative, no overflow)
- [ ] Minting conditions properly restricted
- [ ] Burning conditions validated
- [ ] NFT uniqueness enforced

### Multi-Token Scenarios
- [ ] Multiple token types handled correctly
- [ ] Token filtering logic sound
- [ ] No token substitution attacks
- [ ] Token conservation per type

## 🔢 Context Variables

### Usage Validation
- [ ] HEIGHT used appropriately for time-locks
- [ ] SELF referenced correctly
- [ ] INPUTS accessed safely
- [ ] OUTPUTS accessed safely
- [ ] Context variables not manipulable

### Box Properties
- [ ] Box.value validated
- [ ] Box.propositionBytes checked when needed
- [ ] Box.tokens accessed safely
- [ ] Box registers (R4-R9) validated

## 🛡️ Cryptographic Operations

### Sigma Protocols
- [ ] proveDlog used correctly
- [ ] proveDHTuple used correctly (if applicable)
- [ ] Threshold signatures (atLeast) configured properly
- [ ] Compound propositions composed correctly

### Hash Functions
- [ ] blake2b256 used appropriately
- [ ] sha256 used appropriately
- [ ] Hash outputs validated
- [ ] No hash collision assumptions

## 📊 Collection Operations

### Safety
- [ ] No unbounded operations (map, fold, etc. on unlimited collections)
- [ ] Existence checks before reduce/fold
- [ ] Size limits considered
- [ ] forall/exists used correctly

### Efficiency
- [ ] Unnecessary iterations avoided
- [ ] Collection filtering optimized
- [ ] Early termination where possible

## ⚡ Performance & Gas

### Complexity
- [ ] Script complexity reasonable
- [ ] No exponential operations
- [ ] Collection operations bounded
- [ ] Redundant computations eliminated

### Cost Estimation
- [ ] Transaction size estimated
- [ ] Execution cost within limits
- [ ] Fee requirements documented

## 🐛 Common Vulnerabilities

### Critical Checks
- [ ] ❌ No arithmetic overflow/underflow
- [ ] ❌ No value extraction vulnerabilities
- [ ] ❌ No spending condition bypass
- [ ] ❌ No state corruption possible
- [ ] ❌ No token theft possible

### Security Properties
- [ ] ✅ Value conservation enforced
- [ ] ✅ Token conservation enforced
- [ ] ✅ All signatures required
- [ ] ✅ Time-locks enforced
- [ ] ✅ State transitions valid

## 🧪 Testing Requirements

### Unit Tests
- [ ] All execution paths tested
- [ ] Edge cases covered
  - [ ] Zero values
  - [ ] Maximum values
  - [ ] Minimum values
  - [ ] Boundary conditions
- [ ] Failure scenarios tested
- [ ] Register variations tested

### Integration Tests
- [ ] Multi-transaction flows tested
- [ ] State transitions validated
- [ ] Interaction with other contracts
- [ ] Off-chain component integration

### Adversarial Testing
- [ ] Attack vectors enumerated
- [ ] Exploitation attempts tested
- [ ] Economic incentive analysis
- [ ] Front-running scenarios
- [ ] Griefing attacks considered

## 📝 Documentation Review

### Code Quality
- [ ] Contract purpose documented
- [ ] Complex logic explained
- [ ] Assumptions stated clearly
- [ ] Register usage documented
- [ ] Constants explained

### Security Notes
- [ ] Known limitations documented
- [ ] Trust assumptions stated
- [ ] Upgrade/migration path defined
- [ ] Emergency procedures outlined

## 🚀 Deployment Readiness

### Pre-Deployment
- [ ] All critical issues resolved
- [ ] All high issues resolved
- [ ] Medium issues addressed or documented
- [ ] Test coverage >= 90%
- [ ] Testnet deployment successful

### Monitoring
- [ ] Event monitoring setup
- [ ] Alert thresholds defined
- [ ] Incident response plan ready
- [ ] Upgrade mechanism (if needed)

## ✅ Final Checklist

- [ ] No critical vulnerabilities
- [ ] No high severity issues
- [ ] Medium issues acceptable or fixed
- [ ] Comprehensive testing completed
- [ ] Documentation complete
- [ ] Testnet validation successful
- [ ] Second auditor review (for high value)
- [ ] Community feedback incorporated
- [ ] Emergency response plan ready
- [ ] Deployment approval obtained

## 📊 Risk Assessment Matrix

| Category | Risk Level | Status | Notes |
|----------|-----------|--------|-------|
| Arithmetic Safety | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |
| Value Conservation | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |
| Spending Conditions | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |
| State Management | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |
| Token Management | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |
| Cryptography | ⬜ Critical / ⬜ High / ⬜ Medium / ⬜ Low | ⬜ Pass / ⬜ Fail | |

## 🎓 Severity Guidelines

| Severity | Deployment Decision |
|----------|-------------------|
| **Critical** 🔴 | DO NOT DEPLOY - Fix immediately |
| **High** ⚠️ | Fix before deployment |
| **Medium** ⚠️ | Fix before deployment or document risk |
| **Low** ℹ️ | Consider fixing, can deploy |
| **Info** ℹ️ | Optional improvement |

---

**How to Use This Checklist:**

1. **Before Audit**: Review all sections to understand scope
2. **During Audit**: Check off items as you verify them
3. **Document Findings**: Note any issues in the "Notes" column
4. **Prioritize**: Fix critical/high issues first
5. **Re-Audit**: Re-check items after fixes
6. **Final Review**: Ensure all critical checks pass before deployment

**Remember**: This checklist is a guide, not exhaustive. Use your judgment and expertise to identify contract-specific risks.

---

**Version**: 1.0  
**Last Updated**: December 2025  
**For**: ErgoScript Security Audits
