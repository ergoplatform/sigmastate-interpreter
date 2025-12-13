# Example: Basic Contract Audit

## User Request
```
Please audit this simple time-lock contract:

{
  val lockDeadline = 1000000L
  val recipient = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9rRcWBTpYf")
  
  sigmaProp(
    HEIGHT > lockDeadline && recipient
  )
}
```

## Claude Response (using ergo-contract-audit skill)

# Ergo Contract Security Audit Report

## Executive Summary
This is a simple time-lock contract with one **HIGH severity** issue and several recommendations. The contract allows funds to be spent by a specific recipient after a certain block height, but has a critical logical flaw.

## Contract Information
- **Contract Name**: Simple Time-Lock
- **Purpose**: Lock funds until a specific block height, then allow recipient to spend
- **Complexity**: Low

## Findings

### HIGH SEVERITY ISSUES

#### H-1: Logical Error in Spending Condition
- **Severity**: HIGH
- **Location**: Line 6 (spending condition)
- **Description**: The spending condition uses `&&` between a boolean condition and a `SigmaProp`, which will not compile or behave as intended. The `HEIGHT > lockDeadline` returns a Boolean, but it's being combined with `sigmaProp` value directly.

**Current Code:**
```scala
sigmaProp(HEIGHT > lockDeadline && recipient)
```

**Issue**: This attempts to combine a Boolean with a SigmaProp, which is a type error.

- **Impact**: Contract will not compile or deploy. If somehow deployed, behavior would be undefined.

- **Recommendation**: Properly combine the conditions using logical operators within sigmaProp:
```scala
sigmaProp(HEIGHT > lockDeadline) && recipient
```

Or more explicitly:
```scala
val timeLockPassed = HEIGHT > lockDeadline
sigmaProp(timeLockPassed) && recipient
```

### MEDIUM SEVERITY ISSUES

#### M-1: Hardcoded Values Reduce Reusability
- **Severity**: MEDIUM
- **Location**: Lines 2-3
- **Description**: The lock deadline and recipient are hardcoded, making the contract non-reusable for different scenarios.

- **Impact**: Each new time-lock requires a new contract deployment, increasing costs and reducing efficiency.

- **Recommendation**: Consider storing these values in registers for runtime configuration:
```scala
{
  val lockDeadline = SELF.R4[Long].get
  val recipient = SELF.R5[SigmaProp].get
  
  sigmaProp(HEIGHT > lockDeadline) && recipient
}
```

### INFORMATIONAL

#### I-1: Missing Input Validation
- **Severity**: INFO
- **Description**: Contract doesn't validate that HEIGHT comparison is meaningful (e.g., lockDeadline > current height at creation).

- **Recommendation**: While this must be checked off-chain during contract creation, document this requirement clearly.

#### I-2: No Emergency Withdrawal
- **Severity**: INFO  
- **Description**: If the recipient loses their private key, funds are permanently locked.

- **Recommendation**: Consider adding an alternative unlock path:
```scala
{
  val lockDeadline = SELF.R4[Long].get
  val recipient = SELF.R5[SigmaProp].get
  val emergencyKey = SELF.R6[SigmaProp].get
  val emergencyDeadline = SELF.R7[Long].get
  
  (sigmaProp(HEIGHT > lockDeadline) && recipient) ||
  (sigmaProp(HEIGHT > emergencyDeadline) && emergencyKey)
}
```

## Corrected Contract
```scala
{
  // Time-lock contract: funds locked until specific height
  val lockDeadline = 1000000L
  val recipient = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE9rRcWBTpYf")
  
  // Check that current height exceeds deadline AND recipient signature is valid
  sigmaProp(HEIGHT > lockDeadline) && recipient
}
```

## Conclusion

**Risk Level**: HIGH (due to compilation error)

The contract has the right concept but contains a critical logical error that prevents proper execution. Once the HIGH severity issue is fixed, this would be a simple, secure time-lock implementation. 

**Recommendation**: Fix the logical error, consider adding register-based configuration for reusability, and document off-chain validation requirements.