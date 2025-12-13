# Example 1: Time-Locked Contract Audit

## Contract Under Review

```scala
{
  val freezeDeadline = 100000  // Block height
  val pkOwner = proveDlog(SELF.R4[GroupElement].get)
  
  val deadlinePassed = HEIGHT > freezeDeadline
  
  deadlinePassed && pkOwner
}
```

## Audit Analysis

### Contract Purpose
This is a simple time-locked contract that freezes funds until a specific block height is reached, after which only the owner (identified by their public key stored in R4) can spend the funds.

### Security Review

#### ✅ **PASS**: Value Conservation
- No explicit value constraints in the script
- **Finding**: While the script doesn't enforce value conservation, the blockchain validates this at the protocol level. However, for clarity and defense-in-depth, consider adding:
```scala
OUTPUTS(0).value >= SELF.value - 1000000  // Allow for fees
```

#### ✅ **PASS**: Spending Conditions
- Requires both time condition AND owner signature
- Uses `&&` operator correctly
- SigmaProp properly constructed

#### ⚠️ **MEDIUM**: Register Access Safety
**Issue**: Uses `.get` on optional register R4 without checking existence
**Impact**: Transaction will fail if R4 is not set, but won't cause security breach
**Recommendation**:
```scala
{
  val freezeDeadline = 100000
  val pkOwner = SELF.R4[GroupElement].get  // Can fail at runtime
  
  val deadlinePassed = HEIGHT > freezeDeadline
  
  deadlinePassed && proveDlog(pkOwner)
}
```

Better approach with explicit check:
```scala
{
  val freezeDeadline = 100000
  
  SELF.R4[GroupElement].isDefined && {
    val pkOwner = SELF.R4[GroupElement].get
    val deadlinePassed = HEIGHT > freezeDeadline
    deadlinePassed && proveDlog(pkOwner)
  }
}
```

#### ✅ **PASS**: Type Safety
- Correct use of `Int` for HEIGHT comparison
- Proper `GroupElement` type for public key
- `proveDlog` correctly applied

#### ℹ️ **INFO**: Time Lock Pattern
- Using block height for time is standard and secure
- Consider making freezeDeadline a named constant for clarity
- Current pattern is simple and effective

### Improved Version

```scala
{
  // Named constants for clarity
  val freezeDeadline: Int = SELF.R5[Int].get
  val pkOwner: GroupElement = SELF.R4[GroupElement].get
  
  // Time condition
  val deadlinePassed = sigmaProp(HEIGHT > freezeDeadline)
  
  // Combine time lock with ownership
  deadlinePassed && proveDlog(pkOwner)
}
```

## Testing Recommendations

1. **Test Cases**:
   - [ ] Attempt to spend before deadline (should fail)
   - [ ] Attempt to spend after deadline with correct signature (should succeed)
   - [ ] Attempt to spend after deadline with wrong signature (should fail)
   - [ ] Test with missing R4 register (should fail gracefully)
   - [ ] Test with wrong type in R4 (should fail at compile/runtime)

2. **Edge Cases**:
   - [ ] Exact boundary: HEIGHT == freezeDeadline
   - [ ] Large value transfers
   - [ ] Multiple outputs from single frozen box

3. **Integration Tests**:
   - [ ] Verify with Ergo Playgrounds
   - [ ] Test with AppKit transaction builder
   - [ ] Validate ErgoTree serialization

## Risk Assessment
**Overall Risk**: LOW
- Basic pattern correctly implemented
- Main issue is potential runtime failure on missing register
- No value extraction vulnerabilities identified
- Cryptographic usage is sound

## Deployment Recommendation
✅ **APPROVED** with minor improvements recommended for production robustness.
