# Example 4: Crowdfunding Contract Audit (Multi-Stage FSM)

## Contract Under Review

This is a multi-stage crowdfunding contract implementing a Finite State Machine pattern. The contract has three states: FUNDING, SUCCESS, and FAILED.

### Stage 1: Funding Stage

```scala
{
  // Crowdfunding contract - Funding stage
  val campaignDeadline = SELF.R4[Int].get     // Block height deadline
  val fundingGoal = SELF.R5[Long].get          // Target amount in nanoERG
  val projectOwner = SELF.R6[GroupElement].get // Project owner's public key
  
  val isStillFunding = HEIGHT < campaignDeadline
  val currentFunds = SELF.value
  
  // Contributor can contribute more funds
  val isContribution = {
    OUTPUTS(0).propositionBytes == SELF.propositionBytes &&
    OUTPUTS(0).value > SELF.value &&
    OUTPUTS(0).R4[Int].get == campaignDeadline &&
    OUTPUTS(0).R5[Long].get == fundingGoal &&
    OUTPUTS(0).R6[GroupElement].get == projectOwner
  }
  
  // Transition to success if goal reached
  val isSuccess = {
    currentFunds >= fundingGoal &&
    OUTPUTS(0).R4[Int].get == campaignDeadline &&
    OUTPUTS(0).R5[Long].get == fundingGoal &&
    OUTPUTS(0).R6[GroupElement].get == projectOwner &&
    OUTPUTS(0).value == SELF.value
  }
  
  // Transition to failed if deadline passed and goal not reached
  val isFailed = {
    HEIGHT >= campaignDeadline &&
    currentFunds < fundingGoal &&
    OUTPUTS(0).R7[Coll[GroupElement]].isDefined  // Contributors list
  }
  
  sigmaProp(isStillFunding && (isContribution || isSuccess || isFailed))
}
```

### Stage 2: Success Stage

```scala
{
  // Crowdfunding contract - Success stage
  val projectOwner = SELF.R6[GroupElement].get
  
  // Owner can withdraw funds
  proveDlog(projectOwner)
}
```

### Stage 3: Failed Stage

```scala
{
  // Crowdfunding contract - Failed stage
  val contributors = SELF.R7[Coll[GroupElement]].get
  val refundAmounts = SELF.R8[Coll[Long]].get
  
  // Anyone can trigger refund process
  val validRefund = {
    contributors.size == refundAmounts.size &&
    OUTPUTS.size >= contributors.size &&
    contributors.indices.forall { (i: Int) =>
      OUTPUTS(i).propositionBytes == proveDlog(contributors(i)).propBytes &&
      OUTPUTS(i).value >= refundAmounts(i)
    }
  }
  
  sigmaProp(validRefund)
}
```

## Audit Analysis

### Contract Purpose
A decentralized crowdfunding platform where:
1. **Funding Stage**: Contributors can add funds until deadline
2. **Success Stage**: If goal reached, project owner can withdraw all funds
3. **Failed Stage**: If deadline passes without reaching goal, contributors get refunds

### Architecture Review

#### FSM State Transitions
```
    ┌─────────────┐
    │   FUNDING   │ ◄──── Initial State
    └──────┬──────┘
           │
           ├──► [Goal Reached] ──────────► ┌─────────┐
           │                               │ SUCCESS │
           │                               └─────────┘
           │                                     │
           │                                     ▼
           │                              [Owner Withdraws]
           │
           └──► [Deadline Passed] ───────► ┌─────────┐
                [Goal Not Reached]         │ FAILED  │
                                            └─────────┘
                                                  │
                                                  ▼
                                           [Contributors Refunded]
```

### Critical Security Review

#### 🔴 **CRITICAL**: Missing State Identifier
**Issue**: No explicit state field to distinguish between stages
**Impact**: Contract can transition to wrong state, funds could be locked or stolen
**Severity**: Critical - fundamental FSM flaw

**Problem**: The contract relies on implicit state (which script is used), but doesn't prevent malicious state transitions.

**Fix**: Add explicit state tracking:
```scala
// In FUNDING stage
val STATE_FUNDING = 1
val STATE_SUCCESS = 2  
val STATE_FAILED = 3

val currentState = SELF.R9[Int].getOrElse(STATE_FUNDING)
```

#### 🔴 **CRITICAL**: No Contributor Tracking in Funding Stage
**Issue**: Funding stage doesn't record who contributed what amounts
**Impact**: If campaign fails, there's no way to refund contributors
**Severity**: Critical - funds could be permanently locked

**Fix**: Track contributors and amounts:
```scala
{
  // Funding stage with contributor tracking
  val campaignDeadline = SELF.R4[Int].get
  val fundingGoal = SELF.R5[Long].get
  val projectOwner = SELF.R6[GroupElement].get
  val currentContributors = SELF.R7[Coll[GroupElement]].getOrElse(Coll[GroupElement]())
  val currentAmounts = SELF.R8[Coll[Long]].getOrElse(Coll[Long]())
  
  val isContribution = {
    val contributorPK = INPUTS(1).R4[GroupElement].get  // From contributor's input
    val contributionAmount = OUTPUTS(0).value - SELF.value
    
    OUTPUTS(0).propositionBytes == SELF.propositionBytes &&
    OUTPUTS(0).value > SELF.value &&
    OUTPUTS(0).R7[Coll[GroupElement]].get == currentContributors.append(Coll(contributorPK)) &&
    OUTPUTS(0).R8[Coll[Long]].get == currentAmounts.append(Coll(contributionAmount))
  }
  
  // ... rest of contract
}
```

#### 🔴 **CRITICAL**: Race Condition in State Transitions
**Issue**: Multiple people could trigger different state transitions simultaneously
**Impact**: Contract could split into multiple incompatible states
**Severity**: Critical - state machine can break

**Example**: 
- Transaction A: Contribution bringing total to goal → SUCCESS
- Transaction B: Deadline passes, total below goal → FAILED
- Both could be valid simultaneously!

**Fix**: Add transition guards with block height checks:
```scala
val canTransitionToSuccess = currentFunds >= fundingGoal && HEIGHT < campaignDeadline
val canTransitionToFailed = HEIGHT >= campaignDeadline && currentFunds < fundingGoal

// These are mutually exclusive
```

#### ⚠️ **HIGH**: Missing Index Bounds in Failed Stage
**Issue**: `OUTPUTS(i)` access without bounds checking
**Impact**: Transaction will fail if not enough outputs
**Severity**: High - DoS, refunds become impossible

**Fix**:
```scala
val validRefund = {
  contributors.size == refundAmounts.size &&
  OUTPUTS.size >= contributors.size &&
  contributors.indices.forall { (i: Int) =>
    i < OUTPUTS.size &&  // Add bounds check
    OUTPUTS(i).propositionBytes == proveDlog(contributors(i)).propBytes &&
    OUTPUTS(i).value >= refundAmounts(i)
  }
}
```

#### ⚠️ **HIGH**: No Validation of Contribution Amount
**Issue**: Contribution amount is not validated (could be negative value change)
**Impact**: Malicious actor could decrease box value
**Severity**: High - value extraction possible

**Fix**:
```scala
val contributionAmount = OUTPUTS(0).value - SELF.value
val validContribution = contributionAmount > 0  // Must be positive
```

#### ⚠️ **HIGH**: Missing Box Script Validation in Transitions
**Issue**: Doesn't verify that output box has correct script for new state
**Impact**: Transition to state with wrong guard script
**Severity**: High - state machine can be corrupted

**Fix**: Store script hashes for each state and validate:
```scala
val successScriptHash = SELF.R10[Coll[Byte]].get
val failedScriptHash = SELF.R11[Coll[Byte]].get

val isSuccess = {
  currentFunds >= fundingGoal &&
  blake2b256(OUTPUTS(0).propositionBytes) == successScriptHash &&
  // ... other validations
}
```

#### ⚠️ **MEDIUM**: Refund Amounts Not Validated
**Issue**: In failed stage, refundAmounts are taken from register without validation
**Impact**: Incorrect refund amounts could be set during transition
**Severity**: Medium - requires malicious state transition, but serious if exploited

**Fix**: Validate during transition to failed state:
```scala
val totalRefunds = refundAmounts.fold(0L, { (acc: Long, amt: Long) => acc + amt })
val refundsMatchValue = totalRefunds <= SELF.value
```

#### ⚠️ **MEDIUM**: No Minimum Contribution Limit
**Issue**: Allows dust contributions (very small amounts)
**Impact**: State bloat, expensive refunds
**Severity**: Medium - UX and efficiency issue

**Fix**:
```scala
val minContribution = 1000000L  // 0.001 ERG minimum
val validContribution = contributionAmount >= minContribution
```

#### ℹ️ **INFO**: Success Stage Too Simple
**Issue**: Owner can withdraw entire amount in one transaction, no validation
**Impact**: Owner could drain funds to unintended destination
**Recommendation**: Add output validation:
```scala
{
  val projectOwner = SELF.R6[GroupElement].get
  val projectRecipient = SELF.R12[Coll[Byte]].get  // Expected output address
  
  val ownerSigned = proveDlog(projectOwner)
  val fundsToCorrectAddress = OUTPUTS(0).propositionBytes == projectRecipient
  
  ownerSigned && sigmaProp(fundsToCorrectAddress)
}
```

### Production-Ready Multi-Stage Crowdfunding Contract

#### Funding Stage (Improved)

```scala
{
  // ===== Configuration from Registers =====
  val STATE_FUNDING = 1
  val STATE_SUCCESS = 2
  val STATE_FAILED = 3
  
  val state = SELF.R4[Int].get
  val campaignDeadline = SELF.R5[Int].get
  val fundingGoal = SELF.R6[Long].get
  val projectOwner = SELF.R7[GroupElement].get
  val contributors = SELF.R8[Coll[GroupElement]].getOrElse(Coll[GroupElement]())
  val contributions = SELF.R9[Coll[Long]].getOrElse(Coll[Long]())
  val successScriptHash = SELF.R10[Coll[Byte]].get
  val failedScriptHash = SELF.R11[Coll[Byte]].get
  
  // Minimum contribution to prevent spam
  val minContribution = 100000L  // 0.0001 ERG
  
  // ===== State must be FUNDING =====
  sigmaProp(state == STATE_FUNDING) && {
    
    val currentFunds = SELF.value
    val isBeforeDeadline = HEIGHT < campaignDeadline
    val goalReached = currentFunds >= fundingGoal
    val deadlinePassed = HEIGHT >= campaignDeadline
    
    // ===== Option 1: Accept Contribution =====
    val isContribution = {
      isBeforeDeadline && !goalReached && OUTPUTS.size >= 1 && {
        val output = OUTPUTS(0)
        val newValue = output.value
        val contributionAmount = newValue - SELF.value
        
        // Validate contribution
        val validAmount = contributionAmount >= minContribution
        val scriptPreserved = output.propositionBytes == SELF.propositionBytes
        val statePreserved = output.R4[Int].get == STATE_FUNDING
        val deadlinePreserved = output.R5[Int].get == campaignDeadline
        val goalPreserved = output.R6[Long].get == fundingGoal
        val ownerPreserved = output.R7[GroupElement].get == projectOwner
        
        // Get contributor public key from transaction context
        // Assume contributor adds their PK in a data input or separate input box
        val contributorPK = INPUTS(1).R4[GroupElement].get
        
        // Update contributor lists
        val newContributors = contributors.append(Coll(contributorPK))
        val newContributions = contributions.append(Coll(contributionAmount))
        val contributorsUpdated = output.R8[Coll[GroupElement]].get == newContributors
        val contributionsUpdated = output.R9[Coll[Long]].get == newContributions
        
        validAmount && scriptPreserved && statePreserved && 
        deadlinePreserved && goalPreserved && ownerPreserved &&
        contributorsUpdated && contributionsUpdated
      }
    }
    
    // ===== Option 2: Transition to SUCCESS =====
    val isSuccess = {
      goalReached && OUTPUTS.size >= 1 && {
        val output = OUTPUTS(0)
        val correctScript = blake2b256(output.propositionBytes) == successScriptHash
        val stateUpdated = output.R4[Int].get == STATE_SUCCESS
        val deadlinePreserved = output.R5[Int].get == campaignDeadline
        val goalPreserved = output.R6[Long].get == fundingGoal
        val ownerPreserved = output.R7[GroupElement].get == projectOwner
        val valuePreserved = output.value == SELF.value
        
        correctScript && stateUpdated && deadlinePreserved && 
        goalPreserved && ownerPreserved && valuePreserved
      }
    }
    
    // ===== Option 3: Transition to FAILED =====
    val isFailed = {
      deadlinePassed && !goalReached && OUTPUTS.size >= 1 && {
        val output = OUTPUTS(0)
        val correctScript = blake2b256(output.propositionBytes) == failedScriptHash
        val stateUpdated = output.R4[Int].get == STATE_FAILED
        val contributorsPreserved = output.R8[Coll[GroupElement]].get == contributors
        val contributionsPreserved = output.R9[Coll[Long]].get == contributions
        val valuePreserved = output.value == SELF.value
        
        // Validate refund amounts match contributions
        val totalContributions = contributions.fold(0L, { (a: Long, b: Long) => a + b })
        val refundsValid = totalContributions <= SELF.value
        
        correctScript && stateUpdated && contributorsPreserved && 
        contributionsPreserved && valuePreserved && refundsValid
      }
    }
    
    sigmaProp(isContribution || isSuccess || isFailed)
  }
}
```

#### Success Stage (Improved)

```scala
{
  // ===== Configuration =====
  val STATE_SUCCESS = 2
  
  val state = SELF.R4[Int].get
  val projectOwner = SELF.R7[GroupElement].get
  
  // ===== Validation =====
  sigmaProp(state == STATE_SUCCESS) && proveDlog(projectOwner)
  
  // Optional: Add destination validation
  // val projectAddress = SELF.R12[Coll[Byte]].get
  // val correctDestination = OUTPUTS(0).propositionBytes == projectAddress
  // sigmaProp(state == STATE_SUCCESS && correctDestination) && proveDlog(projectOwner)
}
```

#### Failed Stage (Improved)

```scala
{
  // ===== Configuration =====
  val STATE_FAILED = 3
  
  val state = SELF.R4[Int].get
  val contributors = SELF.R8[Coll[GroupElement]].get
  val refundAmounts = SELF.R9[Coll[Long]].get
  
  // ===== Validation =====
  sigmaProp(state == STATE_FAILED) && {
    
    // Validate lists match
    val listsValid = contributors.size == refundAmounts.size &&
                     contributors.size > 0
    
    // Validate outputs
    val outputsValid = OUTPUTS.size >= contributors.size
    
    listsValid && outputsValid && {
      
      // Validate each refund output
      val allRefundsCorrect = contributors.indices.forall { (i: Int) =>
        i < OUTPUTS.size && {
          val output = OUTPUTS(i)
          val correctRecipient = output.propositionBytes == proveDlog(contributors(i)).propBytes
          val correctAmount = output.value >= refundAmounts(i)
          correctRecipient && correctAmount
        }
      }
      
      // Validate total refunds don't exceed available funds
      val totalRefunds = refundAmounts.fold(0L, { (a: Long, b: Long) => a + b })
      val refundsSafe = totalRefunds <= SELF.value
      
      sigmaProp(allRefundsCorrect && refundsSafe)
    }
  }
}
```

## Testing Recommendations

### Funding Stage Tests

1. **Basic Contributions**:
   - [ ] First contribution (empty lists → 1 contributor)
   - [ ] Multiple sequential contributions
   - [ ] Contribution meeting exact goal
   - [ ] Contribution exceeding goal

2. **Invalid Contributions**:
   - [ ] Contribution below minimum (should fail)
   - [ ] Contribution after deadline (should fail)
   - [ ] Contribution after goal reached (should fail)
   - [ ] Missing contributor PK (should fail)

3. **State Transitions**:
   - [ ] Transition to SUCCESS when goal reached before deadline
   - [ ] Transition to FAILED when deadline passes without goal
   - [ ] Attempt transition to FAILED while still funding (should fail)
   - [ ] Attempt transition to SUCCESS without reaching goal (should fail)

4. **Race Conditions**:
   - [ ] Simultaneous contributions
   - [ ] Contribution and transition in same block
   - [ ] Multiple transition attempts

### Success Stage Tests

5. **Withdrawal**:
   - [ ] Owner withdraws all funds
   - [ ] Owner withdraws partial funds
   - [ ] Non-owner attempts withdrawal (should fail)
   - [ ] Withdrawal without owner signature (should fail)

### Failed Stage Tests

6. **Refunds**:
   - [ ] Refund all contributors correctly
   - [ ] Refund with extra outputs (non-contributor outputs)
   - [ ] Partial refunds (some contributors refunded)
   - [ ] Refund amounts mismatch (should fail)

7. **Attack Scenarios**:
   - [ ] Manipulate refund amounts (should fail)
   - [ ] Wrong recipient addresses (should fail)
   - [ ] Insufficient funds for refunds (should fail)

### Integration Tests

8. **Full Campaign Flows**:
   - [ ] Successful campaign: Fund → Reach goal → Owner withdraws
   - [ ] Failed campaign: Fund → Deadline passes → Contributors refunded
   - [ ] Campaign with many contributors (stress test)
   - [ ] Campaign with one large contribution
   - [ ] Campaign that nearly reaches goal

## Common Vulnerabilities Checklist

| Vulnerability | Original | Fixed |
|---------------|----------|-------|
| Missing state identifier | ❌ Critical | ✅ Fixed with R4 |
| No contributor tracking | ❌ Critical | ✅ Fixed with R8, R9 |
| Race conditions in transitions | ❌ Critical | ✅ Fixed with mutually exclusive guards |
| Array bounds violations | ❌ High | ✅ Fixed with size checks |
| Negative contributions | ⚠️ Possible | ✅ Fixed with min validation |
| Value conservation | ⚠️ Partial | ✅ Fixed with explicit checks |
| Script hash validation | ❌ Missing | ✅ Fixed with blake2b256 |
| Refund amount validation | ⚠️ Weak | ✅ Fixed with total validation |

## Risk Assessment

### Original Contract
**Overall Risk**: CRITICAL 🔴
- Fundamental FSM flaws
- No contributor tracking (refunds impossible)
- Race conditions in state transitions
- Multiple validation gaps
- **DO NOT DEPLOY**

### Fixed Contract
**Overall Risk**: LOW 🟢
- Explicit state tracking
- Complete contributor accounting
- Safe state transitions
- Comprehensive validation
- Ready for testnet deployment and further testing

## Deployment Recommendation

### Original Version
❌ **DO NOT DEPLOY** - Fundamental design flaws

### Fixed Version  
⚠️ **CONDITIONAL APPROVAL** after:
1. Extensive testing on testnet with real users
2. Stress testing with many contributors
3. Security audit of off-chain components
4. Economic analysis of attack vectors
5. Front-end safety checks for user contributions
6. Monitoring and emergency response plan

## Advanced Considerations

### Partial Refunds
Consider allowing partial refunds even if campaign succeeds, if project doesn't need all funds:

```scala
val partialRefundEnabled = SELF.R13[Boolean].getOrElse(false)
// Allow owner to trigger partial refunds while keeping needed amount
```

### Contribution Limits
Add maximum contribution per address to prevent whale control:

```scala
val maxContributionPerAddress = SELF.R14[Long].get
val existingContribution = if (contributors.exists({ (pk: GroupElement) => pk == contributorPK })) {
  val index = contributors.indexOf(contributorPK, 0)
  contributions(index)
} else {
  0L
}
val totalContribution = existingContribution + contributionAmount
val underLimit = totalContribution <= maxContributionPerAddress
```

### Milestone-Based Releases
For large campaigns, release funds in milestones:

```scala
val milestones = SELF.R15[Coll[Long]].get  // Amounts for each milestone
val milestonesReached = SELF.R16[Int].getOrElse(0)  // Number of milestones completed
// Allow partial withdrawals as milestones are verified
```

---

**Auditor's Note**: Multi-stage contracts are complex and require careful state management. This example shows how critical it is to validate state transitions and maintain complete accounting throughout the contract lifecycle. Always consider the entire state machine as a whole, not just individual states in isolation.
