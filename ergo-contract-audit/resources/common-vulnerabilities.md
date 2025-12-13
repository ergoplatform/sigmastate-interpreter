# Common ErgoScript Vulnerabilities

## Critical Vulnerabilities

### 1. Missing Signature Verification
**Risk**: Anyone can spend funds
```scala
// Vulnerable
{
  val deadline = 1000000L
  sigmaProp(HEIGHT > deadline)  // No authentication!
}

// Fixed
{
  val deadline = 1000000L
  val owner = PK("...")
  sigmaProp(HEIGHT > deadline) && owner
}
```

### 2. Value Not Conserved
**Risk**: Attacker steals excess funds
```scala
// Vulnerable
{
  OUTPUTS(0).value >= 1000000  // Doesn't check total!
}

// Fixed
{
  val inVal = INPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
  val outVal = OUTPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
  val fee = 1000000L
  inVal == outVal + fee
}
```

### 3. Token Loss
**Risk**: Tokens destroyed or stolen
```scala
// Vulnerable
{
  OUTPUTS(0).tokens.size > 0  // Doesn't verify conservation!
}

// Fixed
{
  val inputTokens = INPUTS(0).tokens
  val outputTokens = OUTPUTS(0).tokens
  inputTokens == outputTokens
}
```

## High Severity

### 4. Integer Overflow
**Risk**: Arithmetic errors lead to incorrect calculations
```scala
// Vulnerable
{
  val amount = SELF.value * 1000000  // Can overflow Long
  amount > 0
}

// Fixed
{
  val amount = SELF.value.toBigInt * 1000000
  amount <= Long.MaxValue && amount > 0
}
```

### 5. Weak Authentication
**Risk**: Insufficient signature requirements
```scala
// Vulnerable
{
  val sig1 = PK("...")
  val sig2 = PK("...")
  anyOf(Coll(sig1, sig2))  // Only needs 1 signature
}

// Fixed
{
  val sig1 = PK("...")
  val sig2 = PK("...")
  atLeast(2, Coll(sig1, sig2))  // Requires both
}
```

### 6. Unvalidated State Transitions
**Risk**: Invalid state changes accepted
```scala
// Vulnerable
{
  val oldState = SELF.R4[Int].get
  val newState = OUTPUTS(0).R4[Int].get
  newState != oldState  // Doesn't validate transition
}

// Fixed
{
  val oldState = SELF.R4[Int].get
  val newState = OUTPUTS(0).R4[Int].get
  val validTransition = (oldState == 0 && newState == 1) ||
                        (oldState == 1 && newState == 2)
  validTransition
}
```

## Medium Severity

### 7. Incomplete Edge Case Handling
```scala
// Vulnerable
{
  val value = SELF.R4[Long].get  // Throws if empty!
}

// Fixed
{
  val value = if (SELF.R4[Long].isDefined) 
                SELF.R4[Long].get 
              else 
                0L
}
```

### 8. Hardcoded Values
**Risk**: Reduced reusability
```scala
// Not flexible
{
  val deadline = 1000000L  // Hardcoded
}

// Configurable
{
  val deadline = SELF.R4[Long].get  // From register
}
```

## Low/Informational

### 9. Unclear Error Conditions
```scala
// Unclear
{
  value > 0  // What if false?
}

// Clear
{
  // Spending requires: value > minimum threshold
  val minValue = 1000000L
  val meetsThreshold = value >= minValue
  sigmaProp(meetsThreshold)
}
```

### 10. Gas Inefficiency
```scala
// Inefficient
{
  val check1 = complexCalculation()
  val check2 = complexCalculation()  // Duplicate!
  check1 && check2
}

// Efficient
{
  val result = complexCalculation()
  result
}
```