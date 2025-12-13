# ErgoScript Security Cheatsheet

## Common Patterns

### Signature Verification
```scala
// Single signature
val owner = PK("9f5ZKbECVTm25JTRQHDHGM5ehC8tUw...")
sigmaProp(owner)

// Multi-signature (2-of-3)
val signer1 = PK("...")
val signer2 = PK("...")
val signer3 = PK("...")
atLeast(2, Coll(signer1, signer2, signer3))

// Combined with conditions
val isOwner = proveDlog(ownerPk)
val isExpired = HEIGHT > deadline
sigmaProp(isExpired) && isOwner
```

### Value Conservation
```scala
// Check total value preserved
val inputValue = INPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
val outputValue = OUTPUTS.fold(0L, {(a: Long, b: Box) => a + b.value})
val fee = 1000000L
inputValue == outputValue + fee

// Ensure minimum output
OUTPUTS(0).value >= minAmount
```

### Token Handling
```scala
// Check token conservation
val inputTokens = INPUTS(0).tokens(0)._2
val outputTokens = OUTPUTS(0).tokens(0)._2
inputTokens == outputTokens

// Verify token ID
val expectedTokenId = fromBase64("...")
OUTPUTS(0).tokens(0)._1 == expectedTokenId
```

### Time Locks
```scala
// Simple timelock
HEIGHT > deadline

// Relative timelock (using creation height)
HEIGHT > (SELF.creationInfo._1 + lockPeriod)

// Combined conditions
(HEIGHT > deadline && recipientSig) || emergencySig
```

### Register Access
```scala
// Read from registers
val config = SELF.R4[Long].get
val recipient = SELF.R5[SigmaProp].get

// Validate register values
val amount = SELF.R4[Long].get
amount > 0 && amount < Long.MaxValue
```

## Security Checklist

- [ ] All spending paths require proper authentication
- [ ] Value is conserved (inputs = outputs + fees)
- [ ] Tokens are properly tracked and conserved
- [ ] No integer overflow in arithmetic operations
- [ ] TIME/HEIGHT conditions cannot be manipulated
- [ ] Register values are validated
- [ ] Edge cases are handled
- [ ] Error conditions are clear
- [ ] Code matches documentation

## Common Vulnerabilities

### ❌ Insecure
```scala
// Missing signature
sigmaProp(HEIGHT > 1000)

// Unchecked value
OUTPUTS(0).value > 0  // Doesn't verify total

// Integer overflow
val amount = SELF.value * 1000000

// Weak multisig
anyOf(Coll(pk1, pk2))  // Needs only 1 signature
```

### ✅ Secure
```scala
// Proper authentication
sigmaProp(HEIGHT > 1000) && recipientSig

// Value conservation
INPUTS.sum(_.value) == OUTPUTS.sum(_.value) + fee

// Safe arithmetic
val amount = SELF.value.toBigInt * 1000000
amount <= Long.MaxValue

// Proper multisig
atLeast(2, Coll(pk1, pk2, pk3))
```

## Built-in Functions

- `blake2b256()` - Hash function
- `proveDlog()` - Discrete log proof
- `atLeast(k, proofs)` - K-of-N multisig
- `anyOf() / allOf()` - Logical combinators
- `byteArrayToBigInt()` - Type conversion
- `.get` - Safe option access (throws on None)
- `.isDefined` - Check option presence
- `.getOrElse()` - Provide default value