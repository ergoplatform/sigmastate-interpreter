# debug() Function Usage Examples

Comprehensive examples demonstrating the `debug()` function for ErgoScript development and testing.

## Table of Contents
1. [Basic Usage](#basic-usage)
2. [Debugging Box Values](#debugging-box-values)
3. [Debugging Oracle Data](#debugging-oracle-data)
4. [Debugging Complex Conditionals](#debugging-complex-conditionals)
5. [Debugging Collection Operations](#debugging-collection-operations)
6. [Best Practices](#best-practices)
7. [Common Pitfalls](#common-pitfalls)

---

## Basic Usage

### Simple Value Inspection
```scala
{
  val height = debug(HEIGHT, "current height")
  sigmaProp(height > 100)
}
// Output: DEBUG [current height]: 150 (type: Int)
```

### Without Label
```scala
{
  val value = debug(SELF.value, "")
  sigmaProp(value > 1000000L)
}
// Output: DEBUG: 5000000 (type: Long)
```

### Pass-Through in Expressions
```scala
{
  // debug() returns the value, so it can be used inline
  sigmaProp(debug(HEIGHT, "h") > 100 && debug(SELF.value, "v") > 1000L)
}
// Output:
// DEBUG [h]: 150 (type: Int)
// DEBUG [v]: 5000 (type: Long)
```

---

## Debugging Box Values

### Input Box Analysis
```scala
{
  val inputs = INPUTS.map { (box: Box) => 
    debug(box.value, "input value") 
  }
  val totalInput = debug(
    inputs.fold(0L, { (a: Long, b: Long) => a + b }),
    "total input"
  )
  
  sigmaProp(totalInput > 0L)
}
// Output:
// DEBUG [input value]: 1000000 (type: Long)
// DEBUG [input value]: 2000000 (type: Long)
// DEBUG [total input]: 3000000 (type: Long)
```

### Output Box Validation
```scala
{
  val outputs = OUTPUTS.map { (box: Box) => 
    debug(box.value, "output value") 
  }
  val totalOutput = debug(
    outputs.fold(0L, { (a: Long, b: Long) => a + b }),
    "total output"
  )
  
  val inputs = INPUTS.map { (box: Box) => debug(box.value, "input value") }
  val totalInput = debug(
    inputs.fold(0L, { (a: Long, b: Long) => a + b }),
    "total input"
  )
  
  sigmaProp(totalInput == totalOutput)
}
```

### Register Access
```scala
{
  val price = debug(SELF.R4[Long].get, "token price")
  val quantity = debug(SELF.R5[Long].get, "token quantity")
  val total = debug(price * quantity, "total value")
  
  sigmaProp(total > 1000000L)
}
// Output:
// DEBUG [token price]: 100 (type: Long)
// DEBUG [token quantity]: 50000 (type: Long)
// DEBUG [total value]: 5000000 (type: Long)
```

---

## Debugging Oracle Data

### Price Feed Validation
```scala
{
  val oracleBox = CONTEXT.dataInputs(0)
  val oraclePrice = debug(oracleBox.R4[Long].get, "BTC/ERG price")
  val oracleTimestamp = debug(oracleBox.R5[Long].get, "oracle timestamp")
  
  val currentTime = debug(CONTEXT.preHeader.timestamp, "current time")
  val timeDiff = debug(currentTime - oracleTimestamp, "time difference")
  
  val priceThreshold = debug(1000000L, "price threshold")
  
  val priceOk = debug(oraclePrice > priceThreshold, "price check")
  val timeOk = debug(timeDiff < 3600000L, "time check (< 1 hour)")
  
  sigmaProp(priceOk && timeOk)
}
// Output:
// DEBUG [BTC/ERG price]: 1500000 (type: Long)
// DEBUG [oracle timestamp]: 1702500000000 (type: Long)
// DEBUG [current time]: 1702501800000 (type: Long)
// DEBUG [time difference]: 1800000 (type: Long)
// DEBUG [price threshold]: 1000000 (type: Long)
// DEBUG [price check]: true (type: Boolean)
// DEBUG [time check (< 1 hour)]: true (type: Boolean)
```

---

## Debugging Complex Conditionals

### Multi-Condition Validation
```scala
{
  val condition1 = debug(HEIGHT > 100, "height check")
  val condition2 = debug(INPUTS.size > 0, "inputs check")
  val condition3 = debug(SELF.value > 1000000L, "value check")
  val condition4 = debug(OUTPUTS.size == 2, "outputs check")
  
  val allConditions = debug(
    condition1 && condition2 && condition3 && condition4,
    "all conditions"
  )
  
  sigmaProp(allConditions)
}
// Output:
// DEBUG [height check]: true (type: Boolean)
// DEBUG [inputs check]: true (type: Boolean)
// DEBUG [value check]: true (type: Boolean)
// DEBUG [outputs check]: false (type: Boolean)
// DEBUG [all conditions]: false (type: Boolean)
```

### Auction Contract Debugging
```scala
{
  val auctionDeadline = debug(SELF.R4[Int].get, "auction deadline")
  val highestBid = debug(SELF.R5[Long].get, "highest bid")
  
  val currentHeight = debug(HEIGHT, "current height")
  val auctionEnded = debug(currentHeight >= auctionDeadline, "auction ended")
  
  val newBid = debug(OUTPUTS(0).value, "new bid")
  val bidHigherThanCurrent = debug(newBid > highestBid, "bid is higher")
  
  val validBid = debug(
    !auctionEnded && bidHigherThanCurrent,
    "bid valid"
  )
  
  sigmaProp(validBid)
}
```

---

## Debugging Collection Operations

### Map Operations
```scala
{
  val numbers = debug(Coll(1, 2, 3, 4, 5), "input numbers")
  val doubled = numbers.map({ (x: Int) => 
    debug(x * 2, "doubled value") 
  })
  val sum = debug(
    doubled.fold(0, { (a: Int, b: Int) => a + b }),
    "sum of doubled"
  )
  
  sigmaProp(sum == 30)
}
// Output:
// DEBUG [input numbers]: Coll(1,2,3,4,5) (type: Coll[Int])
// DEBUG [doubled value]: 2 (type: Int)
// DEBUG [doubled value]: 4 (type: Int)
// ... (for each element)
// DEBUG [sum of doubled]: 30 (type: Int)
```

### Filter Operations
```scala
{
  val values = debug(Coll(10L, 50L, 100L, 500L, 1000L), "all values")
  val filtered = values.filter({ (x: Long) => 
    val check = debug(x > 100L, s"value > 100")
    check
  })
  val count = debug(filtered.size, "count > 100")
  
  sigmaProp(count >= 2)
}
```

---

## Best Practices

### ✅ DO: Use in Development
```scala
// During development and testing
{
  val userPk = debug(SELF.R4[GroupElement].get, "user public key")
  sigmaProp(proveDlog(userPk))
}
```

### ✅ DO: Label Meaningful Values
```scala
// Clear, descriptive labels
val ergPerToken = debug(SELF.R5[Long].get, "ERG per token price")
val tokenAmount = debug(OUTPUTS(1).tokens(0)._2, "token amount sent")
```

### ✅ DO: Chain for Intermediate Steps
```scala
{
  val a = debug(10, "value a")
  val b = debug(20, "value b")
  val sum = debug(a + b, "sum")
  val doubled = debug(sum * 2, "doubled sum")
  
  sigmaProp(doubled > 50)
}
```

### ❌ DON'T: Use in Production
```scala
// ❌ BAD: Production contract with debug()
{
  val secretKey = debug(SELF.R4[GroupElement].get, "my secret key")
  sigmaProp(proveDlog(secretKey))
}
// Risk: Leaks sensitive data to console/logs!
```

### ❌ DON'T: Rely on Side Effects
```scala
// ❌ BAD: Using debug() for logic (it's just for inspection)
{
  val x = 10
  debug(x, "value") // Only prints, doesn't change behavior
  sigmaProp(x > 5)
}
```

---

## Common Pitfalls

### Pitfall 1: Forgetting Pass-Through Semantics
```scala
// ❌ WRONG: Thinking debug() consumes the value
val x = debug(10, "x")
// If you don't use x, it's lost
sigmaProp(true)

// ✅ CORRECT: Use the returned value
val x = debug(10, "x")
sigmaProp(x > 5) // Use x here
```

### Pitfall 2: Overusing debug()
```scala
// ❌ BAD: Too verbose
{
  val a = debug(debug(debug(10, "raw"), "processed"), "final")
  sigmaProp(a > 5)
}

// ✅ BETTER: Strategic placement
{
  val raw = 10
  val processed = raw * 2
  val final = debug(processed, "final value")
  sigmaProp(final > 5)
}
```

### Pitfall 3: Using in Performance-Critical Code
```scala
// ⚠️ CAUTION: println has overhead
INPUTS.map({ (box: Box) => 
  debug(box.value, "input") // Called for EVERY input!
})

// ✅ BETTER: Debug summary instead
val total = INPUTS.map({ (box: Box) => box.value }).fold(0L, { (a: Long, b: Long) => a + b })
debug(total, "total input value") // Single debug call
```

---

## Removing debug() for Production

### Conditional Compilation Pattern
```scala
// Development version
#if DEBUG
  val price = debug(SELF.R5[Long].get, "price")
#else
  val price = SELF.R5[Long].get
#endif

sigmaProp(price > 1000L)
```

### Script Replacement Pattern
```scala
// During development: use debug version
// scripts/dev/auction_contract.es

// For production: strip debug calls
// scripts/prod/auction_contract.es
// (automated via build script)
```

---

**Remember**: `debug()` is a powerful development tool. Use it to understand contract behavior, but always remove it before deploying to production!
