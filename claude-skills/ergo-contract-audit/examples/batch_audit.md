# Batch Audit Example

This example shows how to audit multiple contracts simultaneously for comparative analysis.

---

## Scenario: Auditing Multiple Auction Contracts

You have 3 different auction implementations and want to compare their security.

---

## Audit Request

```
Please audit these 3 auction contracts and compare their security:

CONTRACT 1 - Simple Auction:
{
  val minBid = SELF.R4[Long].get
  sigmaProp(OUTPUTS(0).value >= minBid)
}

CONTRACT 2 - Timed Auction:
{
  val minBid = SELF.R4[Long].get
  val endHeight = SELF.R5[Int].get
  val active = HEIGHT < endHeight
  sigmaProp(active && OUTPUTS(0).value >= minBid)
}

CONTRACT 3 - Secure Auction:
{
  val seller = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  val endHeight = SELF.R6[Int].get
  
  val isBid = HEIGHT < endHeight && OUTPUTS(0).value >= minBid
  val isClose = HEIGHT >= endHeight && proveDlog(seller)
  
  sigmaProp(isBid || isClose)
}

Please provide a comparative security analysis.
```

---

## Expected Output

### Comparative Security Matrix

| Feature | Contract 1 | Contract 2 | Contract 3 |
|---------|-----------|-----------|-----------|
| Signature Verification | ❌ | ❌ | ✅ |
| Time Limit | ❌ | ✅ | ✅ |
| Output Validation | ❌ | ❌ | ❌ |
| Seller Control | ❌ | ❌ | ✅ |
| **Security Rating** | **CRITICAL** | **HIGH** | **MEDIUM** |

### Contract 1 Analysis

**Critical Issues:**
- No signature verification
- No time limit
- No output validation
- Anyone can claim at any time

**Security Rating:** ❌ **CRITICAL** - Completely insecure

**Recommendation:** Do not use

---

### Contract 2 Analysis

**Critical Issues:**
- No signature verification
- No output validation

**High Issues:**
- No seller control after end

**Improvements over Contract 1:**
- ✅ Has time limit

**Security Rating:** ⚠️ **HIGH RISK** - Major vulnerabilities

**Recommendation:** Needs significant fixes

---

### Contract 3 Analysis

**High Issues:**
- Missing output validation

**Medium Issues:**
- No refund mechanism for outbid users

**Strengths:**
- ✅ Signature verification
- ✅ Time-based logic
- ✅ Seller control

**Security Rating:** ⚠️ **MEDIUM RISK** - Usable with fixes

**Recommendation:** Fix output validation, then deploy

---

## Recommended Implementation

Based on the audit, here's the recommended secure version:

```ergoscript
{
  val seller = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  val endHeight = SELF.R6[Int].get
  val currentBidder = SELF.R7[Option[GroupElement]]
  
  val isBid = {
    val active = HEIGHT < endHeight
    val validBid = OUTPUTS(0).value >= minBid
    val higherBid = OUTPUTS(0).value > SELF.value
    
    // Validate continuation box
    val continues = OUTPUTS(0).propositionBytes == SELF.propositionBytes
    
    // Refund previous bidder
    val refund = currentBidder.fold(true)({
      (prev: GroupElement) =>
        OUTPUTS(1).value == SELF.value &&
        OUTPUTS(1).propositionBytes == prev.getEncoded
    })
    
    active && validBid && higherBid && continues && refund
  }
  
  val isClose = {
    val ended = HEIGHT >= endHeight
    val sellerSigned = proveDlog(seller)
    val sellerGets = OUTPUTS(0).propositionBytes == seller.getEncoded
    
    ended && sellerSigned && sellerGets
  }
  
  sigmaProp(isBid || isClose)
}
```

---

## Summary Table

| Contract | Critical | High | Medium | Low | Rating |
|----------|----------|------|--------|-----|--------|
| Contract 1 | 3 | 1 | 0 | 2 | ❌ CRITICAL |
| Contract 2 | 2 | 1 | 0 | 2 | ⚠️ HIGH |
| Contract 3 | 0 | 1 | 1 | 1 | ⚠️ MEDIUM |
| **Recommended** | 0 | 0 | 0 | 0 | ✅ SECURE |

---

## Batch Audit Benefits

1. **Comparative Analysis** - See which implementation is most secure
2. **Pattern Recognition** - Identify common mistakes across contracts
3. **Best Practices** - Learn from secure implementations
4. **Time Efficiency** - Audit multiple contracts at once
5. **Consistency** - Ensure all contracts meet same security standard

---

## Use Cases for Batch Auditing

- **Multi-contract dApp** - Audit all contracts before deployment
- **Contract upgrades** - Compare old vs new versions
- **Team review** - Audit contracts from multiple developers
- **Security assessment** - Evaluate entire protocol security
- **Learning** - Study different approaches to same problem

---

This example shows how batch auditing helps make informed decisions about contract security.
