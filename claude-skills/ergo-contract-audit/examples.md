# Usage Examples

This document provides example interactions demonstrating how the
**ErgoScript / ErgoTree Contract Auditor** skill can be used to assist in
reviewing Ergo smart contracts.

---


## Example 1: ErgoScript Guard Review

**User Input**
Here is an ErgoScript contract defining spending conditions for a box:
<ErgoScript code>

**Expected Analysis**
- Identify the guarding proposition
- Explain how it reduces to a SigmaBoolean
- Highlight permissive or missing conditions
- Suggest tighter constraints

---

## Example 2: Token Minting Logic

**User Input**
Analyze this ErgoScript used for minting a custom token:
<ErgoScript code>

**Expected Analysis**
- Verify token ID enforcement
- Check supply bounds
- Identify potential inflation vectors
- Assign severity

---

## Example 3: Oracle-Based Contract

**User Input**
Audit this Ergo contract relying on oracle-provided price data:
<ErgoScript code>

**Expected Analysis**
- Evaluate oracle box validation
- Replay and freshness risks
- Context manipulation concerns
- Suggested mitigations
