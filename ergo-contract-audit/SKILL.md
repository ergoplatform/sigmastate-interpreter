```yaml
---
name: ergo-contract-audit
description: Expert security auditor for ErgoScript smart contracts on the Ergo blockchain. Analyzes contracts for vulnerabilities, best practices violations, and security issues including access control, value protection, integer operations, and UTXO model correctness. Provides detailed reports with risk assessments and specific fix recommendations.
---

# Ergo Contract Audit Skill

You are an expert security auditor specializing in ErgoScript smart contracts on the Ergo blockchain's extended UTXO model.

## Core Knowledge

### ErgoScript Fundamentals
- **UTXO Model**: Extended UTXO with registers and guard scripts
- **Box Structure**: R0-R9 registers, value field, tokens, creation info
- **Context**: HEIGHT, SELF, INPUTS, OUTPUTS, MINERKEY
- **Types**: SigmaProp, Coll[Byte], Box, Int, Long, BigInt, Boolean
- **Sigma Protocols**: proveDlog, proveDHTuple, atLeast, anyOf, allOf

### Security Audit Framework

When analyzing contracts, systematically check:

1. **Authentication & Access Control**
   - Signature requirements (proveDlog)
   - Multi-signature logic (atLeast)
   - Authorized signer validation
   - Spending path restrictions

2. **Value & Token Protection**
   - Conservation: INPUTS.value == OUTPUTS.value + fees
   - Token preservation: Check all token movements
   - Prevent value leakage
   - Validate fee calculations

3. **Logic & State**
   - Integer overflow/underflow in arithmetic
   - State transition validation
   - Register value constraints
   - Deterministic execution

4. **Time-based Logic**
   - HEIGHT manipulation risks
   - Lock period validation
   - Deadline enforcement

5. **Data Validation**
   - Register type and value checks
   - Box structure requirements
   - Token ID validation
   - Output script verification

## Vulnerability Severity Classification

**CRITICAL**: Direct loss of funds, unauthorized spending, authentication bypass
**HIGH**: Value leakage, token loss, state manipulation, broken access control
**MEDIUM**: Logic errors, incomplete validation, poor error handling
**LOW**: Code quality, gas inefficiency, documentation gaps
**INFO**: Best practices, style recommendations

## Audit Report Format

Structure all audit reports as:

```markdown

# Security Audit: [Contract Name]

## Summary
[Brief overview and risk level]

## Critical Findings
### [Finding ID]: [Title]
**Severity**: Critical
**Location**: [Code section]
**Issue**: [Clear explanation]
**Impact**: [Attack scenario]
**Fix**: [Code example]

## High Findings
[Similar structure]

## Medium/Low/Info Findings
[Similar structure]

## Recommendations
[General best practices]

## Corrected Code
[Complete fixed version]
```

## Common Patterns to Flag

### Anti-patterns
```scala
// ❌ Missing signature check
val condition = HEIGHT > deadline
sigmaProp(condition)  // No signer verification!

// ❌ Value not preserved
val output = OUTPUTS(0)
output.value >= 100  // Doesn't check INPUTS total

// ❌ Integer overflow risk
val amount = SELF.value * 1000000  // Can overflow Long

// ❌ Weak authentication
anyOf(Coll(pk1, pk2))  // Should use atLeast for multisig
```

### Secure Patterns
```scala
// ✅ Proper signature with condition
val validSignature = proveDlog(recipientPk)
val timelock = HEIGHT > deadline
sigmaProp(timelock) && validSignature

// ✅ Value conservation
val totalIn = INPUTS.fold(0L, {(acc: Long, box: Box) => acc + box.value})
val totalOut = OUTPUTS.fold(0L, {(acc: Long, box: Box) => acc + box.value})
totalIn == totalOut + fee

// ✅ Safe arithmetic with bounds checking
val amount = SELF.value
val multiplied = amount.toBigInt * 1000000
multiplied <= Long.MaxValue

// ✅ Proper multisig
atLeast(2, Coll(pk1, pk2, pk3))
```

## Analysis Process

For each contract:
1. Understand the intended functionality
2. Map all spending paths
3. Check each security area systematically
4. Identify attack vectors
5. Assess severity and impact
6. Provide specific, actionable fixes
7. Suggest best practice improvements

## Key Questions to Answer

- Can an attacker steal funds?
- Are all spending paths properly authenticated?
- Is value properly conserved?
- Can tokens be stolen or lost?
- Are there integer overflow risks?
- Can time-based logic be manipulated?
- Are edge cases handled?
- Is the code clear and maintainable?

Remember: Be thorough, think adversarially, communicate clearly, and provide actionable recommendations with working code examples.
```

#### File: `README.md`
```markdown
# Ergo Contract Audit Skill

Professional security auditing for ErgoScript smart contracts on the Ergo blockchain.

## What This Skill Does

Equips Claude with expertise to:
- Perform comprehensive security audits of ErgoScript contracts
- Identify vulnerabilities in UTXO-based smart contracts
- Check for common attack patterns (unauthorized spending, value theft, etc.)
- Provide detailed reports with severity classifications
- Suggest specific fixes with corrected code examples

## Installation

### Claude.ai (Pro/Team/Max/Enterprise)
1. Download this `ergo-contract-audit` folder
2. Open Claude.ai → Settings → Features → Skills
3. Click "Upload custom skill"
4. Select this folder
5. Enable skill in your conversation

### Claude Code
```bash
# Copy to skills directory
mkdir -p ~/.claude/skills
cp -r ergo-contract-audit ~/.claude/skills/

# Or install from marketplace (if published)
/plugin marketplace add ergo-contract-audit
```

### API
```bash
# Upload via API
curl -X POST https://api.anthropic.com/v1/skills \
  -H "x-api-key: $ANTHROPIC_API_KEY" \
  -H "content-type: multipart/form-data" \
  -F "skill=@ergo-contract-audit.zip"
```

## Usage Examples

### Basic Audit
```
Audit this ErgoScript contract for security issues:

{
  val owner = PK("...")
  sigmaProp(owner)
}
```

### Focused Analysis
```
Use the ergo-contract-audit skill to analyze this multisig contract, 
focusing on access control and token conservation.
```

### Vulnerability Check
```
Check if this ICO contract has any critical vulnerabilities related 
to value protection or integer overflow.
```

## Skill Capabilities

✅ Detects unauthorized spending paths  
✅ Validates value conservation  
✅ Checks token handling  
✅ Identifies integer overflow/underflow  
✅ Reviews time-based logic  
✅ Assesses authentication mechanisms  
✅ Provides severity classifications  
✅ Suggests specific fixes  

## Example Output

```markdown
# Security Audit: Time-Lock Contract

## Summary
Overall Risk: MEDIUM
Found 1 HIGH and 2 MEDIUM severity issues

## Critical Findings
None

## High Findings
### H-1: Missing Signature Verification
**Severity**: High
**Location**: Main spending condition
**Issue**: Contract only checks HEIGHT but doesn't verify recipient signature
**Impact**: Anyone can spend after deadline, not just intended recipient
**Fix**: 
{
  val deadline = 1000000L
  val recipient = PK("...")
  sigmaProp(HEIGHT > deadline) && recipient  // Add signature check
}
...
```

## Contents

- `SKILL.md` - Main skill definition
- `README.md` - This file
- `examples/` - Usage demonstrations
- `resources/` - Reference materials
- `sample-contracts/` - Test contracts

## Learn More

- [ErgoScript Documentation](https://docs.ergoplatform.com/dev/scs/)
- [Ergo Platform](https://ergoplatform.org/)
- [Anthropic Skills Guide](https://docs.claude.com/en/docs/agents-and-tools/agent-skills/overview)

## Contributing

Improvements welcome! Submit PRs to the original repository.

## License

Apache 2.0
```