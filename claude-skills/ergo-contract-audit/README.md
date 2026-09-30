# Ergo Contract Auditor - Claude AI Skill

**Comprehensive security auditing for ErgoScript smart contracts**

---

## Overview

This Claude AI skill enables comprehensive security audits of ErgoScript smart contracts on the Ergo blockchain. It identifies vulnerabilities, validates best practices, and generates detailed audit reports with actionable recommendations.

**Key Features:**
- 🔍 Automated vulnerability detection
- 🛡️ Security best practice validation
- 📊 Complexity and quality metrics
- 📝 Detailed audit reports
- 🎓 Educational examples

---

## Quick Start

### Using the Skill

1. **Load the skill** in Claude (if using Claude Desktop or API)
2. **Provide your contract:**
   ```
   Please audit this ErgoScript contract:
   [paste your contract code]
   ```
3. **Review the audit report** with findings and recommendations

### Example

```
Please audit this contract:

{
  val owner = SELF.R4[GroupElement].get
  val amount = OUTPUTS(0).value
  sigmaProp(amount > 0)
}
```

**Claude will respond with:**
- Severity-rated findings
- Specific vulnerabilities detected
- Recommendations for fixes
- Best practice suggestions

---

## What Gets Checked

### Security Categories

1. **Access Control** (Critical)
   - Signature verification
   - Permission boundaries
   - Multi-sig implementation

2. **Arithmetic Safety** (High)
   - Integer overflow/underflow
   - Division by zero
   - Type conversions

3. **Context Validation** (High)
   - Input/output validation
   - Box value checks
   - Token verification

4. **Cryptographic Security** (Critical)
   - Hash function usage
   - Signature schemes
   - Randomness quality

5. **Logic Vulnerabilities** (Critical)
   - Reentrancy
   - Front-running
   - Oracle manipulation

6. **Resource Management** (Medium)
   - Cost limits
   - Box creation
   - Loop bounds

7. **Code Quality** (Low)
   - Documentation
   - Error messages
   - Readability

---

## Files in This Skill

```
ergo-contract-audit/
├── SKILL.md                          # Main skill definition
├── README.md                         # This file
├── scripts/
│   ├── analyze_contract.py           # Contract analyzer
│   └── check_patterns.py             # Pattern checker
├── resources/
│   ├── vulnerability_patterns.json   # Vulnerability database
│   ├── best_practices.md             # Best practices guide
│   ├── audit_template.md             # Report template
│   └── example_contracts/
│       ├── secure_auction.es         # Secure example
│       ├── vulnerable_oracle.es      # Vulnerable example
│       └── token_sale.es             # Token sale example
└── examples/
    ├── basic_audit.md                # Basic usage
    ├── advanced_audit.md             # Advanced usage
    └── batch_audit.md                # Batch auditing
```

---

## Usage Examples

### Basic Audit

```
Audit this simple contract for security issues:

{
  val owner = SELF.R4[GroupElement].get
  sigmaProp(proveDlog(owner))
}
```

### Comprehensive Audit

```
Perform a comprehensive security audit of this auction contract:
[paste full contract]

Focus on:
- Access control
- Arithmetic safety
- Front-running vulnerabilities
```

### Focused Check

```
Check this contract specifically for oracle manipulation vulnerabilities:
[paste contract]
```

---

## Common Vulnerabilities Detected

| Vulnerability | Severity | Description |
|--------------|----------|-------------|
| Missing Signature Verification | Critical | No proveDlog() check |
| Integer Overflow | High | Arithmetic without toBigInt |
| Missing Output Validation | High | OUTPUTS not validated |
| Weak Randomness | High | Predictable random source |
| Front-Running | High | No commit-reveal |
| Oracle Manipulation | High | Single oracle dependency |
| Division by Zero | Medium | No divisor check |
| Timestamp Dependence | Medium | Exact HEIGHT == check |

---

## Best Practices

### ✅ DO

- **Always verify signatures** for privileged operations
- **Use .toBigInt** for calculations that might overflow
- **Validate all outputs** (value, script, tokens)
- **Add error messages** to all require() statements
- **Document contract** purpose and parameters
- **Use multiple oracles** for price feeds
- **Implement commit-reveal** for auctions

### ❌ DON'T

- **Don't skip signature checks** on state changes
- **Don't use predictable randomness** (HEIGHT % N)
- **Don't rely on single oracle** for critical data
- **Don't use exact height equality** (HEIGHT ==)
- **Don't skip output validation**
- **Don't ignore overflow** in arithmetic
- **Don't omit error messages**

---

## Integration

### Pre-Deployment Checklist

- [ ] Run comprehensive audit
- [ ] Fix all Critical issues
- [ ] Fix all High issues
- [ ] Review Medium issues
- [ ] Document acknowledged risks
- [ ] Write tests for all paths
- [ ] Test on testnet
- [ ] Get external audit (for high-value contracts)

### CI/CD Integration

```python
# Example: Automated audit in CI pipeline
from scripts.analyze_contract import analyze_contract
from scripts.check_patterns import check_patterns

contract_code = open('contract.es').read()

# Run analysis
analysis = analyze_contract(contract_code)
findings = check_patterns(contract_code)

# Fail build if critical issues found
critical = [f for f in findings if f['severity'] == 'CRITICAL']
if critical:
    print(f"❌ Found {len(critical)} critical issues!")
    exit(1)
```

---

## Limitations

**This skill provides automated analysis but:**

⚠️ Cannot detect all possible vulnerabilities  
⚠️ May produce false positives  
⚠️ Requires human review for complex logic  
⚠️ Should supplement, not replace, manual auditing  
⚠️ Not a substitute for professional audits on high-value contracts  

**Always:**
- Perform thorough testing
- Get external audits for critical contracts
- Follow defense-in-depth principles
- Stay updated on new vulnerabilities

---

## Resources

- [ErgoScript Documentation](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [ErgoTree Specification](https://ergoplatform.org/docs/ErgoTree.pdf)
- [Ergo Smart Contract Examples](https://github.com/ergoplatform/ergo-appkit-examples)
- [Sigma Protocols](https://en.wikipedia.org/wiki/Proof_of_knowledge#Sigma_protocols)

---

## Contributing

Found a new vulnerability pattern? Want to improve detection?

1. Add pattern to `vulnerability_patterns.json`
2. Update `check_patterns.py` if needed
3. Add example to `example_contracts/`
4. Update documentation

---

## License

This skill is part of the sigmastate-interpreter project.

---

**Version:** 1.0.0  
**Created by:** Team Dev Engers (LNMIIT Hackathon 2025)  
**Issue:** #1090 - Build a Claude skill for auditing Ergo contracts
