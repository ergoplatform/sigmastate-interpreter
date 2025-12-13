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
Copy to skills directory
mkdir -p ~/.claude/skills
cp -r ergo-contract-audit ~/.claude/skills/Or install from marketplace (if published)
/plugin marketplace add ergo-contract-audit
```
### API
```bash 
Upload via API
curl -X POST https://api.anthropic.com/v1/skills 
-H "x-api-key: $ANTHROPIC_API_KEY" 
-H "content-type: multipart/form-data" 
-F "skill=@ergo-contract-audit.zip"
```
## Usage Examples

### Basic AuditAudit this ErgoScript contract for security issues:
```{
val owner = PK("...")
sigmaProp(owner)
}
```

### Focused AnalysisUse the ergo-contract-audit skill to analyze this multisig contract, focusing on access control and token conservation.

### Vulnerability CheckCheck if this ICO contract has any critical vulnerabilities related to value protection or integer overflow.

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
Security Audit: Time-Lock ContractSummary
Overall Risk: MEDIUM
Found 1 HIGH and 2 MEDIUM severity issuesCritical Findings
NoneHigh Findings
H-1: Missing Signature Verification
Severity: High
Location: Main spending condition
Issue: Contract only checks HEIGHT but doesn't verify recipient signature
Impact: Anyone can spend after deadline, not just intended recipient
Fix:
{
val deadline = 1000000L
val recipient = PK("...")
sigmaProp(HEIGHT > deadline) && recipient  // Add signature check
}
```
---

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