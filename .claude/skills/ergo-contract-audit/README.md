# Ergo Contract Audit Skill - Quick Reference

## 🎯 Purpose
Comprehensive security auditing of ErgoScript smart contracts on the Ergo blockchain.

## 📋 Quick Start

### Simple Audit Request
```
Please audit this ErgoScript contract:

{
  [your contract code here]
}
```

### Comprehensive Audit
```
Please perform a security audit on this [contract type] contract.

Purpose: [what it should do]
Key Features:
- [feature 1]
- [feature 2]

[contract code]

Focus areas: [list any specific concerns]
```

## 🔍 What Gets Checked

### Critical Issues
- ❌ Arithmetic overflow/underflow
- ❌ Value conservation violations
- ❌ Spending condition bypass
- ❌ State transition vulnerabilities
- ❌ Register access errors
- ❌ Token ID confusion

### Security Properties
- ✅ SigmaProp correctness
- ✅ eUTXO model compliance
- ✅ Cryptographic soundness
- ✅ Type safety
- ✅ Collection operation safety

### Best Practices
- 💡 Code clarity
- 💡 Gas efficiency
- 💡 Testing coverage
- 💡 Error handling

## 📊 Severity Levels

| Level | Icon | Meaning |
|-------|------|---------|
| Critical | 🔴 | Funds at risk - fix immediately |
| High | ⚠️ | Significant issue - fix before deployment |
| Medium | ⚠️ | Potential problem - should fix |
| Low/Info | ℹ️ | Improvement recommended |

## 📚 Examples

Quick access to example audits:

1. **[Time-Lock Contract](examples/01-timelock-audit.md)** - Simple time-based spending conditions
2. **[Multi-Sig Wallet](examples/02-multisig-audit.md)** - Threshold signatures (2-of-3)
3. **[Token Sale](examples/03-token-sale-audit.md)** - Token exchange with payment validation
4. **[Crowdfunding FSM](examples/04-crowdfunding-audit.md)** - Multi-stage state machine

## 🛠️ Common Patterns Audited

- Time-locked contracts
- Multi-signature wallets
- Token sales/ICOs
- Crowdfunding campaigns
- Oracle integrations
- Atomic swaps
- NFT minting
- DAO governance
- Lending protocols
- DEX contracts

## ✅ Output You'll Receive

Every audit includes:

1. **Executive Summary** - Overall risk and key findings
2. **Detailed Findings** - Each issue with severity, location, impact, and fix
3. **Improved Version** - Production-ready code with all fixes
4. **Testing Guide** - Specific test cases to implement
5. **Security Checklist** - Comprehensive validation checklist

## 🎓 Learning Resources

- **[Full Skill Documentation](skill.md)** - Complete methodology and reference
- **[Usage Guide](USAGE.md)** - Detailed usage instructions
- **[Ergo Docs](https://docs.ergoplatform.com/dev/scs/ergoscript/)** - Official ErgoScript guide
- **[Language Spec](../../../docs/LangSpec.md)** - ErgoScript language specification

## ⚡ Quick Commands

### Basic Audit
```
Audit this contract: [paste code]
```

### Focused Audit
```
Check this contract for [specific vulnerability]: [paste code]
```

### Comparison
```
Which is more secure?
Option 1: [code]
Option 2: [code]
```

### Re-Audit After Fixes
```
I've fixed the issues. Please verify: [paste fixed code]
```

## 🚨 When to Use

✅ **Use for:**
- Pre-deployment review
- Security assessment
- Code review
- Learning vulnerabilities
- Best practice validation

❌ **Don't rely solely on for:**
- High-value contracts (get professional audit)
- Formal verification needs
- Real-time exploit detection
- Contract execution testing

## 💡 Pro Tips

1. **Provide context** - Explain what the contract should do
2. **Be specific** - Mention any particular concerns
3. **Iterate** - Fix issues and re-audit
4. **Test recommendations** - Implement all suggested tests
5. **Study examples** - Learn from example audits

## 🔗 Key References

- [ErgoScript Best Practices](https://docs.ergoplatform.com/dev/scs/ergoscript/#best-practices)
- [Common Pitfalls](https://docs.ergoplatform.com/dev/scs/ergoscript/#common-pitfalls-to-avoid)
- [Sigma Protocols](https://docs.ergoplatform.com/dev/scs/sigma/)
- [eUTXO Model](https://docs.ergoplatform.com/dev/protocol/eutxo/)

## 📞 Get Help

- **Discord**: [Ergo Platform Discord](https://discord.gg/ergo-platform-668903786361651200) - #ergoscript channel
- **Forum**: [Ergo Forum](https://www.ergoforum.org/)
- **Telegram**: [Ergo Developers](https://t.me/ergo_dev)

---

**Version**: 1.0  
**Type**: Security Analysis Skill  
**Updated**: December 2025
