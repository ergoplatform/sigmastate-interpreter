# Ergo Contract Audit Skill

## Overview
This skill enables comprehensive security auditing of ErgoScript smart contracts on the Ergo blockchain. It combines deep knowledge of ErgoScript language specifications, sigma protocols, eUTXO model patterns, and common security vulnerabilities specific to Ergo contracts.

## Expertise Areas

### ErgoScript Language
- **Syntax & Semantics**: Scala-based syntax, statically typed with local type inference
- **Type System**: Primitive types (Byte, Short, Int, Long, BigInt, UnsignedBigInt), SigmaProp, AvlTree, GroupElement, Box, Option[T], Coll[T], tuples
- **Operations**: Binary operations, cryptographic primitives, collection operations, sigma propositions
- **Execution Model**: Call-by-value evaluation, ErgoTree compilation and serialization

### Security-Critical Components

#### 1. **Sigma Protocols & Zero-Knowledge Proofs**
- Schnorr signatures and proofs of Diffie-Hellman tuples
- Composable sigma propositions (AND, OR, threshold signatures)
- Ring signatures and advanced cryptographic patterns
- Proper use of `proveDlog`, `proveDHTuple`, and compound propositions

#### 2. **eUTXO Model Validation**
- Box lifecycle and state transitions
- Input/output box relationships in transactions
- Data inputs for read-only access to shared state
- Guard scripts and spending conditions
- Transaction validation context (HEIGHT, SELF, INPUTS, OUTPUTS)

#### 3. **Box Register Usage**
- Proper typing and usage of R4-R9 registers
- Type safety in register access patterns
- Guard script vs. registers separation of concerns
- Optional register handling

#### 4. **Token Management**
- Token minting and burning patterns
- Token ID verification and uniqueness
- Box token collection validation
- NFT-specific patterns

#### 5. **Multi-Stage Contracts**
- Finite State Machine (FSM) patterns
- State encoding within boxes
- Transition validation between stages
- Turing-completeness through multi-stage transactions

#### 6. **Advanced Patterns**
- MAST (Merkleized Abstract Syntax Trees) for privacy
- Time-locked contracts using HEIGHT
- Multi-signature wallets and threshold schemes
- Atomic swaps and cross-chain interactions
- Oracle data integration
- Crowdfunding and fundraising contracts

## Common Vulnerabilities to Check

### Critical Issues

1. **Arithmetic Overflow/Underflow**
   - Missing bounds checking on numeric operations
   - Unsafe conversions between numeric types (toByte, toShort, toInt, toLong)
   - BigInt/UnsignedBigInt conversion errors

2. **Box Value Preservation**
   - Missing validation that output values equal input values (minus fees)
   - Token conservation violations
   - Unauthorized value extraction

3. **Spending Condition Bypass**
   - Incomplete guard scripts allowing unintended spending paths
   - Missing sigma proposition requirements
   - Improper use of logical operators (&&, ||) in conditions

4. **State Transition Vulnerabilities**
   - Invalid state transitions in multi-stage contracts
   - Missing validation of box structure in outputs
   - Improper continuation of contract state

5. **Register Access Errors**
   - Unsafe `.get` calls on optional registers
   - Type mismatches in register access
   - Missing register validation in guard scripts

6. **Context Manipulation**
   - Reliance on manipulable context variables
   - Missing validation of INPUTS/OUTPUTS structure
   - Improper HEIGHT-based time locks

### Medium Issues

7. **Token ID Confusion**
   - Missing token ID verification
   - Incorrect token filtering logic
   - NFT uniqueness not enforced

8. **Data Input Misuse**
   - Trusting unvalidated data inputs
   - Missing oracle validation
   - Read-only input assumptions violated

9. **Collection Operation Errors**
   - Unbounded collection operations (performance issues)
   - Missing existence checks before fold/reduce
   - Improper filtering leading to empty collections

10. **Cryptographic Misuse**
    - Weak randomness or nonce reuse
    - Incorrect hash function application
    - Improper signature verification

### Low/Informational Issues

11. **Gas/Cost Inefficiencies**
    - Redundant operations or computations
    - Inefficient collection traversals
    - Complex scripts that could be simplified

12. **Code Quality**
    - Unclear contract logic or missing comments
    - Dead code or unreachable branches
    - Non-idiomatic ErgoScript patterns

13. **Best Practice Violations**
    - Contract complexity beyond necessity
    - Missing test coverage considerations
    - Insufficient error handling in off-chain code

## Audit Methodology

### Phase 1: Static Analysis
1. **Parse and understand contract purpose**
   - Identify intended functionality
   - Map out state transitions (if applicable)
   - Document assumptions and invariants

2. **Type and syntax review**
   - Verify type safety throughout
   - Check for proper Option handling
   - Validate collection operations

3. **Logic flow analysis**
   - Trace all execution paths
   - Identify potential bypass conditions
   - Verify completeness of guard scripts

### Phase 2: Security Review
1. **Value conservation checks**
   - Input/output ERG balance
   - Token conservation
   - Fee handling

2. **Spending condition analysis**
   - All SigmaProp requirements
   - Logical operator usage
   - Edge case handling

3. **State transition validation** (for multi-stage contracts)
   - FSM correctness
   - Box structure preservation
   - Transition guards

4. **Cryptographic verification**
   - Proper sigma protocol usage
   - Hash function applications
   - Key management patterns

### Phase 3: Testing Recommendations
1. **Unit test coverage**
   - All execution paths
   - Edge cases and boundary conditions
   - Failure scenarios

2. **Integration testing**
   - Multi-transaction flows
   - Interaction with other contracts
   - Off-chain component integration

3. **Adversarial testing**
   - Attack vectors enumeration
   - Economic exploit scenarios
   - Front-running possibilities

## Reference Documentation

### Official Ergo Documentation
- **ErgoScript Overview**: https://docs.ergoplatform.com/dev/scs/ergoscript/
- **Language Specification**: In-depth type system, operations, and semantics
- **Best Practices**: https://docs.ergoplatform.com/dev/scs/ergoscript/#best-practices
- **Common Pitfalls**: https://docs.ergoplatform.com/dev/scs/ergoscript/#common-pitfalls-to-avoid
- **eUTXO Model**: https://docs.ergoplatform.com/dev/protocol/eutxo/
- **Sigma Protocols**: https://docs.ergoplatform.com/dev/scs/sigma/

### Tooling
- **Sigmastate Interpreter**: Reference implementation for validation
- **Ergo Playgrounds**: Scala environment for testing
- **escript.online**: Online compiler and playground
- **AppKit/Fleet/Sigma-Rust**: SDK testing frameworks

## Output Format

### Audit Report Structure

```markdown
# Ergo Contract Audit Report

## Executive Summary
- Contract Overview
- Audit Scope
- Critical Findings Count
- Overall Risk Assessment

## Contract Analysis
- Purpose and Functionality
- Architecture Overview
- State Machine (if applicable)

## Detailed Findings

### [CRITICAL/HIGH/MEDIUM/LOW/INFO] Finding Title
**Severity**: [Critical/High/Medium/Low/Informational]
**Location**: Line X or function/section
**Description**: Clear explanation of the issue
**Impact**: What can go wrong
**Recommendation**: How to fix it
**Code Example**: Show vulnerable code and fixed version

## Security Checklist
- [ ] Value conservation verified
- [ ] All spending conditions complete
- [ ] No arithmetic overflow/underflow
- [ ] Proper type conversions
- [ ] Register access safe
- [ ] Token handling correct
- [ ] State transitions valid
- [ ] Cryptographic operations sound

## Testing Recommendations
- Specific test cases to write
- Edge cases to cover
- Attack scenarios to verify

## Conclusion
- Summary of findings
- Risk rating
- Deployment readiness
```

## Best Practices for Auditors

1. **Always verify against ErgoScript language spec** - Don't assume Solidity/EVM patterns apply
2. **Understand the eUTXO model deeply** - Box lifecycle is fundamental
3. **Check value conservation explicitly** - Sum of outputs must equal sum of inputs (minus fees)
4. **Validate all sigma propositions** - Security depends on cryptographic correctness
5. **Consider multi-stage interactions** - Contracts may span multiple transactions
6. **Test with Ergo Playgrounds** - Hands-on verification is essential
7. **Review off-chain code too** - Security extends beyond on-chain scripts
8. **Consult community resources** - Discord, Telegram, Ergo Forum for expert opinions

## Example Audit Scenarios

See the `examples/` directory for:
- Simple time-locked contract audit
- Multi-signature wallet review
- Token sale contract analysis
- Complex FSM contract evaluation
- Oracle integration security review

---

**Version**: 1.0  
**Last Updated**: December 2025  
**Maintained By**: Ergo Security Research Community
