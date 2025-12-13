# ErgoScript / ErgoTree Audit Skill for Claude

This skill provides Claude with specialized instructions to assist in
reviewing and auditing Ergo smart contracts at the ErgoScript and ErgoTree
level.

## Context
Ergo smart contracts are compiled from ErgoScript into ErgoTree bytecode,
which defines the guarding proposition for UTXO boxes. Contract correctness
depends on how these propositions are reduced, costed, and verified under
different blockchain contexts.

This skill is designed to reason about these semantics rather than treating
Ergo contracts as procedural programs.

## Purpose
To help developers and reviewers:
- Analyze ErgoScript logic
- Reason about ErgoTree propositions
- Identify logical, economic, and semantic risks
- Review token, oracle, and state-transition constraints

## Scope
- ErgoScript source logic
- ErgoTree reduction semantics
- SigmaBoolean construction
- Token and value constraints
- Height- and context-dependent conditions
- Oracle box validation assumptions

## Out of Scope
- Formal verification
- Performance optimization of the interpreter
- Solidity or EVM-based contracts
- Gas, reentrancy, or account-based models

This skill is an **assistive analysis tool**, not a replacement for expert or
formal audits.
