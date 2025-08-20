# Certified Development of Ergo contracts

## Introduction
 
Ergo as a blockchain platform for contractual money provides an expressive
programming model to build contracts for [simple]() and more 
[advanced]() applications such as a crowdfunding contract and 
an ICO campaign contract, among others.

Contracts specify when and on which conditions the money can be transferred from one party to another. 
This contractual logic should be free from errors, otherwise the money can be lost either accidentally or as a result of an exploit. [TODO citation]

The more complex the application scenario, the more complex contracts should be
developed to implement all the required money transfers.
As a result, the probability of bugs increases with the size of the source code of the contracts. [TODO citation]

There are many software development techniques to build reliable software and
formal verification is one of the techniques to reduce errors in program code, 
along with design techniques, development processes, software testing, code review, etc.
That said, we believe that practical verification methodology should be seamlessly integrated into a development workflow. [TODO citation of verification and quality assurance techniques]

In particular, we want each Ergo contract to satisfy the following properties:
1) *deterministically executable* so that the money transfer conditions can be checked as 
part of network consensus protocol
2) *modular and reusable* so that new contracts can be built from existing ready to use contracts
3) *human-readable and comprehensible* so that any reviewer can understand the logic and check its correctness
4) *testable*, so that contract execution can be tested on multiple examples
5) *formally verifiable*, i.e., some propositions about contract behavior can be proven as 
theorems

All these properties contribute to reliability, financial safety, and long-term survivability
of the blockchain platform. Ergo contracts are written in ErgoScript language 
(which is based on Scala syntax and semantics), and here we describe an environment for certified development of Ergo contracts.

## Scope

We aim to integrate formal contract verification into Ergo contract development toolbox
without sacrificing other important properties, which essential for the development of 
real-world contracts and mentioned in [Introduction](#introduction).

#### Goals
 - G1: allow usage of the same language (which is a subset of Scala) both for writing 
 contracts and for formulation and verification of pre(post)-conditions 
 - G2: allow the definition of reusable lemmas and modular proofs
 - G3: allow verification for all Ergo contracts (both [simple](Crowdfunding) and 
 [complex](ICO) contracts should be supported)
 - G4: allow both verification and execution of the same source code without any modifications
 - G5: allow usage of pre(post)-conditions for automatic property testing
 - G6: allow a certified definition of ErgoScript primitive operations
 - G7: allow automatic property testing of runtime implementation of ErgoScript primitives 
 against their certified definitions
 
#### Non-Goals
 - development of new verification techniques
 - development of a verification approach disconnected from the rest of contract development
   toolchain.
 - demonstration of verification on toy examples 

## Ergo Contract Development Environment

We have chosen the Scala language and tool ecosystem as the development environment for both 
contract development and also for implementation of the ErgoScript language.

Moreover, we designed ErgoScript to be a subset of Scala language, inheriting its syntax, 
types and semantics of the selected Scala subset. We call this subset ErgoScala, keeping in mind that it is equivalent to ErgoScript and is also a valid Scala.

Such defined ErgoScala allows us to meet the [goals](#goals) by:
1) writing, executing, testing and debugging Ergo contracts directly in Scala using existing 
tools, libraries and IDE support
2) implementing Scala macros to compile ErgoScala fragments (contracts) into [ErgoTree](ref) 
the intermediate representation of Ergo contracts suitable for storage in Ergo blockchain. (G4)
3) using [Stainless](https://github.com/epfl-lara/stainless) verification framework to specify pre(post)-conditions for Ergo contracts and formally prove them using auxiliary lemmas. (PureScala is a superset of ErgoScala, 
thus any ErgoScala fragment is verifiable as long as it is verifiable as PureScala fragment
in Stainless) (G1, G2, G3)
4) check the declared pre(post)-conditions using ScalaCheck property testing (G5)
5) allow the certified implementation of ErgoScript primitives and automatic property
testing of their equivalence with performance-optimized implementation, which is used in Ergo mainnet. (G6, G7)

In the following sections, we give a detailed presentation of the goals and how they 
can be achieved.

### Writing contracts in ErgoScala (G1)

### Modular Proofs (G2)

### Contract Verification (G3)

### ErgoScala compilation to ErgoTree (G4)

### Testing Pre(Post)-conditions at Runtime (G5)

### Certified Definition of Primitives (G6)

### Property Testing of Language Runtime Primitives (G7)

## Related Work

## References


