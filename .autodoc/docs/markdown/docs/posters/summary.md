[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/docs/posters)

The `.autodoc/docs/json/docs/posters` folder contains two files related to the ErgoScript project, which aims to develop a more expressive scripting language for cryptocurrencies as an alternative to Bitcoin Script.

### poster.tex

This LaTeX document provides a detailed technical explanation of ErgoScript, a call-by-value, higher-order functional language without recursion. ErgoScript is designed with concise Scala/Kotlin syntax and supports various features such as single-assignment blocks, tuples, optional values, indexed collections with higher-order operations, short-cutting logicals, and ternary 'if' with lazy branches.

The document explains how ErgoScript defines a guarding proposition for a coin as a logic formula that combines predicates over a context and cryptographic statements provable via $\Sigma$-protocols with AND, OR, k-out-of-n connectives. It also describes the process of spending a coin and verifying a transaction using ErgoScript.

Several examples of ErgoScript applications are provided, including:

- Zero-knowledge ring and threshold signatures
- Pre-issued mining rewards
- Crowd-funding
- Demurrage currency
- Decentralized exchange (DEX)
- Local Exchange Trading System (LETS)
- Initial Coin Offering (ICO)
- Non-interactive CoinJoin

The document also discusses how ErgoScript can be extended with a soft-fork using versioning conventions.

### sources.bib

This file contains a collection of bibliographic references related to blockchain technology, focusing on the Bitcoin protocol, security, consensus mechanisms, and cryptographic techniques. These references cover key topics such as the Bitcoin Backbone Protocol, zero-knowledge proofs, proof-of-work and proof-of-stake, anonymity and privacy, and scalability and performance.

Developers working on the ErgoScript project can use these references as a starting point for further research and development in the field of blockchain technology and cryptocurrencies.

In summary, the `.autodoc/docs/json/docs/posters` folder contains essential documentation and references for the ErgoScript project. The `poster.tex` file provides a comprehensive technical explanation of ErgoScript and its capabilities, while the `sources.bib` file offers a collection of relevant bibliographic references for further research and development.
