[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sc/src/main/scala/org/ergoplatform/dsl)

The `.autodoc/docs/json/sc/src/main/scala/org/ergoplatform/dsl` folder contains Scala code for defining and interacting with smart contracts on the Ergo blockchain platform. The code is organized into several files, each focusing on a specific aspect of contract interaction.

`ContractSpec.scala` provides the main entry point for specifying and interacting with smart contracts. It defines several traits and objects, such as `PropositionSpec`, `ProvingParty`, `VerifyingParty`, `InputBox`, `OutBox`, `TransactionCandidate`, `ChainTransaction`, and `ChainBlock`. These traits and objects represent various components of a smart contract and provide methods for specifying, proving, verifying, and interacting with input and output boxes in transactions.

`ContractSyntax.scala` extends the `SigmaContract` and provides methods and classes for contract implementations. It defines the `ContractSyntax` trait, which offers methods for creating propositions and environments. The `SigmaContractSyntax` trait extends `ContractSyntax` and provides additional methods for defining contracts using logical AND and OR operations between Boolean and SigmaProp values.

`ErgoContractSpec.scala` extends the `ContractSpec` trait and provides methods and traits for defining and interacting with Ergo contracts. It defines the `ErgoOutBox` case class, which represents an output box in an Ergo transaction, and the `TransactionContext` trait, which provides methods for interacting with Ergo transactions. Additionally, it offers utility methods for interacting with the Ergo blockchain, such as `getBlock`, `getBoxesByParty`, and `getBoxById`.

`StdContracts.scala` defines the `StdContracts` trait, which provides methods for transferring assets (Ergs and tokens) in a transaction. The methods `transferErgWithChange` and `transferTokenWithChange` facilitate the transfer of Ergs and tokens between boxes in a transaction while calculating the change returned to the source box.

Example usage:

```scala
val tx = new TransactionCandidate()
val fromBox = new OutBox(...)
val toSpec = new PropositionSpec(...)
val ergAmt = 1000000L
val tokenAmt = new Token(...)
val (destBox, changeBox) = transferTokenWithChange(tx, fromBox, toSpec, tokenAmt)
```

In summary, this folder contains code for defining and interacting with smart contracts on the Ergo blockchain platform. Developers can use the provided traits, objects, and methods to create, prove, verify, and interact with Ergo contracts and transactions. The code also offers utility methods for transferring assets and interacting with the Ergo blockchain.
