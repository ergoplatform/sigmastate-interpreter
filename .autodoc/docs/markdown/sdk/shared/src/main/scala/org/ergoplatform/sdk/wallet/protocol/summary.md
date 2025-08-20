[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk/shared/src/main/scala/org/ergoplatform/sdk/wallet/protocol)

The code in this folder is part of the Ergo Platform SDK wallet protocol and is responsible for managing and validating transactions on the Ergo platform. It consists of three main Scala files: `ErgoLikeParameters.scala`, `ErgoLikeStateContext.scala`, and `TransactionContext.scala`.

`ErgoLikeParameters.scala` defines a trait called ErgoLikeParameters, which represents a set of adjustable blockchain parameters. These parameters can be read and modified via miners voting and voting-related data. The trait contains methods that return the values of various parameters, such as the cost of storing 1 byte in UTXO for four years, the cost of a transaction output, the max block size, and more. This trait is crucial for decentralized decision-making in the larger project involving blockchain technology and mining.

Example usage:

```scala
val params: ErgoLikeParameters = // get parameters from somewhere
val storageFeeFactor: Int = params.storageFeeFactor
println(s"Cost of storing 1 byte in UTXO for four years: $storageFeeFactor nanoErgs")
```

`ErgoLikeStateContext.scala` defines a trait and a case class that represent the blockchain context used in transaction validation for the Ergo platform. The trait, ErgoLikeStateContext, defines methods that return information about the previous blocks, the UTXO set digest from the last header, and the pre-header of the current block. The case class, CErgoLikeStateContext, implements the ErgoLikeStateContext trait and initializes the corresponding methods with the provided parameters. This code is essential for providing the necessary context for validating transactions on the Ergo platform.

Example usage:

```scala
val context = CErgoLikeStateContext(lastHeaders, stateDigest, preHeader)
val tx = ErgoTransaction(inputs, outputs, dataInputs)
val verifier = new ErgoLikeTransactionVerifier()
val result = verifier.verify(tx, context)
```

`TransactionContext.scala` is a Scala class that manages the execution context related to spending transactions. It takes three parameters: boxesToSpend, dataBoxes, and spendingTransaction. The class provides a context for executing spending transactions, allowing developers to manage the inputs and data inputs of a transaction, as well as the transaction itself. This context can be used to validate and execute spending transactions and generate output candidates in ErgoLikeContext. This class is an important component of the Ergo Platform SDK wallet protocol context, providing developers with a powerful tool for managing spending transactions.

In summary, the code in this folder is essential for managing and validating transactions on the Ergo platform. It provides developers with the necessary tools and context to work with adjustable blockchain parameters, transaction validation, and spending transaction execution.
