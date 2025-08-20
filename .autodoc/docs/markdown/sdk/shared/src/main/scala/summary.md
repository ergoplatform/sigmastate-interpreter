[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk/shared/src/main/scala)

The code in the `.autodoc/docs/json/sdk/shared/src/main/scala` folder provides the core functionality for the Ergo Platform SDK, which can be used by other parts of the project or external applications that interact with the Ergo blockchain. The shared components include classes for managing transactions, boxes, and the context of the Ergo blockchain, as well as utility classes for application development, wallet management, and node view management.

For instance, the `ErgoLikeContext.scala` file defines the `ErgoLikeContext` case class, which represents the context of a transaction in the Ergo blockchain. It contains information such as the current height of the blockchain, the transaction's inputs and outputs, and the data inputs. This class is used in various parts of the project, such as transaction validation and smart contract execution.

Example usage:

```scala
val ctx = ErgoLikeContext(currentHeight, spendingTransaction, dataInputs)
```

Similarly, the `ErgoLikeTransaction.scala` file defines the `ErgoLikeTransaction` case class, which represents a transaction in the Ergo blockchain. It contains information such as the transaction's inputs, outputs, and data inputs. This class is used in various parts of the project, such as transaction creation and validation.

Example usage:

```scala
val tx = ErgoLikeTransaction(inputs, dataInputs, outputs)
```

The `ErgoBox.scala` file defines the `ErgoBox` case class, which represents a box (i.e., an unspent transaction output) in the Ergo blockchain. It contains information such as the box's value, the script that locks the box, and any additional registers that store custom data. This class is used in various parts of the project, such as transaction creation and smart contract execution.

Example usage:

```scala
val box = ErgoBox(value, script, additionalRegisters)
```

The subfolders in this folder contain additional components that are essential for the functioning of the Ergo Platform. For example, the `appkit` subfolder contains the source code for the Ergo Appkit, a set of utility classes and functions that simplify the development of applications that interact with the Ergo blockchain. It includes classes for managing wallets, creating and signing transactions, and working with ErgoScript (the smart contract language used by Ergo).

In summary, the code in this folder provides the core functionality for the Ergo Platform SDK, which can be used by other parts of the project or external applications that interact with the Ergo blockchain. The shared components include classes for managing transactions, boxes, and the context of the Ergo blockchain, as well as utility classes for application development, wallet management, and node view management.
