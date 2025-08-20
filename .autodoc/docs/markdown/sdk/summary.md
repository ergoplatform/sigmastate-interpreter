[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk)

The `.autodoc/docs/json/sdk` folder contains essential components for working with the Ergo blockchain platform, enabling developers to build and interact with ErgoScript applications seamlessly. The folder is organized into two subfolders: `js` and `shared`.

The `js` subfolder provides JavaScript-friendly implementations of various Ergo platform components, such as transaction processing, block headers, and ErgoScript types. For example, the `ErgoTree.scala` file allows developers to convert ErgoTree objects, which represent smart contracts, to and from bytes and hexadecimal strings:

```javascript
const ergoTree = ErgoTrees.fromHex(hexString);
const ergoTreeBytes = ergoTree.toBytes();
```

The `shared` subfolder provides the core functionality for the Ergo Platform SDK, which can be used by other parts of the project or external applications that interact with the Ergo blockchain. The shared components include classes for managing transactions, boxes, and the context of the Ergo blockchain, as well as utility classes for application development, wallet management, and node view management.

For instance, the `ErgoLikeContext.scala` file defines the `ErgoLikeContext` case class, which represents the context of a transaction in the Ergo blockchain:

```scala
val ctx = ErgoLikeContext(currentHeight, spendingTransaction, dataInputs)
```

In summary, the `.autodoc/docs/json/sdk` folder contains essential components for working with the Ergo blockchain platform, enabling developers to build and interact with ErgoScript applications seamlessly. The `js` subfolder provides JavaScript-friendly implementations of various Ergo platform components, while the `shared` subfolder provides the core functionality for the Ergo Platform SDK, which can be used by other parts of the project or external applications that interact with the Ergo blockchain.
