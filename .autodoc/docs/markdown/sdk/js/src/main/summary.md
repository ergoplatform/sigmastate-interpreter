[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk/js/src/main)

The `.autodoc/docs/json/sdk/js/src/main/scala/org/ergoplatform/sdk/js` folder contains essential components for working with the Ergo blockchain platform in a JavaScript environment. These components enable developers to build and interact with ErgoScript applications seamlessly, providing JavaScript-friendly implementations of various Ergo platform components, such as transaction processing, block headers, and ErgoScript types.

For instance, the `BlockchainParameters.scala` file defines a class representing the parameters of a blockchain network, such as maximum block size and transaction costs. This class can be used to configure the blockchain network parameters in a JavaScript environment:

```javascript
const params = new BlockchainParameters(...);
const maxBlockSize = params.maxBlockSize;
```

The `BlockchainStateContext.scala` file defines a class representing the current state of the blockchain, including recent block headers and the previous state digest. This class can be used to access and manipulate the blockchain state:

```javascript
const context = new BlockchainStateContext(headers, previousDigest, preHeader);
console.log(context.sigmaLastHeaders);
```

The `ErgoTree.scala` file provides a way to convert ErgoTree objects, which represent smart contracts, to and from bytes and hexadecimal strings. This is useful for creating and executing smart contracts on the Ergo blockchain platform:

```javascript
const ergoTree = ErgoTrees.fromHex(hexString);
const ergoTreeBytes = ergoTree.toBytes();
```

The `Header.scala` file defines classes for representing and manipulating AVL trees and block headers in the Ergo blockchain. These classes can be used to perform various operations on the data they represent:

```javascript
const header = new Header(...);
const avlTree = new AvlTree(...);
```

The `ProverBuilder.scala` file provides a way to configure a prover object with the necessary secrets and data to sign a transaction. The resulting `Prover` object can then be used to sign a transaction and submit it to the Ergo blockchain:

```javascript
const builder = new ProverBuilder(parameters, networkPrefix);
builder.withMnemonic("my secret phrase", "my password");
const prover = builder.build();
const signedTx = prover.sign(tx);
```

The `Type.scala` and `Value.scala` files provide a way to represent ErgoScript types and values in a JavaScript-friendly way, which can be useful when working with ErgoScript in a JavaScript environment:

```javascript
const intValue = Values.ofInt(42);
const pairValue = Values.pairOf(intValue, intValue);
const collValue = Values.collOf([intValue, intValue], Type.Int);
```

Overall, this folder contains essential components for working with the Ergo blockchain platform in a JavaScript environment, enabling developers to build and interact with ErgoScript applications seamlessly.
