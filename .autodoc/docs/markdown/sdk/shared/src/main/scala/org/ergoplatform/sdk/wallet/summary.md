[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/sdk/shared/src/main/scala/org/ergoplatform/sdk/wallet)

The code in the `.autodoc/docs/json/sdk/shared/src/main/scala/org/ergoplatform/sdk/wallet` folder provides essential functionality for managing keys, key derivation paths, encryption settings, and transactions in the Ergo Platform SDK wallet. It consists of several Scala files and subfolders, each focusing on a specific aspect of the wallet.

`AssetUtils.scala` provides utility functions for working with token assets in the Ergo blockchain. It offers functions like `mergeAssetsMut`, `mergeAssets`, `subtractAssets`, and `subtractAssetsMut` for merging and subtracting token amounts in `TokensMap`. These functions can be used in various parts of the project, such as in the wallet or in smart contracts that deal with tokens.

`Constants.scala` defines a set of constants used throughout the Ergo Platform SDK wallet, ensuring consistency and standardization in the wallet's functionality. Developers can use these constants in their own code to ensure compatibility with the wallet.

The `protocol` subfolder contains code responsible for managing and validating transactions on the Ergo platform. It provides developers with the necessary tools and context to work with adjustable blockchain parameters, transaction validation, and spending transaction execution.

The `secrets` subfolder provides functionality for handling secret keys, extended keys, and key derivation paths in the Ergo Platform SDK wallet. It is essential for generating and managing keys in a hierarchical deterministic (HD) wallet, allowing users to efficiently manage multiple cryptocurrency assets.

The `settings` subfolder contains the `EncryptionSettings.scala` file, which provides a case class and JSON encoding/decoding functionality for encryption settings used in the PBKDF2 algorithm. This allows the larger project to store and retrieve encryption parameters in a JSON format, making it easier to manage and maintain the encryption settings.

Example usage of `AssetUtils`:

```scala
val initialMap: TokensMap = Map(
  ModifierId @@ Array.fill(32)(0.toByte) -> 100L
)
val map1: TokensMap = Map(
  ModifierId @@ Array.fill(32)(1.toByte) -> 50L
)
val map2: TokensMap = Map(
  ModifierId @@ Array.fill(32)(0.toByte) -> 25L
)
val merged: TokensMap = AssetUtils.mergeAssets(initialMap, map1, map2)
// merged contains:
// Map(
//   ModifierId @@ Array.fill(32)(0.toByte) -> 125L,
//   ModifierId @@ Array.fill(32)(1.toByte) -> 50L
// )
```

Example usage of `Constants`:

```scala
import org.ergoplatform.sdk.wallet.Constants

val coinType = Constants.CoinType
val maxAssets = Constants.MaxAssetsPerBox
val preEip3Path = Constants.preEip3DerivationPath
val eip3Path = Constants.eip3DerivationPath
val sentenceSizes = Constants.MnemonicSentenceSizes
val allowedStrengths = Constants.AllowedStrengths
val allowedLengths = Constants.AllowedEntropyLengths
```

In summary, the code in this folder is crucial for managing keys, key derivation paths, encryption settings, and transactions in the Ergo Platform SDK wallet. It provides a foundation for generating and managing keys in a hierarchical deterministic wallet, allowing users to efficiently manage multiple cryptocurrency assets.
