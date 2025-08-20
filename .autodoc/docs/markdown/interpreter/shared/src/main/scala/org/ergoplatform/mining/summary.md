[View code on GitHub](sigmastate-interpreterhttps://github.com/ScorexFoundation/sigmastate-interpreter/.autodoc/docs/json/interpreter/shared/src/main/scala/org/ergoplatform/mining)

The `EmissionRules.scala` file in the `org.ergoplatform.mining.emission` package is a crucial component of the Ergo blockchain network, as it defines the coin emission curve and the rewards for miners and the foundation. This class is responsible for calculating the number of coins issued at a given height, determining the remaining coins and rewards, and managing the monetary settings of the Ergo blockchain network.

The `EmissionRules` class takes a `MonetarySettings` object as a parameter, which contains the network settings for the blockchain. It then calculates the total number of coins issued (`coinsTotal`) and the total number of blocks mined (`blocksTotal`) on the network using a tail-recursive function called `loop`.

The class provides several methods for calculating coin issuance and rewards at a given height on the blockchain:

- `issuedCoinsAfterHeight`: Returns the number of coins issued at a given height, considering the fixed rate of 75 coins during the first 2 years and the reward reduction of 3 coins every 3 months after that.
- `remainingCoinsAfterHeight`: Returns the number of coins that have not been issued yet after a given height on the blockchain.
- `emissionAtHeight`: Returns the number of coins to be issued at a given height, considering the fixed rate of 75 coins during the first 2 years and the reward reduction of 3 coins every 3 months after that.
- `minersRewardAtHeight`: Returns the number of coins issued at a given height in favor of a miner, considering the fixed rate of 75 coins during the first 2 years and the reward reduction of 3 coins every 3 months after that.
- `remainingFoundationRewardAtHeight`: Returns the number of coins that should be kept in the foundation box at a given height, considering the fixed rate of 75 coins during the first 2 years and the reward reduction of 3 coins every 3 months after that.
- `foundationRewardAtHeight`: Returns the number of coins issued at a given height in favor of the foundation, considering the fixed rate of 75 coins during the first 2 years and the reward reduction of 3 coins every 3 months after that.

The `CoinsInOneErgo` object is a constant that defines the number of nanoErgs (minimal non-divisible parts) in one Erg.

For example, to calculate the miner's reward at a specific height, you can use the `minersRewardAtHeight` method:

```scala
val emissionRules = new EmissionRules(monetarySettings)
val height = 1000
val minersReward = emissionRules.minersRewardAtHeight(height)
```

In summary, the `EmissionRules` class is an essential part of the Ergo blockchain network, as it manages the coin emission curve and rewards for miners and the foundation. Other components of the network can use this class to calculate the number of coins issued at a given height and to determine the remaining coins and rewards.
