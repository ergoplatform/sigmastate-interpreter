package org.ergoplatform

import org.ergoplatform.settings.MonetarySettings
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.ProveDlog
import ErgoTree.{HeaderType, ZeroHeader}
import sigma.crypto.CryptoConstants
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate._

object ErgoTreePredef {
  /** Create ErgoTree with `false` proposition, which is never true.
    *
    * @param header ErgoTree header to be used in the tree
    * @see ErgoTree.headerWithVersion()
    */
  def FalseProp(header: HeaderType): ErgoTree = ErgoTree.withoutSegregation(header, FalseSigmaProp)

  /** Create ErgoTree with `true` proposition, which is always true.
    *
    * @param header ErgoTree header to be used in the tree
    * @see ErgoTree.headerWithVersion()
    */
  def TrueProp(header: HeaderType): ErgoTree = ErgoTree.withoutSegregation(header, TrueSigmaProp)

  /**
    * Byte array value of the serialized reward output script proposition with pk being substituted
    * with given pk
    *
    * @param delta - number of blocks for which miner should hold this box before spending it
    * @param minerPkBytesVal - byte array val for pk to substitute in the reward script
    */
  def expectedMinerOutScriptBytesVal(
      delta: Int,
      minerPkBytesVal: Value[SByteArray]): Value[SByteArray] = {
    val genericPk = ProveDlog(CryptoConstants.dlogGroup.generator)
    val genericMinerProp = rewardOutputScript(delta, genericPk)
    val genericMinerPropBytes = DefaultSerializer.serializeErgoTree(genericMinerProp)
    // first segregated constant is delta, so key is second constant
    val positions = IntArrayConstant(Array[Int](1))
    val minerPubkeySigmaProp = CreateProveDlog(DecodePoint(minerPkBytesVal))
    val newVals = ConcreteCollection(Array[SigmaPropValue](minerPubkeySigmaProp), SSigmaProp)
    SubstConstants(genericMinerPropBytes, positions, newVals)
  }

  /**
    * Required script of the box, that collects mining rewards
    */
  def rewardOutputScript(delta: Int, minerPk: ProveDlog): ErgoTree = {
    ErgoTree.withSegregation(ZeroHeader, SigmaAnd(
      GE(Height, Plus(boxCreationHeight(Self), IntConstant(delta))).toSigmaProp,
      SigmaPropConstant(minerPk)
    ))
  }

  /**
    * Proposition that allows to send coins to a box which is protected by the following proposition:
    * prove dlog of miner's public key and height is at least `delta` blocks bigger then the current one.
    */
  def feeProposition(delta: Int = 720): ErgoTree = {
    val out = ByIndex(Outputs, IntConstant(0))
    ErgoTree.withSegregation(ZeroHeader, AND(
      EQ(Height, boxCreationHeight(out)),
      EQ(ExtractScriptBytes(out), expectedMinerOutScriptBytesVal(delta, MinerPubkey)),
      EQ(SizeOf(Outputs), 1)
    ).toSigmaProp)
  }

  /**
    * A contract that only allows to collect emission reward by a box with miner proposition.
    */
  def emissionBoxProp(s: MonetarySettings): ErgoTree = {
    val rewardOut = ByIndex(Outputs, IntConstant(0))
    val minerOut = ByIndex(Outputs, IntConstant(1))
    val minersReward = s.fixedRate - s.foundersInitialReward
    val minersFixedRatePeriod = s.fixedRatePeriod + 2 * s.epochLength
    val epoch = Plus(IntConstant(1), Divide(Minus(Height, IntConstant(s.fixedRatePeriod)), IntConstant(s.epochLength)))
    val coinsToIssue = If(LT(Height, IntConstant(minersFixedRatePeriod)),
      minersReward,
      Minus(s.fixedRate, Multiply(s.oneEpochReduction, epoch.upcastTo(SLong)))
    )
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(rewardOut))
    val heightCorrect = EQ(boxCreationHeight(rewardOut), Height)
    val heightIncreased = GT(Height, boxCreationHeight(Self))
    val correctCoinsConsumed = EQ(coinsToIssue, Minus(ExtractAmount(Self), ExtractAmount(rewardOut)))
    val lastCoins = LE(ExtractAmount(Self), s.oneEpochReduction)
    val outputsNum = EQ(SizeOf(Outputs), 2)
    val correctMinerOutput = AND(
      EQ(ExtractScriptBytes(minerOut), expectedMinerOutScriptBytesVal(s.minerRewardDelay, MinerPubkey)),
      EQ(Height, boxCreationHeight(minerOut))
    )
    ErgoTree.withSegregation(ZeroHeader,
      AND(
        heightIncreased,
        correctMinerOutput,
        OR(AND(outputsNum, sameScriptRule, correctCoinsConsumed, heightCorrect), lastCoins)
      ).toSigmaProp
    )
  }

  /**
    * Script for Ergo foundation box.
    * The script allows to spend a box, if:
    * - first transaction output contains at least EmissionRules.remainingFoundationAtHeight coins in it
    * - first transaction output is protected by this script
    * - conditions from the first non-mandatory register (R4) are satisfied
    *
    * Thus, this script always controls the level of emission and does not allow to take
    * more coinы than prescribed by emission rules. In addition, it is protected by
    * custom proposition in R4 which is assumed to be a simple 2-of-3 multisignature with
    * public keys of foundation members in the beginning. When foundation members spend
    * this box, they are free to put any new proposition to the R4 register, thus they
    * may add or remove members, or change it to something more complicated like
    * `tokenThresholdScript`.
    */
  def foundationScript(s: MonetarySettings): ErgoTree = {
    // new output of the foundation
    val newFoundationBox = ByIndex(Outputs, IntConstant(0))
    // calculate number of coins, that are not issued yet and should be kept in `newFoundationBox`
    // the same as Emission.remainingFoundationRewardAtHeight rewritten in Ergo script
    val remainingAmount = {
      val full15reward = (s.foundersInitialReward - 2 * s.oneEpochReduction) * s.epochLength
      val full45reward = (s.foundersInitialReward - s.oneEpochReduction) * s.epochLength
      val fixedRatePeriodMinus1: Int = s.fixedRatePeriod - 1
      If(LT(Height, IntConstant(s.fixedRatePeriod)),
        Plus(
          LongConstant(full15reward + full45reward),
          Multiply(s.foundersInitialReward, Upcast(Minus(fixedRatePeriodMinus1, Height), SLong))
        ),
        If(LT(Height, IntConstant(s.fixedRatePeriod + s.epochLength)),
          Plus(
            full15reward,
            Multiply(
              s.foundersInitialReward - s.oneEpochReduction,
              Upcast(Minus(fixedRatePeriodMinus1 + s.epochLength, Height), SLong)
            )
          ),
          If(LT(Height, IntConstant(s.fixedRatePeriod + 2 * s.epochLength)),
            Multiply(
              s.foundersInitialReward - 2 * s.oneEpochReduction,
              Upcast(Minus(fixedRatePeriodMinus1 + 2 * s.epochLength, Height), SLong)
            ),
            LongConstant(0)
          )
        )
      )
    }
    // check, that `newFoundationBox` contains at least `remainingAmount`
    val amountCorrect = GE(ExtractAmount(newFoundationBox), remainingAmount)
    // check, that `newFoundationBox` have the same protecting script
    val sameScriptRule = EQ(ExtractScriptBytes(Self), ExtractScriptBytes(newFoundationBox))
    // check, that additional rules defined by foundation members are satisfied
    val customProposition = DeserializeRegister(ErgoBox.R4, SSigmaProp)
    // combine 3 conditions above with AND conjunction
    ErgoTree.withSegregation(ZeroHeader, SigmaAnd(amountCorrect.toSigmaProp, sameScriptRule.toSigmaProp, customProposition))
  }

  /**
    * Creation height of a box
    */
  def boxCreationHeight(box: Value[SBox.type]): Value[SInt.type] =
    SelectField(ExtractCreationInfo(box), 1).asIntValue
}
