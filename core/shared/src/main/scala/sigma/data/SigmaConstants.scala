package sigma.data

import sigma.util.CollectionUtil.TraversableOps // used in Scala 2.11

/** Descriptor of a constant which represents some size value.
  * @tparam T type of the constant value
  * @param value value of the constant
  * @param description description of the constant purpose
  */
case class SizeConstant[T: Numeric](value: T, description: String)

/** Constants facade that provide access to the values used in sigma's logic and checks.
  * All the constants are collected in a sequence.
  * Each constant has id, value and description.
  * The constant ids are stable and never change.
  * Some constant values may be defined in terms of other more fundamental values.
  * In the future versions of sigma, the values may change,
  * but due to versioned execution, all versions of the values should be
  * available simultaneously.
  */
object SigmaConstants {

  object MaxBoxSize extends SizeConstant[Int](4 * 1024,
    "Box size should not be greater than provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](110,
    "Max tree depth should not be greater then provided value") {
  }

  object MaxTokens extends SizeConstant[Int](255,
    "Tokens count should not be greater than provided value") {
  }

  object MaxRegisters extends SizeConstant[Int](10,
    "Registers count should not be greater than provided value") {
  }

  object MaxPropositionBytes extends SizeConstant[Int](4096 /*4K*/,
    "Max length of Box.propositionBytes collection") {
  }

  object MaxBigIntSizeInBytes extends SizeConstant[Long](32L,
    "BigInt size in bytes should not be greater than provided value") {
  }

  object MaxSigmaPropSizeInBytes extends SizeConstant[Long](1024L,
    "SigmaProp size in bytes should not be greater than provided value") {
  }

  object MaxTupleLength extends SizeConstant[Int](255,
    "Tuple length should not be greater than provided value") {
  }

  object MaxChildrenCountForAtLeastOp extends SizeConstant[Int](255,
    "Max children count should not be greater than provided value") {
  }

  object VotesArraySize extends SizeConstant[Int](3,
    "Size of of Header.votes array") {
  }

  object AutolykosPowSolutionNonceArraySize extends SizeConstant[Int](8,
    "size of nonce array from Autolykos POW solution in Header.powNonce array") {
  }

}
