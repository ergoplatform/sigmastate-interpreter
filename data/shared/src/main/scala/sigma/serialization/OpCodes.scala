package sigma.serialization

import sigma.ast.TypeCodes.LastConstantCode
import supertagged.TaggedType

/** Encoding of values for serialization. */
object ValueCodes {
  object OpCode extends TaggedType[Byte]
  type OpCode = OpCode.Type

}

/** The set of all possible IR graph nodes can be split in two subsets:
  * 1) operations which may appear in ErgoTree (these are defined by `OpCodes` below)
  * 2) operations which are not valid to be in ErgoTree, but serve special purposes. (these are defined by `OpCodesExtra`)
  * We can assume they are both Byte-sized codes, and store as a single byte, but as long as we can differentiate them
  * from context (and where we cannot, we should use special encoding).
  *
  * The general extended encoding is like the following:
  * 0-255 - range of OpCodes
  * 256-511 - range of OpCodesExtra
  * Thus, any code in an extended code range of 0-511 can be saved using `putUShort`.
  * We use Byte to represent OpCodes and OpCodesExtra.
  * We use Short to represent any op code from extended code range.
  * And we use VLQ to serialize Short values of extended codes.
  *
  * Examples:
  * 1) For validation rule CheckValidOpCode we use OpCodes range, so we use single byte encoding.
  * 2) For CheckCostFuncOperation we use 1-511 range and extended encoding (see docs)
  */
object OpCodes {
  import ValueCodes._

  private def newOpCode(shift: Short): OpCode = OpCode @@ (LastConstantCode + shift).toByte

  // serialization is not required
  val Undefined: OpCode = OpCode @@ 0.toByte

  // variables
  val TaggedVariableCode: OpCode = newOpCode(1)
  val ValUseCode: OpCode = newOpCode(2)
  val ConstantPlaceholderCode: OpCode = newOpCode(3)
  val SubstConstantsCode: OpCode = newOpCode(4) // reserved 5 - 9 (5)

  val LongToByteArrayCode  : OpCode = newOpCode(10)
  val ByteArrayToBigIntCode: OpCode = newOpCode(11)
  val ByteArrayToLongCode  : OpCode = newOpCode(12)
  val DowncastCode         : OpCode = newOpCode(13)
  val UpcastCode           : OpCode = newOpCode(14)

  // EvaluatedValue descendants
  val TrueCode              : OpCode = newOpCode(15)
  val FalseCode             : OpCode = newOpCode(16)
  val UnitConstantCode      : OpCode = newOpCode(17)
  val GroupGeneratorCode    : OpCode = newOpCode(18)
  val ConcreteCollectionCode: OpCode = newOpCode(19) // reserved 20 (1)
  val ConcreteCollectionBooleanConstantCode: OpCode = newOpCode(21)

  val TupleCode      : OpCode = newOpCode(22)
  val Select1Code    : OpCode = newOpCode(23)
  val Select2Code    : OpCode = newOpCode(24)
  val Select3Code    : OpCode = newOpCode(25)
  val Select4Code    : OpCode = newOpCode(26)
  val Select5Code    : OpCode = newOpCode(27)
  val SelectFieldCode: OpCode = newOpCode(28) // reserved 29-30 (2)

  // Relation descendants
  val LtCode     : OpCode = newOpCode(31)
  val LeCode     : OpCode = newOpCode(32)
  val GtCode     : OpCode = newOpCode(33)
  val GeCode     : OpCode = newOpCode(34)
  val EqCode     : OpCode = newOpCode(35)
  val NeqCode    : OpCode = newOpCode(36)
  val IfCode     : OpCode = newOpCode(37)
  val AndCode    : OpCode = newOpCode(38)
  val OrCode     : OpCode = newOpCode(39)
  val AtLeastCode: OpCode = newOpCode(40)

  // Arithmetic codes
  val MinusCode        : OpCode = newOpCode(41)
  val PlusCode         : OpCode = newOpCode(42)
  val XorCode          : OpCode = newOpCode(43)
  val MultiplyCode     : OpCode = newOpCode(44)
  val DivisionCode     : OpCode = newOpCode(45)
  val ModuloCode       : OpCode = newOpCode(46)
  val ExponentiateCode : OpCode = newOpCode(47)
  val MultiplyGroupCode: OpCode = newOpCode(48)
  val MinCode          : OpCode = newOpCode(49)
  val MaxCode          : OpCode = newOpCode(50)

  // Environment codes
  val HeightCode               : OpCode = newOpCode(51)
  val InputsCode               : OpCode = newOpCode(52)
  val OutputsCode              : OpCode = newOpCode(53)
  val LastBlockUtxoRootHashCode: OpCode = newOpCode(54)
  val SelfCode                 : OpCode = newOpCode(55) // reserved 56 - 59 (4)

  val MinerPubkeyCode          : OpCode = newOpCode(60)

  // Collection and tree operations codes
  val MapCollectionCode    : OpCode = newOpCode(61)
  val ExistsCode           : OpCode = newOpCode(62)
  val ForAllCode           : OpCode = newOpCode(63)
  val FoldCode             : OpCode = newOpCode(64)
  val SizeOfCode           : OpCode = newOpCode(65)
  val ByIndexCode          : OpCode = newOpCode(66)
  val AppendCode           : OpCode = newOpCode(67)
  val SliceCode            : OpCode = newOpCode(68)
  val FilterCode           : OpCode = newOpCode(69)
  val AvlTreeCode          : OpCode = newOpCode(70)
  val AvlTreeGetCode       : OpCode = newOpCode(71)
  val FlatMapCollectionCode: OpCode = newOpCode(72) // reserved 73 - 80 (8)

  // Type casts codes
  val ExtractAmountCode        : OpCode = newOpCode(81)
  val ExtractScriptBytesCode   : OpCode = newOpCode(82)
  val ExtractBytesCode         : OpCode = newOpCode(83)
  val ExtractBytesWithNoRefCode: OpCode = newOpCode(84)
  val ExtractIdCode            : OpCode = newOpCode(85)
  val ExtractRegisterAs        : OpCode = newOpCode(86)
  val ExtractCreationInfoCode  : OpCode = newOpCode(87) // reserved 88 - 90 (3)

  // Cryptographic operations codes
  val CalcBlake2b256Code         : OpCode = newOpCode(91)
  val CalcSha256Code             : OpCode = newOpCode(92)
  val ProveDlogCode              : OpCode = newOpCode(93)
  val ProveDiffieHellmanTupleCode: OpCode = newOpCode(94)
  val SigmaPropIsProvenCode      : OpCode = newOpCode(95)
  val SigmaPropBytesCode         : OpCode = newOpCode(96)
  val BoolToSigmaPropCode        : OpCode = newOpCode(97)
  // we don't rely on this yet but it's nice to have TrivialPropFalseCode.toUByte < TrivialPropTrueCode.toUByte
  val TrivialPropFalseCode       : OpCode = newOpCode(98)
  val TrivialPropTrueCode        : OpCode = newOpCode(99)

  // Deserialization codes
  val DeserializeContextCode : OpCode = newOpCode(100)
  val DeserializeRegisterCode: OpCode = newOpCode(101) // Block codes
  val ValDefCode: OpCode = newOpCode(102)
  val FunDefCode: OpCode = newOpCode(103)
  val BlockValueCode: OpCode = newOpCode(104)
  val FuncValueCode: OpCode = newOpCode(105)
  val FuncApplyCode: OpCode = newOpCode(106)
  val PropertyCallCode: OpCode = newOpCode(107)
  val MethodCallCode: OpCode = newOpCode(108)
  val GlobalCode    : OpCode = newOpCode(109)

  val SomeValueCode: OpCode = newOpCode(110)
  val NoneValueCode: OpCode = newOpCode(111) // reserved 112 - 114 (3)

  val GetVarCode         : OpCode = newOpCode(115)
  val OptionGetCode      : OpCode = newOpCode(116)
  val OptionGetOrElseCode: OpCode = newOpCode(117)
  val OptionIsDefinedCode: OpCode = newOpCode(118)

  // Modular arithmetic operations codes
  val ModQCode     : OpCode = newOpCode(119)
  val PlusModQCode : OpCode = newOpCode(120)
  val MinusModQCode: OpCode = newOpCode(121)

  val SigmaAndCode : OpCode = newOpCode(122)
  val SigmaOrCode  : OpCode = newOpCode(123)
  val BinOrCode    : OpCode = newOpCode(124)
  val BinAndCode   : OpCode = newOpCode(125)

  val DecodePointCode: OpCode = newOpCode(126)

  val LogicalNotCode : OpCode = newOpCode(127)
  val NegationCode   : OpCode = newOpCode(128)
  val BitInversionCode   : OpCode = newOpCode(129)
  val BitOrCode      : OpCode = newOpCode(130)
  val BitAndCode     : OpCode = newOpCode(131)

  val BinXorCode     : OpCode = newOpCode(132)

  val BitXorCode     : OpCode = newOpCode(133)
  val BitShiftRightCode    : OpCode = newOpCode(134)
  val BitShiftLeftCode     : OpCode = newOpCode(135)
  val BitShiftRightZeroedCode     : OpCode = newOpCode(136)

  val CollShiftRightCode    : OpCode = newOpCode(137)
  val CollShiftLeftCode     : OpCode = newOpCode(138)
  val CollShiftRightZeroedCode     : OpCode = newOpCode(139)

  val CollRotateLeftCode     : OpCode = newOpCode(140)
  val CollRotateRightCode     : OpCode = newOpCode(141)

  val ContextCode             : OpCode = newOpCode(142)
  val XorOfCode               : OpCode = newOpCode(143) // equals to 255

}
