package sigma.ast

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.util.encode.{Base16, Base58, Base64}
import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.ast.SOption.SIntOption
import sigma.ast.syntax._
import sigma.data.Nullable
import sigma.exceptions.InvalidArguments
import sigma.serialization.CoreByteWriter.ArgInfo
import sigma.serialization.ValueSerializer

import java.math.BigInteger

object SigmaPredef {

  type IrBuilderFunc = PartialFunction[(SValue, Seq[SValue]), SValue]

  /** Metadata for predefined function.
    * @param irBuilder Builder of SigmaIR node which is equivalent to function application
    *                  Rule: Apply(f, args) -->  irBuilder(f, args)
    */
  case class PredefFuncInfo(irBuilder: IrBuilderFunc)

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Metadata for this function */
    irInfo: PredefFuncInfo,
    docInfo: OperationInfo) {

    val sym: Ident = Ident(name, declaration.tpe)
    val symNoType: Ident = Ident(name, NoType)

    def argInfo(argName: String): ArgInfo =
      docInfo.args.find(_.name == argName).get
  }

  class PredefinedFuncRegistry(builder: SigmaBuilder) {

    import builder._

    /** Type variable used in the signatures of global functions below. */
    import SType.{paramR, paramT, tK, tL, tO, tR, tT}

    private val undefined: IrBuilderFunc =
      PartialFunction.empty[(SValue, Seq[SValue]), SValue]

    val AllOfFunc = PredefinedFunc("allOf",
      Lambda(Array("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkAND(col) }),
      OperationInfo(AND, "Returns true if \\emph{all} the elements in collection are \\lst{true}.",
       Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val AnyOfFunc = PredefinedFunc("anyOf",
      Lambda(Array("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo( { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkOR(col) }),
      OperationInfo(OR, "Returns true if \\emph{any} the elements in collection are \\lst{true}.",
        Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val XorOfFunc = PredefinedFunc("xorOf",
      Lambda(Array("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkXorOf(col) }),
      OperationInfo(XorOf, "Similar to \\lst{allOf}, but performing logical XOR operation between all conditions instead of \\lst{&&}",
        Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val AllZKFunc = PredefinedFunc("allZK",
      Lambda(Array("propositions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      PredefFuncInfo(undefined),
      OperationInfo(SigmaAnd, "Returns sigma proposition which is proven when \\emph{all} the elements in collection are proven.",
        Seq(ArgInfo("propositions", "a collection of propositions")))
    )

    val AnyZKFunc = PredefinedFunc("anyZK",
      Lambda(Array("propositions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      PredefFuncInfo(undefined),
      OperationInfo(SigmaOr, "Returns sigma proposition which is proven when \\emph{any} of the elements in collection is proven.",
        Seq(ArgInfo("propositions", "a collection of propositions")))
    )

    val AtLeastFunc = PredefinedFunc("atLeast",
      Lambda(Array("k" -> SInt, "conditions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(bound: IntValue@unchecked, arr: Value[SCollection[SSigmaProp.type]]@unchecked)) =>
          mkAtLeast(bound, arr)
        }),
      OperationInfo(AtLeast,
        """ Logical threshold.
         | AtLeast has two inputs: integer \lst{bound} and \lst{children} same as in AND/OR.
         | The result is true if at least \lst{bound} children are proven.
        """.stripMargin, Seq(
          ArgInfo("bound", "required minimum of proven children"),
          ArgInfo("children", "proposition to be proven/validated")))
    )

    val OuterJoinFunc = PredefinedFunc(
      "outerJoin",
      Lambda(
        Array(STypeParam(tK), STypeParam(tL), STypeParam(tR), STypeParam(tO)),
        Array(
          "left" -> SCollection(STuple(tK, tL)),
          "right" -> SCollection(STuple(tK, tR)),
          "l" -> SFunc(Array(tK, tL), tO),
          "r" -> SFunc(Array(tK, tR), tO),
          "inner" -> SFunc(Array(tK, tL, tR), tO)
        ),
        SCollection(STuple(tK, tO)), None),
      PredefFuncInfo(undefined),
      OperationInfo(MethodCall, "",
          Seq(ArgInfo("", "")))
    )

    val ZKProofFunc = PredefinedFunc("ZKProof",
      Lambda(Array("block" -> SSigmaProp), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(block: SigmaPropValue@unchecked)) => mkZKProofBlock(block) }),
      OperationInfo(ZKProofBlock, "",
          Seq(ArgInfo("", "")))
    )

    val SigmaPropFunc = PredefinedFunc("sigmaProp",
      Lambda(Array("condition" -> SBoolean), SSigmaProp, None),
      PredefFuncInfo({ case (_, Seq(b: BoolValue@unchecked)) => mkBoolToSigmaProp(b) }),
      OperationInfo(BoolToSigmaProp,
        """Embedding of \lst{Boolean} values to \lst{SigmaProp} values.
         | As an example, this operation allows boolean experessions
         | to be used as arguments of \lst{atLeast(..., sigmaProp(boolExpr), ...)} operation.
         | During execution results to either \lst{TrueProp} or \lst{FalseProp} values of \lst{SigmaProp} type.
        """.stripMargin,
          Seq(ArgInfo("condition", "boolean value to embed in SigmaProp value")))
    )

    val GetVarFunc = PredefinedFunc("getVar",
      Lambda(Array(paramT), Array("varId" -> SByte), SOption(tT), None),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, SOption(rtpe), _)), Seq(id: Constant[SNumericType]@unchecked)) =>
          mkGetVar(SByte.downcast(id.value.asInstanceOf[AnyVal]), rtpe)
        }),
      OperationInfo(GetVar,
        "Get context variable with given \\lst{varId} and type.",
        Seq(ArgInfo("varId", "\\lst{Byte} identifier of context variable")))
    )

    val GetVarFromInputFunc = PredefinedFunc("getVarFromInput",
      Lambda(Array(paramT), Array("inputId" -> SShort, "varId" -> SByte), SOption(tT), None),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, SOption(rtpe), _)), Seq(inputId: Constant[SNumericType]@unchecked, varId: Constant[SNumericType]@unchecked)) =>
          mkMethodCall(Context, SContextMethods.getVarFromInputMethod, IndexedSeq(SShort.downcast(inputId.value.asInstanceOf[AnyVal]), SByte.downcast(varId.value.asInstanceOf[AnyVal])), Map(tT -> rtpe))
        }),
      OperationInfo(MethodCall,
        "Get context variable with given \\lst{varId} and type.",
        Seq(ArgInfo("inputId", "\\lst{Byte} index of input to read context variable from"),
            ArgInfo("varId", "\\lst{Byte} identifier of context variable")))
    )

    def PKFunc(networkPrefix: NetworkPrefix) = PredefinedFunc("PK",
      Lambda(Array("input" -> SString), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ErgoAddressEncoder(networkPrefix).fromString(arg.value).get match {
            case a: P2PKAddress => SigmaPropConstant(a.pubkey)
            case a => sys.error(s"unsupported address $a")
          }
        }),
      OperationInfo(Constant, "",
          Seq(ArgInfo("", "")))
    )

    val DeserializeFunc = PredefinedFunc("deserialize",
      Lambda(Array(paramT), Array("str" -> SString), tT, None),
      PredefFuncInfo(
      { case (Ident(_, SFunc(_, tpe, _)), args) =>
        if (args.length != 1)
          throw new InvalidArguments(s"Wrong number of arguments in $args: expected one argument")
        val str = args.head match {
          case StringConstant(s) => s
          case _ =>
            throw new InvalidArguments(s"invalid argument in $args: expected a string constant")
        }
        val bytes = Base58.decode(str).get
        val res = ValueSerializer.deserialize(bytes)
        if (res.tpe != tpe)
          throw new InvalidArguments(s"Wrong type after deserialization, expected $tpe, got ${res.tpe}")
        res
      }),
      OperationInfo(Constant, "Deserializes values from Base58 encoded binary data at compile time into a value of type T.",
          Seq(ArgInfo("", "")))
    )

    val BigIntFromStringFunc = PredefinedFunc("bigInt",
      Lambda(Array("input" -> SString), SBigInt, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          BigIntConstant(new BigInteger(arg.value))
        }),
      OperationInfo(Constant,
        """Parsing string literal argument as a 256-bit signed big integer.""".stripMargin,
        Seq(ArgInfo("", "")))
    )

    val UBigIntFromStringFunc = PredefinedFunc("unsignedBigInt",
      Lambda(Array("input" -> SString), SUnsignedBigInt, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          val bi = new BigInteger(arg.value)
          if (bi.compareTo(BigInteger.ZERO) >= 0) {
            UnsignedBigIntConstant(bi)
          } else {
            throw new InvalidArguments(s"Negative argument for unsignedBigInt()")
          }
        }),
      OperationInfo(Constant,
        """Parsing string literal argument as a 256-bit unsigned big integer.""".stripMargin,
        Seq(ArgInfo("", "")))
    )

    val FromBase16Func = PredefinedFunc("fromBase16",
      Lambda(Array("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base16.decode(arg.value).get)
        }),
      OperationInfo(Constant,
        """Transforms Base16 encoded string literal into constant of type Coll[Byte].
         |It is a compile-time operation and only string literal (constant) can be its
         |argument.
        """.stripMargin,
        Seq(ArgInfo("", "")))
    )

    val FromBase58Func = PredefinedFunc("fromBase58",
      Lambda(Array("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base58.decode(arg.value).get)
        }),
      OperationInfo(Constant,
        """Transforms Base58 encoded string literal into constant of type Coll[Byte].
         |It is a compile-time operation and only string literal (constant) can be its
         |argument.
        """.stripMargin,
        Seq(ArgInfo("", "")))
    )

    val FromBase64Func = PredefinedFunc("fromBase64",
      Lambda(Array("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base64.decode(arg.value).get)
        }),
      OperationInfo(Constant,
        """Transforms Base64 encoded string literal into constant of type Coll[Byte].
         |It is a compile-time operation and only string literal (constant) can be its
         |argument.
        """.stripMargin,
          Seq(ArgInfo("", "")))
    )

    val Blake2b256Func = PredefinedFunc("blake2b256",
      Lambda(Array("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcBlake2b256(arg)
        }),
      OperationInfo(CalcBlake2b256, "Calculate Blake2b hash from \\lst{input} bytes.",
          Seq(ArgInfo("input", "collection of bytes")))
    )

    val Sha256Func = PredefinedFunc("sha256",
      Lambda(Array("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcSha256(arg)
        }),
      OperationInfo(CalcSha256, "Calculate Sha256 hash from \\lst{input} bytes.",
          Seq(ArgInfo("input", "collection of bytes")))
    )

    val ByteArrayToBigIntFunc = PredefinedFunc("byteArrayToBigInt",
      Lambda(Array("input" -> SByteArray), SBigInt, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToBigInt(arg)
        }),
      OperationInfo(ByteArrayToBigInt,
        "Convert big-endian bytes representation (Coll[Byte]) to BigInt value.",
        Seq(ArgInfo("input", "collection of bytes in big-endian format")))
    )

    val ByteArrayToLongFunc = PredefinedFunc("byteArrayToLong",
      Lambda(Array("input" -> SByteArray), SLong, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToLong(arg)
        }),
      OperationInfo(ByteArrayToLong, "Convert big-endian bytes representation (Coll[Byte]) to Long value.",
          Seq(ArgInfo("input", "collection of bytes in big-endian format")))
    )

    val DecodePointFunc = PredefinedFunc("decodePoint",
      Lambda(Array("input" -> SByteArray), SGroupElement, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkDecodePoint(arg)
        }),
      OperationInfo(DecodePoint,
        "Convert \\lst{Coll[Byte]} to \\lst{GroupElement} using \\lst{GroupElementSerializer}",
          Seq(ArgInfo("input", "serialized bytes of some \\lst{GroupElement} value")))
    )

    val LongToByteArrayFunc = PredefinedFunc("longToByteArray",
      Lambda(Array("input" -> SLong), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SLong.type]@unchecked)) =>
          mkLongToByteArray(arg)
        }),
      OperationInfo(LongToByteArray,
        "Converts \\lst{Long} value to big-endian bytes representation.",
        Seq(ArgInfo("input", "value to convert")))
    )

    val ProveDHTupleFunc = PredefinedFunc("proveDHTuple",
      Lambda(Array("g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(g, h, u, v)) =>
            mkCreateProveDHTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement)
        }),
      OperationInfo(CreateProveDHTuple,
        """ ErgoTree operation to create a new SigmaProp value representing public key
         | of Diffie Hellman signature protocol.
         | Common input: (g,h,u,v)
        """.stripMargin,
          Seq(ArgInfo("g", ""),ArgInfo("h", ""),ArgInfo("u", ""),ArgInfo("v", "")))
    )

    val ProveDlogFunc = PredefinedFunc("proveDlog",
      Lambda(Array("value" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SGroupElement.type]@unchecked)) =>
          mkCreateProveDlog(arg)
        }),
      OperationInfo(CreateProveDlog,
        """ErgoTree operation to create a new \lst{SigmaProp} value representing public key
         | of discrete logarithm signature protocol.
        """.stripMargin,
          Seq(ArgInfo("value", "element of elliptic curve group")))
    )

    val AvlTreeFunc = PredefinedFunc("avlTree",
      Lambda(Array("operationFlags" -> SByte, "digest" -> SByteArray, "keyLength" -> SInt, "valueLengthOpt" -> SIntOption), SAvlTree, None),
      PredefFuncInfo(
        { case (_, Seq(flags, digest, keyLength, valueLength)) =>
          mkCreateAvlTree(flags.asByteValue, digest.asByteArray, keyLength.asIntValue, valueLength.asOption[SInt.type])
        }),
      OperationInfo(CreateAvlTree,
        "Construct a new authenticated dictionary with given parameters and tree root digest.",
        Seq(
          ArgInfo("operationFlags", "flags of available operations"),
          ArgInfo("digest", "hash of merkle tree root"),
          ArgInfo("keyLength", "length of dictionary keys in bytes"),
          ArgInfo("valueLengthOpt", "optional width of dictionary values in bytes")))
    )

    val SubstConstantsFunc = PredefinedFunc("substConstants",
      Lambda(
        Seq(paramT),
        Array("scriptBytes" -> SByteArray, "positions" -> SIntArray, "newValues" -> SCollection(tT)),
        SByteArray, None
      ),
      PredefFuncInfo(
        { case (_, Seq(scriptBytes, positions, newValues)) =>
          mkSubstConst(scriptBytes.asByteArray, positions.asIntArray, newValues.asInstanceOf[Value[SCollection[SType]]])
        }),
      OperationInfo(SubstConstants,
        """Transforms serialized bytes of ErgoTree with segregated constants by replacing constants
         | at given positions with new values. This operation allow to use serialized scripts as
         | pre-defined templates.
         | The typical usage is "check that output box have proposition equal to given script bytes,
         | where minerPk (constants(0)) is replaced with currentMinerPk".
         | Each constant in original scriptBytes have SType serialized before actual data (see ConstantSerializer).
         | During substitution each value from newValues is checked to be an instance of the corresponding type.
         | This means, the constants during substitution cannot change their types.
         |
         | Returns original scriptBytes array where only specified constants are replaced and all other bytes remain exactly the same.
        """.stripMargin, Seq(
        ArgInfo("scriptBytes", "serialized ErgoTree with ConstantSegregationFlag set to 1."),
        ArgInfo("positions", "zero based indexes in ErgoTree.constants array which should be replaced with new values"),
        ArgInfo("newValues", "new values to be injected into the corresponding positions in ErgoTree.constants array")))
    )

    val ExecuteFromVarFunc = PredefinedFunc("executeFromVar",
      Lambda(
        Seq(paramT),
        Array("id" -> SByte),
        tT, None
      ),
      PredefFuncInfo(undefined),
      OperationInfo(DeserializeContext,
        """Extracts context variable as \lst{Coll[Byte]}, deserializes it to script
         | and then executes this script in the current context.
         | The original \lst{Coll[Byte]} of the script is available as \lst{getVar[Coll[Byte]](id)}.
         | Type parameter \lst{V} result type of the deserialized script.
         | Throws an exception if the actual script type doesn't conform to T.
         | Returns a result of the script execution in the current context
        """.stripMargin,
        Seq(ArgInfo("id", "identifier of the context variable")))
    )

    val ExecuteFromSelfRegFunc = PredefinedFunc("executeFromSelfReg",
      Lambda(
        Seq(paramT),
        Array("id" -> SByte, "default" -> SOption(tT)),
        tT, None
      ),
      PredefFuncInfo(undefined),
      OperationInfo(DeserializeRegister,
        """Extracts SELF register as \lst{Coll[Byte]}, deserializes it to script
         | and then executes this script in the current context.
         | The original \lst{Coll[Byte]} of the script is available as \lst{SELF.getReg[Coll[Byte]](id)}.
         | Type parameter \lst{T} result type of the deserialized script.
         | Throws an exception if the actual script type doesn't conform to \lst{T}.
         | Returns a result of the script execution in the current context
        """.stripMargin,
        Seq(ArgInfo("id", "identifier of the register"),
          ArgInfo("default", "optional default value, if register is not available")))
    )

    val SerializeFunc = PredefinedFunc("serialize",
      Lambda(Seq(paramT), Array("value" -> tT), SByteArray, None),
      irInfo = PredefFuncInfo(
        irBuilder = { case (_, args @ Seq(value)) =>
          MethodCall.typed[Value[SCollection[SByte.type]]](
            Global,
            SGlobalMethods.serializeMethod.withConcreteTypes(Map(tT -> value.tpe)),
            args.toIndexedSeq,
            Map()
          )
        }),
      docInfo = OperationInfo(MethodCall,
        """Serializes the given `value` into bytes using the default serialization format.
        """.stripMargin,
        Seq(ArgInfo("value", "value to serialize"))
      )
    )

    val FromBigEndianBytesFunc = PredefinedFunc("fromBigEndianBytes",
      Lambda(Seq(paramT), Array("bytes" -> SByteArray), tT, None),
      irInfo = PredefFuncInfo(
        irBuilder = { case (u, args) =>
          val resType = u.opType.tRange.asInstanceOf[SFunc].tRange
          MethodCall(
            Global,
            SGlobalMethods.fromBigEndianBytesMethod.withConcreteTypes(Map(tT -> resType)),
            args.toIndexedSeq,
            Map(tT -> resType)
          )
        }),
      docInfo = OperationInfo(MethodCall,
        """Deserializes provided big endian bytes into a numeric value of given type.
        """.stripMargin,
        Seq(ArgInfo("bytes", "bytes to deserialize"))
      )
    )

    val globalFuncs: Map[String, PredefinedFunc] = Seq(
      AllOfFunc,
      AnyOfFunc,
      XorOfFunc,
      AllZKFunc,
      AnyZKFunc,
      AtLeastFunc,
      OuterJoinFunc,
      ZKProofFunc,
      SigmaPropFunc,
      GetVarFunc,
      DeserializeFunc,
      BigIntFromStringFunc,
      UBigIntFromStringFunc,
      FromBase16Func,
      FromBase64Func,
      FromBase58Func,
      Blake2b256Func,
      Sha256Func,
      ByteArrayToBigIntFunc,
      ByteArrayToLongFunc,
      DecodePointFunc,
      LongToByteArrayFunc,
      ProveDHTupleFunc,
      ProveDlogFunc,
      AvlTreeFunc,
      SubstConstantsFunc,
      ExecuteFromVarFunc,
      ExecuteFromSelfRegFunc,
      SerializeFunc,
      GetVarFromInputFunc,
      FromBigEndianBytesFunc
    ).map(f => f.name -> f).toMap

    def comparisonOp(symbolName: String, opDesc: ValueCompanion, desc: String, args: Seq[ArgInfo]) = {
      PredefinedFunc(symbolName,
        Lambda(Seq(paramT), Array("left" -> tT, "right" -> tT), SBoolean, None),
        PredefFuncInfo(undefined),
        OperationInfo(opDesc, desc, args)
      )
    }
    def binaryOp(symbolName: String, opDesc: ValueCompanion, desc: String, args: Seq[ArgInfo]) = {
      PredefinedFunc(symbolName,
        Lambda(Seq(paramT), Array("left" -> tT, "right" -> tT), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(opDesc, desc, args)
      )
    }
    def logicalOp(symbolName: String, opDesc: ValueCompanion, desc: String, args: Seq[ArgInfo]) = {
      PredefinedFunc(symbolName,
        Lambda(Array("left" -> SBoolean, "right" -> SBoolean), SBoolean, None),
        PredefFuncInfo(undefined),
        OperationInfo(opDesc, desc, args)
      )
    }

    val infixFuncs: Map[String, PredefinedFunc] = Seq(
      comparisonOp("==", EQ, "Compare equality of \\lst{left} and \\lst{right} arguments",
          Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      comparisonOp("!=", NEQ, "Compare inequality of \\lst{left} and \\lst{right} arguments",
          Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      comparisonOp("<", LT,
        "Returns \\lst{true} is the left operand is less then the right operand, \\lst{false} otherwise.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      comparisonOp("<=", LE,
        "Returns \\lst{true} is the left operand is less then or equal to the right operand, \\lst{false} otherwise.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      comparisonOp(">", GT,
        "Returns \\lst{true} is the left operand is greater then the right operand, \\lst{false} otherwise.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      comparisonOp(">=", GE,
        "Returns \\lst{true} is the left operand is greater then or equal to the right operand, \\lst{false} otherwise.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      binaryOp("+", ArithOp.Plus, "Returns a sum of two numeric operands",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("-", ArithOp.Minus, "Returns a result of subtracting second numeric operand from the first.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("*", ArithOp.Multiply, "Returns a multiplication of two numeric operands",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("/", ArithOp.Division, "Integer division of the first operand by the second operand.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("%", ArithOp.Modulo, "Remainder from division of the first operand by the second operand.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("min", ArithOp.Min, "Minimum value of two operands.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("max", ArithOp.Max, "Maximum value of two operands.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      binaryOp("bit_|", BitOp.BitOr, "Bitwise OR of two numeric operands.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("bit_&", BitOp.BitAnd, "Bitwise AND of two numeric operands.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("bit_^", BitOp.BitXor, "Bitwise XOR of two numeric operands.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      binaryOp("bit_>>", BitOp.BitShiftRight, "Right shift of bits.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("bit_<<", BitOp.BitShiftLeft, "Left shift of bits.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binaryOp("bit_>>>", BitOp.BitShiftRightZeroed, "Right shift of bits.",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      PredefinedFunc("binary_|",
        Lambda(Array("left" -> SByteArray, "right" -> SByteArray), SByteArray, None),
        PredefFuncInfo(undefined),
        OperationInfo(Xor, "Byte-wise XOR of two collections of bytes",
          Array(ArgInfo("left", "left operand"), ArgInfo("right", "right operand")))
      ),

      logicalOp("||", BinOr, "Logical OR of two operands",
        Array(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      logicalOp("&&", BinAnd, "Logical AND of two operands",
        Array(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      logicalOp("^", BinXor, "Logical XOR of two operands",
        Array(ArgInfo("left", "left operand"), ArgInfo("right", "right operand")))
    ).map(f => f.name -> f).toMap

    val unaryFuncs: Map[String, PredefinedFunc] = Seq(
      PredefinedFunc("unary_!",
        Lambda(Array("input" -> SBoolean), SBoolean, None),
        PredefFuncInfo(undefined),
        OperationInfo(LogicalNot,
          "Logical NOT operation. Returns \\lst{true} if input is \\lst{false} and \\lst{false} if input is \\lst{true}.",
          Seq(ArgInfo("input", "input \\lst{Boolean} value")))
      ),
      PredefinedFunc("unary_-",
        Lambda(Array(paramT), Array("input" -> tT), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(Negation,
          "Negates numeric value \\lst{x} by returning \\lst{-x}.",
          Seq(ArgInfo("input", "value of numeric type")))
      ),
      PredefinedFunc("unary_~",
        Lambda(Seq(paramT), Array("input" -> tT), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(BitInversion,
          "Invert every bit of the numeric value.",
          Seq(ArgInfo("input", "value of numeric type")))
      )
    ).map(f => f.name -> f).toMap

    val funcs: Map[String, PredefinedFunc] = globalFuncs ++ infixFuncs ++ unaryFuncs

    /** WARNING: This operations are not used in frontend, and should not be used.
      * They are used in SpecGen only the source of metadata for the corresponding ErgoTree nodes.
      */
    val specialFuncs: Map[String, PredefinedFunc] = Seq(
      PredefinedFunc("selectField",
        Lambda(Array(paramT, paramR), Array("input" -> tT, "fieldIndex" -> SByte), tR, None),
        PredefFuncInfo(undefined),
        OperationInfo(SelectField,
          "Select tuple field by its 1-based index. E.g. \\lst{input._1} is transformed to \\lst{SelectField(input, 1)}",
          Seq(ArgInfo("input", "tuple of items"), ArgInfo("fieldIndex", "index of an item to select")))
      ),
      PredefinedFunc("treeLookup",
        Lambda(Array("tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SOption(SByteArray), None),
        PredefFuncInfo(undefined),
        OperationInfo(TreeLookup,
          "",
          Seq(ArgInfo("tree", "tree to lookup the key"),
          ArgInfo("key", "a key of an item in the \\lst{tree} to lookup"),
          ArgInfo("proof", "proof to perform verification of the operation")))
      ),
      PredefinedFunc("if",
        Lambda(Array(paramT), Array("condition" -> SBoolean, "trueBranch" -> tT, "falseBranch" -> tT), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(If,
          "Compute condition, if true then compute trueBranch else compute falseBranch",
          Seq(ArgInfo("condition", "condition expression"),
          ArgInfo("trueBranch", "expression to execute when \\lst{condition == true}"),
          ArgInfo("falseBranch", "expression to execute when \\lst{condition == false}")))
      ),
      PredefinedFunc("upcast",
        Lambda(Array(paramT, paramR), Array("input" -> tT), tR, None),
        PredefFuncInfo(undefined),
        OperationInfo(Upcast,
          "Cast this numeric value to a bigger type (e.g. Int to Long)",
          Seq(ArgInfo("input", "value to cast")))
      ),
      PredefinedFunc("downcast",
        Lambda(Array(paramT, paramR), Array("input" -> tT), tR, None),
        PredefFuncInfo(undefined),
        OperationInfo(Downcast,
          "Cast this numeric value to a smaller type (e.g. Long to Int). Throws exception if overflow.",
          Seq(ArgInfo("input", "value to cast")))
      ),
      PredefinedFunc("apply",
        Lambda(Array(paramT, paramR), Array("func" -> SFunc(tT, tR), "args" -> tT), tR, None),
        PredefFuncInfo(undefined),
        OperationInfo(Apply,
          "Apply the function to the arguments. ",
          Seq(ArgInfo("func", "function which is applied"),
            ArgInfo("args", "list of arguments")))
      ),
      PredefinedFunc("placeholder",
        Lambda(Array(paramT), Array("id" -> SInt), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(ConstantPlaceholder,
          "Create special ErgoTree node which can be replaced by constant with given id.",
          Seq(ArgInfo("index", "index of the constant in ErgoTree header")))
      )
    ).map(f => f.name -> f).toMap

    private val funcNameToIrBuilderMap: Map[String, PredefinedFunc] =
      funcs.filter { case (_, f) => f.irInfo.irBuilder != undefined }

    def irBuilderForFunc(name: String): Option[IrBuilderFunc] = funcNameToIrBuilderMap.get(name).map(_.irInfo.irBuilder)
  }

  object PredefinedFuncApply {
    def unapply(apply: Apply)(implicit registry: PredefinedFuncRegistry): Nullable[SValue] = apply.func match {
      case Ident(name, _) =>
        registry.irBuilderForFunc(name)
          .filter(_.isDefinedAt(apply.func, apply.args))
          .map(b => Nullable(b(apply.func, apply.args))).getOrElse(Nullable.None)
      case _ => Nullable.None
    }
  }
}
