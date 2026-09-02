package sigma.ast

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8, R9, RegisterId}
import scorex.util.encode.{Base16, Base58, Base64}
import sigma.data._
import sigma.{Colls}
import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.ast.SOption.SIntOption
import sigma.ast.syntax._
import sigma.data.Nullable
import sigma.data.RType.asType
import sigma.Evaluation.stypeToRType
import scala.reflect.ClassTag
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

    val ExecuteFromVarFunc = PredefinedFunc("executeFromVar",
      Lambda(Array(paramT), Array("id" -> SByte), tT, None),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, rtpe, _)), Seq(id: Constant[SNumericType]@unchecked)) =>
          mkDeserializeContext(SByte.downcast(id.value.asInstanceOf[AnyVal]), rtpe)
        }),
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

    val ExecuteFromSelfRegWithDefaultFunc = PredefinedFunc("executeFromSelfRegWithDefault",
      Lambda(
        Seq(paramT),
        Array("id" -> SInt, "default" -> tT),
        tT, None
      ),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, rtpe, _)), Seq(id: Constant[SNumericType]@unchecked, default)) =>
          val idx: Int = SInt.downcast((id.value.asInstanceOf[AnyVal]))
          if (idx < 0 || idx >= org.ergoplatform.ErgoBox.allRegisters.length) {
            default
          } else {
            val r: RegisterId = org.ergoplatform.ErgoBox.registerByIndex(idx)
            val d = Some(default.asValue[rtpe.type])
            mkDeserializeRegister[rtpe.type](r, rtpe, d)
          }
        }),
      OperationInfo(DeserializeRegister,
        """Extracts SELF register as \lst{Coll[Byte]}, deserializes it to script
         | and then executes this script in the current context.
         | The original \lst{Coll[Byte]} of the script is available as \lst{SELF.getReg[Coll[Byte]](id)}.
         | Type parameter \lst{T} result type of the deserialized script.
         | Throws an exception if the actual script type doesn't conform to \lst{T}.
         | Returns a result of the script execution in the current context or the default value
         | provided when the specified register is unavailable
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

    val DeserializeToFunc = PredefinedFunc("deserializeTo",
      Lambda(Seq(paramT), Array("bytes" -> SByteArray), tT, None),
      irInfo = PredefFuncInfo(
        irBuilder = { case (u, args) =>
          val resType = u.opType.tRange.asInstanceOf[SFunc].tRange
          MethodCall(
            Global,
            SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> resType)),
            args.toIndexedSeq,
            Map(tT -> resType)
          )
        }),
      docInfo = OperationInfo(MethodCall,
        """Deserializes provided bytes into a value of given type using the default serialization format.
        """.stripMargin,
        Seq(ArgInfo("bytes", "bytes to deserialize"))
      )
    )

    val FromBigEndianBytesFunc = PredefinedFunc("fromBigEndianBytes",
      Lambda(Seq(paramT), Array("bytes" -> SByteArray), tT, None),
      irInfo = PredefFuncInfo(
        irBuilder = { case (u, args) =>
          val resType = u.opType.tRange.asInstanceOf[SFunc].tRange
          MethodCall(
            Global,
            SGlobalMethods.FromBigEndianBytesMethod.withConcreteTypes(Map(tT -> resType)),
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

    val ExecuteFromSelfRegFunc = PredefinedFunc("executeFromSelfReg",
      Lambda(
        Seq(paramT),
        Array("id" -> SInt),
        tT, None
      ),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, rtpe, _)), Seq(id: Constant[SNumericType]@unchecked)) =>
          val idx: Int = SInt.downcast((id.value.asInstanceOf[AnyVal]))
          if (idx < 0 || idx >= org.ergoplatform.ErgoBox.allRegisters.length) {
            throw new InvalidArguments(s"Invalid register specified $idx")
          }

          val r: RegisterId = org.ergoplatform.ErgoBox.registerByIndex(idx)
          mkDeserializeRegister[rtpe.type](r, rtpe, None)
        }),
      OperationInfo(DeserializeRegister,
        """Extracts SELF register as \lst{Coll[Byte]}, deserializes it to script
          | and then executes this script in the current context.
          | The original \lst{Coll[Byte]} of the script is available as \lst{SELF.getReg[Coll[Byte]](id)}.
          | Type parameter \lst{T} result type of the deserialized script.
          | Throws an exception if the actual script type doesn't conform to \lst{T}.
          | Returns a result of the script execution in the current context
        """.stripMargin,
        Seq(ArgInfo("id", "identifier of the register")))
    )

    /** Element type of the `tokens` collection of a box: (tokenId, amount). */
    private val STokenElemType: SType = org.ergoplatform.ErgoBox.STokenType

    /** Builds `box.tokens` as a typed collection of (Coll[Byte], Long) tuples. */
    private def boxTokens(box: Value[SBox.type]): Value[SCollection[STuple]] =
      mkMethodCall(box, SBoxMethods.tokensMethod, IndexedSeq.empty, Map.empty).asCollection[STuple]

    /** Checks that two boxes have equal monetary value and equal guarding script. */
    val VerifySameForBasicRequiredRegistersFunc = PredefinedFunc("verifySameForBasicRequiredRegisters",
      Lambda(Array("inBox" -> SBox, "outBox" -> SBox), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(inBox: Value[SBox.type]@unchecked, outBox: Value[SBox.type]@unchecked))
            if inBox.tpe == SBox && outBox.tpe == SBox =>
          mkBinAnd(
            mkEQ(mkExtractAmount(inBox), mkExtractAmount(outBox)),
            mkEQ(mkExtractScriptBytes(inBox), mkExtractScriptBytes(outBox))
          )
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if \lst{inBox} and \lst{outBox} have the same monetary value
          | (\lst{value}) and the same guarding script (\lst{propositionBytes}).
        """.stripMargin,
        Seq(ArgInfo("inBox", "the first box"), ArgInfo("outBox", "the second box")))
    )

    /** Same as [[VerifySameForBasicRequiredRegistersFunc]] but additionally requires equal tokens. */
    val VerifySameForRequiredRegistersFunc = PredefinedFunc("verifySameForRequiredRegisters",
      Lambda(Array("inBox" -> SBox, "outBox" -> SBox), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(inBox: Value[SBox.type]@unchecked, outBox: Value[SBox.type]@unchecked))
            if inBox.tpe == SBox && outBox.tpe == SBox =>
          mkBinAnd(
            mkBinAnd(
              mkEQ(mkExtractAmount(inBox), mkExtractAmount(outBox)),
              mkEQ(mkExtractScriptBytes(inBox), mkExtractScriptBytes(outBox))
            ),
            mkEQ(boxTokens(inBox), boxTokens(outBox))
          )
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if \lst{inBox} and \lst{outBox} have the same monetary value
          | (\lst{value}), the same guarding script (\lst{propositionBytes}) and the same
          | secondary \lst{tokens}.
        """.stripMargin,
        Seq(ArgInfo("inBox", "the first box"), ArgInfo("outBox", "the second box")))
    )

    /** Checks that a box does not use any non-mandatory register beyond the first \lst{used} ones. */
    val VerifyUsedAdditionalRegistersFunc = PredefinedFunc("verifyUsedAdditionalRegisters",
      Lambda(Array("box" -> SBox, "used" -> SInt), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(box: Value[SBox.type]@unchecked, used: Value[SInt.type]@unchecked))
            if box.tpe == SBox && used.tpe == SInt =>
          def regIsEmpty(reg: RegisterId): BoolValue =
            mkLogicalNot(mkOptionIsDefined(
              mkExtractRegisterAs(box, reg, SOption(SAny)).asValue[SOption[SAny.type]]))
          // (used >= n || box.R{n+3}[Any].isEmpty) for n = 1..6 (registers R4..R9)
          val clauses: Seq[BoolValue] = Seq[(Int, RegisterId)](
            1 -> R4, 2 -> R5, 3 -> R6, 4 -> R7, 5 -> R8, 6 -> R9
          ).map { case (n, reg) => mkBinOr(mkGE(used, IntConstant(n)), regIsEmpty(reg)) }
          clauses.reduce[BoolValue](mkBinAnd)
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if \lst{box} uses only the first \lst{used} non-mandatory registers,
          | i.e. all registers with index greater than \lst{used} (counting R4 as 1, ..., R9 as 6)
          | are empty.
          | NOTE: when a register beyond the first \lst{used} ones is defined, checking that the
          | register is empty (\lst{isEmpty} of the register read as \lst{Option[Any]}) fails with
          | an exception instead of returning \lst{false}, so the script evaluation fails, which is
          | the desired behavior when the function is used as a guarding predicate of a box.
        """.stripMargin,
        Seq(ArgInfo("box", "the box to check"),
            ArgInfo("used", "number of additional registers (R4..R9) allowed to be used")))
    )

    /** Checks that a box contains at least one unit of the given token. */
    val VerifyBoxHasMarkerTokenFunc = PredefinedFunc("verifyBoxHasMarkerToken",
      Lambda(Array("box" -> SBox, "token" -> SByteArray), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(box: Value[SBox.type]@unchecked, token: Value[SByteArray]@unchecked))
            if box.tpe == SBox && token.tpe == SByteArray =>
          val t = Ident("$t", STokenElemType).asValue[STuple]
          // t._1 == token && t._2 >= 1L
          val body = mkBinAnd(
            mkEQ(mkSelectField(t, 1.toByte).asValue[SByteArray], token),
            mkGE(mkSelectField(t, 2.toByte).asValue[SLong.type], LongConstant(1L))
          )
          val lam = mkLambda(Array[(String, SType)]("$t" -> STokenElemType), SBoolean, Some(body))
          mkExists(boxTokens(box), lam)
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if \lst{box} holds at least one unit of the token with id \lst{token}.""",
        Seq(ArgInfo("box", "the box to check"), ArgInfo("token", "the token id (\\lst{Coll[Byte]})")))
    )

    /** Checks that a box does not contain the given token. */
    val VerifyBoxHasNoMarkerTokenFunc = PredefinedFunc("verifyBoxHasNoMarkerToken",
      Lambda(Array("box" -> SBox, "token" -> SByteArray), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(box: Value[SBox.type]@unchecked, token: Value[SByteArray]@unchecked))
            if box.tpe == SBox && token.tpe == SByteArray =>
          val t = Ident("$t", STokenElemType).asValue[STuple]
          // t._1 != token
          val body = mkNEQ(mkSelectField(t, 1.toByte).asValue[SByteArray], token)
          val lam = mkLambda(Array[(String, SType)]("$t" -> STokenElemType), SBoolean, Some(body))
          mkForAll(boxTokens(box), lam)
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if \lst{box} does not hold the token with id \lst{token}.""",
        Seq(ArgInfo("box", "the box to check"), ArgInfo("token", "the token id (\\lst{Coll[Byte]})")))
    )

    /** Checks that the given token is spent from \lst{inBox} to \lst{outBox} by exactly \lst{amount}. */
    val VerifySpentTokenFunc = PredefinedFunc("verifySpentToken",
      Lambda(Array("inBox" -> SBox, "outBox" -> SBox, "token" -> SByteArray, "amount" -> SLong), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(inBox: Value[SBox.type]@unchecked, outBox: Value[SBox.type]@unchecked,
                       token: Value[SByteArray]@unchecked, amount: Value[SLong.type]@unchecked))
            if inBox.tpe == SBox && outBox.tpe == SBox && token.tpe == SByteArray && amount.tpe == SLong =>
          val it = Ident("$it", STokenElemType).asValue[STuple]
          val itId = mkSelectField(it, 1.toByte).asValue[SByteArray]
          val itAmount = mkSelectField(it, 2.toByte).asValue[SLong.type]
          // builds: outBox.tokens.exists(ot => it._1 == ot._1 && it._2 == ot._2 [+ amount])
          def existsInOut(addAmount: Boolean): Value[SBoolean.type] = {
            val ot = Ident("$ot", STokenElemType).asValue[STuple]
            val otId = mkSelectField(ot, 1.toByte).asValue[SByteArray]
            val otAmount = mkSelectField(ot, 2.toByte).asValue[SLong.type]
            val rhs = if (addAmount) mkPlus(otAmount, amount) else otAmount
            val body = mkBinAnd(mkEQ(itId, otId), mkEQ(itAmount, rhs))
            mkExists(boxTokens(outBox), mkLambda(Array[(String, SType)]("$ot" -> STokenElemType), SBoolean, Some(body)))
          }
          // if (it._1 == token) <spent> else <unchanged>
          val ifBody = mkIf(mkEQ(itId, token), existsInOut(addAmount = true), existsInOut(addAmount = false))
          val outerLam = mkLambda(Array[(String, SType)]("$it" -> STokenElemType), SBoolean, Some(ifBody))
          mkForAll(boxTokens(inBox), outerLam)
        }),
      OperationInfo(MethodCall,
        """Returns \lst{true} if every token of \lst{inBox} is preserved in \lst{outBox}, except for
          | the token with id \lst{token} whose amount in \lst{outBox} must be exactly \lst{amount} less
          | than in \lst{inBox}.
          | NOTE: the check is a \lst{forall} over the tokens of \lst{inBox}, hence it is vacuously
          | \lst{true} when \lst{inBox} does not hold \lst{token} at all (nothing is spent).
          | NOTE: spending the token in full (down to zero, i.e. \lst{token} absent in \lst{outBox})
          | returns \lst{false}: the function requires \lst{outBox} to still hold the token.
          | NOTE: the amount comparison uses checked arithmetic (\lst{outAmount + amount}), so if the
          | sum overflows \lst{Long} the evaluation fails with an exception instead of returning
          | \lst{false}.
        """.stripMargin,
        Seq(ArgInfo("inBox", "the input box"), ArgInfo("outBox", "the output box"),
            ArgInfo("token", "the token id being spent"), ArgInfo("amount", "the spent amount")))
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
      ExecuteFromSelfRegWithDefaultFunc,
      SerializeFunc,
      DeserializeToFunc,
      GetVarFromInputFunc,
      FromBigEndianBytesFunc,
      VerifySameForBasicRequiredRegistersFunc,
      VerifySameForRequiredRegistersFunc,
      VerifyUsedAdditionalRegistersFunc,
      VerifyBoxHasMarkerTokenFunc,
      VerifyBoxHasNoMarkerTokenFunc,
      VerifySpentTokenFunc
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
