package sigma.compiler.ir

import org.ergoplatform.ErgoBox
import sigma.data.{ProveDHTuple, ProveDlog}
import sigma.ast._
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.serialization.OpCodes._

/** The reverse lowering table of the compiler: for a call node whose callee has a dedicated
  * ErgoTree node, the row rebuilds that node from the already built receiver and arguments.
  * Rows are keyed by callee identity ([[MethodCallee]] compares `(objType, methodId)`,
  * [[OpCallee]] is structural), never by name. Callees without a row are emitted as plain
  * `MethodCall` ErgoTree nodes by [[TreeBuilding]].
  */
trait Lowering { IR: IRContext =>
  import SigmaDslBuilder._

  /** Rebuilds an ErgoTree node from the IR call node, its built receiver and built arguments. */
  type Row = (MethodCall, SValue, Seq[SValue]) => SValue

  /** All callees that have a dedicated ErgoTree node. Populated entity by entity. */
  protected lazy val rows: Map[IRCallee, Row] = Map(
    // Global builtins: the callee is the operation's companion, the receiver the global object
    OpCallee(BoolToSigmaProp)    -> ((_, _, args) => builder.mkBoolToSigmaProp(args(0).asBoolValue)),
    OpCallee(AND)                -> ((_, _, args) => builder.mkAND(args(0).asCollection[SBoolean.type])),
    OpCallee(OR)                 -> ((_, _, args) => builder.mkOR(args(0).asCollection[SBoolean.type])),
    OpCallee(XorOf)              -> ((_, _, args) => builder.mkXorOf(args(0).asCollection[SBoolean.type])),
    OpCallee(AtLeast)            -> ((_, _, args) => builder.mkAtLeast(args(0).asIntValue, args(1).asCollection[SSigmaProp.type])),
    OpCallee(CalcBlake2b256)     -> ((_, _, args) => builder.mkCalcBlake2b256(args(0).asByteArray)),
    OpCallee(CalcSha256)         -> ((_, _, args) => builder.mkCalcSha256(args(0).asByteArray)),
    OpCallee(ByteArrayToBigInt)  -> ((_, _, args) => builder.mkByteArrayToBigInt(args(0).asByteArray)),
    OpCallee(LongToByteArray)    -> ((_, _, args) => builder.mkLongToByteArray(args(0).asValue[SLong.type])),
    OpCallee(ByteArrayToLong)    -> ((_, _, args) => builder.mkByteArrayToLong(args(0).asByteArray)),
    OpCallee(DecodePoint)        -> ((_, _, args) => builder.mkDecodePoint(args(0).asByteArray)),
    OpCallee(SubstConstants)     -> ((_, _, args) => builder.mkSubstConst(args(0).asByteArray, args(1).asIntArray, args(2).asCollection[SType])),
    OpCallee(CreateProveDlog)    -> { (_, _, args) => args(0) match {
      case gc: Constant[SGroupElement.type]@unchecked => SigmaPropConstant(ProveDlog(gc.value))
      case g => builder.mkCreateProveDlog(g.asGroupElement)
    }},
    OpCallee(CreateProveDHTuple) -> { (_, _, args) => (args(0), args(1), args(2), args(3)) match {
      case (gc: Constant[SGroupElement.type]@unchecked, hc: Constant[SGroupElement.type]@unchecked,
            uc: Constant[SGroupElement.type]@unchecked, vc: Constant[SGroupElement.type]@unchecked) =>
        SigmaPropConstant(ProveDHTuple(gc.value, hc.value, uc.value, vc.value))
      case (g, h, u, v) => builder.mkCreateProveDHTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement)
    }},
    MethodCallee(SGlobalMethods.xorMethod) -> ((_, _, args) => builder.mkXor(args(0).asByteArray, args(1).asByteArray)),
    // CollBuilder: the callee is the operation's companion
    OpCallee(ConcreteCollection) -> { (mc, _, args) =>
      val elemTpe = elemToSType(mc.resultType).asCollection[SType].elemType
      builder.mkConcreteCollection[elemTpe.type](args.map(_.asValue[elemTpe.type]).toArray[Value[elemTpe.type]], elemTpe)
    },
    OpCallee(Xor) -> ((_, _, args) => builder.mkXor(args(0).asByteArray, args(1).asByteArray)),
    // Coll
    MethodCallee(SCollectionMethods.ApplyMethod)     -> ((_, col, args) => builder.mkByIndex(col.asCollection[SType], args(0).asIntValue, None)),
    MethodCallee(SCollectionMethods.SizeMethod)      -> ((_, col, _)    => SizeOf(col.asCollection[SType])),
    MethodCallee(SCollectionMethods.ExistsMethod)    -> ((_, col, args) => builder.mkExists(col.asCollection[SType], args(0).asFunc)),
    MethodCallee(SCollectionMethods.ForallMethod)    -> ((_, col, args) => builder.mkForAll(col.asCollection[SType], args(0).asFunc)),
    MethodCallee(SCollectionMethods.MapMethod)       -> ((_, col, args) => builder.mkMapCollection(col.asCollection[SType], args(0).asFunc)),
    MethodCallee(SCollectionMethods.GetOrElseMethod) -> ((_, col, args) => builder.mkByIndex(col.asCollection[SType], args(0).asIntValue, Some(args(1)))),
    MethodCallee(SCollectionMethods.AppendMethod)    -> ((_, col, args) => builder.mkAppend(col.asCollection[SType], args(0).asCollection[SType])),
    MethodCallee(SCollectionMethods.SliceMethod)     -> ((_, col, args) => builder.mkSlice(col.asCollection[SType], args(0).asIntValue, args(1).asIntValue)),
    MethodCallee(SCollectionMethods.FoldMethod)      -> ((_, col, args) => builder.mkFold(col.asCollection[SType], args(0), args(1).asFunc)),
    MethodCallee(SCollectionMethods.FilterMethod)    -> ((_, col, args) => builder.mkFilter(col.asCollection[SType], args(0).asFunc)),
    // SigmaProp
    OpCallee(SigmaAnd) -> { (mc, p1, args) =>
      if (mc.receiver.elem.isInstanceOf[SigmaDslBuilderElem]) error(s"Cannot find method 'allZK' on receiver of type ${p1.tpe}")
      else SigmaAnd(Seq(p1.asSigmaProp, args(0).asSigmaProp))
    },
    OpCallee(SigmaOr)  -> { (mc, p1, args) =>
      if (mc.receiver.elem.isInstanceOf[SigmaDslBuilderElem]) error(s"Cannot find method 'anyZK' on receiver of type ${p1.tpe}")
      else SigmaOr(Seq(p1.asSigmaProp, args(0).asSigmaProp))
    },
    MethodCallee(SSigmaPropMethods.PropBytesMethod) -> ((_, p, _) => builder.mkSigmaPropBytes(p.asSigmaProp)),
    // isValid never reaches the tree (rewrite rules and removeIsProven eliminate it); keep the old failure
    MethodCallee(SSigmaPropMethods.IsProvenMethod)  -> ((_, p, _) => error(s"Cannot find method 'isValid' on receiver of type ${p.tpe}")),
    // BigInt arithmetic: the callee is the operation's companion
    OpCallee(ArithOp.operations(PlusCode))     -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, PlusCode)),
    OpCallee(ArithOp.operations(MinusCode))    -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, MinusCode)),
    OpCallee(ArithOp.operations(MultiplyCode)) -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, MultiplyCode)),
    OpCallee(ArithOp.operations(DivisionCode)) -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, DivisionCode)),
    OpCallee(ArithOp.operations(ModuloCode))   -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, ModuloCode)),
    OpCallee(ArithOp.operations(MinCode))      -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, MinCode)),
    OpCallee(ArithOp.operations(MaxCode))      -> ((_, x, args) => builder.mkArith(x.asNumValue, args(0).asNumValue, MaxCode)),
    // GroupElement
    MethodCallee(SGroupElementMethods.ExponentiateMethod) -> ((_, g, args) => builder.mkExponentiate(g.asGroupElement, args(0).asBigInt)),
    MethodCallee(SGroupElementMethods.MultiplyMethod)     -> ((_, g, args) => builder.mkMultiplyGroup(g.asGroupElement, args(0).asGroupElement)),
    // Option
    MethodCallee(SOptionMethods.GetMethod)       -> ((_, opt, _) => builder.mkOptionGet(opt.asValue[SOption[SType]])),
    MethodCallee(SOptionMethods.IsDefinedMethod) -> ((_, opt, _) => builder.mkOptionIsDefined(opt.asValue[SOption[SType]])),
    MethodCallee(SOptionMethods.GetOrElseMethod) -> ((_, opt, args) => builder.mkOptionGetOrElse(opt.asValue[SOption[SType]], args(0))),
    // Context
    MethodCallee(SContextMethods.getVarV5Method) -> { (mc, ctx, args) =>
      val id = mc.args(0).asInstanceOf[Ref[Byte]]
      if (id.isConst) builder.mkGetVar(valueFromRep(id), elemToSType(mc.resultType).asOption.elemType)
      else plainMethodCall(mc, ctx, args)
    },
    // Box
    MethodCallee(SBoxMethods.ValueMethod)            -> ((_, box, _) => builder.mkExtractAmount(box.asBox)),
    MethodCallee(SBoxMethods.PropositionBytesMethod) -> ((_, box, _) => builder.mkExtractScriptBytes(box.asBox)),
    MethodCallee(SBoxMethods.BytesMethod)            -> ((_, box, _) => builder.mkExtractBytes(box.asBox)),
    MethodCallee(SBoxMethods.BytesWithoutRefMethod)  -> ((_, box, _) => builder.mkExtractBytesWithNoRef(box.asBox)),
    MethodCallee(SBoxMethods.IdMethod)               -> ((_, box, _) => builder.mkExtractId(box.asBox)),
    MethodCallee(SBoxMethods.creationInfoMethod)     -> ((_, box, _) => builder.mkExtractCreationInfo(box.asBox)),
    MethodCallee(SBoxMethods.getRegMethodV6)         -> { (mc, box, args) =>
      val regId = mc.args(0).asInstanceOf[Ref[Int]]
      if (regId.isConst)
        builder.mkExtractRegisterAs(box.asBox, ErgoBox.allRegisters(valueFromRep(regId)), elemToSType(mc.resultType).asOption)
      else
        plainMethodCall(mc, box, args)
    }
  )

  /** The row for `callee`, if it has a dedicated ErgoTree node. */
  final def rowFor(callee: IRCallee): Option[Row] = rows.get(callee)
}
