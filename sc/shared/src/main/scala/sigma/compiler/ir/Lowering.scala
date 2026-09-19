package sigma.compiler.ir

import org.ergoplatform.ErgoBox
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

  /** Rebuilds an ErgoTree node from the IR call node, its built receiver and built arguments. */
  type Row = (MethodCall, SValue, Seq[SValue]) => SValue

  /** All callees that have a dedicated ErgoTree node. Populated entity by entity. */
  protected lazy val rows: Map[IRCallee, Row] = Map(
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
