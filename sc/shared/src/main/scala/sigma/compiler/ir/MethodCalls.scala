package sigma.compiler.ir

import sigma.ast.{SMethod, SType, STypeVar, ValueCompanion}

/** Defines the graph-ir representation of method calls and the related utility methods. */
trait MethodCalls extends Base { self: IRContext =>

  /** What a [[MethodCall]] node stands for. The live descriptors come from the `data` module,
    * so the compiler never maps a name string to a method and JVM and JS resolve calls alike.
    */
  sealed trait IRCallee

  /** A method with an [[SMethod]] descriptor (`Coll.map`, `Box.getReg`, `Header.checkPow`, ...).
    * Identity is `(objType, methodId)`: specialised copies of one descriptor differ in `stype`
    * and must still denote the same call. Note that under v5 the numeric types share one
    * `objType` for their common methods, so such callees would not tell `Byte.toBytes` from
    * `Short.toBytes`; `GraphBuilding` rejects those calls under v5 and lowers the v6 copies (whose
    * `objType` is the numeric type) to unary and binary operation nodes, so no numeric method
    * becomes a call node, and a row for one would need the receiver type.
    */
  final case class MethodCallee(method: SMethod) extends IRCallee {
    override def equals(other: Any): Boolean = other match {
      case MethodCallee(m) => (m eq method) || (m.objType == method.objType && m.methodId == method.methodId)
      case _ => false
    }
    override def hashCode: Int = method.objType.hashCode * 31 + method.methodId
    override def toString: String = method.opName
  }

  /** An ErgoTree operation applied to the receiver: `p && q` on sigma propositions (`SigmaAnd`),
    * the per-code `ArithOp` companions on big integers, ...
    */
  final case class OpCallee(op: ValueCompanion) extends IRCallee

  /** A builtin of the global object (`blake2b256`, `allZK`, `Coll(...)`, `xor`, ...): an ErgoTree
    * operation whose call node has the global object as receiver, which its lowering ignores. It is
    * not an [[OpCallee]] because one operation can be both: `SigmaAnd` is `p && q` and `allZK`.
    */
  final case class GlobalOpCallee(op: ValueCompanion) extends IRCallee

  /** Graph node representing a call of `callee` on `receiver`.
    * @param receiver   node ref of the instance the method is called on
    * @param callee     what is called, see [[IRCallee]]
    * @param args       argument node refs
    * @param typeSubst  substitution for the callee's explicit type arguments
    * @param resultType type descriptor of the result
    */
  case class MethodCall private[MethodCalls](receiver: Sym, callee: IRCallee, args: Seq[Sym], typeSubst: Map[STypeVar, SType])
                                            (val resultType: Elem[Any]) extends Def[Any] {

    override def mirror(t: Transformer): Ref[Any] =
      mkMethodCall(t(receiver), callee, args.map(t(_)), typeSubst, resultType).asInstanceOf[Ref[Any]]

    override def toString = s"MethodCall($receiver, $callee, [${args.mkString(", ")}])"

    override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || {
      other match {
        case other: MethodCall =>
          receiver == other.receiver &&
          callee == other.callee &&
          resultType == other.resultType &&
          typeSubst == other.typeSubst &&
          args == other.args
        case _ => false
      }
    }

    override lazy val hashCode: Int = {
      var h = receiver.hashCode() * 31 + callee.hashCode()
      h = h * 31 + resultType.hashCode
      h = h * 31 + typeSubst.hashCode()
      h = h * 31 + args.hashCode()
      h
    }
  }

  /** Pattern for rewrite rules: matches a call node of `callee` (by callee identity) and yields
    * its receiver and argument refs.
    */
  final class CallPattern(callee: IRCallee) {
    def unapply(d: Def[_]): Option[(Sym, Seq[Sym])] = d match {
      case MethodCall(receiver, c, args, _) if c == callee => Some((receiver, args))
      case _ => None
    }
    def unapply(s: Sym): Option[(Sym, Seq[Sym])] = unapply(s.node)
  }
  object CallPattern {
    def apply(method: SMethod): CallPattern = new CallPattern(MethodCallee(method))
    def apply(op: ValueCompanion): CallPattern = new CallPattern(OpCallee(op))
    def apply(callee: IRCallee): CallPattern = new CallPattern(callee)
  }

  /** Creates new MethodCall node and returns its node ref. */
  def mkMethodCall(receiver: Sym,
                   callee: IRCallee,
                   args: Seq[Sym],
                   typeSubst: Map[STypeVar, SType],
                   resultElem: Elem[_]): Sym = {
    reifyObject(MethodCall(receiver, callee, args, typeSubst)(asElem[Any](resultElem)))
  }
}
