package sigma.compiler.ir

import debox.cfor
import sigma.ast.{SMethod, SType, STypeVar, ValueCompanion}
import sigma.compiler.DelayInvokeException
import sigma.reflection.RMethod
import sigma.util.CollectionUtil.TraversableOps

/** Defines graph-ir representation of method calls, new object creation as well as the
  * related utility methods.
  */
trait MethodCalls extends Base { self: IRContext =>

  def delayInvoke = throw new DelayInvokeException

  /** What a [[MethodCall]] node stands for. The live descriptors come from the `data` module,
    * so the compiler never maps a name string to a method and JVM and JS resolve calls alike.
    */
  sealed trait IRCallee

  /** A method with an [[SMethod]] descriptor (`Coll.map`, `Box.getReg`, `Header.checkPow`, ...).
    * Identity is `(objType, methodId)`: specialised copies of one descriptor differ in `stype`
    * and must still denote the same call.
    */
  final case class MethodCallee(method: SMethod) extends IRCallee {
    override def equals(other: Any): Boolean = other match {
      case MethodCallee(m) => (m eq method) || (m.objType == method.objType && m.methodId == method.methodId)
      case _ => false
    }
    override def hashCode: Int = method.objType.hashCode * 31 + method.methodId
    override def toString: String = method.opName
  }

  /** An ErgoTree operation that has no [[SMethod]] (`CalcBlake2b256`, `SigmaAnd`, `ArithOp` with
    * its op code, ...).
    */
  final case class OpCallee(op: ValueCompanion, opCode: Option[Byte] = None) extends IRCallee

  /** Bridge for the generated staged wrappers, which still build calls from reflective handles.
    * Goes away together with the wrappers.
    */
  final case class LegacyCallee(method: RMethod) extends IRCallee {
    override def toString: String =
      method.toString.replace("java.lang.", "").replace("public ", "").replace("abstract ", "")
  }

  /** Graph node representing a call of `callee` on `receiver`.
    * @param receiver   node ref of the instance the method is called on
    * @param callee     what is called, see [[IRCallee]]
    * @param args       arguments: node refs, plus type descriptors for legacy callees
    * @param typeSubst  substitution for the callee's explicit type arguments
    * @param resultType type descriptor of the result
    */
  case class MethodCall private[MethodCalls](receiver: Sym, callee: IRCallee, args: Seq[AnyRef], typeSubst: Map[STypeVar, SType])
                                            (val resultType: Elem[Any]) extends Def[Any] {

    override def mirror(t: Transformer): Ref[Any] = {
      val len = args.length
      val args1 = new Array[AnyRef](len)
      cfor(0)(_ < len, _ + 1) { i =>
        args1(i) = transformProductParam(args(i), t).asInstanceOf[AnyRef]
      }
      mkMethodCall(t(receiver), callee, args1, typeSubst, resultType).asInstanceOf[Ref[Any]]
    }

    override def toString = s"MethodCall($receiver, $callee, [${args.mkString(", ")}])"

    override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || {
      other match {
        case other: MethodCall =>
          receiver == other.receiver &&
          callee == other.callee &&
          resultType.name == other.resultType.name &&
          typeSubst == other.typeSubst &&
          args.length == other.args.length &&
          args.sameElementsNested(other.args) // this is required in case method have T* arguments
        case _ => false
      }
    }

    override lazy val hashCode: Int = {
      var h = receiver.hashCode() * 31 + callee.hashCode()
      h = h * 31 + resultType.name.hashCode
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
      case MethodCall(receiver, c, args, _) if c == callee => Some((receiver, args.collect { case s: Sym => s }))
      case _ => None
    }
    def unapply(s: Sym): Option[(Sym, Seq[Sym])] = unapply(s.node)
  }
  object CallPattern {
    def apply(method: SMethod): CallPattern = new CallPattern(MethodCallee(method))
    def apply(op: ValueCompanion, opCode: Option[Byte] = None): CallPattern = new CallPattern(OpCallee(op, opCode))
  }

  /** Represents invocation of constructor of the class described by `eA`.
    * @param  eA          class descriptor for new instance
    * @param  args        arguments of class constructor
    */
  case class NewObject[A](eA: Elem[A], args: Seq[Any]) extends BaseDef[A]()(eA) {
    override def transform(t: Transformer) = NewObject(eA, t(args))
  }

  /** Creates new MethodCall node and returns its node ref. */
  def mkMethodCall(receiver: Sym,
                   callee: IRCallee,
                   args: Seq[AnyRef],
                   typeSubst: Map[STypeVar, SType],
                   resultElem: Elem[_]): Sym = {
    reifyObject(MethodCall(receiver, callee, args, typeSubst)(asElem[Any](resultElem)))
  }

  /** Signature used by the generated staged wrappers. `neverInvoke` and `isAdapterCall` are
    * ignored: call nodes are never invoked reflectively any more. Goes away with the wrappers.
    */
  def mkMethodCall(receiver: Sym,
                   method: RMethod,
                   args: Seq[AnyRef],
                   neverInvoke: Boolean,
                   isAdapterCall: Boolean,
                   resultElem: Elem[_],
                   typeSubst: Map[STypeVar, SType] = Map.empty): Sym = {
    mkMethodCall(receiver, LegacyCallee(method), args, typeSubst, resultElem)
  }
}
