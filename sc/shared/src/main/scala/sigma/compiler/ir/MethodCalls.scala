package sigma.compiler.ir

import debox.{cfor, Buffer => DBuffer}
import sigma.compiler.DelayInvokeException
import sigma.reflection.RMethod
import sigma.util.CollectionUtil.TraversableOps

import scala.annotation.{tailrec, unused}

/** Defines graph-ir representation of method calls, new object creation as well as the
  * related utility methods.
  */
trait MethodCalls extends Base { self: IRContext =>

  def delayInvoke = throw new DelayInvokeException

  /** Graph node to represent invocation of the method of some class.
    * @param receiver    node ref representing instance on which the method is called
    * @param method      method which is called (descriptor from `sigma.reflection`)
    * @param args        node refs representing arguments passed to the method
    * @param neverInvoke it true this method cannot be performed, even if the
    *                    receiver node allow this
    * @param resultType  type descriptor of the method's result
    * @param isAdapterCall whether this MC was created by generated adapter class.
    *                      This typically means, that receiver node doesn't implement
    *                      given `method`.
    */
  case class MethodCall private[MethodCalls](receiver: Sym, method: RMethod, args: Seq[AnyRef], neverInvoke: Boolean)
                                            (val resultType: Elem[Any], val isAdapterCall: Boolean = false) extends Def[Any] {

    override def mirror(t: Transformer): Ref[Any] = {
      val len = args.length
      val args1 = new Array[AnyRef](len)
      cfor(0)(_ < len, _ + 1) { i =>
        args1(i) = transformProductParam(args(i), t).asInstanceOf[AnyRef]
      }
      val receiver1 = t(receiver)
      // in the case neverInvoke is false, the method is invoked in rewriteDef
      mkMethodCall(receiver1, method, args1, neverInvoke, isAdapterCall, resultType).asInstanceOf[Ref[Any]]
    }

    override def toString = {
      val methodStr = method.toString.replace("java.lang.", "").
        replace("public ", "").replace("abstract ", "")
      s"MethodCall($receiver, $methodStr, [${args.mkString(", ")}], $neverInvoke)"
    }

    /** Try invoke `method` on the node instance refered by `receiver`.
      * Each MC node contains enough information to perform invocation using
      * `sigma.reflection.RMethod.invoke` method. However, this is not possible
      * if the node pointed to by `receiver` don't implement this method,
      * for example when receiver is Lambda variable pointing to Variable node
      * instance (in which case this MC was created by adapter)
      * @return invocation result descriptor.
      * @see InvokeResult
      */
    def tryInvoke: InvokeResult =
      if (neverInvoke && !isAdapterCall) {
        InvokeImpossible
      } else {
        invokeMethod[InvokeResult](
          receiver, method, args.toArray,
          { res => InvokeSuccess(res.asInstanceOf[Sym]) },
          { InvokeFailure(_) },
          { InvokeImpossible }
        )
      }

    override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || {
      other match {
        case other: MethodCall =>
          receiver == other.receiver &&
          method == other.method &&
          resultType.name == other.resultType.name &&
          neverInvoke == other.neverInvoke &&
          isAdapterCall == other.isAdapterCall &&
          args.length == other.args.length &&
          args.sameElementsNested(other.args) // this is required in case method have T* arguments
        case _ => false
      }
    }

    override lazy val hashCode: Int = {
      var h = receiver.hashCode() * 31 + method.hashCode()
      h = h * 31 + resultType.name.hashCode
      h = h * 31 + (if(neverInvoke) 1 else 0)
      h = h * 31 + (if(isAdapterCall) 1 else 0)
      h = h * 31 + args.hashCode()
      h
    }
  }

  /** Represents invocation of constructor of the class described by `eA`.
    * @param  eA          class descriptor for new instance
    * @param  args        arguments of class constructor
    */
  case class NewObject[A](eA: Elem[A], args: Seq[Any]) extends BaseDef[A]()(eA) {
    override def transform(t: Transformer) = NewObject(eA, t(args))
  }

  /** Creates new MethodCall node and returns its node ref. */
  def mkMethodCall(receiver: Sym, method: RMethod, args: Seq[AnyRef],
                   neverInvoke: Boolean, isAdapterCall: Boolean, resultElem: Elem[_]): Sym = {
    reifyObject(MethodCall(receiver, method, args, neverInvoke)(asElem[Any](resultElem), isAdapterCall))
  }

  @tailrec
  private def baseCause(e: Throwable): Throwable = e match {
    case e: ExceptionInInitializerError => baseCause(e.getCause)
    case e => e
  }

  /** This method is called for each MethodCall node which is about to be added to the graph.
    * This means `mc` has been examined by all the rewrite rules, but has not need rewritten.
    * Now, if this method returns null, then mc will be added to the graph.
    * However, in this method, `mc` can be examined by a second set of RW rules
    * (kind of lower priority rules). These rules kind of context dependent, because at this
    * point we know that the first RW set didn't triggered any rewrite. */
  def rewriteNonInvokableMethodCall(@unused mc: MethodCall): Ref[_] = null

  /** Generic helper to call the given method on the given receiver node. */
  private[compiler] def invokeMethod[A](receiver: Sym, m: RMethod, args: Array[AnyRef],
                              onInvokeSuccess: Any => A,
                              onInvokeException: Throwable => A,
                              onInvokeImpossible: => A): A = {
    val d = receiver.node
    if (canBeInvoked(d, m, args)) {
      try {
        val res = m.invoke(d, args:_*)
        onInvokeSuccess(res)
      } catch {
        case e: Exception => onInvokeException(baseCause(e))
      }
    } else {
      onInvokeImpossible
    }
  }

  /** Method invocation enabler.
    * @return  true if the given method can be invoked on the given node. */
  def isInvokeEnabled(d: Def[_], m: RMethod) = true

  /** Method invocation checker. */
  protected def canBeInvoked(d: Def[_], m: RMethod, args: Array[AnyRef]) = {
    m.getDeclaringClass.isAssignableFrom(d.getClass) && isInvokeEnabled(d, m)
  }

  /** Result of MethodCall invocation.
    * @see tryInvoke */
  sealed abstract class InvokeResult
  /** Successful MethodCall invocation with the given result. */
  case class InvokeSuccess(result: Ref[_]) extends InvokeResult
  /** Exception thrown during MethodCall invocation. */
  case class InvokeFailure(exception: Throwable) extends InvokeResult
  /** Invocation is not possible, e.g. when receiver doesn't implemented the method. */
  case object InvokeImpossible extends InvokeResult

  def throwInvocationException(whatFailed: String, cause: Throwable, receiver: Sym, m: RMethod, args: Seq[Any]) = {
    val buf = DBuffer.empty[Sym]
    buf += receiver
    Def.extractSyms(args, buf)
    val deps = buf.toArray()
    !!!(s"$whatFailed (${receiver.varNameWithType}).${m.getName}(${args.mkString(", ")}) failed", baseCause(cause), deps: _*)
  }
}
