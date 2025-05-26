package sigma.compiler.ir

import sigma.compiler.DelayInvokeException
import sigma.data.ExactNumeric

/** Defines methods to implement graph rewriting optimization rules.
  * The rules are defines as a series of pattern matching with stackable overriding.
  * Can be mixed in into traits to add new rules (pattern cases) and call `super`
  * implementation as a fallback case.
  */
trait DefRewriting { scalan: IRContext =>

  /** Rewrites given node to another equivalent node and returns its reference.
    * @param  d  node to be matched against rewrite patterns
    * @return reference of new node if RW pattern is found and applied
    *         null if no rewriting is defined. */
  def rewriteDef[T](d: Def[T]): Ref[_] = d match {
    case First(p) if p.node.isInstanceOf[Tup[_,_]] => p.node.asInstanceOf[Tup[_,_]].a
    case Second(p) if p.node.isInstanceOf[Tup[_,_]] => p.node.asInstanceOf[Tup[_,_]].b
    case Tup(Def(First(a)), Def(Second(b))) if a == b => a

    // Rule: convert(eFrom, eTo, x, conv) if x.elem <:< eFrom  ==>  conv(x)
    case Convert(eFrom: Elem[from], _: Elem[to], x,  conv) if x.elem <:< eFrom =>
      mkApply(conv, x)

    case Apply(f @ Def(l: Lambda[a,b]), x, mayInline) if mayInline && l.mayInline =>
      mkApply(f, x)

    case call @ MethodCall(receiver, m, args, _) =>
      call.tryInvoke match {
        // Rule: receiver.m(args) ==> body(m).subst{xs -> args}
        case InvokeSuccess(res) => res
        case InvokeFailure(e) if !e.isInstanceOf[DelayInvokeException] =>
          throwInvocationException("Method invocation in rewriteDef", e, receiver, m, args)
        case InvokeFailure(ex) => throw ex
        case InvokeImpossible =>
          val res = rewriteNonInvokableMethodCall(call)
          if (res != null) res
          else
            null
      }

    case ThunkForce(th) =>
      th.node match {
        // empty Thunk
        case ThunkDef(root, sch) if sch.isEmpty => root
        // constant in Thunk
        case ConstantThunk(rootConst) => rootConst
        case _ => null
      }

    case ApplyUnOp(op, x) => rewriteUnOp(op, x)

    case ApplyBinOp(op, x, y) => rewriteBinOp(op, x, y)

    case _ => null
  }

  /** Rewrites application of given unary operation to the given argument.
    * @return null if no rewriting is defined. */
  final def rewriteUnOp[A,R](op: UnOp[A,R], x: Ref[A]): Ref[_] = {
    op match {
      case _: NumericNegate[_] => x.node match {
        // -(-x) => x
        case ApplyUnOp(_: NumericNegate[_], x) => x
        case _ => null
      }
      // (x: Int).toInt => x
      case NumericToInt(_) if x.elem == IntElement => x
      // (x: Long).toLong => x
      case NumericToLong(_) if x.elem == LongElement => x

      case _ if op == Not => x.node match {
        // Rule: !(x op y) ==>
        case ApplyBinOp(op, x, y) => op.asInstanceOf[BinOp[_,_]] match {
          case OrderingLT(ord) =>
            OrderingGTEQ(ord)(x, y)
          case OrderingLTEQ(ord) =>
            OrderingGT(ord)(x, y)
          case OrderingGT(ord) =>
            OrderingLTEQ(ord)(x, y)
          case OrderingGTEQ(ord) =>
            OrderingLT(ord)(x, y)
          case _ => null
        }
        // Rule: !(!(x)) ==> x
        case ApplyUnOp(op, x) if op == Not => x
        // Rule: !Const(x) => Const(!x)
        case Const(x: Boolean) if currentPass.config.constantPropagation => Const(!x)
        case _ => null
      }
      case _ => propagateUnOp(op, x)
    }
  }

  /** Rewrites application of given binary operation to the given arguments.
    * @return null if no rewriting is defined. */
  final def rewriteBinOp[A,R](op: BinOp[A,R], x: Ref[A], y: Ref[A]): Ref[_] = {
    op.asInstanceOf[BinOp[_,_]] match {
      case _: Equals[_] =>
        if (x == y) Const(true)
        else {
          y.node match {
            case Const(b: Boolean) if x.elem == BooleanElement =>
              if (b) x else Not(asRep[Boolean](x))
            case _ =>
              x.node match {
                case Const(b: Boolean) if y.elem == BooleanElement =>
                  if (b) y else Not(asRep[Boolean](y))
                case _ =>
                  null
              }
          }
        }
      case _: NotEquals[_] =>
        if (x == y) Const(false)
        else {
          y.node match {
            case Const(b: Boolean) if x.elem == BooleanElement =>
              if (b) Not(asRep[Boolean](x)) else x
            case _ =>
              x.node match {
                case Const(b: Boolean) if y.elem == BooleanElement =>
                  if (b) Not(asRep[Boolean](y)) else y
                case _ =>
                  null
              }
          }
        }
      case And =>
        rewriteBoolConsts(x, y, x => x, _ => false, x => x, _ => false)
      case Or =>
        rewriteBoolConsts(x, y, _ => true, x => x, x => x, _ => true)
      case BinaryXorOp =>
        rewriteBoolConsts(x, y, x => !x.asInstanceOf[Ref[Boolean]], x => x.asInstanceOf[Ref[Boolean]], _ => false, _ => true)

      case NumericPlus(n: ExactNumeric[a]) => (x, y) match {
        // x + 0 => x
        case (x, Def(Const(zero))) if isZero(zero.asInstanceOf[a], n) => x
        // 0 + x => x
        case (Def(Const(zero)), x) if isZero(zero.asInstanceOf[a], n) => x
        case _ => propagateBinOp(op, x, y)
      }

      case NumericMinus(n: ExactNumeric[a]) => (x, y) match {
        // x - 0 => x
        case (x, Def(Const(zero))) if isZero(zero.asInstanceOf[a], n) => x
        // 0 - x => -x
        case (Def(Const(zero)), x) if isZero(zero.asInstanceOf[a], n) =>
          new NumericOpsCls(asRep[a](x))(n).unary_-
        case _ => propagateBinOp(op, x, y)
      }

      case NumericTimes(n: ExactNumeric[a]) => (x, y) match {
        // _ * 0 => 0
        case (_, y@Def(Const(zero))) if isZero(zero.asInstanceOf[a], n) => y
        // 0 * _ => 0
        case (y@Def(Const(zero)), _) if isZero(zero.asInstanceOf[a], n) => y
        // x * 1 => x
        case (x, Def(Const(one))) if isOne(one.asInstanceOf[a], n) => x
        // 1 * x => x
        case (Def(Const(one)), x) if isOne(one.asInstanceOf[a], n) => x
        case _ => propagateBinOp(op, x, y)
      }

      case _ => propagateBinOp(op, x, y)
    }
  }

  /** Perform constant propagation if enabled and argument is Const.
    * @return null if propagation is not done
    */
  def propagateUnOp[T,R](op: UnOp[T,R], x: Ref[T]): Ref[R] = {
    if (currentPass.config.constantPropagation) {
      if (x.isConst) {
        val xVal = valueFromRep(x)
        if (op.shouldPropagate(xVal))
          Const(op.applySeq(xVal))(op.eResult)
        else
          null
      }
      else null
    }
    else null
  }

  /** Perform constant propagation if enabled and both arguments are Const.
    * @return null if propagation is not done
    */
  def propagateBinOp[T,R](op: BinOp[T,R], x: Ref[T], y: Ref[T]): Ref[R] = {
    if (currentPass.config.constantPropagation) {
      if (x.isConst && y.isConst) {
        val xVal = valueFromRep(x)
        val yVal = valueFromRep(y)
        if (op.shouldPropagate(xVal, yVal))
          reifyObject(Const(op.applySeq(xVal, yVal))(op.eResult))
        else
          null
      }
      else null
    } else
      null
  }
}
