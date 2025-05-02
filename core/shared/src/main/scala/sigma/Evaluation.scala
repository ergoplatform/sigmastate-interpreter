package sigma

import debox.cfor
import sigma.ast.SType
import sigma.data.RType._
import sigma.data._
import sigma.ast._


// TODO refactor: find better place for this methods after code cleanup and repo reorganization

/** Helper methods used as part of ErgoTree evaluation. */
object Evaluation {

  /** Transforms a serializable ErgoTree type descriptor to the corresponding RType descriptor of SigmaDsl,
    * which is used during evaluation.
    */
  def stypeToRType[T <: SType](t: T): RType[T#WrappedType] = (t match {  // TODO optimize using memoization
    case SBoolean => BooleanType
    case SByte => ByteType
    case SShort => ShortType
    case SInt => IntType
    case SLong => LongType
    case SString => StringType
    case SAny => AnyType
    case SUnit => UnitType
    case SBigInt => BigIntRType
    case SBox => BoxRType
    case SContext => ContextRType
    case SGlobal => SigmaDslBuilderRType
    case SHeader => HeaderRType
    case SPreHeader => PreHeaderRType
    case SGroupElement => GroupElementRType
    case SAvlTree => AvlTreeRType
    case SSigmaProp => SigmaPropRType
    case tup: STuple if tup.items.length == 2 =>
      val tpeA = tup.items(0)
      val tpeB = tup.items(1)
      pairRType(stypeToRType(tpeA), stypeToRType(tpeB))
    case STuple(items) =>
      val types = items.toArray
      val len = types.length
      val rtypes = new Array[SomeType](len)
      cfor(0)(_ < len, _ + 1) { i =>
        rtypes(i) = stypeToRType(types(i)).asInstanceOf[SomeType]
      }
      tupleRType(rtypes)
    case c: SCollectionType[a] => collRType(stypeToRType(c.elemType))
    case o: SOption[a] => optionRType(stypeToRType(o.elemType))
    case SFunc(args, tpeRange, Nil) if args.length == 1 =>
      val tpeArg = args(0)
      funcRType(stypeToRType(tpeArg), stypeToRType(tpeRange))
    case _ =>
      sys.error(s"We couldn't process the type $t. " +
        "Please ensure that $t is compatible with the expected type. " +
        "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
  }).asInstanceOf[RType[T#WrappedType]]

  /** Transforms RType descriptor of SigmaDsl, which is used during evaluation,
    * to the corresponding serializable ErgoTree type descriptor,
    */
  def rtypeToSType[T](t: RType[T]): SType = t match { // TODO optimize using memoization
    case BooleanType => SBoolean
    case ByteType => SByte
    case ShortType => SShort
    case IntType => SInt
    case LongType => SLong
    case StringType => SString
    case AnyType => SAny
    case UnitType => SUnit
    case BigIntRType => SBigInt
    case GroupElementRType => SGroupElement
    case AvlTreeRType => SAvlTree
    case ot: OptionType[_] => SOption(rtypeToSType(ot.tA))
    case BoxRType => SBox
    case ContextRType => SContext
    case SigmaDslBuilderRType => SGlobal
    case HeaderRType => SHeader
    case PreHeaderRType => SPreHeader
    case SigmaPropRType => SSigmaProp
    case SigmaBooleanRType => SSigmaProp  // TODO remove: this is not used in consensus code
    case tup: TupleType => STuple(tup.items.map(t => rtypeToSType(t)))
    case at: ArrayType[_] => SCollection(rtypeToSType(at.tA))
    case ct: CollType[_] => SCollection(rtypeToSType(ct.tItem))
    case ft: FuncType[_,_] => SFunc(rtypeToSType(ft.tDom), rtypeToSType(ft.tRange))
    case pt: PairType[_,_] => STuple(rtypeToSType(pt.tFst), rtypeToSType(pt.tSnd))
    case _ => sys.error(s"We couldn't process the type $t. " +
      "Please ensure that $t is compatible with the expected type. " +
      "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
  }

  /** Convert SigmaDsl representation of tuple to ErgoTree serializable representation. */
  def fromDslTuple(value: Any, tupleTpe: STuple): Coll[Any] = value match {
    case t: Tuple2[_,_] => TupleColl(t._1, t._2)
    case a: Coll[Any]@unchecked if a.tItem == sigma.AnyType => a
    case _ =>
      sys.error(s"Failed to process the value $value with type $tupleTpe. " +
        "Please make sure the value is the correct type. " +
        "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
  }

  /** Convert ErgoTree serializable representation of tuple to SigmaDsl representation. */
  def toDslTuple(value: Coll[Any], tupleTpe: STuple): Any = tupleTpe match {
    case t if t.items.length == 2 => (value(0), value(1))
    case _ => value
  }

}
