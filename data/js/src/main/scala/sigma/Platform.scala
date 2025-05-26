package sigma

import org.ergoplatform.ErgoBox
import sigma.ast._
import sigma.data._

import java.math.BigInteger

object Platform {
  /** Creates a new Constant instance with an appropriate type derived from the given data `obj`.
    * Uses scalan.Nullable instead of scala.Option to avoid allocation on consensus hot path.
    * This method is part of consensus and is used in [[SubstConstants]] operation.
    */
  private[sigma] def liftToConstant(obj: Any, builder: SigmaBuilder): Nullable[Constant[SType]] = {
    import builder._
    obj match {
      case arr: Array[Boolean] => Nullable(mkCollectionConstant[SBoolean.type](arr, SBoolean))
      case arr: Array[Byte] => Nullable(mkCollectionConstant[SByte.type](arr, SByte))
      case arr: Array[Short] => Nullable(mkCollectionConstant[SShort.type](arr, SShort))
      case arr: Array[Int] => Nullable(mkCollectionConstant[SInt.type](arr, SInt))
      case arr: Array[Long] => Nullable(mkCollectionConstant[SLong.type](arr, SLong))
      case arr: Array[BigInteger] => Nullable(mkCollectionConstant[SBigInt.type](arr.map[BigInt](n => CBigInt(n)), SBigInt))
      case arr: Array[String] => Nullable(mkCollectionConstant[SString.type](arr, SString))
      case v: AnyValue =>
        val tpe = Evaluation.rtypeToSType(v.tVal)
        Nullable(mkConstant[tpe.type](v.value.asInstanceOf[tpe.WrappedType], tpe))
      case v: Int => Nullable(mkConstant[SInt.type](v, SInt))
      case v: Long => Nullable(mkConstant[SLong.type](v, SLong))
      case v: BigInteger => Nullable(mkConstant[SBigInt.type](CBigInt(v), SBigInt))
      case n: sigma.BigInt => Nullable(mkConstant[SBigInt.type](n, SBigInt))
      case ge: GroupElement => Nullable(mkConstant[SGroupElement.type](ge, SGroupElement))
      case b: Boolean => Nullable(if (b) TrueLeaf else FalseLeaf)
      case v: String => Nullable(mkConstant[SString.type](v, SString))

      // The Box lifting was broken in v4.x. `SigmaDsl.Box(b)` was missing which means the
      // isCorrectType requirement would fail in ConstantNode constructor.
      // This method is used as part of consensus in SubstConstants operation, however
      // ErgoBox cannot be passed as argument as it is never valid value during evaluation.
      // Thus we can use activation-based versioning and fix this code when v5.0 is activated.
      case b: ErgoBox =>
        Nullable(mkConstant[SBox.type](CBox(b), SBox)) // fixed in v5.0

      // this case is added in v5.0 and it can be useful when the box value comes from a
      // register or a context variable is passed to SubstConstants.
      case b: sigma.Box =>
        if (VersionContext.current.isJitActivated)
          Nullable(mkConstant[SBox.type](b, SBox))
        else
          Nullable.None // return the same result as in v4.x when there was no this case
      case avl: AvlTreeData => Nullable(mkConstant[SAvlTree.type](CAvlTree(avl), SAvlTree))
      case avl: AvlTree => Nullable(mkConstant[SAvlTree.type](avl, SAvlTree))
      case sb: SigmaBoolean => Nullable(mkConstant[SSigmaProp.type](CSigmaProp(sb), SSigmaProp))
      case p: SigmaProp => Nullable(mkConstant[SSigmaProp.type](p, SSigmaProp))
      case coll: Coll[a] =>
        val tpeItem = Evaluation.rtypeToSType(coll.tItem)
        Nullable(mkCollectionConstant(coll.asInstanceOf[SCollection[SType]#WrappedType], tpeItem))
      case _ =>
        Nullable.None
    }
  }
}
