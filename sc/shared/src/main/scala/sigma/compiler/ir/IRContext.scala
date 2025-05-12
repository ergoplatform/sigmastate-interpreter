package sigma.compiler.ir

import sigma.compiler.ir.core.MutableLazy
import sigma.compiler.ir.primitives._
import sigma.data.{Nullable, RType}
import sigma.util.MemoizedFunc
import sigma.compiler.ir.wrappers.scala.WOptionsModule
import sigma.compiler.ir.wrappers.sigma.{CollsModule, SigmaDslModule, WRTypesModule}

/** Aggregate cake with all inter-dependent modules assembled together.
  * Each instance of this class contains independent IR context, thus many
  * instances can be created simultaneously.
  * However, the inner types declared in the traits are path-dependant.
  * This in particular means that ctx1.Ref[_] and ctx2.Ref[_] are different types.
  * The typical usage is to create `val ctx = new Scalan` and then import inner
  * declarations using `import ctx._`.
  * This way the declaration will be directly available as if they were global
  * declarations.
  * At the same time cake design pattern allow to `override` many methods and values
  * in classed derived from `Scalan`, this is significant benefit over
  * *everything is global* design.
  *
  * It is not used in v5.0 interpreter and thus not part of consensus.
  * Used in ErgoScript compiler only.
  *
  * @see CompiletimeIRContext
  */
trait IRContext
  extends TypeDescs
  with MethodCalls
  with Tuples
  with NumericOps
  with UnBinOps
  with LogicalOps
  with OrderingOps
  with Equal
  with MiscOps
  with Functions
  with IfThenElse
  with Transforming
  with Thunks
  with Entities
  with Modules
  with DefRewriting
  with CollsModule
  with SigmaDslModule
  with TreeBuilding
  with GraphBuilding
  with WOptionsModule
  with WRTypesModule {

  import Coll._
  import CollBuilder._
  import WOption._
  import WRType._

  /** Pass configuration which is used to turn-off constant propagation.
    * USED IN TESTS ONLY.
    * @see `beginPass(noCostPropagationPass)`  */
  lazy val noConstPropagationPass = new DefaultPass(
    "noCostPropagationPass",
    Pass.defaultPassConfig.copy(constantPropagation = false))

  type LazyRep[T] = MutableLazy[Ref[T]]

  private val _liftElemMemo = new MemoizedFunc({
    case eT: Elem[t] =>
      val lT = Liftables.asLiftable[Any, t](eT.liftable)
      liftableRType(lT).lift(eT.sourceType.asInstanceOf[RType[Any]])
  })
  implicit def liftElem[T](eT: Elem[T]): Ref[WRType[T]] = {
    _liftElemMemo(eT).asInstanceOf[Ref[WRType[T]]]  // asRep cannot be used for AnyRef
  }

  override protected def onReset(): Unit = {
    _liftElemMemo.reset()
    super.onReset()
  }

  val CM = CollMethods
  private val CBM = CollBuilderMethods
  private val WOptionM = WOptionMethods

  def colBuilder: Ref[CollBuilder]

  /** Type descriptor for [[WRType]] */
  implicit lazy val wRTypeAnyElement: Elem[WRType[Any]] = wRTypeElement(AnyElement)

  /** During compilation represent a global value Global, see also SGlobal type. */
  def sigmaDslBuilder: Ref[SigmaDslBuilder]

  object IsNumericToInt {
    def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToInt[_], x) => Nullable(x.asInstanceOf[Ref[A] forSome {type A}])
      case _ => Nullable.None
    }
  }
  object IsNumericToLong {
    def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToLong[_], x) => Nullable(x.asInstanceOf[Ref[A] forSome {type A}])
      case _ => Nullable.None
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case CM.length(ys) => ys.node match {
      // Rule: xs.map(f).length  ==> xs.length
      case CM.map(xs, _) =>
        xs.length
      // Rule: replicate(len, v).length => len
      case CBM.replicate(_, len, _) =>
        len
      // Rule: Const[Coll[T]](coll).length =>
      case CollConst(coll, _) =>
        coll.length
      // Rule: Coll(items @ Seq(x1, x2, x3)).length => items.length
      case CBM.fromItems(_, items, _) =>
        items.length
      case _ => super.rewriteDef(d)
    }

    // Rule: replicate(l, x).zip(replicate(l, y)) ==> replicate(l, (x,y))
    case CM.zip(CBM.replicate(b1, l1, v1), CBM.replicate(b2, l2, v2)) if b1 == b2 && l1 == l2 =>
      b1.replicate(l1, Pair(v1, v2))

    case CM.map(xs, _f) => _f.node match {
      case IdentityLambda() => xs
      case _ => xs.node match {
        // Rule: replicate(l, v).map(f) ==> replicate(l, f(v))
        case CBM.replicate(b, l, v: Ref[a]) =>
          val f = asRep[a => Any](_f)
          b.replicate(l, Apply(f, v, false))

        // Rule: xs.map(f).map(g) ==> xs.map(x => g(f(x)))
        case CM.map(_xs, f: RFunc[a, b]) =>
          implicit val ea = f.elem.eDom
          val xs = asRep[Coll[a]](_xs)
          val g  = asRep[b => Any](_f)
          xs.map[Any](fun { x: Ref[a] => g(f(x)) })

        case _ => super.rewriteDef(d)
      }
    }

    case WOptionM.getOrElse(opt, _) => opt.node match {
      case WOptionConst(Some(x), lA) => lA.lift(x)
      case _ => super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }

  override def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): Any = e match {
    case _: CollElem[_,_] => mc match {
      case CollMethods.map(_, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.resultType, mc.isAdapterCall, mc.typeSubst)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _ =>
      super.invokeUnlifted(e, mc, dataEnv)
  }

}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext
