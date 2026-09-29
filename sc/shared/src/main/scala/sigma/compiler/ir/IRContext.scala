package sigma.compiler.ir

import sigma.compiler.ir.core.MutableLazy
import sigma.compiler.ir.primitives._
import sigma.ast.{ConcreteCollection, SCollectionMethods}
import sigma.data.Nullable

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
  with Elems
  with DefRewriting
  with Lowering
  with TreeBuilding
  with GraphBuilding {

  /** Pass configuration which is used to turn-off constant propagation.
    * USED IN TESTS ONLY.
    * @see `beginPass(noCostPropagationPass)`  */
  lazy val noConstPropagationPass = new DefaultPass(
    "noCostPropagationPass",
    Pass.defaultPassConfig.copy(constantPropagation = false))

  type LazyRep[T] = MutableLazy[Ref[T]]

  /** Pattern for `Coll(items)` literal nodes, shared by the rewrite rules here and in GraphBuilding. */
  protected val ConcreteColl = CallPattern(GlobalOpCallee(ConcreteCollection))

  /** During compilation represent a global value Global, see also SGlobal type. */
  def sigmaDslBuilder: Ref[sigma.SigmaDslBuilder]

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

  private val CollLength = CallPattern(SCollectionMethods.SizeMethod)
  private val CollMap    = CallPattern(SCollectionMethods.MapMethod)

  override def rewriteDef[T](d: Def[T]) = d match {
    case CollLength(ys, _) => ys.node match {
      // Rule: xs.map(f).length  ==> xs.length
      case CollMap(xs, _) =>
        collLength(asRep[Any](xs))
      // Rule: Const[sigma.Coll[T]](coll).length =>
      case DslConst(coll: sigma.Coll[_]) =>
        coll.length
      // Rule: Coll(items @ Seq(x1, x2, x3)).length => items.length
      case ConcreteColl(_, items) =>
        items.length
      case _ => super.rewriteDef(d)
    }

    case CollMap(xs, Seq(_f)) => _f.node match {
      case IdentityLambda() => xs
      case _ => xs.node match {
        // Rule: xs.map(f).map(g) ==> xs.map(x => g(f(x)))
        case CollMap(_xs, Seq(f)) =>
          val ff = asRep[Any => Any](f)
          val g = asRep[Any => Any](_f)
          implicit val ea: Elem[Any] = ff.elem.asInstanceOf[FuncElem[Any, Any]].eDom
          collMap(asRep[Any](_xs), fun { x: Ref[Any] => g(ff(x)) })
        case _ => super.rewriteDef(d)
      }
    }

    case _ => super.rewriteDef(d)
  }
}

/** IR context to be used by script development tools to compile ErgoScript into ErgoTree bytecode. */
class CompiletimeIRContext extends IRContext
