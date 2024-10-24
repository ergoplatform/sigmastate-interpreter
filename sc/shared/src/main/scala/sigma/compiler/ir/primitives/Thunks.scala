package sigma.compiler.ir.primitives

import debox.{cfor, Buffer => DBuffer, Set => DSet}
import scalan.core.Covariant
import sigma.compiler.ir.IRContext
import sigma.data.{AVHashMap, DFunc, Lazy, Nullable, RType}
import sigma.reflection.RClass
import sigma.util.GraphUtil

import scala.collection.Seq
import scala.language.{existentials, implicitConversions}

/** Slice in the [[IRContext]] cake with definitions of Thunk operations.
  * See https://en.wikipedia.org/wiki/Thunk.
  * Thunks are used to represent lazy operations in the graph IR.
 *
  * @see ApplyBinOpLazy, IfThenElseLazy
  */
trait Thunks extends Functions { self: IRContext =>

  type Th[+T] = Ref[Thunk[T]]

  /** Phantom type to define thunk-typed graph nodes and thunk based lazy operations.
    * Usually used inside [[Ref]], see for example [[Th]].
    * See also for details http://gigiigig.github.io/tlp-step-by-step/phantom-types.html
    */
  trait Thunk[+A] { def value: A }

  /** A class of factory to create new Thunks by use `Thunk { ... }` expressions. */
  class ThunkCompanion {
    def apply[T](block: => Ref[T]) = thunk_create(block)
    def forced[T](block: => Ref[T]) = thunk_create(block).force
  }

  /** Allow expressions like `Thunk { ... }` to create new Thunks. */
  val Thunk: ThunkCompanion = new ThunkCompanion

  /** Extension methods on `Ref[Thunk[T]]` values. */
  implicit class RepThunkOps[T](t: Th[T]) {
    /** Forces evaluation of the thunk to produce the delayed value. */
    def force() = thunk_force(t)

    /** Creates a new thunk which, when forced, in turn forces `t` and then maps the resulting
      * value using `f`. The application of `f` may be inlined to the new thunk body, or
      * may be reified as [[Apply]] node, this depends on parameters of Lambda node.
      *
      * @param f reference to graph node of type [[Lambda]]
      */
    def map[R](f: Ref[T => R]): Th[R] = thunk_map(t, f)

    /** Creates a new thunk which, when forced, in turn forces `t` and then maps the resulting
      * value using `f`.
      * @param f scala function which is always inlined (staged) into the new think body
      */
    def map[R](f: Ref[T] => Ref[R]): Th[R] = thunk_map1(t, f)
  }

  /** Thunk is an instance of container type class [[Cont]]. */
  implicit val thunkCont: Cont[Thunk] = new Cont[Thunk] {
    override def lift[T](implicit eT: Elem[T]) = element[Thunk[T]]
    override def unlift[T](implicit eFT: Elem[Thunk[T]]) = eFT.eItem
    override def unapply[T](e: Elem[_]) = e match {
      case e: ThunkElem[_] => Some(asElem[Thunk[T]](e))
      case _ => None
    }
  }

  import Liftables._

  /** Runtime representation of lazy values.
    * Each [[Thunk]] typed graph node evaluates to a value of this type.
    */
  type SThunk[T] = () => T

  /** Graph node to represent constants of type [[Thunk]].
    * @see scalan.Base.Liftables.Liftable */
  case class ThunkConst[ST, T](constValue: SThunk[ST], lT: Liftable[ST, T])
      extends BaseDef[Thunk[T]]()(thunkElement(lT.eW))
         with LiftedConst[SThunk[ST], Thunk[T]] {
    val liftable: Liftable[SThunk[ST], Thunk[T]] = liftableThunk(lT)
  }

  /** Implementation of Liftable type class for `Thunk[T]` given liftable for `T`. */
  case class LiftableThunk[ST, T](lT: Liftable[ST, T]) extends Liftable[SThunk[ST], Thunk[T]] {
    import sigma.data.RType._
    override def eW: Elem[Thunk[T]] = thunkElement(lT.eW)
    override def sourceType: RType[SThunk[ST]] = {
      implicit val tST = lT.sourceType
      RType[SThunk[ST]]
    }
    override def lift(x: SThunk[ST]): Ref[Thunk[T]] = ThunkConst(x, lT)
  }

  implicit def liftableThunk[ST,T](implicit lT: Liftable[ST,T]): Liftable[SThunk[ST], Thunk[T]] =
    LiftableThunk(lT)


  /** Implements a type descriptor of `Thunk[A]` type given the instance of `A`. */
  case class ThunkElem[A](override val eItem: Elem[A])
    extends EntityElem1[A, Thunk[A], Thunk](eItem, container[Thunk]) {
    override lazy val liftable = asLiftable[SThunk[_], Thunk[A]](liftableThunk(eItem.liftable))
    override lazy val typeArgs = TypeArgs("A" -> (eItem -> Covariant))
  }

  /** Implicitly defines element type for thunks (aka lazy values). */
  implicit def thunkElement[T](implicit eItem: Elem[T]): Elem[Thunk[T]] =
    cachedElemByClass(eItem)(RClass(classOf[ThunkElem[T]]))

  /** Implicit conversion (downcast) to access `ThunkElem.eItem` field. */
  implicit def extendThunkElement[T](elem: Elem[Thunk[T]]): ThunkElem[T] = elem.asInstanceOf[ThunkElem[T]]

  /** Graph node representing thunk with reified body.
    * Each thunk node is a specialized implementation of AstGraph abstract class.
    * @param _scheduleIds compact representation of thunk body, i.e. a sequence of graph
    *                     nodes, which will be executed when the thunk is forced. Each
    *                     node is given by its id. The node can be resolved using
    *                     `getSym(id).node` expression.
    * @param root graph node, which represents the result value of thunk forcing.
    */
  class ThunkDef[A](val root: Ref[A], _scheduleIds: => ScheduleIds)
    extends AstGraph with Def[Thunk[A]] {

    implicit def eA: Elem[A] = root.elem
    private var _selfType: Elem[Thunk[A]] = _
    override def resultType: Elem[Thunk[A]] =
      if (_selfType != null) _selfType
      else {
        val res = thunkElement(eA)
        if (!root.isPlaceholder) _selfType = res  // memoize once root is assigned
        res
      }

    override lazy val scheduleIds: ScheduleIds = _scheduleIds

    /** NOTE on structural equality implementation
      * Every Def is assigned fresh nodeId in the constructor. As result this ThunkDef
      * instance will have unique nodeId. Thus, different ThunkDef instances will have
      * different nodeIds and hence they are NOT equal.
      * */
    override lazy val hashCode: Int = _nodeId //41 * (41 + root.hashCode) + schedule.hashCode
    override def canEqual(other: Any) = other.isInstanceOf[ThunkDef[_]]
    override def equals(other: Any) =
      other match {
        case that: ThunkDef[_] => _nodeId == that._nodeId
        case _ => false
      }
    override def toString = s"Th($root, [${scheduleIds.toArray.mkString(",")}])"


    // Product implementation
    override def productElement(n: Int): Any = n match {
      case 0 => root
      case _ => throw new NoSuchElementException(s"ThunkDef.productElement($n) is undefined")
    }
    override def productArity: Int = 1

    override def boundVars = EmptySeqOfSym

    override val roots: Seq[Sym] = Array(root)

    override lazy val freeVars: Seq[Sym] = if (schedule.isEmpty) roots else super.freeVars

    override protected def getDeps: Array[Sym] = freeVars.toArray

    override lazy val rootIds: DBuffer[Int] = super.rootIds
    override def isIdentity: Boolean = false
  }
  object ThunkDef {
    def unapply(d: ThunkDef[_]): Option[(Ref[T], Schedule) forSome {type T}] = d match {
      case th: ThunkDef[_] => Some((th.root, th.schedule))
      case _ => None
    }
  }

  /** Helper object to handle construction of nested thunks. One instance is created for
    * each ThunkDef under construction. This corresponds to syntactic nesting of thunks.
    * @param parent  the scope of the parent thunk
    * @param thunkSym reference to the Graph node for which this scope is created.
    */
  class ThunkScope(val parent: ThunkScope, val thunkSym: Ref[Any]) {
    private val bodyIds: DSet[Int] = DSet.ofSize(16)
    private val bodyDefs: AVHashMap[Def[_], Def[_]] = AVHashMap(32)

    @inline final def isEmptyBody: Boolean = bodyIds.isEmpty

    /** Add the given graph node (by symbol) to this scope. */
    def +=(sym: Sym): Unit = {
      val d = sym.node
      bodyIds += d.nodeId
      bodyDefs.put(d, d)
    }

    /** Sort graph nodes of this scope topologically using depth-first search along
      * node dependencies (graph edges). This will give evaluation order of the thunk.
      */
    def scheduleForResult(root: Ref[Any]): DBuffer[Int] = {
      val sch = GraphUtil.depthFirstOrderFrom(
        DBuffer(root.node.nodeId),
        new DFunc[Int, DBuffer[Int]] { def apply(id: Int) = {
          val deps = getSym(id).node.deps
          val res = DBuffer.ofSize[Int](deps.length)
          cfor(0)(_ < deps.length, _ + 1) { i =>
            val s = deps(i)
            val id = s.node.nodeId
            if (bodyIds(id) && !s.isVar)
              res += id
          }
          res
        }}
      )
      sch
    }

    /** Find the given node among definitions accessible from this scope.
      * It searches in the chain of nested scopes for the first match.
      * If not found, searches in the global scope.
      * @return null if not found at all.
      */
    def findDef[T](d: Def[T]): Ref[T] = {
      val existingOpt = bodyDefs.get(d)
      if (existingOpt.isDefined) return existingOpt.get.self.asInstanceOf[Ref[T]]
      if (parent == null)
        findGlobalDefinition(d)
      else
        parent.findDef(d)
    }
  }

  /** The stack of nested thunks during graph construction. */
  class ThunkStack {
    /** Stack is represented as simple list with th first element as a top of the stack. */
    var stack = List[ThunkScope]()

    /** @return optional top scope. */
    @inline def top: Nullable[ThunkScope] = if (stack.isEmpty) Nullable.None else Nullable(stack.head)

    /** Push new scope when start constructing new thunk. */
    def push(e: ThunkScope): this.type = { stack = e :: stack; this }

    /** Pop new scope when thunk has been constructed. */
    @inline def pop: ThunkScope = {
      val res = stack.head
      stack = stack.tail
      res
    }

    /** For a given thunk node, create a new scope and push it on the stack. */
    def beginScope(thunkSym: Ref[Any]): ThunkScope = {
      val parent = if (stack.isEmpty) null else stack.head
      val scope = new ThunkScope(parent, thunkSym)
      this.push(scope)
      scope
    }

    /** End current scope and pop it from the stack. */
    @inline def endScope(): Unit = { this.pop }
  }

  protected val thunkStack = new ThunkStack

  implicit def repToThunk[A](block: Ref[A]): Ref[Thunk[A]] = thunk_create(block)

  /** Constructs a new thunk node by executing the given `block` and collecting all the
    * graph node created along the way.
    * This methods:
    * 1) starts a new nested ThunkScope,
    * 2) executes the `block` to obtain resulting graph node
    * 3) schedule thunk body for execution order
    * 4) adds a new ThunkDef node and returns its reference.
    * @return a reference to the newly created [[ThunkDef]] node
    */
  def thunk_create[A](block: => Ref[A]): Ref[Thunk[A]] = {
    var scheduleIds: ScheduleIds = null
    val resPH = placeholder(Lazy(AnyElement)).asInstanceOf[Ref[A]] // will be known after block is evaluated
    val newThunk = new ThunkDef(resPH, { assert(scheduleIds != null); scheduleIds })
    val newThunkSym = newThunk.self

    val newScope = thunkStack.beginScope(newThunkSym)
    // execute block and add all new definitions to the top scope (see createDefinition)
    // reify all the effects during block execution
    val res = block
    resPH.assignDefFrom(res)
    scheduleIds =
      if (res.isVar) DBuffer.ofSize(0)
      else if (newScope.isEmptyBody) DBuffer.ofSize(0)
      else newScope.scheduleForResult(res)

    val sh = newThunk.scheduleIds  // force lazy value in newThunk (see ThunkDef._scheduleIds argument above)
    thunkStack.endScope()
    toExp(newThunk, newThunkSym)
  }

  def thunk_map[A, B](t: Th[A], f: Ref[A => B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }
  def thunk_map1[A, B](t: Th[A], f: Ref[A] => Ref[B]): Th[B] = {
    Thunk {
      f(thunk_force(t))
    }
  }

  /** Specifies thunk staging strategy with respect to handling thunk_force operation.
    * @see thunk_force
    */
  var isInlineThunksOnForce = false

  /** Inlines the given thunk by cloning all its nodes and applying the given substitution (transformer).
    * @param thunk reference to the thunk node
    * @param subst transformer to be applied for each mirrored (cloned) node.
    * @return the reference to the graph node, which represents the resulting value of the thunk
    */
  def forceThunkByMirror[A](thunk: Th[A], subst: MapTransformer = MapTransformer.empty()): Ref[A] = {
    val th = thunk.node.asInstanceOf[ThunkDef[A]]
    forceThunkDefByMirror(th, subst)
  }

  /** Inlines the given thunk by cloning all its nodes and applying the given substitution (transformer).
    * @param th the thunk node
    * @param subst transformer to be applied for each mirrored (cloned) node.
    * @return the reference to the graph node, which represents the resulting value of the thunk
    */
  def forceThunkDefByMirror[A](th: ThunkDef[A], subst: MapTransformer = MapTransformer.empty()): Ref[A] = {
    val body = th.scheduleIds
    val t = DefaultMirror.mirrorSymbols(subst, NoRewriting, body)
    t(th.root)
  }

  /** Logical force of the thunk. Depending on isInlineThunksOnForce it either inlines the
    * thunk body or creates a new ThunkForce node.
    *
    * @return a reference to the graph node, which represent the result of the thunk's
    *         evaluation.
    */
  def thunk_force[A](t: Th[A]): Ref[A] =
    if (isInlineThunksOnForce)
      t.node match {
        case th @ ThunkDef(_, _) =>
          forceThunkByMirror(t)
        case _ => ThunkForce(t)
      }
    else
      ThunkForce(t)

  /** Graph node to represent thunk forcing operation. */
  case class ThunkForce[A](thunk: Ref[Thunk[A]]) extends Def[A] {
    implicit def resultType = thunk.elem.eItem
    override def transform(t: Transformer) = ThunkForce(t(thunk))
  }

  override protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = d1 match {
    case ThunkDef(root1, sch1) => d2 match {
      case ThunkDef(root2, sch2) =>
        var res = matchIterators(sch1.iterator, sch2.iterator, allowInexactMatch, subst)
        if (res.isDefined)
          res = matchExps(root1, root2, allowInexactMatch, res.get)
        res
      case _ => Nullable.None
    }
    case _ =>
      super.matchDefs(d1, d2, allowInexactMatch, subst)
  }

  object ConstantThunk {
    def unapply(d: Def[_]): Option[Const[_]] = d match {
      case ThunkDef(root @ Def(c @Const(_)), Seq(s1)) if root == s1 => Some(c)
      case _ => None
    }
    def unapply(s: Sym): Option[Const[_]] = unapply(s.node)
  }

}

