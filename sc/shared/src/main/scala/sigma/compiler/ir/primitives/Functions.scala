package sigma.compiler.ir.primitives

import debox.{cfor, Buffer => DBuffer}
import sigma.compiler.ir.{Base, IRContext, ProgramGraphs}
import sigma.data.{Lazy, Nullable, emptyDBufferOfInt}
import sigma.util.GraphUtil

import java.util
import scala.language.implicitConversions

/** Defines IR representation of functions (Lambdas) and function application. */
trait Functions extends Base with ProgramGraphs { self: IRContext =>

  implicit class LambdaOps[A,B](f: Ref[A => B]) {
    /** Apply given function symbol to the given argument symbol.
      * @return  symbol representing result of function application */
    final def apply(x: Ref[A]): Ref[B] = mkApply(f, x)
  }

  /** Global lambda equality mode used by default. It is used in `fun` and `fun2` lambda builders.
    * If this flag is `true` then Lambda nodes are equal if they are the same up to renaming of symbols. (see Lambda.equals()).
    * Each Lambda node has independent equality mode flag which is setup in the constructor. */
  var useAlphaEquality: Boolean = true

  /** Global flag governing lambda reification in `fun` and `mkLambda`.
    * If this flag is `true` then original `f: Ref[A] => Ref[B]` function is stored in Lambda node.
    * As a consequence if `f` is not stored, then `unfoldLambda` is done by `mirrorLambda`. */
  var keepOriginalFunc: Boolean = true

  /** Turns on/off lambda unfolding using original function `f` stored in the Lambda node.
    * If this flag is `false` then this function cannot be used even if it is present in the node. */
  var unfoldWithOriginalFunc: Boolean = true

  /** Executes given lambda to construct Lambda node. The function `f` can be called with any symbol
    * and has an effect of growing a graph starting from the argument symbol.
    * If a reference to `Variable` node is passed as argument, then the constructed graph nodes
    * can be collected to Lambda node forming its body and schedule.
    * @param  f  function which execution will create body nodes
    * @param  eA arguments type descriptor
    */
  implicit final def fun[A,B](f: Ref[A] => Ref[B])(implicit eA: LElem[A]): Ref[A => B] = mkLambda(f, true, useAlphaEquality, keepOriginalFunc)
  implicit final def fun2[A,B,C](f: (Ref[A], Ref[B]) => Ref[C])(implicit eA: LElem[A], eB: LElem[B]): Ref[((A,B))=>C] = mkLambda(f)

  /** Represent lambda expression as IR node.
    * @param f   optional function, which was used to compute `y`
    * @param x   lambda-bound variable
    * @param y   symbol representing result of lambda invocation
    * @param mayInline  whether this lambda can be inlined when applied
    * @param alphaEquality  whether to use alpha-equality in `equals` */
  class Lambda[A, B](val f: Nullable[Ref[A] => Ref[B]], val x: Ref[A], val y: Ref[B], val mayInline: Boolean, val alphaEquality: Boolean = true)
    extends AstGraph with Def[A => B] { thisLambda =>
    def eA = x.elem
    def eB = y.elem

    private var _resultType: Elem[A => B] = _
    def resultType: Elem[A => B] = {
      if (_resultType != null) _resultType
      else {
        val res = funcElement(eA, eB)
        if (!y.isPlaceholder) _resultType = res  // memoize once y is assigned
        res
      }
    }

    // ensure all lambdas of the same type have the same hashcode,
    // so they are tested for alpha-equivalence using equals
    private var _hashCode: Int = 0
    override def hashCode: Int = {
      if (_hashCode == 0) {
        _hashCode = if (alphaEquality)
          41 * (41 + x.elem.hashCode) + y.elem.hashCode
        else
          41 * (41 + x.hashCode) + y.hashCode
      }
      _hashCode
    }

    override def equals(other: Any) = (this eq other.asInstanceOf[AnyRef]) ||
      (other match {
        case other: Lambda[_,_] =>
          if (alphaEquality)
            matchLambdas(this, other, false, emptyMatchSubst).isDefined
          else
            other.x == this.x && other.y == this.y
        case _ => false
      })

    override def toString = s"Lambda(${if (f.isDefined) "f is Some" else "f is None"}, $x => $y})"
    def canEqual(other: Any) = other.isInstanceOf[Lambda[_,_]]

    // Product implementation
    def productElement(n: Int): Any = n match {
      case 0 => x
      case 1 => y
      case _ => throw new NoSuchElementException(s"Lambda.productElement($n) is undefined")
    }
    def productArity: Int = 2

    // AstGraph implementation
    val boundVars: Seq[Sym] = Array[Sym](x)
    val boundVarId = x.node._nodeId
    val roots: Seq[Sym] = Array(y)

    override lazy val rootIds: DBuffer[Int] = super.rootIds

    override lazy val freeVars = super.freeVars

    override def isIdentity: Boolean = x == y

    @inline override def isBoundVar(s: Sym) = s.node.nodeId == boundVarId

    override lazy val  scheduleIds: DBuffer[Int] = {
      val sch = if (isIdentity)
        emptyDBufferOfInt
      else {
        // graph g will contain all Defs reified as part of this Lambda, (due to `filterNode`)
        // BUT not all of them depend on boundVars, thus we need to filter them out
        // 1) we build g.schedule and then g.usageMap
        // 2) collect set of nodes, which depend on `x`
        val g = new PGraph(roots, filterNode = Nullable(s => s.node._nodeId >= boundVarId))
        val usages = new PGraphUsages(g)
        val locals = GraphUtil.depthFirstSetFrom[Int](DBuffer(boundVarId))(usages)
        val gschedule = g.schedule.toArray
        val len = gschedule.length
        val sch = DBuffer.ofSize[Int](len)
        cfor(0)(_ < len, _ + 1) { i =>
          val sym = gschedule(i)
          val id = sym.node.nodeId
          if (locals(id) && !sym.isVar)
            sch += id
        }
        val currSch = if (sch.isEmpty) g.rootIds else sch
        currSch
      }
      if (debugModeSanityChecks) {
        // check that every inner lambda depend on boundVars
        cfor(0)(_ < sch.length, _ + 1) { i =>
          getSym(sch(i)).node match {
            case l: Lambda[_,_] =>
              val varDeps = l.deps intersect(boundVars ++ sch.map(getSym(_)).toArray)
              if (varDeps.isEmpty) {
                //                val cwd = new File("").getAbsoluteFile
                //                val dir = "test-out/errors"
                //                emitDepGraph(roots, FileUtil.file(cwd, dir), "nested_lambda")(defaultGraphVizConfig)
                assert(false, s"Invalid nested lambda $l inside $this")
              }
            case _ =>
          }
        }
      }
      sch
    }

    override protected def getDeps: Array[Sym] = freeVars.toArray
  }

  type LambdaData[A,B] = (Lambda[A,B], Nullable[Ref[A] => Ref[B]], Ref[A], Ref[B])
  object Lambda {
    def unapply[A,B](lam: Lambda[A, B]): Nullable[LambdaData[A,B]] = {
      val res: LambdaData[A,B] =
        if (lam == null) null
        else {
          (lam, lam.f, lam.x, lam.y)
        }
      Nullable(res)
    }
  }

  /**
   * Matcher for lambdas which don't depend on their arguments
   * (but can close over other expressions, unlike VeryConstantLambda).
   */
  object ConstantLambda {
    // if lam.y depends on lam.x indirectly, lam.schedule must contain the dependency path
    // and its length will be > 1
    def unapply[A,B](lam: Lambda[A, B]): Option[Ref[B]] =
      if (lam.schedule.length <= 1 && !lam.y.node.deps.contains(lam.x) && lam.y != lam.x)
        Some(lam.y)
      else
        None
  }

  /**
   * Matcher for lambdas which return staging-time constants.
   * VeryConstantLambda(x) should be equivalent to ConstantLambda(Def(Const(x)))
   */
  object VeryConstantLambda {
    def unapply[A,B](lam: Lambda[A, B]): Option[B] = lam.y.node match {
      case Const(y) => Some(y)
      case _ => None
    }
  }

  // matcher version of Lambda.isIdentity
  object IdentityLambda {
    def unapply[A,B](lam: Lambda[A, B]): Boolean = lam.isIdentity
  }

  case class Apply[A,B](f: Ref[A => B], arg: Ref[A], mayInline: Boolean = true) extends Def[B] {
    def resultType = f.elem.eRange
    override def transform(t: Transformer) = Apply(t(f), t(arg), mayInline)
  }

  implicit class FuncExtensions[A, B](f: Ref[A=>B]) {
    implicit def eA = f.elem.eDom
    def getLambda: Lambda[A,B] = f.node match {
      case lam: Lambda[_,_] => lam.asInstanceOf[Lambda[A,B]]
      case _ => !!!(s"Expected symbol of Lambda node but was $f", f)
    }

    def zip[C](g: Ref[A=>C]): Ref[A=>(B,C)] = {
      implicit val eB = f.elem.eRange
      implicit val eC = g.elem.eRange
      fun { (x: Ref[A]) => Pair(f(x), g(x)) }
    }
  }

  type Subst = java.util.HashMap[Sym, Sym]
  @inline def emptyMatchSubst: Subst = new util.HashMap[Sym,Sym]()

  def alphaEqual(s1: Sym, s2: Sym): Boolean = matchExps(s1, s2, false, emptyMatchSubst).isDefined

  def patternMatch(s1: Sym, s2: Sym): Nullable[Subst] = matchExps(s1, s2, true, emptyMatchSubst)

  protected def matchExps(s1: Sym, s2: Sym, allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = s1.node match {
    case _ if s1 == s2 || subst.get(s1) == s2 || subst.get(s2) == s1 =>
      Nullable(subst)
    case d1 if !d1.isInstanceOf[Variable[_]] =>
        val d2 = s2.node
        val res = matchDefs(d1, d2, allowInexactMatch, subst)
        if (res.isDefined) {
          res.get.put(s1, s2)
        }
        res
    case _ =>
      if (allowInexactMatch && !subst.containsKey(s1)) {
        subst.put(s1, s2)
        Nullable(subst)
      } else {
        Nullable.None
      }
  }

  @inline
  private def matchLambdas(lam1: Lambda[_, _], lam2: Lambda[_, _], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] =
    if (lam1.x.elem == lam2.x.elem) {
      subst.put(lam1.x, lam2.x)
      matchExps(lam1.y, lam2.y, allowInexactMatch, subst)
    }
    else
      Nullable.None

  protected def matchDefs(d1: Def[_], d2: Def[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = d1 match {
    case lam1: Lambda[_, _] => d2 match {
      case lam2: Lambda[_, _] =>
        matchLambdas(lam1, lam2, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case _ =>
      if (d1.getClass == d2.getClass && d1.productArity == d2.productArity && d1.resultType.name == d2.resultType.name) {
        matchIterators(d1.productIterator, d2.productIterator, allowInexactMatch, subst)
      } else
        Nullable.None
  }

  // generalize to Seq or Iterable if we get nodes with deps of these types
  protected def matchIterators(i1: Iterator[_], i2: Iterator[_], allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] =
    if (i1.hasNext) {
      if (i2.hasNext) {
        var res = matchAny(i1.next(), i2.next(), allowInexactMatch, subst)
        if (res.isDefined)
          res = matchIterators(i1, i2, allowInexactMatch, res.get)
        res
      } else Nullable.None
    } else {
      if (i2.hasNext) Nullable.None else Nullable(subst)
    }

  protected def matchAny(a1: Any, a2: Any, allowInexactMatch: Boolean, subst: Subst): Nullable[Subst] = a1 match {
    case s1: Sym => a2 match {
      case s2: Sym =>
        matchExps(s1, s2, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case l1: Iterable[_] => a2 match {
      case l2: Iterable[_] =>
        matchIterators(l1.iterator, l2.iterator, allowInexactMatch, subst)
      case _ => Nullable.None
    }
    case _ => if (a1 == a2) Nullable(subst) else Nullable.None
  }

  //=====================================================================================
  //   Function application

  def mkApply[A,B](f: Ref[A => B], x: Ref[A]): Ref[B] = {
    val d = f.node
    if (d.isInstanceOf[Lambda[_, _]]) {
      val lam = d.asInstanceOf[Lambda[A, B]]
      if (lam.mayInline) {
        return unfoldLambda(lam, x)
      }
    }
    Apply(f, x, mayInline = false)
  }

  def unfoldLambda[A,B](lam: Lambda[A,B], x: Ref[A]): Ref[B] = {
    lam.f match {
      case Nullable(g) if unfoldWithOriginalFunc => g(x) // unfold initial non-recursive function
      case _ => mirrorApply(lam, x)  // f is mirrored, unfold it by mirroring
    }
  }

  def unfoldLambda[A,B](f: Ref[A=>B], x: Ref[A]): Ref[B] = {
    val lam = f.getLambda
    unfoldLambda(lam, x)
  }

  def mirrorApply[A,B](lam: Lambda[A, B], s: Ref[A]): Ref[B] = {
    val body = lam.scheduleIds
    val m = new java.util.HashMap[Sym, Sym](100)
    m.put(lam.x, s)
    val subst = new MapTransformer(m)
    val t = DefaultMirror.mirrorSymbols(subst, NoRewriting, body)
    t(lam.y)
  }

  //=====================================================================================
  //   Function reification

  def mkLambda[A,B](f: Ref[A] => Ref[B],
                    mayInline: Boolean,
                    alphaEquality: Boolean,
                    keepOriginalFunc: Boolean)(implicit eA: LElem[A]): Ref[A=>B] = {
    val x = variable[A]
    lambda(x)(f, mayInline, alphaEquality, keepOriginalFunc)
  }

  def mkLambda[A,B,C](f: Ref[A]=>Ref[B]=>Ref[C])
                     (implicit eA: LElem[A], eB: Elem[B]): Ref[A=>B=>C] = {
    val y = variable[B]
    mkLambda(
      (a: Ref[A]) => lambda(y)((b:Ref[B]) => f(a)(b), mayInline = true, useAlphaEquality, keepOriginalFunc),
      mayInline = true,
      alphaEquality = useAlphaEquality,
      keepOriginalFunc = keepOriginalFunc)
  }

  def mkLambda[A,B,C](f: (Ref[A], Ref[B])=>Ref[C])(implicit eA: LElem[A], eB: LElem[B]): Ref[((A,B))=>C] = {
    implicit val leAB = Lazy(pairElement(eA.value, eB.value))
    mkLambda({ (p: Ref[(A, B)]) =>
      val (x, y) = unzipPair(p)
      f(x, y)
    }, true, useAlphaEquality, keepOriginalFunc)
  }

  var lambdaStack: List[Lambda[_,_]] = Nil

  private def lambda[A,B](x: Ref[A])(f: Ref[A] => Ref[B],
                                     mayInline: Boolean,
                                     alphaEquality: Boolean,
                                     keepOriginalFunc: Boolean)(implicit leA: LElem[A]): Ref[A=>B] = {
    // ySym will be assigned after f is executed
    val ySym = placeholder(LazyAnyElement).asInstanceOf[Ref[B]]

    val orig = if (keepOriginalFunc) Nullable(f) else Nullable.None
    val lam = new Lambda(orig, x, ySym, mayInline, alphaEquality)
    val lamSym = lam.self

    val oldStack = lambdaStack
    try {
      lambdaStack = lam :: lambdaStack
      val y = { // reifyEffects block
        f(x)
      }
      ySym.assignDefFrom(y)
    }
    finally {
      lambdaStack = oldStack
    }

    findOrCreateDefinition(lam, lamSym)
  }

  class LambdaStack {
    var stack = List[Sym]()
    def top: Option[Sym] = stack.isEmpty match { case true => None case _ => Some(stack.head) }
    def push(e: Sym): this.type = { stack = e :: stack; this }
    def pop: Sym = {
      val res = stack.head;
      stack = stack.tail;
      res
    }
  }

  def identityFun[A](implicit e: Elem[A]) = fun[A, A](x => x)

  def upcastFun[A: Elem, B >: A]: Ref[A => B] = fun[A,B](x => x)

  def constFun[A, B](x: Ref[B])(implicit e: Elem[A]) = {
    implicit val eB = x.elem
    fun[A, B](_ => x)
  }

  /** Composition of two functions (in mathematical notation), where first `g` is applied and them `f`. */
  def compose[A, B, C](f: Ref[B => C], g: Ref[A => B]): Ref[A => C] = {
    implicit val eA = g.elem.eDom
    implicit val eC = f.elem.eRange
    fun { x => f(g(x)) }
  }

}
