package sigma.compiler.ir

import debox.{cfor, Buffer => DBuffer}
import sigma.data.{Lazy, Nullable}
import sigma.reflection.RMethod

import java.util
import scala.language.existentials

/** Implements utilites for graph transformation and rewriting.
  * @see Pass
  * @see MapTransformer
  * @see Rewriter
  */
trait Transforming { self: IRContext =>

  /** Descriptor of a current compiler pass.
    * Compiler can be configured to perform one pass after another.
    * Each pass has name, configuration parameters, finalizaton logic etc.
    */
  abstract class Pass {
    /** Unique name of the pass. */
    def name: String
    /** Configuration parameters of this pass. */
    def config: PassConfig = Pass.defaultPassConfig
    /** Called when this pass is being finalized. */
    def doFinalization(): Unit = {}
    /**
      * Pass specific optional decision.
      * @param d receiver of the method
      * @param m method to invoke
      * @return Some(decision) if some this Pass defines some logic, None - then core behavior is used
      */
    def isInvokeEnabled(d: Def[_], m: RMethod): Option[Boolean] = None
  }
  object Pass {
    val defaultPassName = "default"
    /** When this IR is used without a compiler this pass is used as current pass. */
    val defaultPass = new DefaultPass(defaultPassName)
    val defaultPassConfig = defaultPass.config
  }

  /** Configuration parameters of the Pass descriptor. */
  case class PassConfig(
    /** Whether the pair type `(A,B)` should be specialized to `{_1: A, _2:B} struct type.`.
      * This is used in structs flattening transformation and can also be used in other way. */
    shouldUnpackTuples: Boolean = false,
    /** Turn on/off the RW rule to extract a value of the field if the value is known in the graph. */
    shouldExtractFields: Boolean = true,
    /** Turn on/off constant propagation RW rules. */
    constantPropagation: Boolean = true,
    /** Used in SlicingPass */
    shouldSlice: Boolean = false)

  /** Default pass to be used when IR is used without special compiler configuration. */
  class DefaultPass(val name: String, override val config: PassConfig = PassConfig()) extends Pass

  //TODO Current design doesn't allow to run through passes in two Compilers in parallel
  var _currentPass: Pass = Pass.defaultPass

  /** IR global current Pass, changes when the compier switches from one pass to the next one.
    * Should remain constant during the whole pass execution. */
  def currentPass = _currentPass

  /** Called to setup IR before the new pass is executed. */
  def beginPass(pass: Pass): Unit = {
    _currentPass = pass
  }

  /** Concrete and default implementation of Transformer using underlying HashMap.
    * HOTSPOT:  don't beatify the code */
  case class MapTransformer(private val subst: util.HashMap[Sym, Sym]) extends Transformer {
    def this(substPairs: (Sym, Sym)*) {
      this({
        val map = new util.HashMap[Sym, Sym](1000)
        val len = substPairs.length
        cfor(0)(_ < len, _ + 1) { i =>
          val kv = substPairs(i)
          map.put(kv._1, kv._2)
        }
        map
      })
    }
    def apply[A](x: Ref[A]): Ref[A] = {
      val y = subst.get(x)
      if (y == null || y == x) return x
      apply(y.asInstanceOf[Ref[A]]) // apply recursively to obtain transitive closure
    }
    def isDefinedAt(x: Ref[_]) = subst.containsKey(x)
    def domain: Seq[Ref[_]] = subst.keySet.toArray(new Array[Sym](0))

    def +[A](key: Sym, value: Sym): Transformer = {
      subst.put(key, value)
      this
    }
    def merge(other: Transformer): Transformer =
      other.domain.foldLeft[Transformer](this) {
        case (t, s: Sym) => t + (s, other(s))
      }

    override def toString = if (subst.isEmpty) "MapTransformer.Empty" else s"MapTransformer($subst)"
  }

  object MapTransformer {
    def empty(initialCapacity: Int = 100) = new MapTransformer(new util.HashMap[Sym, Sym](initialCapacity))
  }

  abstract class Rewriter { self =>
    def apply[T](x: Ref[T]): Ref[T]

    def orElse(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Ref[T]) = {
        val y = self(x)
        (x == y) match { case true => other(x) case _ => y }
      }
    }
    def andThen(other: Rewriter): Rewriter = new Rewriter {
      def apply[T](x: Ref[T]) = {
        val y = self(x)
        val res = other(y)
        res
      }
    }

    def |(other: Rewriter) = orElse(other)
    def ~(other: Rewriter) = andThen(other)
  }

  /** Turns partial function into rewriter (i.e. set of rewriting rules) */
  implicit class PartialRewriter(pf: PartialFunction[Sym, Sym]) extends Rewriter {
    def apply[T](x: Ref[T]): Ref[T] =
      if (pf.isDefinedAt(x))
        pf(x).asInstanceOf[Ref[T]]
      else
        x
  }

  /** Identity rewriter, i.e. doesn't change the graph when applied. */
  val NoRewriting: Rewriter = new Rewriter {
    def apply[T](x: Ref[T]) = x
  }

  /** Base class for mirrors of graph nodes. Provides default implementations which can be
    * overriden if special logic is required.
    * HOTSPOT: don't beautify the code */
  abstract class Mirror {
    def apply[A](t: Transformer, rewriter: Rewriter, node: Ref[A], d: Def[A]): Sym = d.mirror(t)

    protected def mirrorElem(node: Sym): Elem[_] = node.elem

    // every mirrorXXX method should return a pair (t + (v -> v1), v1)
    protected def mirrorVar[A](t: Transformer, v: Ref[A]): Transformer = {
      val newVar = variable(Lazy(mirrorElem(v)))
      t + (v, newVar)
    }

    protected def mirrorDef[A](t: Transformer, rewriter: Rewriter, node: Ref[A], d: Def[A]): Transformer = {
      val res = apply(t, rewriter, node, d)
      t + (node, res)
    }

    protected def getMirroredLambdaSym[A, B](node: Ref[A => B]): Sym = placeholder(Lazy(mirrorElem(node)))

    // require: should be called after oldlam.schedule is mirrored
    private def getMirroredLambdaDef(t: Transformer, oldLam: Lambda[_,_], newRoot: Sym): Lambda[_,_] = {
      val newVar = t(oldLam.x)
      val newLambdaDef = new Lambda(Nullable.None, newVar, newRoot, oldLam.mayInline, oldLam.alphaEquality)
      newLambdaDef
    }

    protected def mirrorLambda[A, B](t: Transformer, rewriter: Rewriter, node: Ref[A => B], lam: Lambda[A, B]): Transformer = {
      var tRes: Transformer = t
      val t1 = mirrorNode(t, rewriter, lam.x)

      // original root
      val originalRoot = lam.y

      // ySym will be assigned after f is executed
      val ySym = placeholder(Lazy(lam.y.elem))
      val newLambdaCandidate = getMirroredLambdaDef(t1, lam, ySym)
      val newLambdaSym = newLambdaCandidate.self

      // new effects may appear during body mirroring
      // thus we need to forget original Reify node and create a new one
//      val oldStack = lambdaStack
      try {
//        lambdaStack = newLambdaCandidate :: lambdaStack
        val newRoot = { // reifyEffects block
          val schedule = lam.scheduleIds
          val t2 = mirrorSymbols(t1, rewriter, schedule)
          tRes = t2
          tRes(originalRoot) // this will be a new root
        }
        ySym.assignDefFrom(newRoot)
      }
      finally {
//        lambdaStack = oldStack
      }

      // we don't use toExp here to avoid rewriting pass for new Lambda
      val resLam = findOrCreateDefinition(newLambdaCandidate, newLambdaSym)

      tRes + (node, resLam)
    }

    protected def mirrorThunk[A](t: Transformer, rewriter: Rewriter, node: Ref[Thunk[A]], thunk: ThunkDef[A]): Transformer = {
      var scheduleIdsPH: ScheduleIds = null
      val newRootPH = placeholder(Lazy(node.elem.eItem))
      val newThunk = new ThunkDef(newRootPH, { assert(scheduleIdsPH != null); scheduleIdsPH })
      val newThunkSym = newThunk.self

      val newScope = thunkStack.beginScope(newThunkSym)
      val schedule = thunk.scheduleIds
      val t1 = mirrorSymbols(t, rewriter, schedule)
      thunkStack.endScope()

      val newRoot = t1(thunk.root)
      newRootPH.assignDefFrom(newRoot)
      scheduleIdsPH =
          if (newRoot.isVar) DBuffer.ofSize(0)
          else if (newScope.isEmptyBody) DBuffer.ofSize(0)
          else newScope.scheduleForResult(newRoot)

      createDefinition(thunkStack.top, newThunkSym, newThunk)
      t1 + (node, newThunkSym)
    }

    protected def isMirrored(t: Transformer, node: Sym): Boolean = t.isDefinedAt(node)

    def mirrorNode(t: Transformer, rewriter: Rewriter, node: Sym): Transformer = {
      if (isMirrored(t, node)) t
      else {
        node.node match {
          case _: Variable[_] =>
            mirrorVar(t, node)
          case lam: Lambda[a, b] =>
            mirrorLambda(t, rewriter, node.asInstanceOf[Ref[a => b]], lam)
          case th: ThunkDef[a] =>
            mirrorThunk(t, rewriter, node.asInstanceOf[Ref[Thunk[a]]], th)
          case d =>
            mirrorDef(t, rewriter, node, d)
        }
      }
    }

    /** HOTSPOT: */
    def mirrorSymbols(t0: Transformer, rewriter: Rewriter, nodes: DBuffer[Int]): Transformer = {
      var t: Transformer = t0
      cfor(0)(_ < nodes.length, _ + 1) { i =>
        val n = nodes(i)
        val s = getSym(n)
        t = mirrorNode(t, rewriter, s)
      }
      t
    }
  }

  /** Default Mirror instance which is used in core IR methods. */
  val DefaultMirror = new Mirror {}

}

