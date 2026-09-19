package sigma.compiler.ir

import org.ergoplatform._
import sigma.Evaluation.stypeToRType
import sigma.SigmaException
import sigma.ast.{Ident, Select, Val}
import sigma.ast.SType.tT
import sigma.ast.TypeCodes.LastConstantCode
import sigma.ast.Value.Typed
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.ast._
import sigma.compiler.ir.core.MutableLazy
import sigma.crypto.EcPointType
import sigma.data.ExactIntegral.{ByteIsExactIntegral, IntIsExactIntegral, LongIsExactIntegral, ShortIsExactIntegral}
import sigma.data.ExactOrdering.{ByteIsExactOrdering, IntIsExactOrdering, LongIsExactOrdering, ShortIsExactOrdering}
import sigma.data.{CSigmaDslBuilder, ExactIntegral, ExactNumeric, ExactOrdering, Lazy, Nullable}
import sigma.data.UnsignedBigIntNumericOps.{UnsignedBigIntIsExactIntegral, UnsignedBigIntIsExactOrdering}
import sigma.exceptions.GraphBuildingException
import sigma.serialization.OpCodes
import sigma.util.Extensions.ByteOps
import sigmastate.interpreter.Interpreter.ScriptEnv

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions


/** Perform translation of typed expression given by [[Value]] to a graph in IRContext.
  * Which be than be translated to [[ErgoTree]] by using [[TreeBuilding]].
  *
  * Common Sub-expression Elimination (CSE) optimization is performed which reduces
  * serialized size of the resulting ErgoTree.
  * CSE however means the original structure of source code may not be preserved in the
  * resulting ErgoTree.
  * */
trait GraphBuilding extends Base with DefRewriting { IR: IRContext =>
  import AvlTree._
  import BigInt._
  import UnsignedBigInt._
  import Box._
  import Coll._
  import Context._
  import GroupElement._
  import Header._
  import PreHeader._
  import SigmaDslBuilder._
  import SigmaProp._
  import WOption._

  /** Builder used to create ErgoTree nodes. */
  val builder = TransformingSigmaBuilder
  import builder._

  val okMeasureOperationTime: Boolean = false

  this.isInlineThunksOnForce = true  // this required for splitting of cost graph
  this.keepOriginalFunc = false  // original lambda of Lambda node contains invocations of evalNode and we don't want that
  this.useAlphaEquality = false

  /** Whether to save calcF and costF graphs in the file given by ScriptNameProp environment variable */
  var saveGraphsInFile: Boolean = false

  /** Check the tuple type is valid.
    * In v5.x this code is taken from CheckTupleType validation rule which is no longer
    * part of consensus.
    */
  def checkTupleType[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_]): Unit = {
    val condition = e match {
      case _: ctx.PairElem[_, _] => true
      case _ => false
    }
    if (!condition) {
      throw new SigmaException(s"Invalid tuple type $e")
    }
  }

  type ROption[T] = Ref[Option[T]]

  private val IsValid = CallPattern(SSigmaPropMethods.IsProvenMethod)

  /** `p.isValid` as a call node carrying its descriptor. */
  private def isValid(p: Ref[sigma.SigmaProp]): Ref[Boolean] =
    asRep[Boolean](mkMethodCall(p, MethodCallee(SSigmaPropMethods.IsProvenMethod), Seq(), Map(), BooleanElement))

  /** `l && r` and `l || r` on sigma propositions: call nodes of the SigmaAnd / SigmaOr operations. */
  private def sigmaAnd(l: Ref[sigma.SigmaProp], r: Ref[sigma.SigmaProp]): Ref[sigma.SigmaProp] =
    asRep[sigma.SigmaProp](mkMethodCall(l, OpCallee(SigmaAnd), Seq(r), Map(), sigmaPropElement))
  private def sigmaOr(l: Ref[sigma.SigmaProp], r: Ref[sigma.SigmaProp]): Ref[sigma.SigmaProp] =
    asRep[sigma.SigmaProp](mkMethodCall(l, OpCallee(SigmaOr), Seq(r), Map(), sigmaPropElement))

  private val SigmaPropOp = CallPattern(GlobalOpCallee(BoolToSigmaProp))
  private val AnyOfOp     = CallPattern(GlobalOpCallee(OR))
  private val AllOfOp     = CallPattern(GlobalOpCallee(AND))
  private val AnyZkOp     = CallPattern(GlobalOpCallee(SigmaOr))
  private val AllZkOp     = CallPattern(GlobalOpCallee(SigmaAnd))

  /** The global builtins as call nodes: `sigmaProp(b)`, `allOf(bools)`, `anyOf(bools)`,
    * `allZK(props)`, `anyZK(props)`. */
  private def sigmaProp(b: Sym): Ref[sigma.SigmaProp] =
    asRep[sigma.SigmaProp](globalOp(BoolToSigmaProp, Seq(asRep[Any](b)), sigmaPropElement))
  private def allOf(bools: Seq[Ref[Boolean]]): Ref[Boolean] =
    asRep[Boolean](globalOp(AND, Seq(asRep[Any](fromItems(bools, BooleanElement))), BooleanElement))
  private def anyOf(bools: Seq[Ref[Boolean]]): Ref[Boolean] =
    asRep[Boolean](globalOp(OR, Seq(asRep[Any](fromItems(bools, BooleanElement))), BooleanElement))
  private def allZK(props: Seq[Ref[sigma.SigmaProp]]): Ref[sigma.SigmaProp] =
    asRep[sigma.SigmaProp](globalOp(SigmaAnd, Seq(asRep[Any](fromItems(props, sigmaPropElement))), sigmaPropElement))
  private def anyZK(props: Seq[Ref[sigma.SigmaProp]]): Ref[sigma.SigmaProp] =
    asRep[sigma.SigmaProp](globalOp(SigmaOr, Seq(asRep[Any](fromItems(props, sigmaPropElement))), sigmaPropElement))

  /** Recognizers of the `anyOf` / `allOf` / `anyZK` / `allZK` builtins applied to a collection
    * literal, yielding the literal's items. */
  object AnyOf {
    def unapply(d: Def[_]): Option[Seq[Sym]] = d match {
      case AnyOfOp(_, Seq(ConcreteColl(_, items))) => Some(items)
      case _ => None
    }
  }

  object AllOf {
    def unapply(d: Def[_]): Option[Seq[Sym]] = d match {
      case AllOfOp(_, Seq(ConcreteColl(_, items))) => Some(items)
      case _ => None
    }
  }

  object AnyZk {
    def unapply(d: Def[_]): Option[Seq[Sym]] = d match {
      case AnyZkOp(_, Seq(ConcreteColl(_, items))) => Some(items)
      case _ => None
    }
  }

  object AllZk {
    def unapply(d: Def[_]): Option[Seq[Sym]] = d match {
      case AllZkOp(_, Seq(ConcreteColl(_, items))) => Some(items)
      case _ => None
    }
  }

  /** Pattern match extractor which recognizes `isValid` nodes among items.
    *
    * @param items list of graph nodes which are expected to be of Ref[Boolean] type
    * @return `None` if there is no `isValid` node among items
    *         `Some((bs, ss)) if there are `isValid` nodes where `ss` are `SigmaProp`
    *         arguments of those nodes and `bs` contains all the other nodes.
    */
  object HasSigmas {
    def unapply(items: Seq[Sym]): Option[(Seq[Ref[Boolean]], Seq[Ref[sigma.SigmaProp]])] = {
      val bs = ArrayBuffer.empty[Ref[Boolean]]
      val ss = ArrayBuffer.empty[Ref[sigma.SigmaProp]]
      for (i <- items) {
        i match {
          case IsValid(s, _) => ss += asRep[sigma.SigmaProp](s)
          case b => bs += asRep[Boolean](b)
        }
      }
      assert(items.length == bs.length + ss.length)
      if (ss.isEmpty) None
      else Some((bs.toSeq, ss.toSeq))
    }
  }

  /** For performance reasons the patterns are organized in special (non-declarative) way.
    * Unfortunately, this is less readable, but gives significant performance boost
    * Look at comments to understand the logic of the rules.
    *
    * HOTSPOT: executed for each node of the graph, don't beautify.
    */
  override def rewriteDef[T](d: Def[T]): Ref[_] = {
    // First we match on node type, and then depending on it, we have further branching logic.
    // On each branching level each node type should be matched exactly once,
    // for the rewriting to be sound.
    d match {
      // Rule: ThunkDef(x, Nil).force => x
      case ThunkForce(Def(ThunkDef(root, sch))) if sch.isEmpty => root

      // Rule: l.isValid op Thunk {... root} => (l op TrivialSigma(root)).isValid
      case ApplyBinOpLazy(op, IsValid(l, _), Def(ThunkDef(root, _))) if root.elem == BooleanElement =>
        // don't need new Thunk because sigma logical ops always strict
        val r = sigmaProp(root)
        val lp = asRep[sigma.SigmaProp](l)
        val res = if (op == And)
          sigmaAnd(lp, r)
        else
          sigmaOr(lp, r)
        isValid(res)

      // Rule: l op Thunk {... prop.isValid} => (TrivialSigma(l) op prop).isValid
      case ApplyBinOpLazy(op, l, Def(ThunkDef(root @ IsValid(prop, _), sch))) if l.elem == BooleanElement =>
        val l1 = sigmaProp(l)
        val p = asRep[sigma.SigmaProp](prop)
        // don't need new Thunk because sigma logical ops always strict
        val res = if (op == And)
          sigmaAnd(l1, p)
        else
          sigmaOr(l1, p)
        isValid(res)

      case SigmaPropOp(_, Seq(IsValid(p, _))) => p
      case IsValid(SigmaPropOp(_, Seq(bool)), _) => bool

      case AllOf(HasSigmas(bools, sigmas)) =>
        val zkAll = allZK(sigmas)
        if (bools.isEmpty) isValid(zkAll)
        else isValid(sigmaAnd(sigmaProp(allOf(bools)), zkAll))

      case AnyOf(HasSigmas(bools, sigmas)) =>
        val zkAny = anyZK(sigmas)
        if (bools.isEmpty) isValid(zkAny)
        else isValid(sigmaOr(sigmaProp(anyOf(bools)), zkAny))

      case AllOf(items) if items.length == 1 => items(0)
      case AnyOf(items) if items.length == 1 => items(0)
      case AllZk(items) if items.length == 1 => items(0)
      case AnyZk(items) if items.length == 1 => items(0)

      case _ =>
        if (currentPass.config.constantPropagation) {
          // additional constant propagation rules (see other similar cases)
          d match {
            case AnyOf(items) if items.forall(_.isConst) =>
              val bs = items.map { case Def(Const(b: Boolean)) => b }
              toRep(bs.exists(_ == true))
            case AllOf(items) if items.forall(_.isConst) =>
              val bs = items.map { case Def(Const(b: Boolean)) => b }
              toRep(bs.forall(_ == true))
            case _ =>
              super.rewriteDef(d)
          }
        }
        else
          super.rewriteDef(d)
    }
  }

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method. */
  private val _sigmaDslBuilder: LazyRep[sigma.SigmaDslBuilder] = MutableLazy(variable[sigma.SigmaDslBuilder])
  @inline def sigmaDslBuilder: Ref[sigma.SigmaDslBuilder] = _sigmaDslBuilder.value

  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise bevavior after resetContext is undefined and may throw.
    Array(_sigmaDslBuilder)
      .foreach(_.reset())
  }

  /** If `f` returns `isValid` graph node, then it is filtered out. */
  def removeIsProven[T,R](f: Ref[T] => Ref[R]): Ref[T] => Ref[R] = { x: Ref[T] =>
    val y = f(x);
    val res = y match {
      case IsValid(p, _) => p
      case v => v
    }
    asRep[R](res)
  }

  /** Translates SType descriptor to Elem descriptor used in graph IR.
    * Should be inverse to `elemToSType`. */
  def stypeToElem[T <: SType](t: T): Elem[T#WrappedType] = (t match {
    case SBoolean => BooleanElement
    case SByte => ByteElement
    case SShort => ShortElement
    case SInt => IntElement
    case SLong => LongElement
    case SString => StringElement
    case SAny => AnyElement
    case SUnit => UnitElement
    case SBigInt => bigIntElement
    case SUnsignedBigInt => unsignedBigIntElement
    case SBox => boxElement
    case SContext => contextElement
    case SGlobal => sigmaDslBuilderElement
    case SHeader => headerElement
    case SPreHeader => preHeaderElement
    case SGroupElement => groupElementElement
    case SAvlTree => avlTreeElement
    case SSigmaProp => sigmaPropElement
    case STuple(Seq(a, b)) => pairElement(stypeToElem(a), stypeToElem(b))
    case c: SCollectionType[a] => collElement(stypeToElem(c.elemType))
    case o: SOption[a] => wOptionElement(stypeToElem(o.elemType))
    case SFunc(Seq(tpeArg), tpeRange, Nil) => funcElement(stypeToElem(tpeArg), stypeToElem(tpeRange))
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asInstanceOf[Elem[T#WrappedType]]

  /** Translates Elem descriptor to SType descriptor used in ErgoTree.
    * Should be inverse to `stypeToElem`. */
  def elemToSType[T](e: Elem[T]): SType = e match {
    case BooleanElement => SBoolean
    case ByteElement => SByte
    case ShortElement => SShort
    case IntElement => SInt
    case LongElement => SLong
    case StringElement => SString
    case AnyElement => SAny
    case UnitElement => SUnit
    case _: BigIntElem => SBigInt
    case _: UnsignedBigIntElem => SUnsignedBigInt
    case _: GroupElementElem => SGroupElement
    case _: AvlTreeElem => SAvlTree
    case oe: WOptionElem[_] => SOption(elemToSType(oe.eItem))
    case _: BoxElem => SBox
    case _: ContextElem => SContext
    case _: SigmaDslBuilderElem => SGlobal
    case _: HeaderElem => SHeader
    case _: PreHeaderElem => SPreHeader
    case _: SigmaPropElem => SSigmaProp
    case ce: CollElem[_] => SCollection(elemToSType(ce.eItem))
    case fe: FuncElem[_, _] => SFunc(elemToSType(fe.eDom), elemToSType(fe.eRange))
    case pe: PairElem[_, _] => STuple(elemToSType(pe.eFst), elemToSType(pe.eSnd))
    case _ => error(s"Don't know how to convert Elem $e to SType")
  }
  import sigma.data.NumericOps._
  private lazy val elemToExactNumericMap = Map[Elem[_], ExactNumeric[_]](
    (ByteElement, ByteIsExactIntegral),
    (ShortElement, ShortIsExactIntegral),
    (IntElement, IntIsExactIntegral),
    (LongElement, LongIsExactIntegral),
    (bigIntElement, BigIntIsExactIntegral),
    (unsignedBigIntElement, UnsignedBigIntIsExactIntegral)
  )
  private lazy val elemToExactIntegralMap = Map[Elem[_], ExactIntegral[_]](
    (ByteElement,   ByteIsExactIntegral),
    (ShortElement,  ShortIsExactIntegral),
    (IntElement,    IntIsExactIntegral),
    (LongElement,   LongIsExactIntegral),
    (bigIntElement, BigIntIsExactIntegral),
    (unsignedBigIntElement, UnsignedBigIntIsExactIntegral)
  )
  protected lazy val elemToExactOrderingMap = Map[Elem[_], ExactOrdering[_]](
    (ByteElement,   ByteIsExactOrdering),
    (ShortElement,  ShortIsExactOrdering),
    (IntElement,    IntIsExactOrdering),
    (LongElement,   LongIsExactOrdering),
    (bigIntElement, BigIntIsExactOrdering),
    (unsignedBigIntElement, UnsignedBigIntIsExactOrdering)
  )

  /** @return [[ExactNumeric]] instance for the given type */
  def elemToExactNumeric [T](e: Elem[T]): ExactNumeric[T]  = elemToExactNumericMap(e).asInstanceOf[ExactNumeric[T]]

  /** @return [[ExactIntegral]] instance for the given type */
  def elemToExactIntegral[T](e: Elem[T]): ExactIntegral[T] = elemToExactIntegralMap(e).asInstanceOf[ExactIntegral[T]]

  /** @return [[ExactOrdering]] instance for the given type */
  def elemToExactOrdering[T](e: Elem[T]): ExactOrdering[T] = elemToExactOrderingMap(e).asInstanceOf[ExactOrdering[T]]

  /** @return binary operation for the given opCode and type */
  def opcodeToEndoBinOp[T](opCode: Byte, eT: Elem[T]): EndoBinOp[T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToExactNumeric(eT))(eT)
    case OpCodes.MinusCode => NumericMinus(elemToExactNumeric(eT))(eT)
    case OpCodes.MultiplyCode => NumericTimes(elemToExactNumeric(eT))(eT)
    case OpCodes.DivisionCode => IntegralDivide(elemToExactIntegral(eT))(eT)
    case OpCodes.ModuloCode => IntegralMod(elemToExactIntegral(eT))(eT)
    case OpCodes.MinCode => OrderingMin(elemToExactOrdering(eT))(eT)
    case OpCodes.MaxCode => OrderingMax(elemToExactOrdering(eT))(eT)
    case _ => error(s"Cannot find EndoBinOp for opcode $opCode")
  }

  /** @return binary operation for the given opCode and type */
  def opcodeToBinOp[A](opCode: Byte, eA: Elem[A]): BinOp[A,_] = opCode match {
    case OpCodes.EqCode  => Equals[A]()(eA)
    case OpCodes.NeqCode => NotEquals[A]()(eA)
    case OpCodes.GtCode  => OrderingGT[A](elemToExactOrdering(eA))
    case OpCodes.LtCode  => OrderingLT[A](elemToExactOrdering(eA))
    case OpCodes.GeCode  => OrderingGTEQ[A](elemToExactOrdering(eA))
    case OpCodes.LeCode  => OrderingLTEQ[A](elemToExactOrdering(eA))
    case _ => error(s"Cannot find BinOp for opcode newOpCode(${opCode.toUByte - LastConstantCode}) and type $eA")
  }

  protected implicit def groupElementToECPoint(g: sigma.GroupElement): EcPointType = CSigmaDslBuilder.toECPoint(g).asInstanceOf[EcPointType]

  def error(msg: String) = throw new GraphBuildingException(msg, None)
  def error(msg: String, srcCtx: Option[SourceContext]) = throw new GraphBuildingException(msg, srcCtx)

  /** Graph node to represent a placeholder of a constant in ErgoTree.
    * @param id Zero based index in ErgoTree.constants array.
    * @param resultType type descriptor of the constant value.
    */
  case class ConstantPlaceholder[T](id: Int, resultType: Elem[T]) extends Def[T]

  /** Smart constructor method for [[ConstantPlaceholder]], should be used instead of the
    * class constructor.
    */
  @inline def constantPlaceholder[T](id: Int, eT: Elem[T]): Ref[T] = ConstantPlaceholder(id, eT)


  /** Translates the given typed expression to IR graph representing a function from
    * Context to some type T.
    * @param env contains values for each named constant used
    */
  def buildGraph[T](env: ScriptEnv, typed: SValue): Ref[sigma.Context => T] = {
    val envVals = env.map { case (name, v) => (name: Any, builder.liftAny(v).get) }
    fun(removeIsProven({ ctxC: Ref[sigma.Context] =>
      val env = envVals.map { case (k, v) => k -> buildNode(ctxC, Map.empty, v) }.toMap
      val res = asRep[T](buildNode(ctxC, env, typed))
      res
    }))
  }

  /** Type of the mapping between variable names (see Ident) or definition ids (see
    * ValDef) and graph nodes. Thus, the key is either String or Int.
    * Used in `buildNode` method.
    */
  protected type CompilingEnv = Map[Any, Ref[_]]

  /** Builds a plain call node for an AST `MethodCall`. The descriptor is the AST node's own and
    * the result type is the AST node's own, so no method resolution happens here.
    */
  protected def buildMethodCall(mc: sigma.ast.MethodCall, objV: Ref[Any], argsV: Seq[Ref[Any]]): Ref[Any] =
    asRep[Any](mkMethodCall(objV, MethodCallee(mc.method), argsV, mc.typeSubst, stypeToElem(mc.tpe)))

  /** Builds a call node for `method` on `objV` that lowers the AST `node`: the result type is
    * the node's own type.
    */
  protected def methodCallFor(node: SValue, method: SMethod, objV: Ref[Any], argsV: Seq[Ref[Any]],
                              typeSubst: Map[STypeVar, SType] = Map.empty): Ref[Any] =
    asRep[Any](mkMethodCall(objV, MethodCallee(method), argsV, typeSubst, stypeToElem(node.tpe)))

  /** A call node of the global builtin `op` (its receiver is the global object); `resultElem` is
    * the builtin's result type. */
  protected def globalOp(op: ValueCompanion, argsV: Seq[Ref[Any]], resultElem: Elem[_]): Ref[Any] =
    asRep[Any](mkMethodCall(asRep[Any](sigmaDslBuilder), GlobalOpCallee(op), argsV, Map(), resultElem))

  /** Same as [[globalOp]] with the result type of the AST `node` it lowers. */
  protected def globalOpFor(node: SValue, op: ValueCompanion, argsV: Seq[Ref[Any]]): Ref[Any] =
    globalOp(op, argsV, stypeToElem(node.tpe))

  /** `Coll(items)` as a call node of the ConcreteCollection builtin. */
  protected def fromItems[A](items: Seq[Ref[A]], eA: Elem[A]): Ref[sigma.Coll[A]] =
    asRep[sigma.Coll[A]](mkMethodCall(asRep[Any](sigmaDslBuilder), GlobalOpCallee(ConcreteCollection), items, Map(), collElement(eA)))

  /** `xs.size` as a call node carrying its descriptor. */
  protected def collLength(xs: Ref[Any]): Ref[Int] =
    asRep[Int](mkMethodCall(xs, MethodCallee(SCollectionMethods.SizeMethod), Seq(), Map(), IntElement))

  /** `xs.map(f)` as a call node; the result element type is the lambda's range. */
  protected def collMap(xs: Ref[Any], f: Ref[Any => Any]): Ref[Any] = {
    val eRange = f.elem.asInstanceOf[FuncElem[Any, Any]].eRange
    asRep[Any](mkMethodCall(xs, MethodCallee(SCollectionMethods.MapMethod), Seq(f), Map(), collElement(eRange)))
  }

  /** Builds IR graph for the given ErgoTree expression `node`.
    *
    * @param ctx  reference to a graph node that represents Context value passed to script interpreter
    * @param env  compilation environment which resolves variables to graph nodes
    * @param node ErgoTree expression to be translated to graph
    * @return reference to the graph node which represents `node` expression as part of in
    *         the IR graph data structure
    */
  protected def buildNode[T <: SType](ctx: Ref[sigma.Context], env: CompilingEnv, node: Value[T]): Ref[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): Ref[T#WrappedType] = buildNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[Ref[Any]] = Nullable(asRep[Any](buildNode(ctx, env, v))) }
    class InColl[T: Elem] {
      def unapply(v: SValue): Nullable[Ref[sigma.Coll[T]]] = {
        val res = asRep[sigma.Coll[T]](buildNode(ctx, env, v))
        Nullable(res)
      }
    }
    val InCollByte = new InColl[Byte]; val InCollAny = new InColl[Any]()(AnyElement); val InCollInt = new InColl[Int]

    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[Ref[Any]]] = {
      val res = items.map { x: SValue =>
        val r = eval(x)
        asRep[Any](r)
      }
      Nullable(res)
    }}
    def throwError(clue: String = "") =
      error((if (clue.nonEmpty) clue + ": " else "") + s"Don't know how to buildNode($node)", node.sourceContext.toOption)

    val res: Ref[Any] = node match {
      case Constant(v, tpe) => v match {
        case p: sigma.SigmaProp =>
          assert(tpe == SSigmaProp)
          DslConst[sigma.SigmaProp](p)
        case bi: sigma.BigInt =>
          assert(tpe == SBigInt)
          DslConst[sigma.BigInt](bi)
        case ubi: sigma.UnsignedBigInt =>
          assert(tpe == SUnsignedBigInt)
          DslConst[sigma.UnsignedBigInt](ubi)
        case p: sigma.GroupElement =>
          assert(tpe == SGroupElement)
          DslConst[sigma.GroupElement](p)
        case coll: sigma.Coll[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              DslConst[sigma.Coll[wa]](coll.asInstanceOf[sigma.Coll[wa]])(collElement(eWA))
          }
        case box: sigma.Box =>
          DslConst[sigma.Box](box)
        case tree: sigma.AvlTree =>
          DslConst[sigma.AvlTree](tree)
        case s: String =>
          val resV = toRep(s)(stypeToElem(tpe).asInstanceOf[Elem[String]])
          resV
        case _ =>
          val e = stypeToElem(tpe)
          val resV = toRep(v)(e)
          resV
      }
      case sigma.ast.ConstantPlaceholder(id, tpe) =>
        constantPlaceholder(id, stypeToElem(tpe))
      case sigma.ast.Context => ctx
      case Global => sigmaDslBuilder
      case Height => methodCallFor(node, SContextMethods.heightMethod, asRep[Any](ctx), Seq())
      case Inputs => methodCallFor(node, SContextMethods.inputsMethod, asRep[Any](ctx), Seq())
      case Outputs => methodCallFor(node, SContextMethods.outputsMethod, asRep[Any](ctx), Seq())
      case Self => methodCallFor(node, SContextMethods.selfMethod, asRep[Any](ctx), Seq())
      case LastBlockUtxoRootHash => methodCallFor(node, SContextMethods.lastBlockUtxoRootHashMethod, asRep[Any](ctx), Seq())
      case MinerPubkey => methodCallFor(node, SContextMethods.minerPubKeyMethod, asRep[Any](ctx), Seq())

      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case sigma.ast.Upcast(Constant(value, _), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      case sigma.ast.Downcast(Constant(value, _), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.downcast(value.asInstanceOf[AnyVal]), toTpe))

      // Rule: col.size --> SizeOf(col)
      case Select(obj, "size", _) =>
        if (obj.tpe.isCollectionLike)
          eval(mkSizeOf(obj.asValue[SCollection[SType]]))
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property", obj.sourceContext.toOption)

      // Rule: proof.isProven --> IsValid(proof)
      case Select(p, SSigmaPropMethods.IsProven, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropIsProven(p.asSigmaProp))

      // Rule: prop.propBytes --> SigmaProofBytes(prop)
      case Select(p, SSigmaPropMethods.PropBytes, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropBytes(p.asSigmaProp))

      // box.R$i[valType] =>
      case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel", sel.sourceContext.toOption))
        eval(mkExtractRegisterAs(box.asBox, reg, SOption(valType)).asValue[SOption[valType.type]])

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (obj.asValue[SBox.type], field) match {
          case (box, SBoxMethods.Value) => eval(mkExtractAmount(box))
          case (box, SBoxMethods.PropositionBytes) => eval(mkExtractScriptBytes(box))
          case (box, SBoxMethods.Id) => eval(mkExtractId(box))
          case (box, SBoxMethods.Bytes) => eval(mkExtractBytes(box))
          case (box, SBoxMethods.BytesWithoutRef) => eval(mkExtractBytesWithNoRef(box))
          case (box, SBoxMethods.CreationInfo) => eval(mkExtractCreationInfo(box))
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found", sel.sourceContext.toOption)
        }

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        eval(mkSelectField(tuple.asTuple, index))

      case Select(obj, method, Some(tRes: SNumericType))
            if obj.tpe.isNumType && SNumericTypeMethods.isCastMethod(method) =>
        val numValue = obj.asNumValue
        if (numValue.tpe == tRes)
          eval(numValue)
        else if ((numValue.tpe max tRes) == numValue.tpe)
          eval(mkDowncast(numValue, tRes))
        else
          eval(mkUpcast(numValue, tRes))

      case sigma.ast.Apply(col, Seq(index)) if col.tpe.isCollection =>
        eval(mkByIndex(col.asCollection[SType], index.asValue[SInt.type], None))

      case GetVar(id, optTpe) =>
        val idV: Ref[Byte] = id
        methodCallFor(node, SContextMethods.getVarV5Method, asRep[Any](ctx), Seq(asRep[Any](idV)), Map(tT -> optTpe.elemType))

      case d: DeserializeContext[T] =>
        val e = stypeToElem(d.tpe)
        DeserializeContextDef(d, e)

      case d: DeserializeRegister[T] =>
        val e = stypeToElem(d.tpe)
        DeserializeRegisterDef[T](d, e)

      case ValUse(valId, _) =>
        env.getOrElse(valId, !!!(s"ValUse $valId not found in environment $env"))

      case Block(binds, res) =>
        var curEnv = env
        for (v @ Val(n, _, b) <- binds) {
          if (curEnv.contains(n))
            error(s"Variable $n already defined ($n = ${curEnv(n)}", v.sourceContext.toOption)
          val bV = buildNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bV)
        }
        val resV = buildNode(ctx, curEnv, res)
        resV

      case BlockValue(binds, res) =>
        var curEnv = env
        for (v @ ValDef(id, _, b) <- binds) {
          if (curEnv.contains(id))
            error(s"Variable $id already defined ($id = ${curEnv(id)}", v.sourceContext.toOption)
          val bV = buildNode(ctx, curEnv, b)
          curEnv = curEnv + (id -> bV)
        }
        val resV = buildNode(ctx, curEnv, res)
        resV

      case CreateProveDlog(In(v)) =>
        globalOpFor(node, CreateProveDlog, Seq(v))

      case CreateProveDHTuple(In(gv), In(hv), In(uv), In(vv)) =>
        globalOpFor(node, CreateProveDHTuple, Seq(gv, hv, uv, vv))

      case Exponentiate(In(l), In(r)) =>
        methodCallFor(node, SGroupElementMethods.ExponentiateMethod, l, Seq(r))

      case MultiplyGroup(In(l), In(r)) =>
        methodCallFor(node, SGroupElementMethods.MultiplyMethod, l, Seq(r))

      case GroupGenerator =>
        methodCallFor(node, SGlobalMethods.groupGeneratorMethod, asRep[Any](sigmaDslBuilder), Seq())

      case ByteArrayToBigInt(In(arr)) =>
        globalOpFor(node, ByteArrayToBigInt, Seq(arr))

      case LongToByteArray(In(x)) =>
        globalOpFor(node, LongToByteArray, Seq(x))

      case OptionGet(In(opt)) =>
        methodCallFor(node, SOptionMethods.GetMethod, opt, Seq())

      case OptionIsDefined(In(opt)) =>
        methodCallFor(node, SOptionMethods.IsDefinedMethod, opt, Seq())

      case OptionGetOrElse(In(opt), In(default)) =>
        methodCallFor(node, SOptionMethods.GetOrElseMethod, opt, Seq(asRep[Any](Thunk(default))))

      // tup._1 or tup._2
      case SelectField(In(tup), fieldIndex) =>
        val eTuple = tup.elem.asInstanceOf[Elem[_]]
        checkTupleType(IR)(eTuple)
        eTuple match {
          case pe: PairElem[a,b] =>
            assert(fieldIndex == 1 || fieldIndex == 2, s"Invalid field index $fieldIndex of the pair $tup: $pe")
            val pair = asRep[(a,b)](tup)
            val res = if (fieldIndex == 1) pair._1 else pair._2
            res
        }

      // (x, y)
      case Tuple(InSeq(Seq(x, y))) =>
        Pair(x, y)

      // xs.exists(predicate) or xs.forall(predicate)
      case node: BooleanTransformer[_] =>
        val tpeIn = node.input.tpe.elemType
        val eIn = stypeToElem(tpeIn)
        val xs = asRep[sigma.Coll[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CollElem[Any]].eItem
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val predicate = asRep[Any => SType#WrappedType](eval(node.condition))
        val res = predicate.elem.eRange match {
          case BooleanElement =>
            node match {
              case _: ForAll[_] =>
                methodCallFor(node, SCollectionMethods.ForallMethod, asRep[Any](xs), Seq(asRep[Any](predicate)))
              case _: Exists[_] =>
                methodCallFor(node, SCollectionMethods.ExistsMethod, asRep[Any](xs), Seq(asRep[Any](predicate)))
            }
          case e if e.isInstanceOf[SigmaPropElem] =>
            val children = asRep[sigma.Coll[sigma.SigmaProp]](collMap(asRep[Any](xs), asRep[Any => Any](predicate)))
            node match {
              case _: ForAll[_] =>
                globalOp(SigmaAnd, Seq(asRep[Any](children)), sigmaPropElement)
              case _: Exists[_] =>
                globalOp(SigmaOr, Seq(asRep[Any](children)), sigmaPropElement)
            }
        }
        res

      // input.map(mapper)
      case MapCollection(In(inputV), sfunc) =>
        methodCallFor(node, SCollectionMethods.MapMethod, inputV, Seq(asRep[Any](eval(sfunc))))

      // input.fold(zero, (acc, x) => op)
      case Fold(input, zero, sfunc) =>
        methodCallFor(node, SCollectionMethods.FoldMethod, asRep[Any](eval(input)), Seq(asRep[Any](eval(zero)), asRep[Any](eval(sfunc))))

      case Slice(In(inputV), In(from), In(until)) =>
        methodCallFor(node, SCollectionMethods.SliceMethod, inputV, Seq(from, until))

      case Append(In(col1), In(col2)) =>
        methodCallFor(node, SCollectionMethods.AppendMethod, col1, Seq(col2))

      case Filter(input, p) =>
        methodCallFor(node, SCollectionMethods.FilterMethod, asRep[Any](eval(input)), Seq(asRep[Any](eval(p))))

      case sigma.ast.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fV = asRep[Any => sigma.Coll[Any]](eval(f))
        val xV = asRep[Any](eval(x))
        Apply(fV, xV, mayInline = false)

      case CalcBlake2b256(In(input)) =>
        globalOpFor(node, CalcBlake2b256, Seq(input))

      case CalcSha256(In(input)) =>
        globalOpFor(node, CalcSha256, Seq(input))

      case SizeOf(In(xs)) =>
        xs.elem.asInstanceOf[Any] match {
          case _: CollElem[_] =>
            methodCallFor(node, SCollectionMethods.SizeMethod, xs, Seq())
          case _: PairElem[_,_] =>
            2: Ref[Int]
        }

      case ByIndex(xs, i, defaultOpt) =>
        val xsV = asRep[Any](eval(xs))
        val iV = asRep[Any](eval(i))
        defaultOpt match {
          case Some(defaultValue) =>
            methodCallFor(node, SCollectionMethods.GetOrElseMethod, xsV, Seq(iV, asRep[Any](eval(defaultValue))))
          case None =>
            methodCallFor(node, SCollectionMethods.ApplyMethod, xsV, Seq(iV))
        }

      case SigmaPropIsProven(p) =>
        isValid(asRep[sigma.SigmaProp](eval(p)))

      case SigmaPropBytes(p) =>
        methodCallFor(node, SSigmaPropMethods.PropBytesMethod, asRep[Any](eval(p)), Seq())

      case ExtractId(In(box)) =>
        methodCallFor(node, SBoxMethods.IdMethod, box, Seq())

      case ExtractBytesWithNoRef(In(box)) =>
        methodCallFor(node, SBoxMethods.BytesWithoutRefMethod, box, Seq())

      case ExtractAmount(In(box)) =>
        methodCallFor(node, SBoxMethods.ValueMethod, box, Seq())

      case ExtractScriptBytes(In(box)) =>
        methodCallFor(node, SBoxMethods.PropositionBytesMethod, box, Seq())

      case ExtractBytes(In(box)) =>
        methodCallFor(node, SBoxMethods.BytesMethod, box, Seq())

      case ExtractCreationInfo(In(box)) =>
        methodCallFor(node, SBoxMethods.creationInfoMethod, box, Seq())

      // One getReg descriptor (the v6 one) represents the operation for every tree version: rows and
      // callee identity are version-independent, and a constant register id always takes the row.
      case ExtractRegisterAs(In(box), regId, optTpe) =>
        val i: Ref[Int] = regId.number.toInt
        methodCallFor(node, SBoxMethods.getRegMethodV6, box, Seq(asRep[Any](i)), Map(tT -> optTpe.elemType))

      case BoolToSigmaProp(bool) =>
        globalOpFor(node, BoolToSigmaProp, Seq(asRep[Any](eval(bool))))

      case AtLeast(bound, input) =>
        val inputV = asRep[sigma.Coll[sigma.SigmaProp]](eval(input))
        val len = collLength(asRep[Any](inputV))
        if (len.isConst) {
          val inputCount = valueFromRep(len)
          if (inputCount > AtLeast.MaxChildrenCount)
            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
        }
        val boundV = eval(bound)
        globalOpFor(node, AtLeast, Seq(asRep[Any](boundV), asRep[Any](inputV)))

      // BigInt is not a numeric primitive of the IR: its arithmetic is a call node of the operation
      case op: ArithOp[_] if op.tpe == SBigInt =>
        val xV = eval(op.left)
        val yV = eval(op.right)
        asRep[Any](mkMethodCall(asRep[Any](xV), OpCallee(ArithOp.operations(op.opCode)), Seq(asRep[Any](yV)), Map(), stypeToElem(op.tpe)))

      case op: ArithOp[_] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = eval(op.left)
        val y = eval(op.right)
        ApplyBinOp(binop, x, y)

      case LogicalNot(input) =>
        val inputV = eval(input)
        ApplyUnOp(Not, inputV)

      case OR(input) => input match {
        case ConcreteCollection(items, _) =>
          val values = items.map(eval)
          globalOpFor(node, OR, Seq(asRep[Any](fromItems(values.map(asRep[Boolean](_)), BooleanElement))))
        case _ =>
          val inputV = asRep[sigma.Coll[Boolean]](eval(input))
          globalOpFor(node, OR, Seq(asRep[Any](inputV)))
      }

      case AND(input) => input match {
        case ConcreteCollection(items, _) =>
          val values = items.map(eval)
          globalOpFor(node, AND, Seq(asRep[Any](fromItems(values.map(asRep[Boolean](_)), BooleanElement))))
        case _ =>
          val inputV = asRep[sigma.Coll[Boolean]](eval(input))
          globalOpFor(node, AND, Seq(asRep[Any](inputV)))
      }

      case XorOf(input) => input match {
        case ConcreteCollection(items, _) =>
          val values = items.map(eval)
          globalOpFor(node, XorOf, Seq(asRep[Any](fromItems(values.map(asRep[Boolean](_)), BooleanElement))))
        case _ =>
          val inputV = asRep[sigma.Coll[Boolean]](eval(input))
          globalOpFor(node, XorOf, Seq(asRep[Any](inputV)))
      }

      case BinOr(l, r) =>
        val lV = eval(l)
        val rV = Thunk(eval(r))
        Or.applyLazy(lV, rV)

      case BinAnd(l, r) =>
        val lV = eval(l)
        val rV = Thunk(eval(r))
        And.applyLazy(lV, rV)

      case BinXor(l, r) =>
        val lV = eval(l)
        val rV = eval(r)
        BinaryXorOp.apply(lV, rV)

      case neg: Negation[SNumericType]@unchecked =>
        val et = stypeToElem(neg.input.tpe)
        val op = NumericNegate(elemToExactNumeric(et))(et)
        val x = buildNode(ctx, env, neg.input)
        ApplyUnOp(op, x)

      case SigmaAnd(items) =>
        val itemsV = items.map(item => asRep[sigma.SigmaProp](eval(item)))
        globalOpFor(node, SigmaAnd, Seq(asRep[Any](fromItems(itemsV, sigmaPropElement))))

      case SigmaOr(items) =>
        val itemsV = items.map(item => asRep[sigma.SigmaProp](eval(item)))
        globalOpFor(node, SigmaOr, Seq(asRep[Any](fromItems(itemsV, sigmaPropElement))))
        
      case If(c, t, e) =>
        val cV = eval(c)
        val resV = IF (cV) THEN {
          eval(t)
        } ELSE {
          eval(e)
        }
        resV

      case rel: Relation[t, _] =>
        val tpe = rel.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(rel.opCode, et)
        val x = eval(rel.left)
        val y = eval(rel.right)
        binop.apply(x, asRep[t#WrappedType](y))

      case sigma.ast.Lambda(_, Seq((n, argTpe)), _, Some(body)) =>
        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
        val f = fun(removeIsProven({ x: Ref[Any] =>
          buildNode(ctx, env + (n -> x), body)
        }))(Lazy(eArg))
        f

      case sigma.ast.Lambda(_, Seq((accN, accTpe), (n, tpe)), _, Some(body)) =>
        (stypeToElem(accTpe), stypeToElem(tpe)) match { case (eAcc: Elem[s], eA: Elem[a]) =>
          val eArg = pairElement(eAcc, eA)
          val f = fun { x: Ref[(s, a)] =>
            buildNode(ctx, env + (accN -> x._1) + (n -> x._2), body)
          }(Lazy(eArg))
          f
        }

      case FuncValue(Seq((n, argTpe)), body) =>
        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
        val f = fun { x: Ref[Any] =>
          buildNode(ctx, env + (n -> x), body)
        }(Lazy(eArg))
        f

      case ConcreteCollection(InSeq(vs), elemType) =>
        val eAny = stypeToElem(elemType).asInstanceOf[Elem[Any]]
        fromItems(vs, eAny)

      case sigma.ast.Upcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        upcast(input)(elem)

      case sigma.ast.Downcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        downcast(input)(elem)

      case ByteArrayToLong(In(arr)) =>
        globalOpFor(node, ByteArrayToLong, Seq(arr))

      case Xor(InCollByte(l), InCollByte(r)) =>
        globalOpFor(node, Xor, Seq(asRep[Any](l), asRep[Any](r)))

      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
        globalOpFor(node, SubstConstants, Seq(asRep[Any](bytes), asRep[Any](positions), asRep[Any](newValues)))

      case DecodePoint(InCollByte(bytes)) =>
        globalOpFor(node, DecodePoint, Seq(asRep[Any](bytes)))

      // fallback rule for MethodCall, should be the last case in the list
      case mc @ sigma.ast.MethodCall(obj, method, args, _) =>
        val objV = eval(obj)
        val argsV = args.map(eval)
        val objAny = asRep[Any](objV)
        val argsAny = argsV.map(asRep[Any](_))
        (objV, method.objType) match {
          case (_, SOptionMethods) =>
            // getOrElse takes its default lazily: the argument is wrapped into a thunk
            val args1 =
              if (method.methodId == SOptionMethods.GetOrElseMethod.methodId) Seq(asRep[Any](Thunk(argsV(0))))
              else argsAny
            buildMethodCall(mc, objAny, args1)
          // The explicit `CONTEXT.getVar[T](id)` form arrives as a call of getVarV5Method, which the
          // IR does not support (the `getVar[T](id)` builtin lowers to GetVar)
          case (_, SContextMethods) if method.methodId == SContextMethods.getVarV5Method.methodId =>
            throwError()
          case (_, SCollectionMethods | SContextMethods | SGroupElementMethods | SBoxMethods | SAvlTreeMethods
                  | SPreHeaderMethods | SHeaderMethods | SGlobalMethods) =>
            buildMethodCall(mc, objAny, argsAny)
          // The numeric methods are shared by every numeric type, so within the group a method is
          // identified by its id (the descriptor's identity minus the receiver type).
          case (x: Ref[tNum], _: SNumericTypeMethods) => method.methodId match {
            case SNumericTypeMethods.ToBytesMethod.methodId =>
              val op = NumericToBigEndianBytes(elemToExactNumeric(x.elem))
              ApplyUnOp(op, x)
            case SNumericTypeMethods.ToBitsMethod.methodId =>
              val op = NumericToBits(elemToExactNumeric(x.elem))
              ApplyUnOp(op, x)
            case SNumericTypeMethods.BitwiseInverseMethod.methodId =>
              val op = NumericBitwiseInverse(elemToExactNumeric(x.elem))(x.elem)
              ApplyUnOp(op, x)
            case SNumericTypeMethods.BitwiseOrMethod.methodId =>
              val y = asRep[tNum](argsV(0))
              val op = NumericBitwiseOr(elemToExactNumeric(x.elem))(x.elem)
              ApplyBinOp(op, x, y)
            case SNumericTypeMethods.BitwiseAndMethod.methodId =>
              val y = asRep[tNum](argsV(0))
              val op = NumericBitwiseAnd(elemToExactNumeric(x.elem))(x.elem)
              ApplyBinOp(op, x, y)
            case SNumericTypeMethods.BitwiseXorMethod.methodId =>
              val y = asRep[tNum](argsV(0))
              val op = NumericBitwiseXor(elemToExactNumeric(x.elem))(x.elem)
              ApplyBinOp(op, x, y)
            case SNumericTypeMethods.ShiftLeftMethod.methodId =>
              val y = asRep[Int](argsV(0))
              val op = NumericShiftLeft(elemToExactNumeric(x.elem))(x.elem)
              ApplyBinOpDiffArgs(op, x, y)
            case SNumericTypeMethods.ShiftRightMethod.methodId =>
              val y = asRep[Int](argsV(0))
              val op = NumericShiftRight(elemToExactNumeric(x.elem))(x.elem)
              ApplyBinOpDiffArgs(op, x, y)
            // methods of BigInt and UnsignedBigInt that are plain calls carrying their descriptor
            case _ if method.objType == SBigIntMethods || method.objType == SUnsignedBigIntMethods =>
              buildMethodCall(mc, objAny, argsAny)
            case _ => throwError()
          }
          case _ => throwError(s"Type ${stypeToRType(obj.tpe).name} doesn't have methods")
        }

      case _ =>
        throwError()
    }
    val resC = asRep[T#WrappedType](res)
    resC
  }

}
