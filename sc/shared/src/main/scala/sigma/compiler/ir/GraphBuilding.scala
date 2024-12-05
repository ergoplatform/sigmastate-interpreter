package sigma.compiler.ir

import org.ergoplatform._
import sigma.ast.TypeCodes.LastConstantCode
import sigma.ast.Value.Typed
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.ast._
import sigma.compiler.ir.core.MutableLazy
import sigma.crypto.EcPointType
import sigma.data.ExactIntegral.{ByteIsExactIntegral, IntIsExactIntegral, LongIsExactIntegral, ShortIsExactIntegral}
import sigma.data.ExactOrdering.{ByteIsExactOrdering, IntIsExactOrdering, LongIsExactOrdering, ShortIsExactOrdering}
import sigma.data.{CSigmaDslBuilder, ExactIntegral, ExactNumeric, ExactOrdering, Lazy, Nullable}
import sigma.exceptions.GraphBuildingException
import sigma.serialization.OpCodes
import sigma.util.Extensions.ByteOps
import sigma.{SigmaException, ast}
import sigmastate.interpreter.Interpreter.ScriptEnv

import scala.collection.mutable.ArrayBuffer

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
  import Box._
  import Coll._
  import CollBuilder._
  import Context._
  import GroupElement._
  import Header._
  import Liftables._
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

  type RColl[T] = Ref[Coll[T]]
  type ROption[T] = Ref[WOption[T]]

  private val CBM      = CollBuilderMethods
  private val SigmaM   = SigmaPropMethods
  private val SDBM     = SigmaDslBuilderMethods

  /** Recognizer of [[SigmaDslBuilder.anyOf]] method call in Graph-IR. This method call
    * represents `anyOf` predefined function.
    */
  object AnyOf {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.anyOf(_, xs) =>
        CBM.fromItems.unapply(xs)
      case _ => Nullable.None
    }
  }

  /** Recognizer of [[SigmaDslBuilder.allOf]] method call in Graph-IR. This method call
    * represents `allOf` predefined function.
    */
  object AllOf {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.allOf(_, xs) =>
        CBM.fromItems.unapply(xs)
      case _ => Nullable.None
    }
  }

  /** Recognizer of [[SigmaDslBuilder.anyZK]] method call in Graph-IR. This method call
    * represents `anyZK` predefined function.
    */
  object AnyZk {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.anyZK(_, xs) =>
        CBM.fromItems.unapply(xs).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
      case _ => Nullable.None
    }
  }

  /** Recognizer of [[SigmaDslBuilder.allZK]] method call in Graph-IR. This method call
    * represents `allZK` predefined function.
    */
  object AllZk {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.allZK(_, xs) =>
        CBM.fromItems.unapply(xs).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
      case _ => Nullable.None
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
    def unapply(items: Seq[Sym]): Option[(Seq[Ref[Boolean]], Seq[Ref[SigmaProp]])] = {
      val bs = ArrayBuffer.empty[Ref[Boolean]]
      val ss = ArrayBuffer.empty[Ref[SigmaProp]]
      for (i <- items) {
        i match {
          case SigmaM.isValid(s) => ss += s
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
      case ApplyBinOpLazy(op, SigmaM.isValid(l), Def(ThunkDef(root, _))) if root.elem == BooleanElement =>
        // don't need new Thunk because sigma logical ops always strict
        val r = asRep[SigmaProp](sigmaDslBuilder.sigmaProp(asRep[Boolean](root)))
        val res = if (op == And)
          l && r
        else
          l || r
        res.isValid

      // Rule: l op Thunk {... prop.isValid} => (TrivialSigma(l) op prop).isValid
      case ApplyBinOpLazy(op, l, Def(ThunkDef(root @ SigmaM.isValid(prop), sch))) if l.elem == BooleanElement =>
        val l1 = asRep[SigmaProp](sigmaDslBuilder.sigmaProp(asRep[Boolean](l)))
        // don't need new Thunk because sigma logical ops always strict
        val res = if (op == And)
          l1 && prop
        else
          l1 || prop
        res.isValid

      case SDBM.Colls(_) => colBuilder
      case SDBM.sigmaProp(_, SigmaM.isValid(p)) => p
      case SigmaM.isValid(SDBM.sigmaProp(_, bool)) => bool

      case AllOf(b, HasSigmas(bools, sigmas), _) =>
        val zkAll = sigmaDslBuilder.allZK(b.fromItems(sigmas:_*))
        if (bools.isEmpty)
          zkAll.isValid
        else
          (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.allOf(b.fromItems(bools:_*))) && zkAll).isValid

      case AnyOf(b, HasSigmas(bs, ss), _) =>
        val zkAny = sigmaDslBuilder.anyZK(b.fromItems(ss:_*))
        if (bs.isEmpty)
          zkAny.isValid
        else
          (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.anyOf(b.fromItems(bs:_*))) || zkAny).isValid

      case AllOf(_,items,_) if items.length == 1 => items(0)
      case AnyOf(_,items,_) if items.length == 1 => items(0)
      case AllZk(_,items,_) if items.length == 1 => items(0)
      case AnyZk(_,items,_) if items.length == 1 => items(0)

      case _ =>
        if (currentPass.config.constantPropagation) {
          // additional constant propagation rules (see other similar cases)
          d match {
            case AnyOf(_,items,_) if (items.forall(_.isConst)) =>
              val bs = items.map { case Def(Const(b: Boolean)) => b }
              toRep(bs.exists(_ == true))
            case AllOf(_,items,_) if (items.forall(_.isConst)) =>
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
  private val _sigmaDslBuilder: LazyRep[SigmaDslBuilder] = MutableLazy(variable[SigmaDslBuilder])
  @inline def sigmaDslBuilder: Ref[SigmaDslBuilder] = _sigmaDslBuilder.value

  private val _colBuilder: LazyRep[CollBuilder] = MutableLazy(variable[CollBuilder])
  @inline def colBuilder: Ref[CollBuilder] = _colBuilder.value

  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise bevavior after resetContext is undefined and may throw.
    Array(_sigmaDslBuilder, _colBuilder)
      .foreach(_.reset())
  }

  /** If `f` returns `isValid` graph node, then it is filtered out. */
  def removeIsProven[T,R](f: Ref[T] => Ref[R]): Ref[T] => Ref[R] = { x: Ref[T] =>
    val y = f(x);
    val res = y match {
      case SigmaPropMethods.isValid(p) => p
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
    case SBigInt => bigIntElement
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
    case _: BigIntElem[_] => SBigInt
    case _: GroupElementElem[_] => SGroupElement
    case _: AvlTreeElem[_] => SAvlTree
    case oe: WOptionElem[_, _] => SOption(elemToSType(oe.eItem))
    case _: BoxElem[_] => SBox
    case _: ContextElem[_] => SContext
    case _: SigmaDslBuilderElem[_] => SGlobal
    case _: HeaderElem[_] => SHeader
    case _: PreHeaderElem[_] => SPreHeader
    case _: SigmaPropElem[_] => SSigmaProp
    case ce: CollElem[_, _] => SCollection(elemToSType(ce.eItem))
    case fe: FuncElem[_, _] => SFunc(elemToSType(fe.eDom), elemToSType(fe.eRange))
    case pe: PairElem[_, _] => STuple(elemToSType(pe.eFst), elemToSType(pe.eSnd))
    case _ => error(s"Don't know how to convert Elem $e to SType")
  }

  /** Translates Elem to the corresponding Liftable instance.
    * @param eWT type descriptor
    */
  def liftableFromElem[WT](eWT: Elem[WT]): Liftable[_,WT] = (eWT match {
    case BooleanElement => BooleanIsLiftable
    case ByteElement => ByteIsLiftable
    case ShortElement => ShortIsLiftable
    case IntElement => IntIsLiftable
    case LongElement => LongIsLiftable
    case StringElement => StringIsLiftable
    case UnitElement => UnitIsLiftable
    case _: BigIntElem[_] => LiftableBigInt
    case _: GroupElementElem[_] => LiftableGroupElement
    case ce: CollElem[t,_] =>
      implicit val lt = liftableFromElem[t](ce.eItem)
      liftableColl(lt)
    case pe: PairElem[a,b] =>
      implicit val la = liftableFromElem[a](pe.eFst)
      implicit val lb = liftableFromElem[b](pe.eSnd)
      PairIsLiftable(la, lb)
    case pe: FuncElem[a,b] =>
      implicit val la = liftableFromElem[a](pe.eDom)
      implicit val lb = liftableFromElem[b](pe.eRange)
      FuncIsLiftable(la, lb)
  }).asInstanceOf[Liftable[_,WT]]

  import sigma.data.NumericOps._
  private lazy val elemToExactNumericMap = Map[Elem[_], ExactNumeric[_]](
    (ByteElement, ByteIsExactIntegral),
    (ShortElement, ShortIsExactIntegral),
    (IntElement, IntIsExactIntegral),
    (LongElement, LongIsExactIntegral),
    (bigIntElement, BigIntIsExactIntegral)
  )
  private lazy val elemToExactIntegralMap = Map[Elem[_], ExactIntegral[_]](
    (ByteElement,   ByteIsExactIntegral),
    (ShortElement,  ShortIsExactIntegral),
    (IntElement,    IntIsExactIntegral),
    (LongElement,   LongIsExactIntegral)
  )
  protected lazy val elemToExactOrderingMap = Map[Elem[_], ExactOrdering[_]](
    (ByteElement,   ByteIsExactOrdering),
    (ShortElement,  ShortIsExactOrdering),
    (IntElement,    IntIsExactOrdering),
    (LongElement,   LongIsExactOrdering),
    (bigIntElement, BigIntIsExactOrdering)
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
  def buildGraph[T](env: ScriptEnv, typed: SValue): Ref[Context => T] = {
    val envVals = env.map { case (name, v) => (name: Any, builder.liftAny(v).get) }
    fun(removeIsProven({ ctxC: Ref[Context] =>
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

  /** Builds IR graph for the given ErgoTree expression `node`.
    *
    * @param ctx  reference to a graph node that represents Context value passed to script interpreter
    * @param env  compilation environment which resolves variables to graph nodes
    * @param node ErgoTree expression to be translated to graph
    * @return reference to the graph node which represents `node` expression as part of in
    *         the IR graph data structure
    */
  protected def buildNode[T <: SType](ctx: Ref[Context], env: CompilingEnv, node: Value[T]): Ref[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): Ref[T#WrappedType] = buildNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[Ref[Any]] = Nullable(asRep[Any](buildNode(ctx, env, v))) }
    class InColl[T: Elem] {
      def unapply(v: SValue): Nullable[Ref[Coll[T]]] = {
        val res = asRep[Coll[T]](buildNode(ctx, env, v))
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
    def throwError =
      error(s"Don't know how to buildNode($node)", node.sourceContext.toOption)

    val res: Ref[Any] = node match {
      case Constant(v, tpe) => v match {
        case p: SSigmaProp =>
          assert(tpe == SSigmaProp)
          val resV = liftConst(p)
          resV
        case bi: SBigInt =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          resV
        case p: SGroupElement =>
          assert(tpe == SGroupElement)
          val resV = liftConst(p)
          resV
        case coll: SColl[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              implicit val l = liftableFromElem[wa](eWA).asInstanceOf[Liftable[a, wa]]
              val resVals = liftConst[SColl[a], Coll[wa]](coll)
              resVals
          }
        case box: SBox =>
          val boxV = liftConst(box)
          boxV
        case tree: sigma.AvlTree =>
          val treeV = liftConst(tree)
          treeV
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
      case Height => ctx.HEIGHT
      case Inputs => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case Self => ctx.SELF
      case LastBlockUtxoRootHash => ctx.LastBlockUtxoRootHash
      case MinerPubkey => ctx.minerPubKey

      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case ast.Upcast(Constant(value, _), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      case ast.Downcast(Constant(value, _), toTpe: SNumericType) =>
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
        val e = stypeToElem(optTpe.elemType)
        ctx.getVar(id)(e)

      case d: DeserializeContext[T] =>
        val e = stypeToElem(d.tpe).asInstanceOf[Elem[T]]
        DeserializeContextDef(d, e)

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

      case CreateProveDlog(In(_v)) =>
        val v = asRep[GroupElement](_v)
        sigmaDslBuilder.proveDlog(v)

      case CreateProveDHTuple(In(_gv), In(_hv), In(_uv), In(_vv)) =>
        val gv = asRep[GroupElement](_gv)
        val hv = asRep[GroupElement](_hv)
        val uv = asRep[GroupElement](_uv)
        val vv = asRep[GroupElement](_vv)
        sigmaDslBuilder.proveDHTuple(gv, hv, uv, vv)

      case Exponentiate(In(l), In(r)) =>
        val lV = asRep[GroupElement](l)
        val rV = asRep[BigInt](r)
        lV.exp(rV)

      case MultiplyGroup(In(_l), In(_r)) =>
        val l = asRep[GroupElement](_l)
        val r = asRep[GroupElement](_r)
        l.multiply(r)

      case GroupGenerator =>
        sigmaDslBuilder.groupGenerator

      case ByteArrayToBigInt(In(arr)) =>
        val arrV = asRep[Coll[Byte]](arr)
        sigmaDslBuilder.byteArrayToBigInt(arrV)

      case LongToByteArray(In(x)) =>
        val xV = asRep[Long](x)
        sigmaDslBuilder.longToByteArray(xV)

      // opt.get
      case OptionGet(In(opt: ROption[_]@unchecked)) =>
        opt.get

      // opt.isDefined
      case OptionIsDefined(In(opt: ROption[_]@unchecked)) =>
        opt.isDefined

      // opt.getOrElse(default)
      case OptionGetOrElse(In(opt: ROption[a]@unchecked), In(default)) =>
        opt.getOrElse(asRep[a](default))

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
        val xs = asRep[Coll[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CollElem[Any,_]].eItem
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val predicate = asRep[Any => SType#WrappedType](eval(node.condition))
        val res = predicate.elem.eRange match {
          case BooleanElement =>
            node match {
              case _: ForAll[_] =>
                xs.forall(asRep[Any => Boolean](predicate))
              case _: Exists[_] =>
                xs.exists(asRep[Any => Boolean](predicate))
            }
          case _: SigmaPropElem[_] =>
            val children = xs.map(asRep[Any => SigmaProp](predicate))
            node match {
              case _: ForAll[_] =>
                sigmaDslBuilder.allZK(children)
              case _: Exists[_] =>
                sigmaDslBuilder.anyZK(children)
            }
        }
        res

      // input.map(mapper)
      case MapCollection(InCollAny(inputV), sfunc) =>
        val mapper = asRep[Any => Any](eval(sfunc))
        inputV.map(mapper)

      // input.fold(zero, (acc, x) => op)
      case Fold(input, zero, sfunc) =>
        val eItem = stypeToElem(input.tpe.elemType)
        val eState = stypeToElem(zero.tpe)
        (eState, eItem) match { case (eState: Elem[s], eItem: Elem[a]) =>
          val inputV = asRep[Coll[a]](eval(input))
          implicit val eA: Elem[a] = inputV.elem.asInstanceOf[CollElem[a,_]].eItem
          assert(eItem == eA, s"Types should be equal: but $eItem != $eA")

          val zeroV = asRep[s](eval(zero))
          implicit val eS: Elem[s] = zeroV.elem
          assert(eState == eS, s"Types should be equal: but $eState != $eS")

          val op = asRep[((s,a)) => s](eval(sfunc))
          val res = inputV.foldLeft(zeroV, op)
          res
        }

      case Slice(InCollAny(inputV), In(from), In(until)) =>
        val fromV = asRep[Int](from)
        val untilV = asRep[Int](until)
        inputV.slice(fromV, untilV)

      case Append(InCollAny(col1), InCollAny(col2)) =>
        col1.append(col2)

      case Filter(input, p) =>
        val inputV = asRep[Coll[Any]](eval(input))
        val pV = asRep[Any => Boolean](eval(p))
        inputV.filter(pV)

      case sigma.ast.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fV = asRep[Any => Coll[Any]](eval(f))
        val xV = asRep[Any](eval(x))
        Apply(fV, xV, mayInline = false)

      case CalcBlake2b256(In(input)) =>
        val inputV = asRep[Coll[Byte]](input)
        val res = sigmaDslBuilder.blake2b256(inputV)
        res

      case CalcSha256(In(input)) =>
        val inputV = asRep[Coll[Byte]](input)
        val res = sigmaDslBuilder.sha256(inputV)
        res

      case ast.SizeOf(In(xs)) =>
        xs.elem.asInstanceOf[Any] match {
          case _: CollElem[a,_] =>
            val xsV = asRep[Coll[a]](xs)
            xsV.length
          case _: PairElem[_,_] =>
            2: Ref[Int]
        }

      case ByIndex(xs, i, defaultOpt) =>
        val xsV = asRep[Coll[Any]](eval(xs))
        val iV = asRep[Int](eval(i))
        val res = defaultOpt match {
          case Some(defaultValue) =>
            val defaultV = asRep[Any](eval(defaultValue))
            xsV.getOrElse(iV, defaultV)
          case None =>
            xsV(iV)
        }
        res

      case SigmaPropIsProven(p) =>
        val pV = asRep[SigmaProp](eval(p))
        pV.isValid

      case SigmaPropBytes(p) =>
        val pV = asRep[SigmaProp](eval(p))
        pV.propBytes

      case ExtractId(In(box: Ref[Box]@unchecked)) =>
        box.id

      case ExtractBytesWithNoRef(In(box: Ref[Box]@unchecked)) =>
        box.bytesWithoutRef

      case ExtractAmount(In(box)) =>
        val boxV = asRep[Box](box)
        boxV.value

      case ExtractScriptBytes(In(box: Ref[Box]@unchecked)) =>
        box.propositionBytes

      case ExtractBytes(In(box: Ref[Box]@unchecked)) =>
        box.bytes

      case ExtractCreationInfo(In(box: Ref[Box]@unchecked)) =>
        box.creationInfo

      case ExtractRegisterAs(In(box: Ref[Box]@unchecked), regId, optTpe) =>
        val elem = stypeToElem(optTpe.elemType).asInstanceOf[Elem[Any]]
        val i: Ref[Int] = regId.number.toInt
        box.getReg(i)(elem)

      case BoolToSigmaProp(bool) =>
        sigmaDslBuilder.sigmaProp(eval(bool))

      case AtLeast(bound, input) =>
        val inputV = asRep[Coll[SigmaProp]](eval(input))
        if (inputV.length.isConst) {
          val inputCount = valueFromRep(inputV.length)
          if (inputCount > AtLeast.MaxChildrenCount)
            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
        }
        val boundV = eval(bound)
        sigmaDslBuilder.atLeast(boundV, inputV)

      case op: ArithOp[_] if op.tpe == SBigInt =>
        import OpCodes._
        val xV = asRep[BigInt](eval(op.left))
        val yV = asRep[BigInt](eval(op.right))
        op.opCode match {
          case PlusCode     => xV.add(yV)
          case MinusCode    => xV.subtract(yV)
          case MultiplyCode => xV.multiply(yV)
          case DivisionCode => xV.divide(yV)
          case ModuloCode   => xV.mod(yV)
          case MinCode      => xV.min(yV)
          case MaxCode      => xV.max(yV)
          case code         => error(s"Cannot perform buildNode($op): unknown opCode ${code}", op.sourceContext.toOption)
        }

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
          sigmaDslBuilder.anyOf(colBuilder.fromItems(values: _*))
        case _ =>
          val inputV = asRep[Coll[Boolean]](eval(input))
          sigmaDslBuilder.anyOf(inputV)
      }

      case AND(input) => input match {
        case ConcreteCollection(items, _) =>
          val values = items.map(eval)
          sigmaDslBuilder.allOf(colBuilder.fromItems(values: _*))
        case _ =>
          val inputV = asRep[Coll[Boolean]](eval(input))
          sigmaDslBuilder.allOf(inputV)
      }

      case XorOf(input) => input match {
        case ConcreteCollection(items, _) =>
          val values = items.map(eval)
          sigmaDslBuilder.xorOf(colBuilder.fromItems(values: _*))
        case _ =>
          val inputV = asRep[Coll[Boolean]](eval(input))
          sigmaDslBuilder.xorOf(inputV)
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
        val itemsV = items.map(item => asRep[SigmaProp](eval(item)))
        sigmaDslBuilder.allZK(colBuilder.fromItems(itemsV: _*))

      case SigmaOr(items) =>
        val itemsV = items.map(item => asRep[SigmaProp](eval(item)))
        sigmaDslBuilder.anyZK(colBuilder.fromItems(itemsV: _*))
        
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
        val values = colBuilder.fromItems(vs: _*)(eAny)
        values

      case ast.Upcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        upcast(input)(elem)

      case ast.Downcast(In(input), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        downcast(input)(elem)

      case ByteArrayToLong(In(arr)) =>
        val coll = asRep[Coll[Byte]](arr)
        sigmaDslBuilder.byteArrayToLong(coll)

      case Xor(InCollByte(l), InCollByte(r)) =>
        colBuilder.xor(l, r)

      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
        sigmaDslBuilder.substConstants(bytes, positions, newValues)

      case DecodePoint(InCollByte(bytes)) =>
        sigmaDslBuilder.decodePoint(bytes)

      // fallback rule for MethodCall, should be the last case in the list
      case sigma.ast.MethodCall(obj, method, args, _) =>
        val objV = eval(obj)
        val argsV = args.map(eval)
        (objV, method.objType) match {
          case (xs: RColl[t]@unchecked, SCollectionMethods) => method.name match {
            case SCollectionMethods.IndicesMethod.name =>
              xs.indices
            case SCollectionMethods.PatchMethod.name =>
              val from = asRep[Int](argsV(0))
              val patch = asRep[Coll[t]](argsV(1))
              val replaced = asRep[Int](argsV(2))
              xs.patch(from, patch, replaced)
            case SCollectionMethods.UpdatedMethod.name =>
              val index = asRep[Int](argsV(0))
              val value = asRep[t](argsV(1))
              xs.updated(index, value)
            case SCollectionMethods.AppendMethod.name =>
              val ys = asRep[Coll[t]](argsV(0))
              xs.append(ys)
            case SCollectionMethods.SliceMethod.name =>
              val from = asRep[Int](argsV(0))
              val until = asRep[Int](argsV(1))
              xs.slice(from, until)
            case SCollectionMethods.UpdateManyMethod.name =>
              val indexes = asRep[Coll[Int]](argsV(0))
              val values = asRep[Coll[t]](argsV(1))
              xs.updateMany(indexes, values)
            case SCollectionMethods.IndexOfMethod.name =>
              val elem = asRep[t](argsV(0))
              val from = asRep[Int](argsV(1))
              xs.indexOf(elem, from)
            case SCollectionMethods.ZipMethod.name =>
              val ys = asRep[Coll[Any]](argsV(0))
              xs.zip(ys)
            case SCollectionMethods.FlatMapMethod.name =>
              val f = asRep[Any => Coll[Any]](argsV(0))
              xs.flatMap(f)
            case SCollectionMethods.MapMethod.name =>
              val f = asRep[Any => Any](argsV(0))
              xs.map(f)
            case SCollectionMethods.FilterMethod.name =>
              val p = asRep[Any => Boolean](argsV(0))
              xs.filter(p)
            case SCollectionMethods.ForallMethod.name =>
              val p = asRep[Any => Boolean](argsV(0))
              xs.forall(p)
            case SCollectionMethods.ExistsMethod.name =>
              val p = asRep[Any => Boolean](argsV(0))
              xs.exists(p)
            case SCollectionMethods.FoldMethod.name =>
              val zero = asRep[Any](argsV(0))
              val op = asRep[((Any, Any)) => Any](argsV(1))
              xs.foldLeft(zero, op)
            case SCollectionMethods.GetOrElseMethod.name =>
              val i = asRep[Int](argsV(0))
              val d = asRep[t](argsV(1))
              xs.getOrElse(i, d)
            case _ => throwError
          }
          case (opt: ROption[t]@unchecked, SOptionMethods) => method.name match {
            case SOptionMethods.GetMethod.name =>
              opt.get
            case SOptionMethods.GetOrElseMethod.name =>
              val defaultTh = asRep[t](argsV(0))
              opt.getOrElse(Thunk(defaultTh))
            case SOptionMethods.IsDefinedMethod.name =>
              opt.isDefined
            case SOptionMethods.MapMethod.name =>
              opt.map(asRep[t => Any](argsV(0)))
            case SOptionMethods.FilterMethod.name =>
              opt.filter(asRep[t => Boolean](argsV(0)))
            case _ => throwError
          }
          case (ge: Ref[GroupElement]@unchecked, SGroupElementMethods) => method.name match {
            case SGroupElementMethods.GetEncodedMethod.name =>
              ge.getEncoded
            case SGroupElementMethods.NegateMethod.name =>
              ge.negate
            case SGroupElementMethods.MultiplyMethod.name =>
              val g2 = asRep[GroupElement](argsV(0))
              ge.multiply(g2)
            case SGroupElementMethods.ExponentiateMethod.name =>
              val k = asRep[BigInt](argsV(0))
              ge.exp(k)
            case _ => throwError
          }
          case (box: Ref[Box]@unchecked, SBoxMethods) => method.name match {
            case SBoxMethods.tokensMethod.name =>
              box.tokens
            case _ => throwError
          }
          case (ctx: Ref[Context]@unchecked, SContextMethods) => method.name match {
            case SContextMethods.dataInputsMethod.name =>
              ctx.dataInputs
            case SContextMethods.headersMethod.name =>
              ctx.headers
            case SContextMethods.preHeaderMethod.name =>
              ctx.preHeader
            case SContextMethods.inputsMethod.name =>
              ctx.INPUTS
            case SContextMethods.outputsMethod.name =>
              ctx.OUTPUTS
            case SContextMethods.heightMethod.name =>
              ctx.HEIGHT
            case SContextMethods.selfMethod.name =>
              ctx.SELF
            case SContextMethods.selfBoxIndexMethod.name =>
              ctx.selfBoxIndex
            case SContextMethods.lastBlockUtxoRootHashMethod.name =>
              ctx.LastBlockUtxoRootHash
            case SContextMethods.minerPubKeyMethod.name =>
              ctx.minerPubKey
            case _ => throwError
          }
          case (tree: Ref[AvlTree]@unchecked, SAvlTreeMethods) => method.name match {
            case SAvlTreeMethods.digestMethod.name =>
              tree.digest
            case SAvlTreeMethods.keyLengthMethod.name =>
              tree.keyLength
            case SAvlTreeMethods.valueLengthOptMethod.name =>
              tree.valueLengthOpt
            case SAvlTreeMethods.enabledOperationsMethod.name =>
              tree.enabledOperations
            case SAvlTreeMethods.isInsertAllowedMethod.name =>
              tree.isInsertAllowed
            case SAvlTreeMethods.isRemoveAllowedMethod.name =>
              tree.isRemoveAllowed
            case SAvlTreeMethods.isUpdateAllowedMethod.name =>
              tree.isUpdateAllowed
            case SAvlTreeMethods.updateDigestMethod.name =>
              val digest = asRep[Coll[Byte]](argsV(0))
              tree.updateDigest(digest)
            case SAvlTreeMethods.updateOperationsMethod.name =>
              val operations = asRep[Byte](argsV(0))
              tree.updateOperations(operations)
            case SAvlTreeMethods.getMethod.name =>
              val key = asRep[Coll[Byte]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.get(key, proof)
            case SAvlTreeMethods.getManyMethod.name =>
              val keys = asRep[Coll[Coll[Byte]]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.getMany(keys, proof)
            case SAvlTreeMethods.containsMethod.name =>
              val key = asRep[Coll[Byte]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.contains(key, proof)
            case SAvlTreeMethods.insertMethod.name =>
              val operations = asRep[Coll[(Coll[Byte], Coll[Byte])]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.insert(operations, proof)
            case SAvlTreeMethods.removeMethod.name =>
              val operations = asRep[Coll[Coll[Byte]]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.remove(operations, proof)
            case SAvlTreeMethods.updateMethod.name =>
              val operations = asRep[Coll[(Coll[Byte], Coll[Byte])]](argsV(0))
              val proof = asRep[Coll[Byte]](argsV(1))
              tree.update(operations, proof)
            case _ => throwError
          }
          case (ph: Ref[PreHeader]@unchecked, SPreHeaderMethods) => method.name match {
            case SPreHeaderMethods.versionMethod.name =>
              ph.version
            case SPreHeaderMethods.parentIdMethod.name =>
              ph.parentId
            case SPreHeaderMethods.timestampMethod.name =>
              ph.timestamp
            case SPreHeaderMethods.nBitsMethod.name =>
              ph.nBits
            case SPreHeaderMethods.heightMethod.name =>
              ph.height
            case SPreHeaderMethods.minerPkMethod.name =>
              ph.minerPk
            case SPreHeaderMethods.votesMethod.name =>
              ph.votes
            case _ => throwError
          }
          case (h: Ref[Header]@unchecked, SHeaderMethods) => method.name match {
            case SHeaderMethods.idMethod.name =>
              h.id
            case SHeaderMethods.versionMethod.name =>
              h.version
            case SHeaderMethods.parentIdMethod.name =>
              h.parentId
            case SHeaderMethods.ADProofsRootMethod.name =>
              h.ADProofsRoot
            case SHeaderMethods.stateRootMethod.name =>
              h.stateRoot
            case SHeaderMethods.transactionsRootMethod.name =>
              h.transactionsRoot
            case SHeaderMethods.timestampMethod.name =>
              h.timestamp
            case SHeaderMethods.nBitsMethod.name =>
              h.nBits
            case SHeaderMethods.heightMethod.name =>
              h.height
            case SHeaderMethods.extensionRootMethod.name =>
              h.extensionRoot
            case SHeaderMethods.minerPkMethod.name =>
              h.minerPk
            case SHeaderMethods.powOnetimePkMethod.name =>
              h.powOnetimePk
            case SHeaderMethods.powNonceMethod.name =>
              h.powNonce
            case SHeaderMethods.powDistanceMethod.name =>
              h.powDistance
            case SHeaderMethods.votesMethod.name =>
              h.votes
            case _ => throwError
          }
          case (g: Ref[SigmaDslBuilder]@unchecked, SGlobalMethods) => method.name match {
            case SGlobalMethods.groupGeneratorMethod.name =>
              g.groupGenerator
            case SGlobalMethods.xorMethod.name =>
              val c1 = asRep[Coll[Byte]](argsV(0))
              val c2 = asRep[Coll[Byte]](argsV(1))
              g.xor(c1, c2)
            case _ => throwError
          }
          case _ => throwError
        }

      case _ =>
        throwError
    }
    val resC = asRep[T#WrappedType](res)
    resC
  }

}
