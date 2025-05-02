package sigma.ast

import debox.cfor
import sigma.Extensions.ArrayOps
import sigma._
import sigma.ast.FuncValue.{AddToEnvironmentDesc, AddToEnvironmentDesc_CostKind}
import sigma.ast.SCollection.SByteArray
import sigma.ast.TypeCodes.ConstantCode
import sigma.ast.syntax._
import sigma.crypto.{CryptoConstants, EcPointType}
import sigma.data.OverloadHack.Overloaded1
import sigma.data.{CSigmaDslBuilder, CSigmaProp, Nullable, RType, SigmaBoolean}
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.eval.{ErgoTreeEvaluator, SigmaDsl}
import sigma.exceptions.InterpreterException
import sigma.kiama.rewriting.Rewriter.count
import sigma.serialization.OpCodes._
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization._
import sigma.util.CollectionUtil._
import sigma.util.Extensions._

import java.math.BigInteger
import java.util.{Arrays, Objects}
import scala.annotation.unused
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.language.implicitConversions

/** Base class for all ErgoTree expression nodes.
  *
  * @see [[ErgoTree]]
  */
abstract class Value[+S <: SType] extends SigmaNode {
  /** The companion node descriptor with opCode, cost and other metadata. */
  def companion: ValueCompanion

  /** Unique id of the node class used in serialization of ErgoTree. */
  def opCode: OpCode = companion.opCode

  /** The type of the value represented by this node. If the value is an operation it is
    * the type of operation result. */
  def tpe: S

  /** Every value represents an operation and that operation can be associated with a function type,
    * describing functional meaning of the operation, kind of operation signature.
    * Thus, we can obtain global operation identifiers by combining Value.opName with Value.opType,
    * so that if (v1.opName == v2.opName) && (v1.opType == v2.opType) then v1 and v2 are functionally
    * point-wise equivalent.
    * This in particular means that if two _different_ ops have the same opType they _should_ have
    * different opNames.
    * Thus defined op ids are used in a v4.x Cost Model - a table of all existing primitives coupled with
    * performance parameters.
    * */
  def opType: SFunc

  /** Name of the operation. */
  def opName: String = this.getClass.getSimpleName

  /** Transforms this expression to SigmaProp expression or throws an exception. */
  def toSigmaProp: SigmaPropValue = this match {
    case b if b.tpe == SBoolean => BoolToSigmaProp(this.asBoolValue)
    case p if p.tpe == SSigmaProp => p.asSigmaProp
    case _ => sys.error(s"Expected SBoolean or SSigmaProp typed value, but was: $this")
  }

  /** Parser has some source information like line,column in the text. We need to keep it up until RuntimeCosting.
    * The way to do this is to add Nullable property to every Value. Since Parser is always using SigmaBuilder
    * to create nodes,
    * Adding additional (implicit source: SourceContext) parameter to every builder method would pollute its API
    * and also doesn't make sence during deserialization, where Builder is also used.
    * We can assume some indirect mechanism to pass current source context into every mkXXX method of Builder.
    * We can pass it using `scala.util.DynamicVariable` by wrapping each mkXXX call into `withValue { }` calls.
    * The same will happen in Typer.
    * We can take sourceContext from untyped nodes and use it while creating typed nodes.
    * And we can make sourceContext of every Value writeOnce value, i.e. it will be Nullable.Null by default,
    * but can be set afterwards, but only once.
    * This property will not participate in equality and other operations, so will be invisible for existing code.
    * But Builder can use it to set sourceContext if it is present.
    */
  private[ast] var _sourceContext: Nullable[SourceContext] = Nullable.None

  def sourceContext: Nullable[SourceContext] = _sourceContext

  def sourceContext_=(srcCtx: Nullable[SourceContext]): Unit =
    if (_sourceContext.isEmpty) {
      _sourceContext = srcCtx
    } else {
      sys.error("_sourceContext can be set only once")
    }

  /** Defines an evaluation semantics of this tree node (aka Value or expression) in the given data environment.
    * Should be implemented by all the ErgoTree nodes (aka operations).
    * Thus, the ErgoTree interpreter implementation consists of combined implementations of this method.
    * NOTE, this method shouldn't be called directly, instead use `evalTo` method.
    *
    * @param E   Evaluator which defines evaluation context, cost accumulator, settings etc.
    * @param env immutable map, which binds variables (given by ids) to the values
    * @return the data value which is the result of evaluation
    */
  protected def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any =
    sys.error(s"Should be overriden in ${this.getClass}: $this")

  /** Evaluates this node to the value of the given expected type.
    * This method should called from all `eval` implementations.
    *
    * @tparam T expected type of the resulting value
    * @param E   Evaluator which defines evaluation context, cost accumulator, settings etc.
    * @param env immutable map, which binds variables (given by ids) to the values
    * @return the data value which is the result of evaluation
    */
  @inline
  final def evalTo[T](env: DataEnv)(implicit E: ErgoTreeEvaluator): T = {
    if (E.settings.isMeasureOperationTime) E.profiler.onBeforeNode(this)
    val v = eval(env)
    if (E.settings.isMeasureOperationTime) E.profiler.onAfterNode(this)
    v.asInstanceOf[T]
  }

  /** Add the cost given by the kind to the accumulator and associate it with this operation
    * node.
    */
  @inline
  final def addCost(costKind: FixedCost)(implicit E: ErgoTreeEvaluator): Unit = {
    E.addCost(costKind, this.companion.opDesc)
  }

  /** Add the cost given by the descriptor to the accumulator and associate it with this operation
    * node.
    */
  @inline
  final def addCost[R](costKind: TypeBasedCost, tpe: SType)
      (block: () => R)
      (implicit E: ErgoTreeEvaluator): R = {
    E.addTypeBasedCost(costKind, tpe, this.companion.opDesc)(block)
  }

  /** Add the cost of a repeated operation to the accumulator and associate it with this
    * operation. The number of items (loop iterations) is known in advance (like in
    * Coll.map operation)
    *
    * @param costKind cost descriptor of the operation
    * @param nItems   number of operations known in advance (before loop execution)
    */
  @inline
  final def addSeqCostNoOp(costKind: PerItemCost, nItems: Int)
      (implicit E: ErgoTreeEvaluator): Unit = {
    E.addSeqCostNoOp(costKind, nItems, this.companion.opDesc)
  }

  /** Add the cost of a repeated operation to the accumulator and associate it with this
    * operation. The number of items (loop iterations) is known in advance (like in
    * Coll.map operation)
    *
    * @param costKind cost descriptor of the operation
    * @param nItems   number of operations known in advance (before loop execution)
    * @param block    operation executed under the given cost
    * @tparam R result type of the operation
    */
  @inline
  final def addSeqCost[R](costKind: PerItemCost, nItems: Int)
      (block: () => R)(implicit E: ErgoTreeEvaluator): R = {
    E.addSeqCost(costKind, nItems, this.companion.opDesc)(block)
  }
}

object Value {
  type PropositionCode = Byte

  implicit def liftByte(n: Byte): Value[SByte.type] = ByteConstant(n)

  implicit def liftShort(n: Short): Value[SShort.type] = ShortConstant(n)

  implicit def liftInt(n: Int): Value[SInt.type] = IntConstant(n)

  implicit def liftLong(n: Long): Value[SLong.type] = LongConstant(n)

  implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray] = ByteArrayConstant(arr)

  implicit def liftBigInt(arr: BigInt): Value[SBigInt.type] = BigIntConstant(arr)

  implicit def liftGroupElement(g: GroupElement): Value[SGroupElement.type] = GroupElementConstant(g)

  implicit def liftECPoint(g: EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

  implicit def liftSigmaProp(g: SigmaProp): Value[SSigmaProp.type] = SigmaPropConstant(g)

  implicit def liftSigmaBoolean(sb: SigmaBoolean): Value[SSigmaProp.type] = SigmaPropConstant(SigmaDsl.SigmaProp(sb))

  object Typed {
    def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
  }

  def notSupportedError(v: Any, opName: String) =
    throw new IllegalArgumentException(s"Method $opName is not supported for node $v")

  /** Immutable empty array of values. Can be used to avoid allocation. */
  val EmptyArray = Array.empty[SValue]

  /** Immutable empty Seq of values. Can be used to avoid allocation. */
  val EmptySeq: IndexedSeq[SValue] = EmptyArray

  /** Traverses the given expression tree and counts the number of deserialization
    * operations. If it is non-zero, returns true. */
  def hasDeserialize(exp: SValue): Boolean = {
    val deserializeNode: PartialFunction[Any, Int] = {
      case _: DeserializeContext[_] => 1
      case _: DeserializeRegister[_] => 1
    }
    val c                                          = count(deserializeNode)(exp)
    c > 0
  }

  /** Traverses the given expression tree and counts the number of operations
   * which read the blockchain context. If it is non-zero, returns true. */
  def isUsingBlockchainContext(exp: SValue): Boolean = {
    val blockchainContextNode: PartialFunction[Any, Int] = {
      case Height => 1
      case LastBlockUtxoRootHash => 1
      case MinerPubkey => 1
      case MethodCall(_, method, _, _) =>
        method.objType match {
          case SContextMethods =>
            if (SContextMethods.BlockchainContextMethodNames.contains(method.name)) 1 else 0
          case _ => 0
        }
      case _ => 0
    }
    val c = count(blockchainContextNode)(exp)
    c > 0
  }

  def typeError(node: SValue, evalResult: Any) = {
    val tpe = node.tpe
    throw new InterpreterException(
      s"""Invalid type returned by evaluator:
        |  expression: $node
        |  expected type: $tpe
        |  resulting value: $evalResult
           """.stripMargin)
  }

  def typeError(tpe: SType, evalResult: Any) = {
    throw new InterpreterException(
      s"""Invalid type returned by evaluator:
        |  expected type: $tpe
        |  resulting value: $evalResult
           """.stripMargin)
  }

  def checkType(node: SValue, evalResult: Any) = {
    val tpe = node.tpe
    if (!SType.isValueOfType(evalResult, tpe))
      typeError(node, evalResult)
  }

  def checkType(tpe: SType, evalResult: Any) = {
    if (!SType.isValueOfType(evalResult, tpe))
      typeError(tpe, evalResult)
  }
}

/** Base class for all companion objects which are used as operation descriptors. */
trait ValueCompanion extends SigmaNodeCompanion {
  import ValueCompanion._

  /** Unique id of the node class used in serialization of ErgoTree. */
  def opCode: OpCode

  /** Returns cost descriptor of this operation. */
  def costKind: CostKind

  override def toString: String = s"${this.getClass.getSimpleName}(${opCode.toUByte})"

  def typeName: String = this.getClass.getSimpleName.replace("$", "")

  def init() {
    if (this.opCode != 0 && _allOperations.contains(this.opCode))
      throw sys.error(s"Operation $this already defined")
    _allOperations += (this.opCode -> this)
  }

  init()
  val opDesc = CompanionDesc(this)
}

object ValueCompanion {
  private val _allOperations: mutable.HashMap[Byte, ValueCompanion] = mutable.HashMap.empty

  lazy    val allOperations                                         = _allOperations.toMap
}

/** Should be inherited by companion objects of operations with fixed cost kind. */
trait FixedCostValueCompanion extends ValueCompanion {
  /** Returns cost descriptor of this operation. */
  override def costKind: FixedCost
}

/** Should be inherited by companion objects of operations with per-item cost kind. */
trait PerItemCostValueCompanion extends ValueCompanion {
  /** Returns cost descriptor of this operation. */
  override def costKind: PerItemCost
}

/** Base class for ErgoTree nodes which represents a data value which has already been
  * evaluated and no further evaluation (aka reduction) is necessary by the interpreter.
  *
  * @see Constant, ConcreteCollection, Tuple
  */
abstract class EvaluatedValue[+S <: SType] extends Value[S] {
  /** The evaluated data value of the corresponding underlying data type. */
  val value: S#WrappedType

  override def opType: SFunc = {
    val resType = tpe match {
      case ct: SCollection[_] =>
        SCollection(ct.typeParams.head.ident)
      case ft @ SFunc(_, _, _) =>
        ft.getGenericType
      case _ => tpe
    }
    SFunc(ArraySeq.empty, resType)
  }
}

/** Base class for all constant literals whose data value is already known and never
  * changes.
  *
  * @see ConstantNode
  */
abstract class Constant[+S <: SType] extends EvaluatedValue[S]

/** ErgoTree node which represents data literals, i.e. data values embedded in an
  * expression.
  *
  * @param value data value of the underlying Scala type
  * @param tpe   type descriptor of the data value and also the type of the value
  *              represented by this node.
  * @see Constant
  */
case class ConstantNode[S <: SType](value: S#WrappedType, tpe: S) extends Constant[S] {
  require(sigma.crypto.Platform.isCorrectType(value, tpe),
    s"Invalid type of constant value $value, expected type $tpe")

  override def companion: ValueCompanion = Constant

  override def opCode: OpCode = companion.opCode

  override def opName: String = s"Const"

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(Constant.costKind)
    value
  }

  override def equals(obj: scala.Any): Boolean = (obj != null) && (this.eq(obj.asInstanceOf[AnyRef]) || (obj match {
    case c: Constant[_] => tpe == c.tpe && Objects.deepEquals(value, c.value)
    case _ => false
  }))

  override def hashCode(): Int = Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))

  override def toString: String = tpe.asInstanceOf[SType] match {
    case SGroupElement if value.isInstanceOf[GroupElement] =>
      s"ConstantNode(${value.asInstanceOf[GroupElement].showToString},$tpe)"
    case SGroupElement =>
      sys.error(s"Invalid value in Constant($value, $tpe)")
    case SInt => s"IntConstant($value)"
    case SLong => s"LongConstant($value)"
    case SBoolean if value == true => "TrueLeaf"
    case SBoolean if value == false => "FalseLeaf"
    case _ => s"ConstantNode($value,$tpe)"
  }
}

object Constant extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCode @@ ConstantCode

  /** Cost of: returning value from Constant node. */
  override val costKind = FixedCost(JitCost(5))

  /** Immutable empty array, can be used to save allocations in many places. */
  val EmptyArray = Array.empty[Constant[SType]]

  /** Immutable empty IndexedSeq, can be used to save allocations in many places. */
  val EmptySeq: IndexedSeq[Constant[SType]] = Array.empty[Constant[SType]]

  /** Helper factory method. */
  def apply[S <: SType](
      value: S#WrappedType,
      tpe: S): Constant[S] = ConstantNode(value, tpe)

  /** Recognizer of Constant tree nodes used in patterns. */
  def unapply[S <: SType](v: EvaluatedValue[S]): Option[(S#WrappedType, S)] = v match {
    case ConstantNode(value, tpe) => Some((value, tpe))
    case _ => None
  }
}

/** Placeholder for a constant in ErgoTree. Zero based index in ErgoTree.constants array. */
case class ConstantPlaceholder[S <: SType](
    id: Int,
    override val tpe: S) extends Value[S] {
  def opType = SFunc(SInt, tpe)

  override def companion: ValueCompanion = ConstantPlaceholder

  override protected def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val c = E.constants(id)
    addCost(ConstantPlaceholder.costKind)
    val res = c.value
    Value.checkType(c, res)
    res
  }
}

object ConstantPlaceholder extends ValueCompanion {
  override def opCode: OpCode = ConstantPlaceholderCode

  /** Cost of: accessing Constant in array by index. */
  override val costKind = FixedCost(JitCost(1))
}

trait NotReadyValue[S <: SType] extends Value[S] {
}

// TODO v6.0: remove these TaggedVariable and TaggedVariableNode (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/584)

/** Reference a context variable by id. */
trait TaggedVariable[T <: SType] extends NotReadyValue[T] {
  val varId: Byte
}

case class TaggedVariableNode[T <: SType](varId: Byte, override val tpe: T)
    extends TaggedVariable[T] {
  override def companion = TaggedVariable

  def opType: SFunc = Value.notSupportedError(this, "opType")
}

object TaggedVariable extends ValueCompanion {
  override def opCode: OpCode = TaggedVariableCode

  override def costKind: CostKind = FixedCost(JitCost(1))

  def apply[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
    TaggedVariableNode(varId, tpe)
}

/** High-level interface to internal representation of Unit constants in ErgoTree. */
object UnitConstant {
  /** ErgoTree node that represent a literal of Unit type. It is global immutable value
    * which should be reused wherever necessary to avoid allocations.
    */
  val instance = apply()

  /** Constucts a fresh new instance of Unit literal node. */
  def apply() = Constant[SUnit.type]((), SUnit)

  /** Recognizer to pattern match on Unit constant literal nodes (aka Unit constants). */
  def unapply(node: SValue): Boolean = node match {
    case ConstantNode(_, SUnit) => true
    case _ => false
  }
}

object ByteConstant {
  def apply(value: Byte): Constant[SByte.type] = Constant[SByte.type](value, SByte)
}

object ShortConstant {
  def apply(value: Short): Constant[SShort.type] = Constant[SShort.type](value, SShort)
}

object IntConstant {
  def apply(value: Int): Constant[SInt.type] = Constant[SInt.type](value, SInt)

  def unapply(v: SValue): Option[Int] = v match {
    case Constant(value: Int, SInt) => Some(value)
    case _ => None
  }

  def Zero = IntConstant(0)
}

object LongConstant {
  def apply(value: Long): Constant[SLong.type] = Constant[SLong.type](value, SLong)

  def unapply(v: SValue): Option[Long] = v match {
    case Constant(value: Long, SLong) => Some(value)
    case _ => None
  }
}

object BigIntConstant {
  def apply(value: BigInt): Constant[SBigInt.type] = Constant[SBigInt.type](value, SBigInt)

  def apply(value: BigInteger): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(value), SBigInt)

  def apply(value: Long): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(BigInteger.valueOf(value)), SBigInt)
}

object StringConstant {
  def apply(value: String): Constant[SString.type] = Constant[SString.type](value, SString)

  def unapply(v: SValue): Option[String] = v match {
    case Constant(value: String, SString) => Some(value)
    case _ => None
  }
}

object BoxConstant {
  def apply(value: Box): Constant[SBox.type] = Constant[SBox.type](value, SBox)
}

object GroupElementConstant {
  def apply(value: EcPointType): Constant[SGroupElement.type] = apply(SigmaDsl.GroupElement(value))

  def apply(value: GroupElement): Constant[SGroupElement.type] = Constant[SGroupElement.type](value, SGroupElement)

  def unapply(v: SValue): Option[GroupElement] = v match {
    case Constant(value: GroupElement, SGroupElement) => Some(value)
    case _ => None
  }
}

object SigmaPropConstant {
  def apply(value: SigmaProp): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](value, SSigmaProp)

  def apply(value: SigmaBoolean): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](CSigmaProp(value), SSigmaProp)

  def unapply(v: SValue): Option[SigmaProp] = v match {
    case Constant(value: SigmaProp, SSigmaProp) => Some(value)
    case _ => None
  }
}

object AvlTreeConstant {
  def apply(value: AvlTree): Constant[SAvlTree.type] = Constant[SAvlTree.type](value, SAvlTree)
}

object PreHeaderConstant {
  def apply(value: PreHeader): Constant[SPreHeader.type] = Constant[SPreHeader.type](value, SPreHeader)

  def unapply(v: SValue): Option[PreHeader] = v match {
    case Constant(value: PreHeader, SPreHeader) => Some(value)
    case _ => None
  }
}

object HeaderConstant {
  def apply(value: Header): Constant[SHeader.type] = Constant[SHeader.type](value, SHeader)

  def unapply(v: SValue): Option[Header] = v match {
    case Constant(value: Header, SHeader) => Some(value)
    case _ => None
  }
}

trait NotReadyValueInt extends NotReadyValue[SInt.type] {
  override def tpe = SInt
}

trait NotReadyValueLong extends NotReadyValue[SLong.type] {
  override def tpe = SLong
}

trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
  override def tpe = SBigInt
}

/** Base type for evaluated tree nodes of Coll type. */
trait EvaluatedCollection[T <: SType, C <: SCollection[T]] extends EvaluatedValue[C] {
  /** Type descriptor of the collection elements. */
  def elementType: T
}

object CollectionConstant {
  def apply[T <: SType](
      value: Coll[T#WrappedType],
      elementType: T): Constant[SCollection[T]] =
    Constant[SCollection[T]](value, SCollection(elementType))

  def unapply[T <: SType](node: Value[SCollection[T]]): Option[(Coll[T#WrappedType], T)] = node match {
    case c: Constant[SCollection[a]]@unchecked if c.tpe.isCollection =>
      val v = c.value.asInstanceOf[Coll[T#WrappedType]]
      val t = c.tpe.elemType
      Some((v, t))
    case _ => None
  }
}

object ByteArrayConstant {
  val ByteArrayTypeCode = (SCollectionType.CollectionTypeCode + SByte.typeCode).toByte

  def apply(value: Coll[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value, SByte)

  def apply(value: Array[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value.toColl, SByte)

  def unapply(node: SValue): Option[Coll[Byte]] = node match {
    case coll: CollectionConstant[SByte.type]@unchecked => coll match {
      case CollectionConstant(arr, SByte) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

object ShortArrayConstant {
  def apply(value: Coll[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value, SShort)

  def apply(value: Array[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value.toColl, SShort)

  def unapply(node: SValue): Option[Coll[Short]] = node match {
    case coll: CollectionConstant[SShort.type]@unchecked => coll match {
      case CollectionConstant(arr, SShort) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

object IntArrayConstant {
  def apply(value: Coll[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value, SInt)

  def apply(value: Array[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value.toColl, SInt)

  def unapply(node: SValue): Option[Coll[Int]] = node match {
    case coll: CollectionConstant[SInt.type]@unchecked => coll match {
      case CollectionConstant(arr, SInt) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

object LongArrayConstant {
  def apply(value: Coll[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value, SLong)

  def apply(value: Array[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value.toColl, SLong)

  def unapply(node: SValue): Option[Coll[Long]] = node match {
    case coll: CollectionConstant[SLong.type]@unchecked => coll match {
      case CollectionConstant(arr, SLong) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

object BigIntArrayConstant {
  def apply(value: Coll[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value, SBigInt)

  def apply(value: Array[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value.toColl, SBigInt)

  def unapply(node: SValue): Option[Coll[BigInt]] = node match {
    case coll: CollectionConstant[SBigInt.type]@unchecked => coll match {
      case CollectionConstant(arr, SBigInt) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

object BoolArrayConstant {
  val BoolArrayTypeCode = (SCollectionType.CollectionTypeCode + SBoolean.typeCode).toByte

  def apply(value: Coll[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value, SBoolean)

  def apply(value: Array[Boolean]): CollectionConstant[SBoolean.type] = apply(value.toColl)

  def unapply(node: SValue): Option[Coll[Boolean]] = node match {
    case coll: CollectionConstant[SBoolean.type]@unchecked => coll match {
      case CollectionConstant(arr, SBoolean) => Some(arr)
      case _ => None
    }
    case _ => None
  }
}

trait NotReadyValueByteArray extends NotReadyValue[SByteArray] {
  override def tpe = SByteArray
}

trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
  override def tpe = SAvlTree
}

/** ErgoTree node that represents the operation of obtaining the generator of elliptic curve group.
  * The generator g of the group is an element of the group such that, when written
  * multiplicative form, every element of the group is a power of g.
  */
case object GroupGenerator extends EvaluatedValue[SGroupElement.type] with ValueCompanion {
  override def opCode: OpCode = OpCodes.GroupGeneratorCode

  override val costKind = FixedCost(JitCost(10))

  override def tpe = SGroupElement

  override val value = SigmaDsl.GroupElement(CryptoConstants.dlogGroup.generator)

  override def companion = this

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(costKind)
    SigmaDsl.groupGenerator
  }
}

trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
  override def tpe = SGroupElement
}

object BooleanConstant {
  def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf

  def apply(value: Boolean): BooleanConstant = Constant[SBoolean.type](value, SBoolean)

  def unapply(v: SValue): Option[Boolean] = v match {
    case Constant(value: Boolean, SBoolean) => Some(value)
    case _ => None
  }
}

/** ErgoTree node which represents `true` literal. */
object TrueLeaf extends ConstantNode[SBoolean.type](true, SBoolean) with ValueCompanion {
  override def companion = this

  override def opCode: OpCode = TrueCode

  override def costKind: FixedCost = Constant.costKind

  override def toString: String = "TrueLeaf"
}

/** ErgoTree node which represents `false` literal. */
object FalseLeaf extends ConstantNode[SBoolean.type](false, SBoolean) with ValueCompanion {
  override def companion = this

  override def opCode: OpCode = FalseCode

  override def costKind: FixedCost = Constant.costKind

  override def toString: String = "FalseLeaf"
}

trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
  override def tpe = SBoolean
}

trait NotReadyValueBox extends NotReadyValue[SBox.type] {
  def tpe = SBox
}

/** ErgoTree node which converts a collection of expressions into a tuple of data values
  * of different types. Each data value of the resulting collection is obtained by
  * evaluating the corresponding expression in `items`. All items may have different
  * types.
  *
  * @param items source collection of expressions
  */
case class Tuple(items: IndexedSeq[Value[SType]])
    extends EvaluatedValue[STuple] // note, this superclass is required as Tuple can be in a register
        with EvaluatedCollection[SAny.type, STuple] {
  override def companion = Tuple

  override lazy val tpe = STuple(items.map(_.tpe))

  override def opType: SFunc = ???

  override def elementType: SAny.type = SAny

  override lazy val value = {
    val xs = items.cast[EvaluatedValue[SAny.type]].map(_.value)
    implicit val tAny: RType[Any] = sigma.AnyType
    Colls.fromArray(xs.toArray)
  }

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    // in v5.0 version we support only tuples of 2 elements to be equivalent with v4.x
    if (items.length != 2)
      syntax.error(s"Invalid tuple $this")
    val item0 = items(0)
    val x     = item0.evalTo[Any](env)
    Value.checkType(item0, x)
    val item1 = items(1)
    val y     = item1.evalTo[Any](env)
    Value.checkType(item1, y)
    val res = (x, y) // special representation for pairs (to pass directly to Coll primitives)
    addCost(Tuple.costKind)
    res
  }
}

object Tuple extends FixedCostValueCompanion {
  override def opCode: OpCode = TupleCode

  /** Cost of: 1) allocating a new tuple (of limited max size) */
  override val costKind = FixedCost(JitCost(15))

  def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
}

/** ErgoTree node which converts a collection of expressions into a collection of data
  * values. Each data value of the resulting collection is obtained by evaluating the
  * corresponding expression in `items`. All items must have the same type.
  *
  * @param items       source collection of expressions
  * @param elementType type descriptor of elements in the resulting collection
  */
case class ConcreteCollection[V <: SType](items: Seq[Value[V]], elementType: V)
    extends EvaluatedCollection[V, SCollection[V]] {
  // TODO uncomment and make sure Ergo works with it, i.e. complex data types are never used for `items`.
  //      There is nothing wrong in using List, Vector and other fancy types as a concrete representation
  //      of `items`, but these types have sub-optimal performance (2-3x overhead comparing to WrappedArray)
  //      which is immediately visible in profile.
  //      NOTE, the assert below should be commented before production release.
  //      Is it there for debuging only, basically to catch call stacks where the fancy types may
  //      occasionally be used.
  //    assert(
  //      items.isInstanceOf[mutable.WrappedArray[_]] ||
  //      items.isInstanceOf[ArrayBuffer[_]] ||
  //      items.isInstanceOf[mutable.ArraySeq[_]],
  //      s"Invalid types of items ${items.getClass}")

  private val isBooleanConstants = elementType == SBoolean && items.forall(_.isInstanceOf[Constant[_]])

  override def companion =
    if (isBooleanConstants) ConcreteCollectionBooleanConstant
    else ConcreteCollection

  val tpe = SCollection[V](elementType)

  implicit lazy val tElement: RType[V#WrappedType] = Evaluation.stypeToRType(elementType)

  // TODO refactor: this method is not used and can be removed
  lazy val value = {
    val xs = items.cast[EvaluatedValue[V]].map(_.value)
    Colls.fromArray(xs.toArray(tElement.classTag))
  }

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val len = items.length
    addCost(ConcreteCollection.costKind)
    val is = Array.ofDim[V#WrappedType](len)(tElement.classTag)
    cfor(0)(_ < len, _ + 1) { i =>
      val item  = items(i)
      val itemV = item.evalTo[V#WrappedType](env)
      Value.checkType(item, itemV) // necessary because cast to V#WrappedType is erased
      is(i) = itemV
    }
    Colls.fromArray(is)
  }
}

object ConcreteCollection extends FixedCostValueCompanion {
  override def opCode: OpCode = ConcreteCollectionCode

  /** Cost of: allocating new collection
    *
    * @see ConcreteCollection_PerItem */
  override val costKind = FixedCost(JitCost(20))

  def fromSeq[V <: SType](items: Seq[Value[V]])(implicit tV: V): ConcreteCollection[V] =
    ConcreteCollection(items, tV)

  def fromItems[V <: SType](items: Value[V]*)(implicit tV: V): ConcreteCollection[V] =
    ConcreteCollection(items, tV)
}

object ConcreteCollectionBooleanConstant extends ValueCompanion {
  override def opCode: OpCode = ConcreteCollectionBooleanConstantCode

  override def costKind = ConcreteCollection.costKind
}

trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]

sealed trait BlockItem extends NotReadyValue[SType] {
  def id: Int

  def rhs: SValue

  def isValDef: Boolean
}

object BlockItem {
  /** Immutable empty array, can be used to save allocations in many places. */
  val EmptyArray = Array.empty[BlockItem]

  /** Immutable empty IndexedSeq to save allocations in many places. */
  val EmptySeq: IndexedSeq[BlockItem] = EmptyArray
}

/** IR node for let-bound expressions `let x = rhs` which is ValDef, or `let f[T] = rhs` which is FunDef.
  * These nodes are used to represent ErgoTrees after common sub-expression elimination.
  * This representation is more compact in serialized form.
  *
  * @param id unique identifier of the variable in the current scope. */
case class ValDef(
    override val id: Int,
    tpeArgs: Seq[STypeVar],
    override val rhs: SValue) extends BlockItem {
  require(id >= 0, "id must be >= 0")

  override def companion = if (tpeArgs.isEmpty) ValDef else FunDef

  override def tpe: SType = rhs.tpe

  override def isValDef: Boolean = tpeArgs.isEmpty

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = Value.notSupportedError(this, "opType")
}

object ValDef extends ValueCompanion {
  override def opCode: OpCode = ValDefCode

  override def costKind = Value.notSupportedError(this, "costKind")

  def apply(id: Int, rhs: SValue): ValDef = ValDef(id, Nil, rhs)
}

object FunDef extends ValueCompanion {
  override def opCode: OpCode = FunDefCode

  override def costKind = Value.notSupportedError(this, "costKind")

  def unapply(d: BlockItem): Option[(Int, Seq[STypeVar], SValue)] = d match {
    case ValDef(id, targs, rhs) if !d.isValDef => Some((id, targs, rhs))
    case _ => None
  }
}

/** Special node which represents a reference to ValDef it was introduced as result of
  * CSE. */
case class ValUse[T <: SType](valId: Int, tpe: T) extends NotReadyValue[T] {
  override def companion = ValUse

  /** This is not used as operation, but rather to form a program structure */
  def opType: SFunc = Value.notSupportedError(this, "opType")

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(ValUse.costKind)
    val res = env.getOrElse(valId, syntax.error(s"cannot resolve $this"))
    Value.checkType(this, res)
    res
  }
}

object ValUse extends FixedCostValueCompanion {
  override def opCode: OpCode = ValUseCode

  /** Cost of: 1) Lookup in immutable HashMap by valId: Int 2) alloc of Some(v) */
  override val costKind = FixedCost(JitCost(5))
}

/** The order of ValDefs in the block is used to assign ids to ValUse(id) nodes
  * For all i: items(i).id == {number of ValDefs preceded in a graph} with respect to topological order.
  * Specific topological order doesn't really matter, what is important is to preserve semantic linkage
  * between ValUse(id) and ValDef with the corresponding id.
  * This convention allow to valid serializing ids because we always serializing and deserializing
  * in a fixed well defined order.
  */
case class BlockValue(
    items: IndexedSeq[BlockItem],
    result: SValue) extends NotReadyValue[SType] {
  override def companion = BlockValue

  def tpe: SType = result.tpe

  /** This is not used as operation, but rather to form a program structure */
  def opType: SFunc = Value.notSupportedError(this, "opType")

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    var curEnv = env
    val len    = items.length
    addSeqCostNoOp(BlockValue.costKind, len)
    cfor(0)(_ < len, _ + 1) { i =>
      val vd = items(i).asInstanceOf[ValDef]
      val v  = vd.rhs.evalTo[Any](curEnv)
      Value.checkType(vd, v)
      E.addFixedCost(FuncValue.AddToEnvironmentDesc_CostKind,
        FuncValue.AddToEnvironmentDesc) {
        curEnv = curEnv + (vd.id -> v)
      }
    }
    val res = result.evalTo[Any](curEnv)
    Value.checkType(result, res)
    res
  }
}

object BlockValue extends ValueCompanion {
  override def opCode: OpCode = BlockValueCode

  override val costKind = PerItemCost(
    baseCost = JitCost(1), perChunkCost = JitCost(1), chunkSize = 10)
}

/**
  * @param args parameters list, where each parameter has an id and a type.
  * @param body expression, which refers function parameters with ValUse.
  */
case class FuncValue(
    args: IndexedSeq[(Int, SType)],
    body: Value[SType]) extends NotReadyValue[SFunc] {
  import FuncValue._

  override def companion = FuncValue

  lazy val tpe: SFunc = {
    val nArgs    = args.length
    val argTypes = new Array[SType](nArgs)
    cfor(0)(_ < nArgs, _ + 1) { i =>
      argTypes(i) = args(i)._2
    }
    SFunc(argTypes, body.tpe)
  }

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = SFunc(ArraySeq.empty, tpe)

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(FuncValue.costKind)
    if (args.length == 1) {
      val arg0 = args(0)
      (vArg: Any) => {
        Value.checkType(arg0._2, vArg)
        var env1: DataEnv = null
        E.addFixedCost(AddToEnvironmentDesc_CostKind, AddToEnvironmentDesc) {
          env1 = env + (arg0._1 -> vArg)
        }
        val res = body.evalTo[Any](env1)
        Value.checkType(body, res)
        res
      }
    } else {
      syntax.error(s"Function must have 1 argument, but was: $this")
    }
  }
}

object FuncValue extends FixedCostValueCompanion {
  val AddToEnvironmentDesc          = NamedDesc("AddToEnvironment")

  /** Cost of: adding value to evaluator environment */
  val AddToEnvironmentDesc_CostKind = FixedCost(JitCost(5))

  override def opCode: OpCode = FuncValueCode

  /** Cost of: 1) switch on the number of args 2) allocating a new Scala closure
    * Old cost: ("Lambda", "() => (D1) => R", lambdaCost), */
  override val costKind = FixedCost(JitCost(5))

  def apply(argId: Int, tArg: SType, body: SValue): FuncValue =
    FuncValue(IndexedSeq((argId, tArg)), body)
}

/** Frontend representation of a block of Val definitions.
  * { val x = ...; val y = ... }
  * This node is not part of ErgoTree and hence have Undefined opCode. */
case class Block(bindings: Seq[Val], result: SValue) extends Value[SType] {
  override def companion = Block

  override def tpe: SType = result.tpe

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = Value.notSupportedError(this, "opType")
}

object Block extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")

  /** Create a Block node with a single Val definition. */
  def apply(let: Val, result: SValue)(implicit @unused o1: Overloaded1): Block =
    Block(Seq(let), result)
}

/** IR node to represent explicit Zero Knowledge scope in ErgoTree.
  * Compiler checks Zero Knowledge properties and issue error message is case of violations.
  * ZK-scoping is optional, it can be used when the user want to ensure Zero Knowledge of
  * specific set of operations.
  * Usually it will require simple restructuring of the code to make the scope body explicit.
  * Invariants checked by the compiler:
  *  - single ZKProof in ErgoTree in a root position
  *  - no boolean operations in the body, because otherwise the result may be disclosed
  *  - all the operations are over SigmaProp values
  *
  * For motivation and details see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/236
  * */
case class ZKProofBlock(body: SigmaPropValue) extends BoolValue {
  override def companion = ZKProofBlock

  override def tpe = SBoolean

  override def opType: SFunc = ZKProofBlock.OpType
}

object ZKProofBlock extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")

  val OpType = SFunc(SSigmaProp, SBoolean)
}

trait Val extends Value[SType] {
  val name     : String
  val givenType: SType
  val body     : SValue
}

object Val {
  def apply(name: String, body: SValue): Val = ValNode(name, NoType, body)

  def apply(
      name: String,
      givenType: SType,
      body: SValue): Val = ValNode(name, givenType, body)

  def unapply(v: SValue): Option[(String, SType, SValue)] = v match {
    case ValNode(name, givenType, body) => Some((name, givenType, body))
    case _ => None
  }
}

case class ValNode(
    name: String,
    givenType: SType,
    body: SValue) extends Val {
  override def companion = ValNode

  override def tpe: SType = givenType ?: body.tpe

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = Value.notSupportedError(this, "opType")
}

object ValNode extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
}

/** Frontend node to select a field from an object. Should be transformed to SelectField */
case class Select(
    obj: Value[SType],
    field: String,
    resType: Option[SType] = None) extends Value[SType] {
  override def companion = Select

  override val tpe: SType = resType.getOrElse(obj.tpe match {
    case p: SProduct =>
      MethodsContainer.getMethod(p, field) match {
        case Some(m) => m.stype
        case None => NoType
      }
    case _ => NoType
  })

  override def opType: SFunc = SFunc(obj.tpe, tpe)
}

object Select extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
}

/** Frontend node to represent variable names parsed in a source code.
  * Should be resolved during compilation to lambda argument, Val definition or
  * compilation environment value. */
case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
  override def companion = Ident

  override def opType: SFunc = SFunc(ArraySeq.empty, tpe)
}

object Ident extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")

  def apply(name: String): Ident = Ident(name, NoType)
}

// TODO refactor: move to sigma.ast

/** ErgoTree node which represents application of function `func` to the given arguments.
  *
  * @param func expression which evaluates to a function
  * @param args arguments of the function application
  */
case class Apply(
    func: Value[SType],
    args: IndexedSeq[Value[SType]]) extends Value[SType] {
  override def companion = Apply

  override lazy val tpe   : SType = func.tpe match {
    case SFunc(_, r, _) => r
    case tColl: SCollectionType[_] => tColl.elemType
    case _ => NoType
  }

  override lazy val opType: SFunc = {
    val nArgs    = args.length
    val argTypes = new Array[SType](nArgs + 1)
    argTypes(0) = func.tpe
    cfor(0)(_ < nArgs, _ + 1) { i =>
      argTypes(i + 1) = args(i).tpe
    }
    SFunc(argTypes, tpe)
  }

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(Apply.costKind)
    if (args.length == 1) {
      val fV   = func.evalTo[Any => Any](env)
      val argV = args(0).evalTo[Any](env)
      fV(argV)
    } else {
      // zero or more than 1 argument functions are not supported in v4.x, v5.0
      // see `case Terms.Apply(f, Seq(x))` in RuntimeCosting which means other cases are not supported.
      syntax.error(s"Function application must have 1 argument, but was: $this")
    }
  }
}

object Apply extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.FuncApplyCode

  /** Cost of: 1) switch on the number of args 2) Scala method call 3) add args to env
    * Old cost: lambdaInvoke == 30 */
  override val costKind = FixedCost(JitCost(30))
}

/** Apply types for type parameters of input value. */
case class ApplyTypes(
    input: Value[SType],
    tpeArgs: Seq[SType]) extends Value[SType] { node =>
  override def companion = ApplyTypes

  override lazy val tpe: SType = input.tpe match {
    case funcType: SFunc =>
      val subst = funcType.tpeParams.map(_.ident).zip(tpeArgs).toMap
      applySubst(input.tpe, subst)
    case _ => input.tpe
  }

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = Value.notSupportedError(this, "opType")
}

object ApplyTypes extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
}

/** Frontend node to represent potential method call in a source code.
  * Should be resolved during compilation to MethodCall.
  * Cannot be serialized to ErgoTree. */
case class MethodCallLike(
    obj: Value[SType],
    name: String,
    args: IndexedSeq[Value[SType]],
    tpe: SType = NoType) extends Value[SType] {
  override def companion = MethodCallLike

  override def opType: SFunc = SFunc(obj.tpe +: args.map(_.tpe), tpe)
}

object MethodCallLike extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
}

/** Represents in ErgoTree an invocation of method of the object `obj` with arguments `args`.
  * The SMethod instances in STypeCompanions may have type STypeIdent in methods types,
  * but valid ErgoTree should have SMethod instances specialized for specific types of
  * obj and args using `specializeFor`.
  * This means, if we save typeId, methodId, and we save all the arguments,
  * we can restore the specialized SMethod instance.
  * This work by induction, if we assume all arguments are monomorphic,
  * then we can make MethodCall monomorphic.
  * Thus, all ErgoTree instances are monomorphic by construction.
  *
  * @param obj       object on which method will be invoked
  * @param method    method to be invoked
  * @param args      arguments passed to the method on invocation
  * @param typeSubst a map of concrete type for each generic type parameter
  */
case class MethodCall(
    obj: Value[SType],
    method: SMethod,
    args: IndexedSeq[Value[SType]],
    typeSubst: Map[STypeVar, SType]) extends Value[SType] {
  override def companion = if (args.isEmpty) PropertyCall else MethodCall

  override def opType: SFunc = SFunc(obj.tpe +: args.map(_.tpe), tpe)

  override val tpe: SType = method.stype match {
    case f: SFunc => f.tRange.withSubstTypes(typeSubst)
    case t => t.withSubstTypes(typeSubst)
  }

  /** @hotspot don't beautify this code */
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val objV = obj.evalTo[Any](env)
    addCost(MethodCall.costKind) // MethodCall overhead
    method.costKind match {
      case fixed: FixedCost =>
        val extra    = method.extraDescriptors
        val extraLen = extra.length
        val len      = args.length
        val argsBuf  = new Array[Any](len + extraLen)
        cfor(0)(_ < len, _ + 1) { i =>
          argsBuf(i) = args(i).evalTo[Any](env)
        }
        cfor(0)(_ < extraLen, _ + 1) { i =>
          argsBuf(len + i) = extra(i)
        }
        var res: Any = null
        E.addFixedCost(fixed, method.opDesc) {
          res = method.invokeFixed(objV, argsBuf)
        }
        res
      case _ =>
        val len     = args.length
        val argsBuf = new Array[Any](len + 3)
        argsBuf(0) = this
        argsBuf(1) = objV
        cfor(0)(_ < len, _ + 1) { i =>
          argsBuf(i + 2) = args(i).evalTo[Any](env)
        }
        argsBuf(argsBuf.length - 1) = E
        val evalMethod = method.genericMethod.evalMethod
        evalMethod.invoke(method.objType, argsBuf.asInstanceOf[Array[AnyRef]]: _*)
    }
  }
}

object MethodCall extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.MethodCallCode

  /** Cost of: 1) packing args into Array 2) RMethod.invoke */
  override val costKind = FixedCost(JitCost(4))

  /** Helper constructor which allows to cast the resulting node to the specified
    * [[sigma.ast.Value]] type `T`.
    *
    * @see [[sigmastate.lang.Terms.MethodCall]]
    */
  def typed[T <: SValue](
      obj: Value[SType],
      method: SMethod,
      args: IndexedSeq[Value[SType]],
      typeSubst: Map[STypeVar, SType]): T = {
    MethodCall(obj, method, args, typeSubst).asInstanceOf[T]
  }
}

object PropertyCall extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.PropertyCallCode

  /** Cost of: 1) packing args into Array 2) RMethod.invoke */
  override val costKind = FixedCost(JitCost(4))
}

/** Frontend implementation of lambdas. Should be transformed to FuncValue. */
case class Lambda(
    tpeParams: Seq[STypeParam],
    args: IndexedSeq[(String, SType)],
    givenResType: SType,
    body: Option[Value[SType]]) extends Value[SFunc] {
  require(!(tpeParams.nonEmpty && body.nonEmpty), s"Generic function definitions are not supported, but found $this")

  override def companion = Lambda

  override lazy val tpe: SFunc = {
    val sRange = givenResType ?: body.fold(NoType: SType)(_.tpe)
    SFunc(args.map(_._2), sRange, tpeParams)
  }

  /** This is not used as operation, but rather to form a program structure */
  override def opType: SFunc = SFunc(Vector(), tpe)

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(FuncValue.costKind)
    if (args.length == 1) {
      val arg0 = args(0)
      (vArg: Any) => {
        Value.checkType(arg0._2, vArg)
        var env1: DataEnv = null
        E.addFixedCost(AddToEnvironmentDesc_CostKind, AddToEnvironmentDesc) {
          env1 = env + (arg0._1 -> vArg)
        }
        body.map { b =>
          val res = b.evalTo[Any](env1)
          Value.checkType(b, res)
          res
        }.getOrElse(error(s"Lambda body is missing: $this"))
      }
    } else {
      error(s"Function must have 1 argument, but was: $this")
    }
  }

}

object Lambda extends ValueCompanion {
  override def opCode: OpCode = OpCodes.Undefined

  override def costKind: CostKind = Value.notSupportedError(this, "costKind")

  def apply(
      args: IndexedSeq[(String, SType)],
      resTpe: SType,
      body: Value[SType]): Lambda =
    Lambda(Nil, args, resTpe, Some(body))

  def apply(
      args: IndexedSeq[(String, SType)],
      resTpe: SType,
      body: Option[Value[SType]]): Lambda =
    Lambda(Nil, args, resTpe, body)

  def apply(
      args: IndexedSeq[(String, SType)],
      body: Value[SType]): Lambda = Lambda(Nil, args, NoType, Some(body))
}

/** When interpreted evaluates to a ByteArrayConstant built from Context.minerPubkey */
case object MinerPubkey extends NotReadyValueByteArray with ValueCompanion {
  override def opCode: OpCode = OpCodes.MinerPubkeyCode
  /** Cost of calling Context.minerPubkey Scala method. */
  override val costKind = FixedCost(JitCost(20))
  override val opType = SFunc(SContext, SCollection.SByteArray)
  override def companion = this
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.minerPubKey
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.HeightCode
  /** Cost of: 1) Calling Context.HEIGHT Scala method. */
  override val costKind = FixedCost(JitCost(26))
  override val opType = SFunc(SContext, SInt)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.HEIGHT
  }
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.InputsCode
  /** Cost of: 1) Calling Context.INPUTS Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override def tpe = SCollection.SBoxArray
  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.INPUTS
  }
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.OutputsCode
  /** Cost of: 1) Calling Context.OUTPUTS Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override def tpe = SCollection.SBoxArray
  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.OUTPUTS
  }
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  /** Cost of: 1) Calling Context.LastBlockUtxoRootHash Scala method. */
  override val costKind = FixedCost(JitCost(15))

  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.LastBlockUtxoRootHash
  }
}

/** When interpreted evaluates to a BoxConstant built from context.boxesToSpend(context.selfIndex) */
case object Self extends NotReadyValueBox with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.SelfCode
  /** Cost of: 1) Calling Context.SELF Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override val opType = SFunc(SContext, SBox)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.SELF
  }
}

/** When interpreted evaluates to the singleton instance of [[sigma.Context]].
  * Corresponds to `CONTEXT` variable in ErgoScript which can be used like `CONTEXT.headers`.
  */
case object Context extends NotReadyValue[SContext.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.ContextCode

  /** Cost of: 1) accessing global Context instance. */
  override val costKind = FixedCost(JitCost(1))

  override def tpe: SContext.type = SContext
  override val opType: SFunc = SFunc(SUnit, SContext)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context
  }
}

/** When interpreted evaluates to the singleton instance of [[sigma.SigmaDslBuilder]].
  * Corresponds to `Global` variable in ErgoScript which can be used like `Global.groupGenerator`.
  */
case object Global extends NotReadyValue[SGlobal.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.GlobalCode
  /** Cost of: 1) accessing Global instance. */
  override val costKind = FixedCost(JitCost(5))
  override def tpe: SGlobal.type = SGlobal
  override val opType: SFunc = SFunc(SUnit, SGlobal)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    CSigmaDslBuilder
  }
}


