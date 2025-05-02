package sigma.ast

import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.data.{AvlTreeData, CSigmaProp, GeneralType, Nullable, RType, SigmaBoolean, TrivialProp}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate}
import sigma.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
import StdSigmaBuilder.mkUpcast
import sigma.SigmaDataReflection
import sigma.exceptions.InterpreterException

import scala.annotation.nowarn
import scala.reflect.classTag

/** Contains global definitions which define syntactic extensions for working with classes
  * of  sigma.ast package.
  */
object syntax {
  /** Force initialization of reflection. */
  val reflection = SigmaDataReflection

  /** The following type synonyms are used throughout the codebase to simplify type annotations. */
  type SValue = Value[SType]
  type BoolValue = Value[SBoolean.type]
  type ByteValue = Value[SByte.type]
  type ShortValue = Value[SShort.type]
  type IntValue = Value[SInt.type]
  type LongValue = Value[SLong.type]
  type BigIntValue = Value[SBigInt.type]
  type BoxValue = Value[SBox.type]
  type GroupElementValue = Value[SGroupElement.type]
  type SigmaPropValue = Value[SSigmaProp.type]
  type AvlTreeValue = Value[SAvlTree.type]
  type SAnyValue = Value[SAny.type]
  type CollectionValue[T <: SType] = Value[SCollection[T]]

  type BooleanConstant = Constant[SBoolean.type]
  type ByteConstant = Constant[SByte.type]
  type ShortConstant = Constant[SShort.type]
  type IntConstant = Constant[SInt.type]
  type LongConstant = Constant[SLong.type]
  type StringConstant = Constant[SString.type]
  type BigIntConstant = Constant[SBigInt.type]
  type BoxConstant = Constant[SBox.type]
  type GroupElementConstant = Constant[SGroupElement.type]
  type SigmaPropConstant = Constant[SSigmaProp.type]
  type AvlTreeConstant = Constant[SAvlTree.type]
  type CollectionConstant[T <: SType] = Constant[SCollection[T]]

  /** Sigma proposition with trivial false proposition. */
  val FalseSigmaProp = SigmaPropConstant(CSigmaProp(TrivialProp.FalseProp))

  /** Sigma proposition with trivial true proposition. */
  val TrueSigmaProp  = SigmaPropConstant(CSigmaProp(TrivialProp.TrueProp))

  /** Methods to work with tree nodes of [[SCollection]] type. */
  implicit class CollectionOps[T <: SType](val coll: Value[SCollection[T]]) extends AnyVal {
    /** Returns number of items in the collection expression. */
    def length: Int = matchCase(_.items.length, _.value.length, _.items.length)

    /** Returns a sequence of items in the collection expression. */
    def items: Seq[Value[SType]] = matchCase(_.items, _ => sys.error(s"Cannot access the 'items' property of node $coll. " +
      "Please ensure the node is properly initialized and includes the 'items' property. " +
      "If the issue keeps happening, contact <a href=\"#\">Customer care</a>."), _.items)

    /** Abstracts from details of pattern matching collection expressions.
      * Folds over given `coll` structure.
      */
    def matchCase[R](
        whenConcrete: ConcreteCollection[T] => R,
        whenConstant: CollectionConstant[T] => R,
        whenTuple: Tuple => R
    ): R = coll match {
      case cc: ConcreteCollection[T]@unchecked => whenConcrete(cc)
      case const: CollectionConstant[T]@unchecked => whenConstant(const)
      case tuple: Tuple => whenTuple(tuple)
      case _ => sys.error(s"We found an unexpected element: $coll. " +
        "Please review your input for errors or unsupported elements. " +
        "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
    }
  }

  /** Methods to work with tree nodes of [[SSigmaProp]] type. */
  implicit class SigmaPropValueOps(val p: Value[SSigmaProp.type]) extends AnyVal {
    def propBytes: Value[SByteArray] = SigmaPropBytes(p)
  }

  /** Methods to work with tree nodes of [[SSigmaProp]] type. */
  implicit class OptionValueOps[T <: SType](val p: Value[SOption[T]]) extends AnyVal {
    def get: Value[T] = OptionGet(p)

    def getOrElse(default: Value[T]): Value[T] = OptionGetOrElse(p, default)

    def isDefined: Value[SBoolean.type] = OptionIsDefined(p)
  }


  def GetVarBoolean(varId: Byte): GetVar[SBoolean.type] = GetVar(varId, SBoolean)

  def GetVarByte(varId: Byte): GetVar[SByte.type] = GetVar(varId, SByte)

  def GetVarShort(varId: Byte): GetVar[SShort.type] = GetVar(varId, SShort)

  def GetVarInt(varId: Byte): GetVar[SInt.type] = GetVar(varId, SInt)

  def GetVarLong(varId: Byte): GetVar[SLong.type] = GetVar(varId, SLong)

  def GetVarBigInt(varId: Byte): GetVar[SBigInt.type] = GetVar(varId, SBigInt)

  def GetVarBox(varId: Byte): GetVar[SBox.type] = GetVar(varId, SBox)

  def GetVarSigmaProp(varId: Byte): GetVar[SSigmaProp.type] = GetVar(varId, SSigmaProp)

  def GetVarByteArray(varId: Byte): GetVar[SCollection[SByte.type]] = GetVar(varId, SByteArray)

  def GetVarIntArray(varId: Byte): GetVar[SCollection[SInt.type]] = GetVar(varId, SIntArray)

  implicit def boolToSigmaProp(b: BoolValue): SigmaPropValue = BoolToSigmaProp(b)

  /** Shadow the implicit from sigma package so it doesn't interfere with the resolution
    * of ClassTags below.
    */
  @nowarn private def rtypeToClassTag = ???

  /** RType descriptors for predefined types used in AOTC-based interpreter. */
  implicit val ErgoBoxRType: RType[ErgoBox] = RType.fromClassTag(classTag[ErgoBox])

  implicit val ErgoBoxCandidateRType: RType[ErgoBoxCandidate] = RType.fromClassTag(classTag[ErgoBoxCandidate])

  implicit val AvlTreeDataRType: RType[AvlTreeData] = GeneralType(classTag[AvlTreeData])

  /** Type casting methods for [[Value]] nodes.
    * Each `asX` method casts the value to the corresponding `X` type of node.
    * No runtime checks are performed, so the caller should ensure that the value has the
    * expected type.
    * */
  implicit class ValueOps(val v: Value[SType]) extends AnyVal {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]

    def asNumValue: Value[SNumericType] = v.asInstanceOf[Value[SNumericType]]

    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]

    def asByteValue: Value[SByte.type] = v.asInstanceOf[Value[SByte.type]]

    def asIntValue: Value[SInt.type] = v.asInstanceOf[Value[SInt.type]]

    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]

    def asBox: Value[SBox.type] = v.asInstanceOf[Value[SBox.type]]

    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]

    def asSigmaProp: Value[SSigmaProp.type] = v.asInstanceOf[Value[SSigmaProp.type]]

    def asByteArray: Value[SByteArray] = v.asInstanceOf[Value[SByteArray]]

    def asIntArray: Value[SIntArray] = v.asInstanceOf[Value[SIntArray]]

    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]

    def asOption[T <: SType]: Value[SOption[T]] = v.asInstanceOf[Value[SOption[T]]]

    def asTuple: Value[STuple] = v.asInstanceOf[Value[STuple]]

    def asFunc: Value[SFunc] = v.asInstanceOf[Value[SFunc]]

    /** Upcast the value `v` to the given type checking that `v` if of a numeric type.
      * @param targetType type to upcast to
      * @return upcasted value
      */
    def upcastTo[T <: SNumericType](targetType: T): Value[T] = {
      assert(v.tpe.isInstanceOf[SNumericType],
        s"Cannot upcast value of type ${v.tpe} to $targetType: only numeric types can be upcasted.")
      val tV = v.asValue[SNumericType]
      assert(targetType.max(tV.tpe) == targetType,
        s"Invalid upcast from $tV to $targetType: target type should be larger than source type.")
      if (targetType == tV.tpe) v.asValue[T]
      else
        mkUpcast(tV, targetType).withSrcCtx(v.sourceContext)
    }

    /** Assigns optional [[SourceContext]] to the value `v`.
      * The is effectful operation changing state of `v`.
      * @return the same instance of `v` with the given source context assigned
      */
    def withSrcCtx[T <: SType](sourceContext: Nullable[SourceContext]): Value[T] = {
      v.sourceContext = sourceContext
      v.asValue[T]
    }

    /**
      * Sets the source context of the `v` instance only if it's empty (i.e. not set yet).
      */
    def withEnsuredSrcCtx[T <: SType](sourceContext: Nullable[SourceContext]): Value[T] = {
      if (v.sourceContext.isEmpty) v.sourceContext = sourceContext
      v.asValue[T]
    }

    /**
      * Set source context to all nodes missing source context in the given tree.
      *
      * @param srcCtx source context to set
      * @return AST where all nodes with missing source context are set to the given srcCtx
      */
    def withPropagatedSrcCtx[T <: SType](srcCtx: Nullable[SourceContext]): Value[T] = {
      rewrite(everywherebu(rule[Any] {
        case node: SValue if node != null && node.sourceContext.isEmpty =>
          node.withSrcCtx(srcCtx)
      }))(v).asValue[T]
    }
  }

  /** Helper method to throw errors from Interpreter. */
  def error(msg: String) = throw new InterpreterException(msg)
}
