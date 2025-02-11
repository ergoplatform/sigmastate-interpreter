package sigma.ast

import org.ergoplatform.ErgoBox.RegisterId
import sigma.ast.Operations._
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.SValue
import sigma.data.{CSigmaProp, RType}
import sigma.eval.ErgoTreeEvaluator
import sigma.eval.ErgoTreeEvaluator.DataEnv
import sigma.serialization.CoreByteWriter.ArgInfo
import sigma.serialization.OpCodes
import sigma.serialization.ValueCodes.OpCode
import sigma.{Box, Coll, Evaluation}

// TODO refactor: remove this trait as it doesn't have semantic meaning

/** Every operation is a transformer of some kind.
  * This trait is used merely to simplify implementation and avoid copy-paste.
  */
trait Transformer[IV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val input: Value[IV]
}

/** Builds a new collection by applying a function to all elements of this collection.
  *
  * @param input  the collection to be mapped
  * @param mapper the function to apply to each element.
  * @tparam IV     the element type of the input collection.
  * @tparam OV     the element type of the returned collection.
  * @return       a new collection of type `Coll[OV]` resulting from applying the given function
  *                `mapper` to each element of this collection and collecting the results.
  */
case class MapCollection[IV <: SType, OV <: SType](
                                                    input: Value[SCollection[IV]],
                                                    mapper: Value[SFunc])
  extends Transformer[SCollection[IV], SCollection[OV]] {
  override def companion = MapCollection
  override val tpe = SCollection[OV](mapper.tpe.tRange.asInstanceOf[OV])
  override val opType = SCollectionMethods.MapMethod.stype.asFunc
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    val mapperV = mapper.evalTo[Any => Any](env)
    val tResItem = Evaluation.stypeToRType(mapper.tpe.tRange).asInstanceOf[RType[Any]]
    addSeqCostNoOp(MapCollection.costKind, inputV.length)
    inputV.map(mapperV)(tResItem)
  }
}
object MapCollection extends ValueCompanion {
  override def opCode: OpCode = OpCodes.MapCollectionCode
  /** Cost of: 1) obtain result RType 2) invoke map method 3) allocation of resulting
    * collection */
  override val costKind = PerItemCost(
    baseCost = JitCost(20), perChunkCost = JitCost(1), chunkSize = 10)
}

/** Puts the elements of other collection `col2` after the elements of `input` collection
  * (concatenation of two collections).
  */
case class Append[IV <: SType](input: Value[SCollection[IV]], col2: Value[SCollection[IV]])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Append
  override val tpe = input.tpe
  override val opType = SCollectionMethods.AppendMethod.stype
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[IV#WrappedType]](env)
    val col2V = col2.evalTo[Coll[IV#WrappedType]](env)
    addSeqCost(Append.costKind, inputV.length + col2V.length) { () =>
      inputV.append(col2V)
    }
  }
}
object Append extends ValueCompanion {
  override def opCode: OpCode = OpCodes.AppendCode
  override val costKind = PerItemCost(
    baseCost = JitCost(20), perChunkCost = JitCost(2), chunkSize = 100)
}

/** Selects an interval of elements.  The returned collection is made up
  *  of all elements `x` which satisfy the invariant:
  *  {{{
  *    from <= indexOf(x) < until
  *  }}}
  *  @param from   the lowest index to include from this collection.
  *  @param until  the lowest index to EXCLUDE from this collection.
  */
case class Slice[IV <: SType](input: Value[SCollection[IV]], from: Value[SInt.type], until: Value[SInt.type])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Slice
  override val tpe = input.tpe
  override def opType = {
    val tpeColl = SCollection(input.tpe.typeParams.head.ident)
    SFunc(Array(tpeColl, SInt, SInt), tpeColl)
  }
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    val fromV = from.evalTo[Int](env)
    val untilV = until.evalTo[Int](env)
    val len = Math.max(0, untilV - fromV)
    addSeqCost(Slice.costKind, len) { () =>
      inputV.slice(fromV, untilV)
    }
  }
}
object Slice extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SliceCode
  override val costKind = PerItemCost(
    baseCost = JitCost(10), perChunkCost = JitCost(2), chunkSize = 100)
}

/** Selects all elements of `input` collection which satisfy the condition.
  *
  * @param input     the collection to be filtered
  * @param condition the predicate used to test elements.
  * @return a new collection consisting of all elements of this collection that satisfy
  *         the given `condition`. The order of the elements is preserved.
  */
case class Filter[IV <: SType](input: Value[SCollection[IV]],
                               condition: Value[SFunc])
  extends Transformer[SCollection[IV], SCollection[IV]] {
  override def companion = Filter
  override def tpe: SCollection[IV] = input.tpe
  override val opType = SCollectionMethods.FilterMethod.stype
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    val conditionV = condition.evalTo[Any => Boolean](env)
    addSeqCostNoOp(Filter.costKind, inputV.length)
    inputV.filter(conditionV)
  }
}
object Filter extends ValueCompanion {
  override def opCode: OpCode = OpCodes.FilterCode
  /** Cost of: 1) invoke Coll.filter method 2) allocation of resulting
    * collection */
  override val costKind = PerItemCost(
    baseCost = JitCost(20), perChunkCost = JitCost(1), chunkSize = 10)
}

/** Transforms a collection of values to a boolean (see [[Exists]], [[ForAll]]). */
trait BooleanTransformer[IV <: SType] extends Transformer[SCollection[IV], SBoolean.type] {
  override val input: Value[SCollection[IV]]
  val condition: Value[SFunc]
  override def tpe = SBoolean
}
trait BooleanTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

/** Tests whether a predicate holds for at least one element of this collection.
  *
  * @param input     the collection to be tested
  * @param condition the predicate used to test elements.
  * @return `true` if the given `condition` is satisfied by at least one element of this
  *         collection, otherwise `false`
  */
case class Exists[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = Exists
  override val opType = SCollectionMethods.ExistsMethod.stype
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    val conditionV = condition.evalTo[Any => Boolean](env)
    addSeqCostNoOp(Exists.costKind, inputV.length)
    inputV.exists(conditionV)
  }
}
object Exists extends BooleanTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExistsCode
  /** Cost of:  invoke exists method */
  override val costKind = PerItemCost(
    baseCost = JitCost(3), perChunkCost = JitCost(1), chunkSize = 10)
  override def argInfos: Seq[ArgInfo] = ExistsInfo.argInfos
}

/** Tests whether a predicate holds for all elements of this collection.
  *
  * @param input     the collection to be tested
  * @param condition the predicate used to test elements.
  * @return `true` if this collection is empty or the given `condition`
  *         holds for all elements of this collection, otherwise `false`.
  */
case class ForAll[IV <: SType](override val input: Value[SCollection[IV]],
                               override val condition: Value[SFunc])
  extends BooleanTransformer[IV] {
  override def companion = ForAll
  override val opType = SCollectionMethods.ForallMethod.stype
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    val conditionV = condition.evalTo[Any => Boolean](env)
    addSeqCostNoOp(ForAll.costKind, inputV.length)
    inputV.forall(conditionV)
  }
}
object ForAll extends BooleanTransformerCompanion {
  override def opCode: OpCode = OpCodes.ForAllCode
  /** Cost of:  invoke forall method */
  override val costKind = PerItemCost(
    baseCost = JitCost(3), perChunkCost = JitCost(1), chunkSize = 10)
  override def argInfos: Seq[ArgInfo] = ForAllInfo.argInfos
}

/** Applies a binary function to a start value and all elements of this collection,
  * going left to right.
  *
  *  @param   input the collection to iterate
  *  @param   zero  the start value.
  *  @param   foldOp the binary function.
  *  @tparam  OV the result type of the binary operator.
  *  @return  the result of inserting `foldOp` between consecutive elements of this collection,
  *           going left to right with the start value `zero` on the left:
  *           {{{
  *             foldOp(...foldOp(zero, x_1), x_2, ..., x_n)
  *           }}}
  *           where `x_1, ..., x_n` are the elements of this collection.
  *           Returns `zero` if this collection is empty.
  */
case class Fold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                          zero: Value[OV],
                                          foldOp: Value[SFunc])
  extends Transformer[SCollection[IV], OV] {
  override def companion = Fold
  implicit override def tpe: OV = zero.tpe
  val opType: SFunc = SCollectionMethods.FoldMethod.stype
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[IV#WrappedType]](env)
    val zeroV = zero.evalTo[OV#WrappedType](env)
    Value.checkType(zero, zeroV) // necessary because cast to OV#WrappedType is erased
    val foldOpV = foldOp.evalTo[((OV#WrappedType, IV#WrappedType)) => OV#WrappedType](env)
    addSeqCostNoOp(Fold.costKind, inputV.length)
    inputV.foldLeft(zeroV, foldOpV)
  }
}

object Fold extends ValueCompanion {
  override def opCode: OpCode = OpCodes.FoldCode
  override val costKind = PerItemCost(
    baseCost = JitCost(3), perChunkCost = JitCost(1), chunkSize = 10)
}

/** The element of the collection or default value.
  * If an index is out of bounds (`i < 0 || i >= length`) then `default` value is returned.
  *
  * @param  input the zero-based indexed collection
  * @param  index the index of the requested element (zero-based)
  * @tparam V the type of elements
  * @return the element at the given index or `default` value if index is out or bounds
  * @throws ArrayIndexOutOfBoundsException if `index < 0` or `length <= index`
  */
case class ByIndex[V <: SType](input: Value[SCollection[V]],
                               index: Value[SInt.type],
                               default: Option[Value[V]] = None)
  extends Transformer[SCollection[V], V] with NotReadyValue[V] {
  override def companion = ByIndex
  override val tpe = input.tpe.elemType
  override val opType = SCollectionMethods.ApplyMethod.stype.asFunc
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[V#WrappedType]](env)
    val indexV = index.evalTo[Int](env)
    default match {
      case Some(d) =>
        val dV = d.evalTo[V#WrappedType](env)
        Value.checkType(d, dV) // necessary because cast to V#WrappedType is erased
        addCost(ByIndex.costKind)
        inputV.getOrElse(indexV, dV)
      case _ =>
        addCost(ByIndex.costKind)
        inputV.apply(indexV)
    }
  }
}
object ByIndex extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.ByIndexCode
  override val costKind = FixedCost(JitCost(30))
}

/** Select tuple field by its 1-based index. E.g. input._1 is transformed to
  * SelectField(input, 1)
  */
case class SelectField(input: Value[STuple], fieldIndex: Byte)
  extends Transformer[STuple, SType] with NotReadyValue[SType] {
  override def companion = SelectField
  override val tpe = input.tpe.items(fieldIndex - 1)
  override val opType = SFunc(input.tpe, tpe)

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Any](env)
    addCost(SelectField.costKind)
    inputV match {
      case p: Tuple2[_,_] =>
        if (fieldIndex == 1) p._1
        else if (fieldIndex == 2) p._2
        else sys.error(s"We found an unknown field index ($fieldIndex) while trying to select from $p in the structure evaluation. " +
          "Please check that the field index is correct. " +
          "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
      case _ =>
        Value.typeError(input, inputV)
    }
  }
}
object SelectField extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.SelectFieldCode
  /** Cost of: 1) Calling Tuple2.{_1, _2} Scala methods.
    * Old cost: ("SelectField", "() => Unit", selectField) */
  override val costKind = FixedCost(JitCost(10))
  def typed[T <: SValue](input: Value[STuple], fieldIndex: Byte): T = {
    SelectField(input, fieldIndex).asInstanceOf[T]
  }
}

/** Represents execution of Sigma protocol that validates the given input SigmaProp. */
case class SigmaPropIsProven(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SBoolean.type] with NotReadyValueBoolean {
  override def companion = SigmaPropIsProven
  override val opType = SFunc(input.tpe, SBoolean)
}
object SigmaPropIsProven extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SigmaPropIsProvenCode
  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
}

/** Extract serialized bytes of a SigmaProp value */
case class SigmaPropBytes(input: Value[SSigmaProp.type])
  extends Transformer[SSigmaProp.type, SByteArray] with NotReadyValue[SByteArray] {
  override def companion = SigmaPropBytes
  override def tpe = SByteArray
  override val opType = SFunc(input.tpe, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[CSigmaProp](env)
    val numNodes = inputV.wrappedValue.size
    addSeqCost(SigmaPropBytes.costKind, numNodes) { () =>
      inputV.propBytes
    }
  }
}
object SigmaPropBytes extends PerItemCostValueCompanion {
  override def opCode: OpCode = OpCodes.SigmaPropBytesCode
  /** BaseCost: serializing one node of SigmaBoolean proposition
    * PerChunkCost: serializing one node of SigmaBoolean proposition */
  override val costKind = PerItemCost(
    baseCost = JitCost(35), perChunkCost = JitCost(6), chunkSize = 1)
}
trait SimpleTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

/** The length of the collection (aka size). */
case class SizeOf[V <: SType](input: Value[SCollection[V]])
  extends Transformer[SCollection[V], SInt.type] with NotReadyValueInt {
  override def companion = SizeOf
  override def opType = SizeOf.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Any]](env)
    addCost(SizeOf.costKind)
    inputV.length
  }
}
object SizeOf extends SimpleTransformerCompanion with FixedCostValueCompanion {
  val OpType = SFunc(SCollection(SType.tIV), SInt)
  override def opCode: OpCode = OpCodes.SizeOfCode
  /** Cost of: 1) calling Coll.length method (guaranteed to be O(1))
    * Twice the cost of SelectField.
    * Old cost: ("SizeOf", "(Coll[IV]) => Int", collLength) */
  override val costKind = FixedCost(JitCost(14))
  override def argInfos: Seq[ArgInfo] = SizeOfInfo.argInfos
}

sealed trait Extract[V <: SType] extends Transformer[SBox.type, V] {
}

/** Extracts the monetary value, in Ergo tokens (NanoErg unit of measure) from input Box. */
case class ExtractAmount(input: Value[SBox.type]) extends Extract[SLong.type] with NotReadyValueLong {
  override def companion = ExtractAmount
  override def opType = ExtractAmount.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractAmount.costKind)
    inputV.value
  }
}
object ExtractAmount extends SimpleTransformerCompanion with FixedCostValueCompanion {
  val OpType = SFunc(SBox, SLong)
  override def opCode: OpCode = OpCodes.ExtractAmountCode
  /** Cost of: 1) access `value` property of a [[sigma.Box]] */
  override val costKind = FixedCost(JitCost(8))
  override def argInfos: Seq[ArgInfo] = ExtractAmountInfo.argInfos
}

/** Extract serialized bytes of guarding script.
  * As a reminder, the script should be evaluated to true in order to
  * open this box. (aka spend it in a transaction).
  */
case class ExtractScriptBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractScriptBytes
  override def opType = ExtractScriptBytes.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractScriptBytes.costKind)
    inputV.propositionBytes
  }
}
object ExtractScriptBytes extends SimpleTransformerCompanion with FixedCostValueCompanion {
  val OpType = SFunc(SBox, SByteArray)
  override def opCode: OpCode = OpCodes.ExtractScriptBytesCode

  /** The cost is fixed and doesn't include serialization of ErgoTree because
    * the ErgoTree is expected to be constructed with non-null propositionBytes.
    * This is (and must be) guaranteed by ErgoTree deserializer.
    * CostOf: accessing ErgoBox.propositionBytes
    */
  override val costKind = FixedCost(JitCost(10))
  override def argInfos: Seq[ArgInfo] = ExtractScriptBytesInfo.argInfos
}

/** Extracts serialized bytes of this box's content, including proposition bytes. */
case class ExtractBytes(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytes
  override def opType = ExtractBytes.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractBytes.costKind)
    inputV.bytes
  }
}
object ExtractBytes extends SimpleTransformerCompanion {
  val OpType = SFunc(SBox, SByteArray)
  override def opCode: OpCode = OpCodes.ExtractBytesCode
  /** The cost is fixed and doesn't include serialization of ErgoBox because
    * the ErgoBox is expected to be constructed with non-null `bytes`.
    */
  override val costKind = FixedCost(JitCost(12))
  override def argInfos: Seq[ArgInfo] = ExtractBytesInfo.argInfos
}

/** Extracts serialized bytes of this box's content, excluding transactionId and index of output. */
case class ExtractBytesWithNoRef(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractBytesWithNoRef
  override def opType = ExtractBytesWithNoRef.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractBytesWithNoRef.costKind)
    inputV.bytesWithoutRef
  }
}
object ExtractBytesWithNoRef extends SimpleTransformerCompanion {
  val OpType = SFunc(SBox, SByteArray)
  override def opCode: OpCode = OpCodes.ExtractBytesWithNoRefCode

  /** The cost if fixed and doesn't include serialization of ErgoBox because
    * the ErgoBox is expected to be constructed with non-null `bytes`. */
  override val costKind = FixedCost(JitCost(12))

  override def argInfos: Seq[ArgInfo] = ExtractBytesWithNoRefInfo.argInfos
}

/** Extracts Blake2b256 hash of this box's content, basically equals to `blake2b256(bytes)` */
case class ExtractId(input: Value[SBox.type]) extends Extract[SByteArray] with NotReadyValueByteArray {
  override def companion = ExtractId
  override def opType = ExtractId.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractId.costKind)
    inputV.id
  }
}
object ExtractId extends SimpleTransformerCompanion {
  val OpType = SFunc(SBox, SByteArray)
  override def opCode: OpCode = OpCodes.ExtractIdCode
  /** CostOf: cost of computing hash from `ErgoBox.bytes` */
  override val costKind = FixedCost(JitCost(12))
  override def argInfos: Seq[ArgInfo] = ExtractIdInfo.argInfos
}

/** See [[Box.getReg()]]*/
case class ExtractRegisterAs[V <: SType]( input: Value[SBox.type],
                                          registerId: RegisterId,
                                          override val tpe: SOption[V])
  extends Extract[SOption[V]] with NotReadyValue[SOption[V]] {
  override def companion = ExtractRegisterAs
  override val opType = SFunc(ExtractRegisterAs.BoxAndByte, tpe)
  lazy val tV = Evaluation.stypeToRType(tpe.elemType)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractRegisterAs.costKind)
    inputV.getReg(registerId.number)(tV)
  }
}
object ExtractRegisterAs extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.ExtractRegisterAs
  /** CostOf: 1) accessing `registers` collection 2) comparing types 3) allocating Some()*/
  override val costKind = FixedCost(JitCost(50))

  //HOTSPOT:: avoids thousands of allocations per second
  private val BoxAndByte: IndexedSeq[SType] = Array(SBox, SByte)

  def apply[V <: SType](input: Value[SBox.type],
                        registerId: RegisterId)(implicit tpe: V): ExtractRegisterAs[V] =
    ExtractRegisterAs(input, registerId, SOption(tpe))
}

/**
  * Tuple of height when block got included into the blockchain and transaction identifier with box index in the transaction outputs serialized to the byte array.
  * @param input box
  */
case class ExtractCreationInfo(input: Value[SBox.type]) extends Extract[STuple] with NotReadyValue[STuple] {
  import ExtractCreationInfo._
  override def companion = ExtractCreationInfo
  override def tpe: STuple = ResultType
  override def opType = OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Box](env)
    addCost(ExtractCreationInfo.costKind)
    inputV.creationInfo
  }
}
object ExtractCreationInfo extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.ExtractCreationInfoCode
  override val costKind = FixedCost(JitCost(16))
  override def argInfos: Seq[ArgInfo] = ExtractCreationInfoInfo.argInfos
  val ResultType = STuple(SInt, SByteArray)
  val OpType = SFunc(SBox, ResultType)
}

trait Deserialize[V <: SType] extends NotReadyValue[V]

/** This ErgoTree operation work as macros (i.e. executed before ErgoTree reduction) and
  * allows to inline arbitrary expression at the point where it is used in ErgoTree.
  *
  * This operation is executed before ErgoTree reduction as the following:
  * 1) the context variable `id` is checked to have type Coll[Byte], if not, then this
  *    node remains in the ErgoTree which means the reduction will fail later.
  * 2) the bytes collection is deserialized to `expr: Value[V]` using ValueSerializer.deserialize
  * 3) `this` node is replaced with the deserialized `expr`
  *
  * This step are performed for each [[DeserializeContext]] node via single traverse of
  * ErgoTree. The resulting ErgoTree is passed to reduction.
  *
  * NOTE, the original `Coll[Byte]` from context variables is available as `getVar[Coll[Byte]](id)`
  *
  * @param id identifier of the context variable
  * @param tpe expected type of the deserialized script (i.e. expected type of `expr`).
  */
case class DeserializeContext[V <: SType](id: Byte, tpe: V) extends Deserialize[V] {
  override def companion = DeserializeContext
  override val opType = SFunc(SContextMethods.ContextFuncDom, tpe)
}
object DeserializeContext extends ValueCompanion {
  override def opCode: OpCode = OpCodes.DeserializeContextCode
  override val costKind = PerItemCost(
    baseCost = JitCost(1), perChunkCost = JitCost(10), chunkSize = 128)
}

/** Extract register of SELF box as Coll[Byte], deserialize it into Value and inline into executing script.
  * NOTE: it only applicable to SELF box
  */
case class DeserializeRegister[V <: SType](reg: RegisterId, tpe: V, default: Option[Value[V]] = None) extends Deserialize[V] {
  override def companion = DeserializeRegister
  override val opType = SFunc(Array(SBox, SByte, SOption(tpe)), tpe)
}
object DeserializeRegister extends ValueCompanion {
  override def opCode: OpCode = OpCodes.DeserializeRegisterCode
  override val costKind = PerItemCost(
    baseCost = JitCost(1), perChunkCost = JitCost(10), chunkSize = 128)
}

/** See [[sigma.Context.getVar()]] for detailed description. */
case class GetVar[V <: SType](varId: Byte, override val tpe: SOption[V]) extends NotReadyValue[SOption[V]] {
  override def companion = GetVar
  override val opType = SFunc(SContextMethods.ContextFuncDom, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val t = Evaluation.stypeToRType(tpe.elemType)
    addCost(GetVar.costKind)
    E.context.getVar(varId)(t)
  }
}
object GetVar extends FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.GetVarCode
  /** Cost of: 1) accessing to array of context vars by index
    * Old cost: ("GetVar", "(Context, Byte) => Option[T]", getVarCost) */
  override val costKind = FixedCost(JitCost(10))
  def apply[V <: SType](varId: Byte, innerTpe: V): GetVar[V] = GetVar[V](varId, SOption(innerTpe))
}

/** Returns the option's value.
  *
  *  @note The option must be nonempty.
  *  @throws java.util.NoSuchElementException if the option is empty.
  */
case class OptionGet[V <: SType](input: Value[SOption[V]]) extends Transformer[SOption[V], V] {
  override def companion = OptionGet
  override val opType = SFunc(input.tpe, tpe)
  override def tpe: V = input.tpe.elemType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val opt = input.evalTo[Option[V#WrappedType]](env)
    addCost(OptionGet.costKind)
    opt.get
  }
}
object OptionGet extends SimpleTransformerCompanion with FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.OptionGetCode
  /** Cost of: 1) Calling Option.get Scala method. */
  override val costKind = FixedCost(JitCost(15))
  override def argInfos: Seq[ArgInfo] = OptionGetInfo.argInfos
}

/** Returns the option's value if the option is nonempty, otherwise
  * return the result of evaluating `default`.
  * NOTE: the `default` is evaluated even if the option contains the value
  * i.e. not lazily.
  *
  *  @param default  the default expression.
  */
case class OptionGetOrElse[V <: SType](input: Value[SOption[V]], default: Value[V])
  extends Transformer[SOption[V], V] {
  override def companion = OptionGetOrElse
  override val opType = SFunc(IndexedSeq(input.tpe, tpe), tpe)
  override def tpe: V = input.tpe.elemType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Option[V#WrappedType]](env)
    val dV = default.evalTo[V#WrappedType](env)  // TODO v6.0: execute lazily (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/906)
    Value.checkType(default, dV) // necessary because cast to V#WrappedType is erased
    addCost(OptionGetOrElse.costKind)
    inputV.getOrElse(dV)
  }
}
object OptionGetOrElse extends ValueCompanion with FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.OptionGetOrElseCode
  /** Cost of: 1) Calling Option.getOrElse Scala method. */
  override val costKind = FixedCost(JitCost(20))
}

/** Returns false if the option is None, true otherwise. */
case class OptionIsDefined[V <: SType](input: Value[SOption[V]])
  extends Transformer[SOption[V], SBoolean.type] {
  override def companion = OptionIsDefined
  override val opType = SFunc(input.tpe, SBoolean)
  override def tpe= SBoolean
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Option[V#WrappedType]](env)
    addCost(OptionIsDefined.costKind)
    inputV.isDefined
  }
}
object OptionIsDefined extends SimpleTransformerCompanion with FixedCostValueCompanion {
  override def opCode: OpCode = OpCodes.OptionIsDefinedCode
  /** Cost of: 1) Calling Option.isDefined Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override def argInfos: Seq[ArgInfo] = OptionIsDefinedInfo.argInfos
}
