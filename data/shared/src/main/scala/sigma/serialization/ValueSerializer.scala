package sigma.serialization

import org.ergoplatform.validation.ValidationRules.CheckValidOpCode
import sigma.VersionContext
import sigma.ast.SCollection.SByteArray
import sigma.ast.TypeCodes.LastConstantCode
import sigma.ast._
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.util.Extensions.toUByte
import sigma.serialization.ValueCodes.OpCode
import sigma.serialization.transformers._
import sigma.serialization.trees.{QuadrupleSerializer, Relation2Serializer}
import sigma.utils.SparseArrayContainer
import sigma.exceptions.BuilderException

import scala.collection.mutable

abstract class ValueSerializer[V <: Value[SType]] extends SigmaSerializer[Value[SType], V] {

  def opDesc: ValueCompanion
  /** Code of the corresponding tree node (Value.opCode) which is used to lookup this serizalizer
    * during deserialization. It is emitted immediately before the body of this node in serialized byte array. */
  @inline final def opCode: OpCode = opDesc.opCode

  /** Returns only the direct [[Value]] fields emitted by this serializer, in their exact
    * serialization order. Non-Value metadata (types, ids, method descriptors, collection
    * lengths, compact bit payloads, etc.) is deliberately opaque.
    *
    * Every serializer registered for consensus traversal must declare this shape. Legacy
    * external subclasses inherit the fail-closed default below. This keeps structural
    * traversals tied to the consensus serializer instead of to reflection or an
    * independently maintained AST-product convention.
    */
  protected def getValueChildren(obj: V): IndexedSeq[Value[SType]] =
    error(s"Structural Value children are not declared for ${opDesc.typeName}")

  /** Reconstructs `obj` after replacing exactly the children returned by
    * [[getValueChildren]]. Implementations must preserve every serialized non-Value
    * field of `obj`. Arity and child types are checked by [[rebuildValue]] before this
    * method is called. Non-consensus source annotations are outside this contract.
    */
  protected def rebuildValueNode(
      obj: V,
      children: IndexedSeq[Value[SType]]): Value[SType] =
    error(s"Structural Value rebuild is not declared for ${opDesc.typeName}")

  /** Verifies that [[rebuildValueNode]] retained the replacement children and
    * preserved the serializer-owned wire shape. Serializers whose encoding has
    * an explicit compact/non-compact transition may override this hook, but
    * must still prove directly from the rebuilt AST that every replacement was
    * retained in order.
    */
  protected def validateRebuiltValue(
      obj: V,
      children: IndexedSeq[Value[SType]],
      rebuilt: Value[SType]): Unit = {
    if (rebuilt.companion != obj.companion || rebuilt.opCode != obj.opCode)
      error(s"Cannot rebuild ${opDesc.typeName}: serializer changed the node companion or opcode")

    val rebuiltChildren = getValueChildren(rebuilt.asInstanceOf[V])
    if (rebuiltChildren.length != children.length)
      error(s"Cannot rebuild ${opDesc.typeName}: serializer changed the structural arity")
    var i = 0
    while (i < children.length) {
      if (!(rebuiltChildren(i).asInstanceOf[AnyRef] eq children(i).asInstanceOf[AnyRef]))
        error(s"Cannot rebuild ${opDesc.typeName}: serializer did not retain replacement child $i")
      i += 1
    }
  }

  /** Untyped, registry-facing view of [[getValueChildren]]. */
  final def valueChildren(obj: Value[SType]): IndexedSeq[Value[SType]] = {
    val typedObj = checkedObject(obj)
    getValueChildren(typedObj)
  }

  /** Rebuilds a node from its direct serialized-Value children.
    *
    * The replacement is deliberately shape-preserving: the number of children and each
    * child's Sigma type must match [[valueChildren]] for the original node. Failures use
    * [[SerializerException]] with stable, serializer-owned diagnostics.
    */
  final def rebuildValue(
      obj: Value[SType],
      children: IndexedSeq[Value[SType]]): Value[SType] = {
    val typedObj = checkedObject(obj)
    if (children == null)
      error(s"Cannot rebuild ${opDesc.typeName}: children is null")

    val expected = getValueChildren(typedObj)
    if (children.length != expected.length)
      error(s"Cannot rebuild ${opDesc.typeName}: expected ${expected.length} Value children, got ${children.length}")

    var i = 0
    while (i < expected.length) {
      val child = children(i)
      if (child == null)
        error(s"Cannot rebuild ${opDesc.typeName}: child $i is null")
      val expectedType = expected(i).tpe
      if (child.tpe != expectedType)
        error(s"Cannot rebuild ${opDesc.typeName}: child $i has type ${child.tpe}, expected $expectedType")
      i += 1
    }

    val rebuilt = try rebuildValueNode(typedObj, children)
    catch {
      case e: SerializerException => throw e
      case _: ClassCastException =>
        error(s"Cannot rebuild ${opDesc.typeName}: a replacement child has an incompatible node class")
      case _: BuilderException =>
        error(s"Cannot rebuild ${opDesc.typeName}: constructor rejected the replacement children")
      case _: IllegalArgumentException =>
        error(s"Cannot rebuild ${opDesc.typeName}: constructor rejected the replacement children")
      case _: AssertionError =>
        error(s"Cannot rebuild ${opDesc.typeName}: constructor rejected the replacement children")
    }

    if (rebuilt == null)
      error(s"Cannot rebuild ${opDesc.typeName}: serializer returned null")
    if (rebuilt.tpe != obj.tpe)
      error(s"Cannot rebuild ${opDesc.typeName}: serializer changed type ${obj.tpe} to ${rebuilt.tpe}")
    validateRebuiltValue(typedObj, children, rebuilt)
    rebuilt
  }

  private def checkedObject(obj: Value[SType]): V = {
    if (obj == null)
      error(s"Cannot inspect ${opDesc.typeName}: value is null")
    if (obj.companion != opDesc)
      error(s"Cannot inspect ${opDesc.typeName}: got ${obj.companion.typeName}")
    obj.asInstanceOf[V]
  }
}

/** Implements serialization of ErgoTree expressions. Contains global collection of
  * serializers for each ErgoTree operation (see `serializers` field).
  *
  * It also implements optional (see collectSerInfo flag) metadata collection during serialization
  * to generate serializer specification tables in LaTeX.
  *
  * @see GenSerializers
  */
object ValueSerializer extends SigmaSerializerCompanion[Value[SType]] {
  type Tag = OpCode

  private val builder: DeserializationSigmaBuilder.type = DeserializationSigmaBuilder
  import builder._

  private val constantSerializer = ConstantSerializer(builder)
  private val constantPlaceholderSerializer = ConstantPlaceholderSerializer(mkConstantPlaceholder)

  val serializers: SparseArrayContainer[ValueSerializer[_ <: Value[SType]]] =
    SparseArrayContainer.buildForSerializers(Seq[ValueSerializer[_ <: Value[SType]]](
    constantSerializer,
    constantPlaceholderSerializer,
    TupleSerializer(mkTuple),
    SelectFieldSerializer(mkSelectField),
    Relation2Serializer(GT, mkGT[SType]),
    Relation2Serializer(GE, mkGE[SType]),
    Relation2Serializer(LT, mkLT[SType]),
    Relation2Serializer(LE, mkLE[SType]),
    Relation2Serializer(EQ, mkEQ[SType]),
    Relation2Serializer(NEQ, mkNEQ[SType]),
    CreateAvlTreeSerializer(mkCreateAvlTree),
    QuadrupleSerializer(TreeLookup, mkTreeLookup),
    Relation2Serializer(BinOr, mkBinOr),
    Relation2Serializer(BinAnd, mkBinAnd),
    Relation2Serializer(BinXor, mkBinXor),
    QuadrupleSerializer[SBoolean.type, SLong.type, SLong.type, SLong.type](If, mkIf),
    TwoArgumentsSerializer(Xor, mkXor),
    TwoArgumentsSerializer(Exponentiate, mkExponentiate),
    TwoArgumentsSerializer(MultiplyGroup, mkMultiplyGroup),
    TwoArgumentsSerializer(ArithOp.Minus, mkMinus[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Multiply, mkMultiply[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Division, mkDivide[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Modulo, mkModulo[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Plus, mkPlus[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Min, mkMin[SNumericType]),
    TwoArgumentsSerializer(ArithOp.Max, mkMax[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitOr, mkBitOr[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitAnd, mkBitAnd[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitXor, mkBitXor[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitShiftLeft, mkBitShiftLeft[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitShiftRight, mkBitShiftRight[SNumericType]),
    TwoArgumentsSerializer(BitOp.BitShiftRightZeroed, mkBitShiftRightZeroed[SNumericType]),
    SigmaPropIsProvenSerializer,
    SigmaPropBytesSerializer,
    ConcreteCollectionBooleanConstantSerializer(mkConcreteCollection),
    CaseObjectSerialization(TrueLeaf, TrueLeaf),
    CaseObjectSerialization(FalseLeaf, FalseLeaf),
    CaseObjectSerialization(Context, Context),
    CaseObjectSerialization(Global, Global),
    CaseObjectSerialization(Height, Height),
    CaseObjectSerialization(MinerPubkey, MinerPubkey),
    CaseObjectSerialization(Inputs, Inputs),
    CaseObjectSerialization(Outputs, Outputs),
    CaseObjectSerialization(LastBlockUtxoRootHash, LastBlockUtxoRootHash),
    CaseObjectSerialization(Self, Self),
    CaseObjectSerialization(GroupGenerator, GroupGenerator),
    ConcreteCollectionSerializer(mkConcreteCollection),
    LogicalTransformerSerializer(AND, mkAND),
    LogicalTransformerSerializer(OR, mkOR),
    LogicalTransformerSerializer(XorOf, mkXorOf),
    // Opcode 0x71. Deprecated; scheduled for version-gated rejection in Phase 2 of the retirement plan.
    // The rejection target is v7.0. DO NOT remove the entry and DO NOT reassign opcode 0x71.
    // Doing so changes consensus.
    TaggedVariableSerializer(mkTaggedVariable),
    GetVarSerializer(mkGetVar),
    MapCollectionSerializer(mkMapCollection),
    BooleanTransformerSerializer[SType](Exists, mkExists),
    BooleanTransformerSerializer[SType](ForAll, mkForAll),
    FoldSerializer(mkFold),
    SimpleTransformerSerializer[SCollection[SType], SInt.type](SizeOf, mkSizeOf),
    SimpleTransformerSerializer[SBox.type, SLong.type](ExtractAmount, mkExtractAmount),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractScriptBytes, mkExtractScriptBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytes, mkExtractBytes),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractBytesWithNoRef, mkExtractBytesWithNoRef),
    SimpleTransformerSerializer[SBox.type, SByteArray](ExtractId, mkExtractId),
    SimpleTransformerSerializer[SBox.type, STuple](ExtractCreationInfo, mkExtractCreationInfo),
    SimpleTransformerSerializer[SLong.type, SByteArray](LongToByteArray, mkLongToByteArray),
    SimpleTransformerSerializer[SByteArray, SLong.type](ByteArrayToLong, mkByteArrayToLong),
    SimpleTransformerSerializer[SByteArray, SBigInt.type](ByteArrayToBigInt, mkByteArrayToBigInt),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcBlake2b256, mkCalcBlake2b256),
    SimpleTransformerSerializer[SByteArray, SByteArray](CalcSha256, mkCalcSha256),
    SimpleTransformerSerializer[SByteArray, SGroupElement.type](DecodePoint, mkDecodePoint),
    SimpleTransformerSerializer[SOption[SType], SType](OptionGet, mkOptionGet),
    SimpleTransformerSerializer[SOption[SType], SBoolean.type](OptionIsDefined, mkOptionIsDefined),
    OptionGetOrElseSerializer(mkOptionGetOrElse),
    DeserializeContextSerializer(mkDeserializeContext),
    DeserializeRegisterSerializer(mkDeserializeRegister),
    ExtractRegisterAsSerializer(mkExtractRegisterAs),
    FilterSerializer(mkFilter),
    SliceSerializer(mkSlice),
    AtLeastSerializer(mkAtLeast),
    ByIndexSerializer(mkByIndex),
    AppendSerializer(mkAppend),
    NumericCastSerializer(Upcast, mkUpcast),
    NumericCastSerializer(Downcast, mkDowncast),
    ValDefSerializer(ValDef),
    ValDefSerializer(FunDef),
    BlockValueSerializer(mkBlockValue),
    ValUseSerializer(mkValUse),
    FuncValueSerializer(mkFuncValue),
    ApplySerializer(mkApply),
    PropertyCallSerializer(mkMethodCall),
    MethodCallSerializer(mkMethodCall),
    SigmaTransformerSerializer(SigmaAnd, mkSigmaAnd),
    SigmaTransformerSerializer(SigmaOr, mkSigmaOr),
    BoolToSigmaPropSerializer(mkBoolToSigmaProp),

    // NOTE: these ModQ serializers can be removed only as part of hard-fork
    // because their removal may break deserialization of transaction, when for example
    // ModQ operation happen to be in one of the outputs (i.e. script is not executed
    // during validation, however deserializer is still used)
    ModQSerializer,
    ModQArithOpSerializer(ModQArithOp.PlusModQ, mkPlusModQ),
    ModQArithOpSerializer(ModQArithOp.MinusModQ, mkMinusModQ),

    SubstConstantsSerializer,
    CreateProveDlogSerializer(mkCreateProveDlog),
    CreateProveDHTupleSerializer(mkCreateProveDHTuple),
    LogicalNotSerializer(mkLogicalNot),
    OneArgumentOperationSerializer(Negation, mkNegation[SNumericType]),
    OneArgumentOperationSerializer(BitInversion, mkBitInversion[SNumericType]),
    VerifyStarkSerializer
  ))

  private def serializable(v: Value[SType]): Value[SType] = {
    // starting with v3 trees, we preserve tree tree as is during
    // serialization, before, serializer strips leading Upcast
    if (VersionContext.current.isV3OrLaterErgoTreeVersion) {
      v
    } else {
      v match {
        case upcast: Upcast[SNumericType, _] @unchecked =>
          upcast.input
        case _ => v
      }
    }
  }

  override def getSerializer(opCode: OpCode): ValueSerializer[_ <: Value[SType]] = {
    val serializer = serializers(opCode)
    CheckValidOpCode(serializer, opCode)
    serializer
  }
  def addSerializer(opCode: OpCode, ser: ValueSerializer[_ <: Value[SType]]): Unit = {
    serializers.add(opCode, ser)
  }
  def removeSerializer(opCode: OpCode): Unit = {
    serializers.remove(opCode)
  }

  type ChildrenMap = mutable.ArrayBuffer[(String, Scope)]
  trait Scope {
    def name: String
    def parent: Scope
    def children: ChildrenMap
    def get(name: String): Option[Scope] = children.find(_._1 == name).map(_._2)
    def add(name: String, s: Scope): ChildrenMap = {
      assert(get(name).isEmpty, s"Error while adding scope $s: name $name already exists in $this")
      children += (name -> s)
    }
    def showInScope(v: String): String

    def provideScope(n: String, createNewScope: => Scope): Scope = {
      val scope = get(n) match {
        case Some(saved) => saved
        case None =>
          val newScope = createNewScope
          add(n, newScope)
          newScope
      }
      scope
    }
  }

  case class SerScope(opCode: OpCode, children: ChildrenMap) extends Scope {
    private def serializer = getSerializer(opCode)
    def name = s"Serializer of ${serializer.opDesc}"
    override def parent: Scope = null
    override def showInScope(v: String): String = name + "/" + v
    override def toString: String = s"SerScope(${serializer.opDesc}, $children)"
  }

  case class DataScope(parent: Scope, data: DataInfo[_]) extends Scope {
    override def name: String = data.info.name
    override def children: ChildrenMap = mutable.ArrayBuffer.empty
    override def showInScope(v: String): String = parent.showInScope(s"DataInfo($data)")
    override def toString = s"DataScope($data)"
  }

  case class OptionalScope(parent: Scope, name: String, children: ChildrenMap) extends Scope {
    override def showInScope(v: String): String = parent.showInScope(s"/opt[$name]/$v")
    override def toString = s"OptionalScope($name, $children)"
  }

  case class CasesScope(parent: Scope, matchExpr: String, children: ChildrenMap) extends Scope {
    override def name: String = matchExpr
    def cases: Seq[WhenScope] = children.map {
      case (_, when: WhenScope) => when
      case s => sys.error(s"Invalid child scope $s in $this")
    }.sortBy(_.pos).toSeq
    override def showInScope(v: String): String = parent.showInScope(s"/cases[$name]/$v")
    override def toString = s"CasesScope($name, $children)"
  }

  case class WhenScope(parent: Scope, pos: Int, condition: String, children: ChildrenMap) extends Scope {
    override def name: String = condition
    override def showInScope(v: String): String = parent.showInScope(s"/when[$pos: $condition]/$v")
    override def toString = s"WhenScope($pos, $condition, $children)"
    def isOtherwise: Boolean = condition == otherwiseCondition
  }

  case class ForScope(parent: Scope, name: String, limitVar: String, children: ChildrenMap) extends Scope {
    override def showInScope(v: String): String = parent.showInScope(s"/for[$name]/$v")
    override def toString = s"ForScope($name, $children)"
  }

  case class OptionScope(parent: Scope, name: String, children: ChildrenMap) extends Scope {
    override def showInScope(v: String): String = parent.showInScope(s"/option[$name]/$v")
    override def toString = s"OptionScope($name, $children)"
  }

  val collectSerInfo: Boolean = false
  val serializerInfo: mutable.Map[OpCode, SerScope] = mutable.HashMap.empty
  private var scopeStack: List[Scope] = Nil

  def printSerInfo(): String = {
    serializerInfo.map { case (_, s) =>
      getSerializer(s.opCode) // sanity check to make sure only resolvable serializers got printed
      s.toString
    }.mkString("\n")
  }

  def optional(name: String)(block: => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val scope = parent.provideScope(name, OptionalScope(parent, name, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      block
      scopeStack = scopeStack.tail
    } else {
      block
    }
  }

  def cases(matchExpr: String)(block: => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val scope = parent.provideScope(matchExpr, CasesScope(parent, matchExpr, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      block
      scopeStack = scopeStack.tail
    } else {
      block
    }
  }

  def when(pos: Int, condition: String)(block: => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val scope = parent.provideScope(condition, WhenScope(parent, pos, condition, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      block
      scopeStack = scopeStack.tail
    } else {
      block
    }
  }

  val otherwiseCondition = "otherwise"

  def otherwise(block: => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val scope = parent.provideScope(otherwiseCondition, WhenScope(parent, Int.MaxValue, otherwiseCondition, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      block
      scopeStack = scopeStack.tail
    } else {
      block
    }
  }

  def foreach[T](sizeVar: String, seq: Seq[T])(f: T => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val forName = sizeVar + "*"
      val scope = parent.provideScope(forName, ForScope(parent, forName, sizeVar, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      seq.foreach(f)
      scopeStack = scopeStack.tail
    } else {
      seq.foreach(f)
    }
  }

  def opt[T](w: SigmaByteWriter, name: String, o: Option[T])(f: (SigmaByteWriter, T) => Unit): Unit = {
    if (scopeStack.nonEmpty) {
      val parent = scopeStack.head
      val scope = parent.provideScope(name, OptionScope(parent, name, mutable.ArrayBuffer.empty))

      scopeStack ::= scope
      w.putOption(o)(f)
      scopeStack = scopeStack.tail
    } else {
      w.putOption(o)(f)
    }
  }

  def addArgInfo[T](prop: DataInfo[T]): Unit = {
    if (scopeStack.isEmpty) return
    val scope = scopeStack.head
    scope.get(prop.info.name) match {
      case None =>
        scope.add(prop.info.name, DataScope(scope, prop))
        println(s"Added $prop to $scope")
      case Some(saved) => saved match {
        case DataScope(_, data) =>
          assert(data == prop, s"Saved property $data is different from being added $prop: scope $scope")
        case _ =>
          sys.error(s"Expected DataScope, but found $saved: while adding $prop to scope $scope")
      }
    }
  }

  override def serialize(v: Value[SType], w: SigmaByteWriter): Unit = serializable(v) match {
    case c: Constant[SType] =>
      w.constantExtractionStore match {
        case Some(constantStore) =>
          val ph = constantStore.put(c)(DeserializationSigmaBuilder)
          w.put(ph.opCode)
          constantPlaceholderSerializer.serialize(ph, w)
        case None =>
          constantSerializer.serialize(c, w)
      }
    case _ =>
      val opCode = v.opCode
      // help compiler recognize the type
      val ser = getSerializer(opCode).asInstanceOf[ValueSerializer[v.type]]
      if (collectSerInfo) {
        val scope = serializerInfo.get(opCode) match {
          case None =>
            val newScope = SerScope(opCode, mutable.ArrayBuffer.empty)
            serializerInfo += (opCode -> newScope)
            println(s"Added: ${ser.opDesc}")
            newScope
          case Some(scope) => scope
        }
        w.put(opCode)

        scopeStack ::= scope
        ser.serialize(v, w)
        scopeStack = scopeStack.tail
      } else {
        w.put(opCode)
        ser.serialize(v, w)
      }
  }

  override def deserialize(r: SigmaByteReader): Value[SType] = {
    val depth = r.level
    r.level = depth + 1
    val firstByte = toUByte(r.peekByte())
    val v = if (firstByte <= LastConstantCode) {
      // look ahead byte tell us this is going to be a Constant
      constantSerializer.deserialize(r)
    }
    else {
      val opCode = r.getByte().asInstanceOf[OpCode]
      val ser = getSerializer(opCode)
      ser.parse(r)
    }
    r.level = r.level - 1
    v
  }

  def serialize(v: Value[SType]): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serialize(v, w)
    w.toBytes
  }

  def deserialize(bytes: Array[Byte], pos: SigmaSerializer.Position = 0): Value[SType] =
    deserialize(SigmaSerializer.startReader(bytes, pos))

}
