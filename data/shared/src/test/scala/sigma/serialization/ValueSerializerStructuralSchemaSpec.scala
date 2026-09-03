package sigma.serialization

import org.ergoplatform.ErgoBox
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.VLQByteBufferWriter
import sigma.VersionContext
import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.ast._
import sigma.serialization.OpCodes.{PlusCode, PlusModQCode}

import scala.collection.mutable.ArrayBuffer

class ValueSerializerStructuralSchemaSpec extends AnyPropSpec with Matchers {

  private final case class Fixture(name: String, node: Value[SType])
  private final case class StructuralSchema(
      name: String,
      fixtureName: String,
      opDescs: IndexedSeq[ValueCompanion])

  private final class RecordingWriter
    extends SigmaByteWriter(
      new VLQByteBufferWriter(new ByteArrayBuilder()),
      constantExtractionStore = None,
      addFixedCostCallbackOpt = None,
      addPerItemCostCallbackOpt = None) {

    val values: ArrayBuffer[Value[SType]] = ArrayBuffer.empty

    override def putValue[T <: SType](x: Value[T]): this.type = {
      values += x
      this
    }

    override def putValue[T <: SType](
        x: Value[T],
        info: CoreByteWriter.DataInfo[sigma.ast.syntax.SValue]): this.type = {
      values += x
      this
    }
  }

  private final class BrokenTupleSerializer(
      result: (Tuple, IndexedSeq[Value[SType]]) => Value[SType])
    extends ValueSerializer[Tuple] {
    override def opDesc: ValueCompanion = Tuple
    override protected def getValueChildren(obj: Tuple): IndexedSeq[Value[SType]] = obj.items
    override protected def rebuildValueNode(
        obj: Tuple,
        children: IndexedSeq[Value[SType]]): Value[SType] = result(obj, children)
    override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = ()
    override def parse(r: SigmaByteReader): Value[SType] = Tuple(Value.EmptySeq)
  }

  /** Models a source/binary extension compiled before the structural API was
    * added. The inherited defaults must preserve linkage but fail closed.
    */
  private final class LegacyTupleSerializer extends ValueSerializer[Tuple] {
    override def opDesc: ValueCompanion = Tuple
    override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = ()
    override def parse(r: SigmaByteReader): Value[SType] = Tuple(Value.EmptySeq)
  }

  private final class ChildrenOnlyTupleSerializer extends ValueSerializer[Tuple] {
    override def opDesc: ValueCompanion = Tuple
    override protected def getValueChildren(
        obj: Tuple): IndexedSeq[Value[SType]] = obj.items
    override def serialize(obj: Tuple, w: SigmaByteWriter): Unit = ()
    override def parse(r: SigmaByteReader): Value[SType] = Tuple(Value.EmptySeq)
  }

  private def fixtures: IndexedSeq[Fixture] = {
    val intCollection = ConcreteCollection[SInt.type](
      IndexedSeq(IntConstant(1), IntConstant(2)), SInt)
    val otherIntCollection = ConcreteCollection[SInt.type](
      IndexedSeq(IntConstant(3)), SInt)
    val compactBooleans = ConcreteCollection[SBoolean.type](
      IndexedSeq(TrueLeaf, FalseLeaf), SBoolean)
    val booleanExpressions = ConcreteCollection[SBoolean.type](
      IndexedSeq(TaggedVariable(1.toByte, SBoolean)), SBoolean)
    val identity = FuncValue(
      IndexedSeq(1 -> SInt),
      ValUse(1, SInt))
    val predicate = FuncValue(
      IndexedSeq(1 -> SInt),
      TrueLeaf)
    val foldFunction = FuncValue(
      IndexedSeq(1 -> SInt, 2 -> SInt),
      ArithOp(ValUse(1, SInt), ValUse(2, SInt), PlusCode))
    val sigmaProp = CreateProveDlog(GroupGenerator)
    val sigmaProps = ConcreteCollection[SSigmaProp.type](IndexedSeq(sigmaProp), SSigmaProp)
    val optionInt = GetVar(1.toByte, SInt)
    val valDef = ValDef(1, STypeVar.EmptySeq, IntConstant(1))
    val funDef = ValDef(2, IndexedSeq(STypeVar("T")), IntConstant(2))
    val tuple = Tuple(IndexedSeq(IntConstant(1), LongConstant(2L)))
    val methodCall = MethodCall(
      intCollection,
      SCollectionMethods.IndexOfMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
      IndexedSeq(IntConstant(1), IntConstant(0)),
      EmptySubst)
    val propertyCall = MethodCall(Self, SBoxMethods.ValueMethod, Value.EmptySeq, EmptySubst)
    val proofChunks = ConcreteCollection[SByteArray](
      IndexedSeq(ByteArrayConstant(Array[Byte](1, 2))), SByteArray)

    IndexedSeq(
      Fixture("BlockValueSerializer", BlockValue(IndexedSeq(valDef), ValUse(1, SInt))),
      Fixture("CaseObjectSerialization", Height),
      Fixture("ApplySerializer", Apply(identity, IndexedSeq(IntConstant(7)))),
      Fixture("BoolToSigmaPropSerializer", BoolToSigmaProp(TrueLeaf)),
      Fixture("FuncValueSerializer", identity),
      Fixture("ConcreteCollectionBooleanConstantSerializer", compactBooleans),
      Fixture("ModQArithOpSerializer", ModQArithOp(BigIntConstant(1L), BigIntConstant(2L), PlusModQCode)),
      Fixture("MethodCallSerializer", methodCall),
      Fixture("OneArgumentOperationSerializer", Negation(IntConstant(1))),
      Fixture("LogicalNotSerializer", LogicalNot(TrueLeaf)),
      Fixture("ConstantSerializer", IntConstant(1)),
      Fixture("OptionGetOrElseSerializer", OptionGetOrElse(optionInt, IntConstant(0))),
      Fixture("ModQSerializer", ModQ(BigIntConstant(1L))),
      Fixture("CreateProveDlogSerializer", sigmaProp),
      Fixture("GetVarSerializer", optionInt),
      Fixture("CreateAvlTreeSerializer", CreateAvlTree(
        ByteConstant(1), ByteArrayConstant(Array[Byte](1)), IntConstant(32), optionInt)),
      Fixture("ConstantPlaceholderSerializer", ConstantPlaceholder(0, SInt)),
      Fixture("ConcreteCollectionSerializer", intCollection),
      Fixture("PropertyCallSerializer", propertyCall),
      Fixture("SelectFieldSerializer", SelectField(tuple, 1.toByte)),
      Fixture("SigmaPropBytesSerializer", SigmaPropBytes(sigmaProp)),
      Fixture("VerifyStarkSerializer", VerifyStark(
        proofChunks,
        ByteArrayConstant(Array[Byte](3)),
        ByteArrayConstant(Array[Byte](4)),
        ByteArrayConstant(Array[Byte](5)))),
      Fixture("ValUseSerializer", ValUse(1, SInt)),
      Fixture("TupleSerializer", tuple),
      Fixture("SubstConstantsSerializer", SubstConstants(
        ByteArrayConstant(Array[Byte](1)),
        ConcreteCollection[SInt.type](IndexedSeq(IntConstant(0)), SInt).asInstanceOf[Value[SIntArray]],
        intCollection)),
      Fixture("TaggedVariableSerializer", TaggedVariable(1.toByte, SInt)),
      Fixture("SigmaPropIsProvenSerializer", SigmaPropIsProven(sigmaProp)),
      Fixture("TwoArgumentsSerializer", ArithOp(IntConstant(1), IntConstant(2), PlusCode)),
      Fixture("ValDefSerializer", valDef),
      Fixture("ValDefSerializer-FunDef-branch", funDef),
      Fixture("AppendSerializer", Append(intCollection, otherIntCollection)),
      Fixture("AtLeastSerializer", AtLeast(IntConstant(1), sigmaProps)),
      Fixture("BooleanTransformerSerializer", Exists(intCollection, predicate)),
      Fixture("Relation2Serializer", EQ(IntConstant(1), IntConstant(2))),
      Fixture("ByIndexSerializer", ByIndex(intCollection, IntConstant(0), Some(IntConstant(9)))),
      Fixture("CreateProveDHTupleSerializer", CreateProveDHTuple(
        GroupGenerator, GroupGenerator, GroupGenerator, GroupGenerator)),
      Fixture("QuadrupleSerializer", If(TrueLeaf, IntConstant(1), IntConstant(2))),
      Fixture("DeserializeContextSerializer", DeserializeContext(1.toByte, SInt)),
      Fixture("DeserializeRegisterSerializer", DeserializeRegister(
        ErgoBox.R4, SInt, Some(IntConstant(1)))),
      Fixture("ExtractRegisterAsSerializer", ExtractRegisterAs(Self, ErgoBox.R4, SOption(SInt))),
      Fixture("FilterSerializer", Filter(intCollection, predicate)),
      Fixture("FoldSerializer", Fold(intCollection, IntConstant(0), foldFunction)),
      Fixture("LogicalTransformerSerializer", AND(booleanExpressions)),
      Fixture("MapCollectionSerializer", MapCollection(intCollection, identity)),
      Fixture("SliceSerializer", Slice(intCollection, IntConstant(0), IntConstant(1))),
      Fixture("SimpleTransformerSerializer", SizeOf(intCollection)),
      Fixture("SigmaTransformerSerializer", SigmaAnd(IndexedSeq(sigmaProp))),
      Fixture("NumericCastSerializer", Upcast(ByteConstant(1), SInt))
    )
  }

  private def serializerFor(node: Value[SType]): ValueSerializer[Value[SType]] =
    ValueSerializer.getSerializer(node.opCode).asInstanceOf[ValueSerializer[Value[SType]]]

  private lazy val fixtureByName: Map[String, Fixture] =
    fixtures.map(fixture => fixture.name -> fixture).toMap

  /**
    * One schema entry per direct-Value child order/arity witness. A shared
    * serializer class may cover several opcode instances only when every
    * listed opcode is registered with that exact implementation class.
    */
  private lazy val structuralSchemas: IndexedSeq[StructuralSchema] = IndexedSeq(
    StructuralSchema("BlockValueSerializer", "BlockValueSerializer", IndexedSeq(BlockValue)),
    StructuralSchema("CaseObjectSerialization", "CaseObjectSerialization", IndexedSeq(
      TrueLeaf, FalseLeaf, Context, Global, Height, MinerPubkey, Inputs, Outputs,
      LastBlockUtxoRootHash, Self, GroupGenerator)),
    StructuralSchema("ApplySerializer", "ApplySerializer", IndexedSeq(Apply)),
    StructuralSchema("BoolToSigmaPropSerializer", "BoolToSigmaPropSerializer", IndexedSeq(BoolToSigmaProp)),
    StructuralSchema("FuncValueSerializer", "FuncValueSerializer", IndexedSeq(FuncValue)),
    StructuralSchema("ConcreteCollectionBooleanConstantSerializer", "ConcreteCollectionBooleanConstantSerializer", IndexedSeq(ConcreteCollectionBooleanConstant)),
    StructuralSchema("ModQArithOpSerializer", "ModQArithOpSerializer", IndexedSeq(
      ModQArithOp.PlusModQ, ModQArithOp.MinusModQ)),
    StructuralSchema("MethodCallSerializer", "MethodCallSerializer", IndexedSeq(MethodCall)),
    StructuralSchema("OneArgumentOperationSerializer", "OneArgumentOperationSerializer", IndexedSeq(Negation, BitInversion)),
    StructuralSchema("LogicalNotSerializer", "LogicalNotSerializer", IndexedSeq(LogicalNot)),
    StructuralSchema("ConstantSerializer", "ConstantSerializer", IndexedSeq(Constant)),
    StructuralSchema("OptionGetOrElseSerializer", "OptionGetOrElseSerializer", IndexedSeq(OptionGetOrElse)),
    StructuralSchema("ModQSerializer", "ModQSerializer", IndexedSeq(ModQ)),
    StructuralSchema("CreateProveDlogSerializer", "CreateProveDlogSerializer", IndexedSeq(CreateProveDlog)),
    StructuralSchema("GetVarSerializer", "GetVarSerializer", IndexedSeq(GetVar)),
    StructuralSchema("CreateAvlTreeSerializer", "CreateAvlTreeSerializer", IndexedSeq(CreateAvlTree)),
    StructuralSchema("ConstantPlaceholderSerializer", "ConstantPlaceholderSerializer", IndexedSeq(ConstantPlaceholder)),
    StructuralSchema("ConcreteCollectionSerializer", "ConcreteCollectionSerializer", IndexedSeq(ConcreteCollection)),
    StructuralSchema("PropertyCallSerializer", "PropertyCallSerializer", IndexedSeq(PropertyCall)),
    StructuralSchema("SelectFieldSerializer", "SelectFieldSerializer", IndexedSeq(SelectField)),
    StructuralSchema("SigmaPropBytesSerializer", "SigmaPropBytesSerializer", IndexedSeq(SigmaPropBytes)),
    StructuralSchema("VerifyStarkSerializer", "VerifyStarkSerializer", IndexedSeq(VerifyStark)),
    StructuralSchema("ValUseSerializer", "ValUseSerializer", IndexedSeq(ValUse)),
    StructuralSchema("TupleSerializer", "TupleSerializer", IndexedSeq(Tuple)),
    StructuralSchema("SubstConstantsSerializer", "SubstConstantsSerializer", IndexedSeq(SubstConstants)),
    StructuralSchema("TaggedVariableSerializer", "TaggedVariableSerializer", IndexedSeq(TaggedVariable)),
    StructuralSchema("SigmaPropIsProvenSerializer", "SigmaPropIsProvenSerializer", IndexedSeq(SigmaPropIsProven)),
    StructuralSchema("TwoArgumentsSerializer", "TwoArgumentsSerializer", IndexedSeq(
      Xor, Exponentiate, MultiplyGroup, ArithOp.Minus, ArithOp.Multiply,
      ArithOp.Division, ArithOp.Modulo, ArithOp.Plus, ArithOp.Min, ArithOp.Max,
      BitOp.BitOr, BitOp.BitAnd, BitOp.BitXor, BitOp.BitShiftLeft,
      BitOp.BitShiftRight, BitOp.BitShiftRightZeroed)),
    StructuralSchema("ValDefSerializer", "ValDefSerializer", IndexedSeq(ValDef)),
    StructuralSchema("ValDefSerializer-FunDef", "ValDefSerializer-FunDef-branch", IndexedSeq(FunDef)),
    StructuralSchema("AppendSerializer", "AppendSerializer", IndexedSeq(Append)),
    StructuralSchema("AtLeastSerializer", "AtLeastSerializer", IndexedSeq(AtLeast)),
    StructuralSchema("BooleanTransformerSerializer", "BooleanTransformerSerializer", IndexedSeq(Exists, ForAll)),
    StructuralSchema("Relation2Serializer", "Relation2Serializer", IndexedSeq(
      GT, GE, LT, LE, EQ, NEQ, BinOr, BinAnd, BinXor)),
    StructuralSchema("ByIndexSerializer", "ByIndexSerializer", IndexedSeq(ByIndex)),
    StructuralSchema("CreateProveDHTupleSerializer", "CreateProveDHTupleSerializer", IndexedSeq(CreateProveDHTuple)),
    StructuralSchema("QuadrupleSerializer", "QuadrupleSerializer", IndexedSeq(TreeLookup, If)),
    StructuralSchema("DeserializeContextSerializer", "DeserializeContextSerializer", IndexedSeq(DeserializeContext)),
    StructuralSchema("DeserializeRegisterSerializer", "DeserializeRegisterSerializer", IndexedSeq(DeserializeRegister)),
    StructuralSchema("ExtractRegisterAsSerializer", "ExtractRegisterAsSerializer", IndexedSeq(ExtractRegisterAs)),
    StructuralSchema("FilterSerializer", "FilterSerializer", IndexedSeq(Filter)),
    StructuralSchema("FoldSerializer", "FoldSerializer", IndexedSeq(Fold)),
    StructuralSchema("LogicalTransformerSerializer", "LogicalTransformerSerializer", IndexedSeq(AND, OR, XorOf)),
    StructuralSchema("MapCollectionSerializer", "MapCollectionSerializer", IndexedSeq(MapCollection)),
    StructuralSchema("SliceSerializer", "SliceSerializer", IndexedSeq(Slice)),
    StructuralSchema("SimpleTransformerSerializer", "SimpleTransformerSerializer", IndexedSeq(
      SizeOf, ExtractAmount, ExtractScriptBytes, ExtractBytes, ExtractBytesWithNoRef,
      ExtractId, ExtractCreationInfo, LongToByteArray, ByteArrayToLong,
      ByteArrayToBigInt, CalcBlake2b256, CalcSha256, DecodePoint, OptionGet,
      OptionIsDefined)),
    StructuralSchema("SigmaTransformerSerializer", "SigmaTransformerSerializer", IndexedSeq(SigmaAnd, SigmaOr)),
    StructuralSchema("NumericCastSerializer", "NumericCastSerializer", IndexedSeq(Upcast, Downcast))
  )

  private def recordedChildren(
      serializer: ValueSerializer[Value[SType]],
      node: Value[SType]): IndexedSeq[Value[SType]] = {
    val writer = new RecordingWriter
    serializer.serialize(node, writer)
    writer.values.toIndexedSeq
  }

  property("all 103 registered opcode instances have an explicit structural schema") {
    val registered = ArrayBuffer.empty[(Byte, ValueSerializer[_ <: Value[SType]])]
    (0 to 255).foreach { code =>
      val serializer = ValueSerializer.serializers(code.toByte)
      if (serializer != null) registered += code.toByte -> serializer
    }
    val schemaEntries = structuralSchemas.flatMap { schema =>
      schema.opDescs.map(_ -> schema)
    }
    val registeredOpDescs = registered.map(_._2.opDesc).toSet
    val schemaOpDescs = schemaEntries.map(_._1).toSet

    registered.size shouldBe 103
    schemaEntries.size shouldBe 103
    schemaOpDescs.size shouldBe 103
    schemaOpDescs shouldBe registeredOpDescs

    registered.foreach { case (opCode, serializer) =>
      withClue(f"opcode 0x${opCode & 0xff}%02x (${serializer.opDesc.typeName}): ") {
        val schema = schemaEntries.collectFirst {
          case (opDesc, candidate) if opDesc == serializer.opDesc => candidate
        }.getOrElse(fail("missing structural schema"))
        val fixture = fixtureByName.getOrElse(schema.fixtureName,
          fail(s"missing fixture for structural schema ${schema.name}"))

        opCode shouldBe serializer.opCode
        serializer.opCode shouldBe serializer.opDesc.opCode
        serializer.getClass shouldBe serializerFor(fixture.node).getClass
      }
    }
  }

  property("declared structural children match the direct putValue stream and identity-rebuild") {
    VersionContext.withVersions(4, 4) {
      fixtures.foreach { fixture =>
        withClue(fixture.name + ": ") {
          val serializer = serializerFor(fixture.node)
          val declared = serializer.valueChildren(fixture.node)
          val recorded = recordedChildren(serializer, fixture.node)

          declared.length shouldBe recorded.length
          declared.indices.foreach { i =>
            (declared(i).asInstanceOf[AnyRef] eq recorded(i).asInstanceOf[AnyRef]) shouldBe true
          }

          serializer.rebuildValue(fixture.node, declared) shouldBe fixture.node
        }
      }
    }
  }

  property("conditional compact and optional branches expose exactly serialized Value fields") {
    val compactRelation = EQ(TrueLeaf, FalseLeaf)
    val ordinaryRelation = EQ(TaggedVariable(1.toByte, SBoolean), TrueLeaf)
    val intCollection = ConcreteCollection[SInt.type](IndexedSeq(IntConstant(1)), SInt)
    val byIndexWithoutDefault = ByIndex(intCollection, IntConstant(0), None)
    val byIndexWithDefault = ByIndex(intCollection, IntConstant(0), Some(IntConstant(2)))
    val registerWithoutDefault = DeserializeRegister(ErgoBox.R4, SInt, None)
    val registerWithDefault = DeserializeRegister(ErgoBox.R4, SInt, Some(IntConstant(2)))

    val cases = IndexedSeq[(Value[SType], Int)](
      compactRelation -> 0,
      ordinaryRelation -> 2,
      byIndexWithoutDefault -> 2,
      byIndexWithDefault -> 3,
      registerWithoutDefault -> 0,
      registerWithDefault -> 1)

    cases.foreach { case (node, expectedArity) =>
      val serializer = serializerFor(node)
      serializer.valueChildren(node).length shouldBe expectedArity
      recordedChildren(serializer, node).length shouldBe expectedArity
    }
  }

  property("rebuild accepts only the two serializer-owned compact encoding transitions") {
    val dynamicBoolean = DeserializeContext(1.toByte, SBoolean)
    val ordinaryCollection = ConcreteCollection[SBoolean.type](
      IndexedSeq(dynamicBoolean, FalseLeaf), SBoolean)
    ordinaryCollection.companion shouldBe ConcreteCollection
    val rebuiltCollection = serializerFor(ordinaryCollection).rebuildValue(
      ordinaryCollection, IndexedSeq(TrueLeaf, FalseLeaf))
      .asInstanceOf[ConcreteCollection[SBoolean.type]]
    rebuiltCollection.companion shouldBe ConcreteCollectionBooleanConstant
    (rebuiltCollection.items(0).asInstanceOf[AnyRef] eq TrueLeaf) shouldBe true
    (rebuiltCollection.items(1).asInstanceOf[AnyRef] eq FalseLeaf) shouldBe true
    serializerFor(rebuiltCollection).valueChildren(rebuiltCollection) shouldBe empty

    val ordinaryRelations = IndexedSeq[Value[SType]](
      EQ(dynamicBoolean, FalseLeaf),
      NEQ(dynamicBoolean, FalseLeaf),
      BinOr(dynamicBoolean, FalseLeaf),
      BinAnd(dynamicBoolean, FalseLeaf),
      BinXor(dynamicBoolean, FalseLeaf))
    ordinaryRelations.foreach { ordinaryRelation =>
      serializerFor(ordinaryRelation).valueChildren(ordinaryRelation).length shouldBe 2
      val rebuiltRelation = serializerFor(ordinaryRelation).rebuildValue(
        ordinaryRelation, IndexedSeq(TrueLeaf, FalseLeaf)).asInstanceOf[Relation[_, _]]
      (rebuiltRelation.left.asInstanceOf[AnyRef] eq TrueLeaf) shouldBe true
      (rebuiltRelation.right.asInstanceOf[AnyRef] eq FalseLeaf) shouldBe true
      serializerFor(rebuiltRelation).valueChildren(rebuiltRelation) shouldBe empty
    }
  }

  property("Seq-backed structural collections accept every serializable Seq implementation") {
    val listCollection = ConcreteCollection[SInt.type](List(IntConstant(1), IntConstant(2)), SInt)
    val vectorCollection = ConcreteCollection[SInt.type](Vector(IntConstant(3), IntConstant(4)), SInt)
    val listSigma = SigmaAnd(List(CreateProveDlog(GroupGenerator), CreateProveDlog(GroupGenerator)))
    val vectorSigma = SigmaOr(Vector(CreateProveDlog(GroupGenerator), CreateProveDlog(GroupGenerator)))

    IndexedSeq[Value[SType]](listCollection, vectorCollection, listSigma, vectorSigma).foreach { node =>
      val serializer = serializerFor(node)
      val declared = serializer.valueChildren(node)
      val recorded = recordedChildren(serializer, node)
      declared.length shouldBe recorded.length
      declared.indices.foreach { i =>
        (declared(i).asInstanceOf[AnyRef] eq recorded(i).asInstanceOf[AnyRef]) shouldBe true
      }
      serializer.rebuildValue(node, declared).tpe shouldBe node.tpe
    }
  }

  property("constant AST opcode dispatch remains a leaf across serialized type codes") {
    val constants = IndexedSeq[Value[SType]](
      ByteConstant(1),
      IntConstant(2),
      LongConstant(3L),
      ByteArrayConstant(Array[Byte](4, 5)))
    val serializer = ValueSerializer.getSerializer(Constant.opCode)
      .asInstanceOf[ValueSerializer[Value[SType]]]

    constants.foreach { constant =>
      constant.opCode shouldBe Constant.opCode
      serializer.valueChildren(constant) shouldBe empty
      serializer.rebuildValue(constant, Value.EmptySeq) shouldBe constant
      ValueSerializer.deserialize(ValueSerializer.serialize(constant)) shouldBe constant
    }
  }

  property("rebuild preserves non-Value metadata for blocks, functions, and calls") {
    val replacementBody = IntConstant(9)
    val function = FuncValue(IndexedSeq(7 -> SInt), IntConstant(1))
    val functionSerializer = serializerFor(function)
    val rebuiltFunction = functionSerializer.rebuildValue(
      function, IndexedSeq(replacementBody)).asInstanceOf[FuncValue]
    rebuiltFunction.args shouldBe function.args
    (rebuiltFunction.body.asInstanceOf[AnyRef] eq replacementBody) shouldBe true

    val originalDef = ValDef(3, STypeVar.EmptySeq, IntConstant(1))
    val replacementDef = ValDef(3, STypeVar.EmptySeq, IntConstant(2))
    val block = BlockValue(IndexedSeq(originalDef), IntConstant(4))
    val blockSerializer = serializerFor(block)
    val rebuiltBlock = blockSerializer.rebuildValue(
      block, IndexedSeq(replacementDef, block.result)).asInstanceOf[BlockValue]
    rebuiltBlock.items.map(_.id) shouldBe block.items.map(_.id)

    val property = MethodCall(Self, SBoxMethods.ValueMethod, Value.EmptySeq, EmptySubst)
    val rebuiltProperty = serializerFor(property).rebuildValue(
      property, IndexedSeq(Self)).asInstanceOf[MethodCall]
    rebuiltProperty.method shouldBe property.method
    rebuiltProperty.typeSubst shouldBe property.typeSubst
    rebuiltProperty.args shouldBe empty

    val collection = ConcreteCollection[SInt.type](IndexedSeq(IntConstant(1)), SInt)
    val method = MethodCall(
      collection,
      SCollectionMethods.IndexOfMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
      IndexedSeq(IntConstant(1), IntConstant(0)),
      EmptySubst)
    val rebuiltMethod = serializerFor(method).rebuildValue(
      method, serializerFor(method).valueChildren(method)).asInstanceOf[MethodCall]
    rebuiltMethod.method shouldBe method.method
    rebuiltMethod.typeSubst shouldBe method.typeSubst
  }

  property("rebuild rejects wrong owner, arity, null, and child type deterministically") {
    val tuple = Tuple(IndexedSeq(IntConstant(1), IntConstant(2)))
    val serializer = serializerFor(tuple)

    an[SerializerException] should be thrownBy serializer.valueChildren(IntConstant(1))
    an[SerializerException] should be thrownBy serializer.rebuildValue(tuple, IndexedSeq(IntConstant(1)))
    an[SerializerException] should be thrownBy serializer.rebuildValue(
      tuple, IndexedSeq(IntConstant(1), LongConstant(2L)))
    an[SerializerException] should be thrownBy serializer.rebuildValue(tuple, null)

    val block = BlockValue(
      IndexedSeq(ValDef(1, STypeVar.EmptySeq, IntConstant(1))),
      IntConstant(2))
    an[SerializerException] should be thrownBy serializerFor(block).rebuildValue(
      block, IndexedSeq(IntConstant(1), block.result))
  }

  property("legacy serializer extensions retain linkage and fail closed without a structural schema") {
    val tuple = Tuple(IndexedSeq(IntConstant(1), IntConstant(2)))

    an[SerializerException] should be thrownBy
      new LegacyTupleSerializer().valueChildren(tuple)
    an[SerializerException] should be thrownBy
      new ChildrenOnlyTupleSerializer().rebuildValue(tuple, tuple.items)
  }

  property("rebuild postconditions reject null, changed owner/type, and reordered children") {
    val tuple = Tuple(IndexedSeq(IntConstant(1), IntConstant(2)))
    val children = tuple.items

    an[SerializerException] should be thrownBy
      new BrokenTupleSerializer((_, _) => null).rebuildValue(tuple, children)
    an[SerializerException] should be thrownBy
      new BrokenTupleSerializer((_, _) => IntConstant(1)).rebuildValue(tuple, children)
    an[SerializerException] should be thrownBy
      new BrokenTupleSerializer((_, xs) => Tuple(xs :+ IntConstant(3))).rebuildValue(tuple, children)
    an[SerializerException] should be thrownBy
      new BrokenTupleSerializer((_, xs) => Tuple(xs.reverse)).rebuildValue(tuple, children)
  }
}
