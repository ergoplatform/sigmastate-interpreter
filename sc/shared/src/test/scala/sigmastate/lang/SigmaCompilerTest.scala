package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform._
import scorex.util.encode.Base58
import sigma.ast.{ByIndex, ExtractAmount, GetVar, _}
import sigma.ast.syntax._
import sigmastate._
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigma.ast.{Apply, MethodCall, ZKProofBlock}
import sigma.exceptions.{GraphBuildingException, InvalidArguments, TyperException}
import sigma.serialization.ValueSerializer
import sigma.serialization.generators.ObjectGenerators

import java.math.BigInteger
import scala.annotation.unused

class SigmaCompilerTest extends CompilerTestingCommons with LangTests with ObjectGenerators {
  import CheckingSigmaBuilder._
  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }

  private def comp(env: ScriptEnv, x: String): Value[SType] = compile(env, x)
  private def comp(x: String): Value[SType] = compile(env, x)

  private def testMissingCosting(script: String, @unused expected: SValue): Unit = {
    an [GraphBuildingException] should be thrownBy comp(env, script)
  }

  private def testMissingCostingWOSerialization(script: String, @unused expected: SValue): Unit = {
    an [GraphBuildingException] should be thrownBy comp(env, script)
  }

  private def costerFail(env: ScriptEnv, x: String, expectedLine: Int, expectedCol: Int): Unit = {
    val exception = the[GraphBuildingException] thrownBy comp(env, x)
    withClue(s"Exception: $exception, is missing source context:") { exception.source shouldBe defined }
    val sourceContext = exception.source.get
    sourceContext.line shouldBe expectedLine
    sourceContext.column shouldBe expectedCol
  }

  private def costerFail(x: String, expectedLine: Int, expectedCol: Int): Unit =
    costerFail(env, x, expectedLine, expectedCol)

  property("array indexed access") {
    comp(env, "Coll(1)(0)") shouldBe
      ByIndex(ConcreteCollection.fromSeq(Array(IntConstant(1)))(SInt), 0)
    comp(env, "Coll(Coll(1))(0)(0)") shouldBe
        ByIndex(ByIndex(ConcreteCollection.fromSeq(Array(ConcreteCollection.fromItems(IntConstant(1))))(SCollection(SInt)), 0), 0)
    comp(env, "arr1(0)") shouldBe ByIndex(ByteArrayConstant(Array[Byte](1, 2)), 0)
  }

  property("array indexed access with default value") {
    comp(env, "Coll(1).getOrElse(0, 1)") shouldBe
      ByIndex(ConcreteCollection.fromSeq(Array(IntConstant(1)))(SInt), 0, Some(IntConstant(1)))
    comp(env, "Coll(Coll(1)).getOrElse(0, Coll(2))(0)") shouldBe
      ByIndex(
        ByIndex(
          ConcreteCollection.fromSeq(Array(ConcreteCollection.fromSeq(Array(IntConstant(1)))))(SCollection(SInt)),
          0,
          Some(ConcreteCollection.fromSeq(Array(IntConstant(2))))),
        0)
    comp(env, "arr1.getOrElse(999, 0.toByte)") shouldBe
      ByIndex(ByteArrayConstant(Array[Byte](1, 2)), IntConstant(999), Some(ByteConstant(0)))
  }

  property("predefined functions") {
    comp(env, "anyOf(Coll(c1, c2))") shouldBe OR(ConcreteCollection.fromSeq(Array(TrueLeaf, FalseLeaf)))
    comp(env, "blake2b256(getVar[Coll[Byte]](10).get)") shouldBe CalcBlake2b256(GetVarByteArray(10).get)
    comp(env, "sha256(getVar[Coll[Byte]](10).get)") shouldBe CalcSha256(GetVarByteArray(10).get)
    comp(env, "10.toByte") shouldBe ByteConstant(10)
    comp(env, "Coll(1)(0).toByte") shouldBe
      Downcast(ByIndex(ConcreteCollection(Array(IntConstant(1)),SInt),IntConstant(0),None), SByte)
    comp(env, "allOf(Coll(c1, c2))") shouldBe AND(ConcreteCollection.fromSeq(Array(TrueLeaf, FalseLeaf)))
    comp(env, "getVar[Byte](10).get") shouldBe GetVarByte(10).get
    comp(env, "getVar[Short](10).get") shouldBe GetVarShort(10).get
    comp(env, "getVar[Long](10).get") shouldBe GetVarLong(10).get
    comp(env, "getVar[Coll[Byte]](10).get") shouldBe GetVarByteArray(10).get
  }

  property("global methods") {
    comp(env, "{ groupGenerator }") shouldBe MethodCall(Global, SGlobalMethods.groupGeneratorMethod, IndexedSeq(), EmptySubst)
    comp(env, "{ Global.groupGenerator }") shouldBe MethodCall(Global, SGlobalMethods.groupGeneratorMethod, IndexedSeq(), EmptySubst)
    comp(env, "{ Global.xor(arr1, arr2) }") shouldBe Xor(ByteArrayConstant(arr1), ByteArrayConstant(arr2))
    comp(env, "{ xor(arr1, arr2) }") shouldBe Xor(ByteArrayConstant(arr1), ByteArrayConstant(arr2))
  }

  property("user-defined functions") {
    comp("{ def f(i: Int) = { i + 1 }; f(2) }") shouldBe Apply(
      FuncValue(Vector((1,SInt)),Plus(ValUse(1,SInt), IntConstant(1))),
      Vector(IntConstant(2)))
  }

  property("allOf") {
    comp("allOf(Coll[Boolean](true, false))") shouldBe AND(TrueLeaf, FalseLeaf)
  }

  property("anyOf") {
    comp("anyOf(Coll[Boolean](true, false))") shouldBe OR(TrueLeaf, FalseLeaf)
  }

  property("atLeast") {
    comp("atLeast(2, Coll[SigmaProp](p1, p2))") shouldBe AtLeast(2, p1, p2)
  }

  property("ZKProof") {
    testMissingCostingWOSerialization("ZKProof { sigmaProp(HEIGHT > 1000) }",
      ZKProofBlock(BoolToSigmaProp(GT(Height, IntConstant(1000)))))
  }

  property("sigmaProp") {
    comp("sigmaProp(HEIGHT > 1000)") shouldBe BoolToSigmaProp(GT(Height, IntConstant(1000)))
  }

  property("getVar") {
    comp("getVar[Byte](10).get") shouldBe GetVar(10.toByte, SByte).get
    comp("getVar[Byte](10L).get") shouldBe GetVar(10.toByte, SByte).get
    an[TyperException] should be thrownBy comp("getVar[Byte](\"ha\")")
  }

  property("PK (testnet network prefix)") {
    implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
    val dk1 = proveDlogGen.sample.get
    val encodedP2PK = P2PKAddress(dk1).toString
    val code = s"""PK("$encodedP2PK")"""
    val res = comp(code)
    res shouldEqual SigmaPropConstant(dk1)
  }

  property("bigInt") {
    comp(""" bigInt("326674862673836209462483453386286740270338859283019276168539876024851191344") """) shouldBe
      BigIntConstant(new BigInteger("326674862673836209462483453386286740270338859283019276168539876024851191344"))
    comp(""" bigInt("-10") """) shouldBe
      BigIntConstant(-10L)
  }

  property("fromBaseX") {
    comp(""" fromBase16("31") """) shouldBe ByteArrayConstant(Array[Byte](49))
    comp(""" fromBase58("r") """) shouldBe ByteArrayConstant(Array[Byte](49))
    comp(""" fromBase64("MQ") """) shouldBe ByteArrayConstant(Array[Byte](49))
    comp(""" fromBase64("M" + "Q") """) shouldBe ByteArrayConstant(Array[Byte](49))
  }

  property("deserialize") {
    def roundtrip[T <: SType](c: Value[T], typeSig: String) = {
      val bytes = ValueSerializer.serialize(c)
      val str = Base58.encode(bytes)
      comp(env, s"deserialize[$typeSig](" + "\"" + str + "\")") shouldBe c
    }
    roundtrip(ByteArrayConstant(Array[Byte](2)), "Coll[Byte]")
    roundtrip(Tuple(ByteArrayConstant(Array[Byte](2)), LongConstant(4)), "(Coll[Byte], Long)")
    an[InvalidArguments] should be thrownBy roundtrip(ByteArrayConstant(Array[Byte](2)), "Coll[Long]")
  }

  property("deserialize fails") {
    // more then one argument
    an[TyperException] should be thrownBy comp(env, """deserialize[Int]("test", "extra argument")""")
    // invalid chat in Base58 string
    an[AssertionError] should be thrownBy comp(env, """deserialize[Int]("0")""")
    // more than one type
    an[TyperException] should be thrownBy comp(env, """deserialize[Int, Byte]("test")""")
    // not a string constant
    an[TyperException] should be thrownBy comp(env, """deserialize[Int](1)""")
  }

  property("longToByteArray") {
    comp("longToByteArray(1L)") shouldBe LongToByteArray(LongConstant(1))
  }

  property("byteArrayToBigInt") {
    comp("byteArrayToBigInt(longToByteArray(1L))") shouldBe ByteArrayToBigInt(LongToByteArray(LongConstant(1)))
  }

  property("failed fromBaseX (invalid input)") {
    an[AssertionError] should be thrownBy comp(""" fromBase58("^%$#@")""")
    an[IllegalArgumentException] should be thrownBy comp(""" fromBase64("^%$#@")""")
  }

  property("decodePoint") {
    comp(env, "decodePoint(Coll[Byte](1.toByte))") shouldBe DecodePoint(ConcreteCollection.fromItems(ByteConstant(1)))
  }

  property("logicalNot") {
    comp("!true") shouldBe LogicalNot(TrueLeaf)
  }

  property("Negation") {
    comp("-HEIGHT") shouldBe Negation(Height)
  }

  property("BitInversion") {
    testMissingCosting("~1", BitInversion(IntConstant(1)))
  }

  property("LogicalXor") {
    comp("false ^ false") shouldBe FalseLeaf
    comp("true ^ true") shouldBe FalseLeaf
    comp("false ^ true") shouldBe TrueLeaf
    comp("true ^ false") shouldBe LogicalNot(FalseLeaf)
  }

  property("BitwiseOr") {
    testMissingCosting("1 | 2", mkBitOr(IntConstant(1), IntConstant(2)))
  }

  property("BitwiseAnd") {
    testMissingCosting("1 & 2", mkBitAnd(IntConstant(1), IntConstant(2)))
  }

  property("BitwiseXor") {
    testMissingCosting("1 ^ 2", mkBitXor(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftRight") {
    testMissingCosting("1 >> 2", mkBitShiftRight(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftLeft") {
    testMissingCosting("1 << 2", mkBitShiftLeft(IntConstant(1), IntConstant(2)))
  }

  property("BitShiftRightZeroed") {
    testMissingCosting("1 >>> 2", mkBitShiftRightZeroed(IntConstant(1), IntConstant(2)))
  }

  property("Collection.indices") {
    comp("Coll(true, false).indices") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(TrueLeaf, FalseLeaf),
        SCollectionMethods.IndicesMethod.withConcreteTypes(Map(SCollection.tIV -> SBoolean)),
        Vector()
      )
  }

  property("SBox.tokens") {
    comp("SELF.tokens") shouldBe
      mkMethodCall(Self, SBoxMethods.tokensMethod, IndexedSeq())
  }

  property("SContext.dataInputs") {
    comp("CONTEXT.dataInputs") shouldBe
      mkMethodCall(Context, SContextMethods.dataInputsMethod, IndexedSeq())
  }

  property("SAvlTree.digest") {
    comp("getVar[AvlTree](1).get.digest") shouldBe
      mkMethodCall(GetVar(1.toByte, SAvlTree).get, SAvlTreeMethods.digestMethod, IndexedSeq())
  }

  property("SGroupElement.exp") {
    comp("g1.exp(1.toBigInt)") shouldBe
      mkExponentiate(GroupElementConstant(ecp1),
        BigIntConstant(1))
  }

  property("SOption.map") {
    comp("getVar[Int](1).map({(i: Int) => i + 1})") shouldBe
      mkMethodCall(GetVarInt(1),
        SOptionMethods.MapMethod.withConcreteTypes(Map(SType.tT -> SInt, SType.tR -> SInt)),
        IndexedSeq(FuncValue(
          Vector((1, SInt)),
          Plus(ValUse(1, SInt), IntConstant(1)))), Map()
      )
  }

  property("SOption.filter") {
    comp("getVar[Int](1).filter({(i: Int) => i > 0})") shouldBe
      mkMethodCall(GetVarInt(1),
        SOptionMethods.FilterMethod.withConcreteTypes(Map(SType.tT -> SInt)),
        IndexedSeq(FuncValue(
          Vector((1, SInt)),
          GT(ValUse(1, SInt), IntConstant(0)))), Map()
      )
  }

  property("SCollection.patch") {
    comp("Coll(1, 2).patch(1, Coll(3), 1)") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(IntConstant(1), IntConstant(2)),
        SCollectionMethods.PatchMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
        Vector(IntConstant(1), ConcreteCollection.fromItems(IntConstant(3)), IntConstant(1)),
        Map())
  }

  property("SCollection.updated") {
    comp("Coll(1, 2).updated(1, 1)") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(IntConstant(1), IntConstant(2)),
        SCollectionMethods.UpdatedMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
        Vector(IntConstant(1), IntConstant(1)),
        Map())
  }

  property("SCollection.updateMany") {
    comp("Coll(1, 2).updateMany(Coll(1), Coll(3))") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(IntConstant(1), IntConstant(2)),
        SCollectionMethods.UpdateManyMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
        Vector(ConcreteCollection.fromItems(IntConstant(1)), ConcreteCollection.fromItems(IntConstant(3))),
        Map())
  }

  property("SCollection.indexOf") {
    comp("Coll(1, 2).indexOf(1, 0)") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(IntConstant(1), IntConstant(2)),
        SCollectionMethods.IndexOfMethod.withConcreteTypes(Map(SCollection.tIV -> SInt)),
        Vector(IntConstant(1), IntConstant(0)),
        Map())
  }

  property("SCollection.zip") {
    comp("Coll(1, 2).zip(Coll(1, 1))") shouldBe
      mkMethodCall(
        ConcreteCollection.fromItems(IntConstant(1), IntConstant(2)),
        SCollectionMethods.ZipMethod.withConcreteTypes(Map(SCollection.tIV -> SInt, SCollection.tOV -> SInt)),
        Vector(ConcreteCollection.fromItems(IntConstant(1), IntConstant(1)))
      )
  }

  property("SCollection.filter") {
    comp("OUTPUTS.filter({ (out: Box) => out.value >= 1L })") shouldBe
      mkFilter(Outputs,
        FuncValue(
          Vector((1, SBox)),
          GE(ExtractAmount(ValUse(1, SBox)), LongConstant(1))
        )
      )
  }

  property("byteArrayToLong") {
    comp("byteArrayToLong(longToByteArray(1L))") shouldBe ByteArrayToLong(LongToByteArray(LongConstant(1)))
  }

  property("xorOf") {
    comp("xorOf(Coll[Boolean](true, false))") shouldBe XorOf(Array(TrueLeaf, FalseLeaf))
  }

  property("substConst") {
    comp("substConstants(getVar[Coll[Byte]](1).get, getVar[Coll[Int]](2).get, getVar[Coll[SigmaProp]](3).get)") shouldBe
      SubstConstants(
        GetVarByteArray(1).get,
        GetVarIntArray(2).get,
        GetVar(3.toByte, SCollection(SSigmaProp)).get
      )
  }
}
