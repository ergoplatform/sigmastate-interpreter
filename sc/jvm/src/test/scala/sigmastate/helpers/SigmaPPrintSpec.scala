package sigmastate.helpers

import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.ErgoBox
import scorex.util.ModifierId
import sigma.Extensions.ArrayOps
import sigma.SigmaDslTesting
import sigma.ast._
import sigma.data.{AvlTreeData, AvlTreeFlags, CBox, CollType, Digest32Coll}
import ErgoTree.HeaderType
import sigma.ast.MethodCall
import sigma.serialization.OpCodes
import sigmastate.utils.Helpers

import java.math.BigInteger
import scala.collection.mutable.ArrayBuffer

class SigmaPPrintSpec extends SigmaDslTesting {


  property("typeName") {
    def test(t: SType, exp: String) = {
      SigmaPPrint.typeName(t) shouldBe exp
    }
    test(SInt, "SInt.type")
    test(SCollection(SBox), "SCollection[SBox.type]")
    test(SOption(SBox), "SOption[SBox.type]")
    test(STuple(SBox, SInt), "STuple")
  }

  property("Special cases") {
    def test(x: Any, expected: String) = {
      val res = SigmaPPrint(x).plainText
      res shouldBe expected
    }

    // type handlers
    test(SCollectionType(SByte), "SByteArray")
    test(SCollectionType(SCollectionType(SByte)), "SByteArray2")
    test(SCollectionType(SBoolean), "SBooleanArray")
    test(STuple(Vector(SBoolean, SInt)), "SPair(SBoolean, SInt)")

    test(sigma.BooleanType, "RType.BooleanType")
    test(CollType(sigma.ByteType), "CollType(RType.ByteType)")

    // exception handlers
    test(new ArithmeticException("msg"), "new ArithmeticException(\"msg\")")

    // data handlers
    test(10.toByte, "10.toByte")
    test(255.toByte, "-1.toByte")
    test(10.toShort, "10.toShort")

    test(new BigInteger("a", 16), """new BigInteger("a", 16)""")

    val negative = new BigInteger("-a", 16)
    negative.toString(10) shouldBe "-10"
    test(negative, """new BigInteger("-a", 16)""")

    test(ErgoAlgos.decodeUnsafe("00ffaa"), "ErgoAlgos.decodeUnsafe(\"00ffaa\")")
    test(ErgoAlgos.decodeUnsafe("00ffaa"): Seq[Byte], "ErgoAlgos.decodeUnsafe(\"00ffaa\")")
    
    test(Array(10), "Array(10)")
    test(Array(10): Seq[Int], "Array(10)")
    test({val buf = ArrayBuffer.empty[Int]; buf += (10); buf}, "Seq(10)")
    test(Helpers.decodeBytes("00ff"), "Helpers.decodeBytes(\"00ff\")")
    val ge1 = "03358d53f01276211f92d0aefbd278805121d4ff6eb534b777af1ee8abae5b2056"

    test(Helpers.decodeGroupElement(ge1), s"""Helpers.decodeGroupElement("${ge1}")""")
    test(Helpers.decodeECPoint(ge1), s"""Helpers.decodeECPoint("${ge1}")""")

    val t1 = AvlTreeData(
      ErgoAlgos.decodeUnsafe("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094").toColl,
      AvlTreeFlags(false, true, true),
      1,
      Some(1)
    )
    test(t1,
      """AvlTreeData(
        |  ADDigest @@ (
        |    Helpers.decodeBytes("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094")
        |  ),
        |  AvlTreeFlags(false, true, true),
        |  1,
        |  Some(1)
        |)""".stripMargin)
    test(
      new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(IntArrayConstant(Array(10, 20))),
        Right(BoolToSigmaProp(TrueLeaf))
      ),
      """new ErgoTree(
        |  16.toByte,
        |  Vector(IntArrayConstant(Coll[Int](10, 20))),
        |  Right(BoolToSigmaProp(TrueLeaf))
        |)""".stripMargin)
    test(
      CBox(
        new ErgoBox(
            9223372036854775807L,
          new ErgoTree(HeaderType @@ 0.toByte, Vector(), Right(BoolToSigmaProp(FalseLeaf))),
          Coll(
            (Digest32Coll @@ (ErgoAlgos.decodeUnsafe("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001").toColl), 10000000L)
          ),
          Map(),
          ModifierId @@ ("bc80ffc00100d60101ffd3d3ab7f73800aff80487fff7fffbb010080ff7f0837"),
          0.toShort,
            1000000
        )
      ),
      """CBox(
        |  new ErgoBox(
        |    9223372036854775807L,
        |    new ErgoTree(0.toByte, Vector(), Right(BoolToSigmaProp(FalseLeaf))),
        |    Coll(
        |      (
        |        Digest32 @@ (
        |          Helpers.decodeBytes("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001")
        |        ),
        |        10000000L
        |      )
        |    ),
        |    Map(),
        |    ModifierId @@ ("bc80ffc00100d60101ffd3d3ab7f73800aff80487fff7fffbb010080ff7f0837"),
        |    0.toShort,
        |    1000000
        |  )
        |)""".stripMargin
    )
    // additionalHandlers
    test(SGlobal, "SGlobal")
    test(SCollection, "SCollection")
    test(SOption, "SOption")
    test(SInt, "SInt")
    test(Outputs, "Outputs")
    test(ErgoBox.R0, "ErgoBox.R0")
    test(ErgoBox.R9, "ErgoBox.R9")
    test(
      SelectField.typed[Value[SByte.type]](ValUse(1, STuple(Vector(SByte, SByte))), 1.toByte),
      "SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)), 1.toByte)"
    )
    test(TrueLeaf, "TrueLeaf")
    test(FalseLeaf, "FalseLeaf")
    test(IntConstant(10), "IntConstant(10)")
    test(ArithOp(IntConstant(1), IntConstant(1), OpCodes.PlusCode), "ArithOp(IntConstant(1), IntConstant(1), OpCode @@ (-102.toByte))")
    test(
      MethodCall.typed[Value[SCollection[SBox.type]]](
        ValUse(1, SContext),
        SContextMethods.getMethodByName("dataInputs"),
        Vector(),
        Map()
      ),
      """MethodCall.typed[Value[SCollection[SBox.type]]](
        |  ValUse(1, SContext),
        |  SContextMethods.getMethodByName("dataInputs"),
        |  Vector(),
        |  Map()
        |)""".stripMargin)

    test(SCollection.tIV, """STypeVar("IV")""")
    test(Map(SCollection.tIV -> SInt), """Map(STypeVar("IV") -> SInt)""")
    test(
      MethodCall.typed[Value[SCollection[SInt.type]]](
        ValUse(1, SCollectionType(SBox)),
        SCollectionMethods.IndicesMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
        Vector(),
        Map()
      ),
      """MethodCall.typed[Value[SCollection[SInt.type]]](
        |  ValUse(1, SCollectionType(SBox)),
        |  SCollectionMethods.getMethodByName("indices").withConcreteTypes(Map(STypeVar("IV") -> SBox)),
        |  Vector(),
        |  Map()
        |)""".stripMargin)
  }
}
