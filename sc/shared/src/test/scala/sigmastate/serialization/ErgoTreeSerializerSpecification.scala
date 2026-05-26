package sigma.serialization

import org.ergoplatform.ErgoBox
import org.ergoplatform.validation.ValidationRules.CheckDeserializedScriptIsSigmaProp
import sigma.{SigmaProp, VersionContext}
import sigma.ast._
import sigma.ast.syntax.SigmaPropValue
import sigma.data.{CAND, CBigInt, CSigmaProp, ProveDHTuple, ProveDlog}
import sigmastate.utils.Helpers
import sigma.util.Extensions.SigmaPropOps
import sigma.validation.ValidationException
import ErgoTree.EmptyConstants
import ErgoTree.HeaderType
import scorex.util.encode.Base16
import sigma.compiler.ir.IRContext
import sigma.eval.Extensions.SigmaBooleanOps
import sigmastate._
import sigmastate.helpers.CompilerTestingCommons
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer

import java.math.BigInteger

class ErgoTreeSerializerSpecification extends SerializationSpecification
  with CompilerTestingCommons with CompilerCrossVersionProps {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }



  private def extractConstants(prop: SigmaPropValue)(implicit IR: IRContext): Seq[ErgoTree] = {
    import ErgoTree._
    val env = Map[String, Any]()
    val res = compiler.compileTyped(env, prop)
    checkCompilerResult(res)
    val calcF = res.compiledGraph
    val constantsStore = new ConstantStore()
    val outExpr = IR.buildTree(calcF, Some(constantsStore))
    val constants = constantsStore.getAll
    val trees = if (constants.isEmpty) {
      Seq(ErgoTree(ergoTreeHeaderInTests, constants, outExpr))
    } else {
      Seq(
        ErgoTree(setConstantSegregation(ergoTreeHeaderInTests), constants, outExpr),
        ErgoTree(ergoTreeHeaderInTests, EmptyConstants, prop)
      )
    }
    trees
  }

  property("(de)serialization round trip using treeBytes()") {
    val exprs = Seq(
      EQ(Plus(10.toByte, 20.toByte), ByteConstant(30)).toSigmaProp,
      EQ(Plus(10.toShort, 20.toShort), ShortConstant(30)).toSigmaProp,
      EQ(Plus(10, 20), IntConstant(30)).toSigmaProp,
      EQ(Plus(CBigInt(BigInteger.valueOf(10L)), BigIntConstant(20L)), BigIntConstant(30L)).toSigmaProp
    )
    exprs.foreach { expr =>
      extractConstants(expr).foreach { ergoTree =>
        val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
        val (_, _, deserializedConstants, treeBytes) = DefaultSerializer
          .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
        deserializedConstants shouldEqual ergoTree.constants
        val r = SigmaSerializer.startReader(
          treeBytes,
          new ConstantStore(deserializedConstants),
          resolvePlaceholdersToConstants = true)
        val deserializedTree = ValueSerializer.deserialize(r)
        deserializedTree shouldEqual expr
      }
    }
  }

  property("Constant extraction via compiler pass: (de)serialization round trip") {
    val prop = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp
    extractConstants(prop).foreach { ergoTree =>
      val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual ergoTree
    }
  }

  property("failed type check on tree deserialization") {
    forAll(numExprTreeNodeGen) { numProp =>
      val prop = numProp.asInstanceOf[SigmaPropValue] // this typecast doesn't check the actual type
      extractConstants(prop).foreach { ergoTree =>
        val bytes = DefaultSerializer.serializeErgoTree(ergoTree)

        if (ergoTreeVersionInTests == 0) {
          assertExceptionThrown(
            DefaultSerializer.deserializeErgoTree(bytes),
            rootCauseLike[SerializerException]("Failed deserialization, expected deserialized script to have type SigmaProp;"))
        } else {
          val tree = DefaultSerializer.deserializeErgoTree(bytes)
          tree.root match {
            case Left(UnparsedErgoTree(unparsedBytes,
                ValidationException(_, CheckDeserializedScriptIsSigmaProp, _, Some(cause)))) =>
              unparsedBytes shouldBe bytes
              rootCauseLike[SerializerException](
                "Failed deserialization, expected deserialized script to have type SigmaProp;")
                .apply(cause) shouldBe true
            case _ => fail()
          }
        }
      }
    }
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val bytes = DefaultSerializer.serializeErgoTree(tree)
    val (_, _, deserializedConstants, _) = DefaultSerializer.
      deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants.length shouldBe 3
    val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
    deserializedTree shouldEqual tree
  }

  property("tree with placeholders bytes should be equal if only constants are different") {
    val tree1 = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val tree2 = mkTestErgoTree(EQ(Plus(30, 40), IntConstant(70)).toSigmaProp)
    val bytes1 = DefaultSerializer.serializeErgoTree(tree1)
    val bytes2 = DefaultSerializer.serializeErgoTree(tree2)
    val (_, _, _, treeBytes1) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes1))
    val (_, _, _, treeBytes2) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes2))
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip") {
    // increased minimum number of successes
    // for better coverage of all possible combinations (with/without constants, segregation option, etc.)
    forAll(ergoTreeGen, minSuccessful(500)) { tree: ErgoTree =>
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual tree
    }
  }

  property("max ergo tree byte size check") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    if (ergoTreeVersionInTests == 0) {
      assertExceptionThrown({
        DefaultSerializer.deserializeErgoTree(r, 1)
      }, {
        case e: SerializerException => rootCause(e).isInstanceOf[ReaderPositionLimitExceeded]
      })
    } else {
      val tree = DefaultSerializer.deserializeErgoTree(r, 1)
      tree.root match {
        case Left(UnparsedErgoTree(_, ve: ValidationException)) =>
          rootCauseLike[ReaderPositionLimitExceeded]().apply(ve.cause.get) shouldBe true
        case _ => fail()
      }
    }
  }

  property("restore reader's positionLimit") {
    val tree = mkTestErgoTree(EQ(Plus(10, 20), IntConstant(30)).toSigmaProp)
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    r.positionLimit = 1
    DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize) shouldEqual tree
    r.positionLimit shouldBe 1
  }

  property("should compute hasDeserialize during parsing") {
    val const = IntConstant(10)
    val dc = DeserializeContext(1.toByte, SInt)
    val dr = DeserializeRegister(ErgoBox.R4, SInt)

    val samples = Table(("exp", "hasDeserialize"),
      const -> false,
      dc -> true,
      dr -> true,
      Plus(Plus(const, dc), dr) -> true,
      Plus(Plus(const, const), const) -> false
    )

    forAll(samples) { (exp, hasDeserialize) =>
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), exp)))
      )
      t._hasDeserialize shouldBe None

      val parsedTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(t.bytes)
      parsedTree shouldBe t
      parsedTree._hasDeserialize.isDefined shouldBe true
      parsedTree.hasDeserialize shouldBe hasDeserialize
    }
  }

  property("getPositionsBackref") {
    def test(positions: Array[Int], expected: Array[Int]) = {
      val backrefs = ErgoTreeSerializer.DefaultSerializer.getPositionsBackref(positions, expected.length)
      backrefs shouldBe expected
    }

    test(positions = Array(), expected = Array()) // no positions, no constants
    test(positions = Array(), expected = Array(-1)) // no positions, 1 constant
    test(positions = Array(0), expected = Array())  // 1 position, no constants
    test(positions = Array(1), expected = Array(-1)) // 1 position, but out of range
    test(positions = Array(0), expected = Array(0))  // 1 position, 1 constant
    test(positions = Array(-1), expected = Array())  // 1 invalid (out of range) position, no constants
    test(positions = Array(-2), expected = Array(-1))  // 1 invalid position, 1 constants

    test(positions = Array(0, 0), expected = Array(0))  // duplicate positions, 1 constant
    test(positions = Array(-1, 0), expected = Array(1))  // invalid positions ignored
    test(positions = Array(-1, 0, 0), expected = Array(1))  // only first of the duplicates used
     
    test(positions = Array(), expected = Array(-1, -1, -1, -1, -1))  // no positions => no backrefs

    test(positions = Array(1, 2), expected = Array(-1, 0, 1, -1, -1))
    test(positions = Array(1, 2, 4), expected = Array(-1, 0, 1, -1, 2))
  }

  property("SigmaProp.propBytes vs ErgoTree.serializer equivalence") {
    forAll(MinSuccessful(100)) { sp: SigmaProp =>
      val propBytes = sp.propBytes
      val ergoTree = new ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, Right(sp.toSigmaBoolean.toSigmaPropValue), null, None, None)
      val treeBytes = DefaultSerializer.serializeErgoTree(ergoTree)
      treeBytes shouldBe propBytes.toArray
    }
  }

  property("SigmaProp.propBytes(version) vs ErgoTree.serializer equivalence (v0..v4)") {
    forAll(MinSuccessful(50)) { sp: SigmaProp =>
      (0 to 4).foreach { vInt =>
        val v = vInt.toByte
        val activated = (VersionContext.MaxSupportedScriptVersion: Byte).max(v)
        VersionContext.withVersions(activated, v) {
          val header = ErgoTree.defaultHeaderWithVersion(v)
          val tree = new ErgoTree(
            header, EmptyConstants,
            Right(sp.toSigmaBoolean.toSigmaPropValue),
            null, None, None)
          val treeBytes = DefaultSerializer.serializeErgoTree(tree)
          sp.propBytes(v).toArray shouldBe treeBytes
        }
      }
    }
  }

  property("SigmaProp.propBytes (no-arg) == SigmaProp.propBytes(0)") {
    forAll(MinSuccessful(100)) { sp: SigmaProp =>
      sp.propBytes.toArray shouldBe sp.propBytes(0.toByte).toArray
    }
  }

  // Golden vectors covering the SigmaBoolean shapes that matter on-chain: bare ProveDlog,
  // ProveDHTuple, and a CAND composition. v0 bytes are reused from `LanguageSpecificationV5
  // / "SigmaProp.propBytes equivalence"`. v1..v4 bytes are derived from the v0 content via
  // the documented header layout, so any drift in either layer breaks the test.
  property("SigmaProp.propBytes(version) golden vectors (v0..v4)") {
    val pk = ProveDlog(
      Helpers.decodeECPoint("039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6f"))
    val dht = ProveDHTuple(
      Helpers.decodeECPoint("03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb"),
      Helpers.decodeECPoint("023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d03"),
      Helpers.decodeECPoint("03d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72"),
      Helpers.decodeECPoint("037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441"))
    val and = CAND(Array(pk, dht))

    val cases: Seq[(SigmaProp, Array[Byte])] = Seq(
      CSigmaProp(pk) -> Helpers.decodeBytes(
        "0008cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6f").toArray,
      CSigmaProp(dht) -> Helpers.decodeBytes(
        "0008ce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441").toArray,
      CSigmaProp(and) -> Helpers.decodeBytes(
        "00089602cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441").toArray
    )

    cases.foreach { case (sp, v0Bytes) =>
      sp.propBytes(0.toByte).toArray shouldBe v0Bytes
      val content = v0Bytes.tail  // drop the 0x00 v0 header
      (1 to 4).foreach { vInt =>
        val v = vInt.toByte
        val w = CoreSerializer.startWriter()
        w.put((0x08 | v).toByte)  // SizeFlag | version
        w.putUInt(content.length)
        w.putBytes(content)
        sp.propBytes(v).toArray shouldBe w.toBytes
      }
    }
  }

  property("PropBytesMethodV2 is gated by V7SoftForkVersion (ergoTree v4+)") {
    val v2MethodId = SSigmaPropMethods.PropBytesMethodV2.methodId

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, VersionContext.V6SoftForkVersion) {
      SSigmaPropMethods.methods.map(_.methodId) should not contain v2MethodId
    }

    VersionContext.withVersions(VersionContext.V7SoftForkVersion, VersionContext.V7SoftForkVersion) {
      val ids = SSigmaPropMethods.methods.map(_.methodId)
      ids should contain (v2MethodId)
      ids should contain (SSigmaPropMethods.PropBytesMethod.methodId)
    }
  }

  property("v3 tree with upcast") {
    val treeBytes = Base16.decode("0b15ea02e4dc650cfe020300020008b20e020102020100").get

    VersionContext.withVersions(3, 3) {
      val tree = DefaultSerializer.deserializeErgoTree(treeBytes)
      val treeBytes2 = DefaultSerializer.serializeErgoTree(tree)

      treeBytes.sameElements(treeBytes2) shouldBe true
    }
  }

}
