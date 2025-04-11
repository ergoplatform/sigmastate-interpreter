package sigma

import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, ErgoLikeContext}
import sigma.VersionContext._
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax.{SValue, SigmaPropValue, TrueSigmaProp}
import sigma.data.RType.asType
import sigma.data.{CBox, Nullable, RType, TrivialProp}
import sigma.validation.ValidationException
import sigma.validation.ValidationRules.CheckTypeCode
import ErgoTree.HeaderType
import sigmastate.eval.CProfiler
import sigmastate.helpers.{ErgoLikeContextTesting, SigmaPPrint}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.interpreter.CErgoTreeEvaluator
import sigma.ast.syntax._
import sigma.compiler.CompilerSettings
import sigma.eval.EvalSettings
import sigma.exceptions.{CostLimitException, InterpreterException}
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.Plus
import sigmastate.utils.Helpers.TryOps


/** Regression tests with ErgoTree related test vectors.
  * This test vectors verify various constants which are consensus critical and should not change.
  */
class ErgoTreeSpecification extends SigmaDslTesting with ContractsTestkit {

  property("Value.sourceContext") {
    val srcCtx = SourceContext.fromParserIndex(0, "")
    val value = IntConstant(10)
    value.sourceContext shouldBe Nullable.None

    value.withSrcCtx(Nullable(srcCtx))
    value.sourceContext shouldBe Nullable(srcCtx)

    assertExceptionThrown(
      value.withSrcCtx(Nullable(srcCtx)),
      t => t.isInstanceOf[RuntimeException] && t.getMessage.contains("can be set only once"))
  }

  property("Value.opType") {
    Seq(
      Block(Seq(), null),
      ValNode("x", SInt, null),
      ApplyTypes(null, Seq()),
      ValDef(1, null),
      ValUse(1, SInt),
      BlockValue(IndexedSeq(), null)
      ).foreach { node =>
      assertExceptionThrown(node.opType, t => t.getMessage.contains("is not supported for node"))
    }
    FuncValue(Vector((1, SInt)), ValUse(1, SInt)).opType shouldBe
      SFunc(Vector(), SFunc(SInt, SInt))
  }

  property("ErgoTree.toProposition") {
    val t1 = new ErgoTree(
      HeaderType @@ 16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )

    val t = new ErgoTree(
      HeaderType @@ 16.toByte,
      Array(IntConstant(1)),
      Left(UnparsedErgoTree(t1.bytes, ValidationException("", CheckTypeCode, Seq())))
    )
    assertExceptionThrown(
      t.toProposition(true),
      t => t.isInstanceOf[ValidationException]
    )
  }

  property("ErgoTree.template") {
    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
      )
      t.template shouldBe ErgoAlgos.decodeUnsafe("d19373000402")
    }

    {
      val t = ErgoTree.fromHex("100604000e000400040005000500d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202")
       t.templateHex shouldBe "d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202"
    }
  }

  property("ErgoTree.bytes") {
    val t = new ErgoTree(
      HeaderType @@ 16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    val expectedBytes = DefaultSerializer.serializeErgoTree(t)
    t._bytes shouldBe expectedBytes
  }

  property("Value.hasDeserialize") {
    val const = IntConstant(10)
    Value.hasDeserialize(const) shouldBe false
    val dc = DeserializeContext(1.toByte, SInt)
    Value.hasDeserialize(dc) shouldBe true
    val dr = DeserializeRegister(ErgoBox.R4, SInt)
    Value.hasDeserialize(dr) shouldBe true
    Value.hasDeserialize(EQ(const, dc)) shouldBe true
    Value.hasDeserialize(Plus(Plus(const, dc), dr)) shouldBe true
    val t = new ErgoTree(
      HeaderType @@ 16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    Value.hasDeserialize(t.toProposition(replaceConstants = true)) shouldBe false
  }

  property("ErgoTree.hasDeserialize") {
    {
      val t = new ErgoTree(
        HeaderType @@ 0.toByte,
        Array[Constant[SType]](),
        Right(TrueSigmaProp))
      t._hasDeserialize shouldBe None
      t.hasDeserialize shouldBe false
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), DeserializeContext(1.toByte, SInt))))
      )
      t._hasDeserialize shouldBe None
      t.hasDeserialize shouldBe true
    }
  }

  property("ErgoTree.isUsingBlockchainContext") {
    {
      val t = new ErgoTree(
        HeaderType @@ 0.toByte,
        Array[Constant[SType]](),
        Right(TrueSigmaProp))
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe false
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(GT(Height, ConstantPlaceholder(0, SInt))))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          LastBlockUtxoRootHash, SAvlTreeMethods.getMethod,
          IndexedSeq(ExtractId(GetVarBox(22: Byte).get), GetVarByteArray(23: Byte).get)).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(BoolToSigmaProp(EQ(MinerPubkey, ErgoLikeContextTesting.dummyPubkey)))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          Context, SContextMethods.headersMethod, Vector()).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          Context, SContextMethods.preHeaderMethod, Vector()).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          Context, SContextMethods.heightMethod, Vector()).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          Context, SContextMethods.lastBlockUtxoRootHashMethod, Vector()).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }

    {
      val t = new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(),
        Right(OptionIsDefined(IR.builder.mkMethodCall(
          Context, SContextMethods.minerPubKeyMethod, Vector()).asOption[SByteArray]))
      )
      t._isUsingBlockchainContext shouldBe None
      t.isUsingBlockchainContext shouldBe true
    }
  }

  property("ErgoTree equality") {
    val t1 = new ErgoTree(
      HeaderType @@ 16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    val t2 = new ErgoTree(HeaderType @@ 16.toByte, Array(IntConstant(1)), Right(TrueSigmaProp))
    val t3 = new ErgoTree(HeaderType @@ 16.toByte, Array(IntConstant(1)), Right(TrueSigmaProp))
    val t4 = new ErgoTree(HeaderType @@ 16.toByte, Vector(), Right(TrueSigmaProp))
    val t5 = new ErgoTree(ErgoTree.DefaultHeader, Vector(), Right(TrueSigmaProp))
    assert(t1 != t2)
    assert(t2 == t3)
    assert(t3 != t4)
    assert(t4 != t5)
    assert(t5 != t1)
  }

  property("ConstantNode equality") {
    assert(IntConstant(10) == IntConstant(10))
    assert(ShortConstant(10) == ShortConstant(10))
    assert(IntConstant(10) != IntConstant(11))
    assert(IntConstant(10) != ShortConstant(10))
  }

  val typeCodes = Table(
    ("constant", "expectedValue"),
    (SPrimType.LastPrimTypeCode, 8),
    (SPrimType.MaxPrimTypeCode, 11)
  )
  
  property("Expected values of constants") {
    forAll(typeCodes) { (const, expValue) =>
      const shouldBe expValue
    }
  }

  // Expected meta-parameters of predefined types (see Predefined Types section in docs/spec/spec.pdf)
  val types = Table(
    ("type", "Code", "IsPrim", "IsEmbed", "IsNum"),
    (SBoolean, 1, true, true, false),
    (SByte,    2, true, true, true),
    (SShort,   3, true, true, true),
    (SInt,     4, true, true, true),
    (SLong,    5, true, true, true),
    (SBigInt,  6, true, true, true),
    (SGroupElement, 7, true, true, false),
    (SSigmaProp,    8, true, true, false),
    (SBox,       99,  false, false, false),
    (SAvlTree,   100, false, false, false),
    (SContext,   101, false, false, false),
    (SHeader,    104, false, false, false),
    (SPreHeader, 105, false, false, false),
    (SGlobal,    106, false, false, false)
  )

  property("Predefined Types") {
    forAll(types) { (t, code, isPrim, isEmbed, isNum) =>
      t.typeCode shouldBe code
      t.typeId shouldBe code
      t.isInstanceOf[SPrimType] shouldBe isPrim
      t.isEmbeddable shouldBe isEmbed
      t.isNumType shouldBe isNum
      whenever(isPrim) {
        t.typeCode should be <= SPrimType.LastPrimTypeCode
      }
    }
  }

  /** Expected parameters of resolved method (see `methods` table below).
    *
    * @param isResolvableFromIds if true, them SMethod.fromIds must resolve, otherwise
    *                            ValidationException must be thrown
    */
  case class MInfo(methodId: Byte, method: SMethod, isResolvableFromIds: Boolean = true)

  // NOTE, the type code constants are checked above
  // The methodId codes as checked here, they MUST be PRESERVED.
  // The following table should be made dependent on HF activation
  val methods = Table(
    ("typeId",        "methods",               "CanHaveMethods"),
    (SBoolean.typeId, Seq.empty[MInfo], true),
    (SByte.typeId,    Seq.empty[MInfo], false),
    (SShort.typeId,   Seq.empty[MInfo], false),
    (SInt.typeId,     Seq.empty[MInfo], false),
    (SLong.typeId,    Seq.empty[MInfo], false),

    { // SNumericType.typeId is erroneously shadowed by SGlobal.typeId
      // this should be preserved in v3.x and fixed in v4.0
      (SNumericType.typeId,  Seq(
        MInfo(methodId = 1, SGlobalMethods.groupGeneratorMethod),
        MInfo(2, SGlobalMethods.xorMethod)
      ), true)
    },

    { // SBigInt inherit methods from SNumericType.methods
      // however they are not resolvable via SBigInt.typeId
      import SNumericTypeMethods._
      (SBigInt.typeId,  Seq(
        MInfo(methodId = 1, ToByteMethod, isResolvableFromIds = false),
        MInfo(2, ToShortMethod, isResolvableFromIds = false),
        MInfo(3, ToIntMethod, isResolvableFromIds = false),
        MInfo(4, ToLongMethod, isResolvableFromIds = false),
        MInfo(5, ToBigIntMethod, isResolvableFromIds = false),
        MInfo(6, ToBytesMethod, isResolvableFromIds = false),
        MInfo(7, ToBitsMethod, isResolvableFromIds = false)
      ), true)
    },
    { import SGroupElementMethods._
      (SGroupElement.typeId,  Seq(
        MInfo(2, GetEncodedMethod),
        MInfo(3, ExponentiateMethod),
        MInfo(4, MultiplyMethod),
        MInfo(5, NegateMethod)
      ), true)
    },
    { import SSigmaPropMethods._
      (SSigmaProp.typeId,  Seq(
        MInfo(1, PropBytesMethod),
        MInfo(2, IsProvenMethod)  // TODO v5.x (3h): this method must be removed (see https://github.com/ScorexFoundation/sigmastate-interpreter/pull/800)
      ), true)
    },
    { import SBoxMethods._
      (SBox.typeId,  Seq(
        MInfo(1, ValueMethod),
        MInfo(2, PropositionBytesMethod),
        MInfo(3, BytesMethod),
        MInfo(4, BytesWithoutRefMethod),
        MInfo(5, IdMethod),
        MInfo(6, creationInfoMethod),
        MInfo(7, getRegMethod),
        MInfo(8, tokensMethod)
      ) ++ registers(idOfs = 8)
        .zipWithIndex
        .map { case (m,i) => MInfo((8 + i + 1).toByte, m) }, true)
    },
    { import SAvlTreeMethods._
      (SAvlTree.typeId,  Seq(
        MInfo(1, digestMethod),
        MInfo(2, enabledOperationsMethod),
        MInfo(3, keyLengthMethod),
        MInfo(4, valueLengthOptMethod),
        MInfo(5, isInsertAllowedMethod),
        MInfo(6, isUpdateAllowedMethod),
        MInfo(7, isRemoveAllowedMethod),
        MInfo(8, updateOperationsMethod),
        MInfo(9, containsMethod),
        MInfo(10, getMethod),
        MInfo(11, getManyMethod),
        MInfo(12, insertMethod),
        MInfo(13, updateMethod),
        MInfo(14, removeMethod),
        MInfo(15, updateDigestMethod)
      ), true)
    },
    { import SHeaderMethods._
      (SHeader.typeId,  Seq(
        MInfo(1, idMethod), MInfo(2, versionMethod), MInfo(3, parentIdMethod),
        MInfo(4, ADProofsRootMethod), MInfo(5, stateRootMethod), MInfo(6, transactionsRootMethod),
        MInfo(7, timestampMethod), MInfo(8, nBitsMethod), MInfo(9, heightMethod),
        MInfo(10, extensionRootMethod), MInfo(11, minerPkMethod), MInfo(12, powOnetimePkMethod),
        MInfo(13, powNonceMethod), MInfo(14, powDistanceMethod), MInfo(15, votesMethod)
      ), true)
    },
    { import SPreHeaderMethods._
      (SPreHeader.typeId,  Seq(
        MInfo(1, versionMethod), MInfo(2, parentIdMethod), MInfo(3, timestampMethod),
        MInfo(4, nBitsMethod), MInfo(5, heightMethod), MInfo(6, minerPkMethod),
        MInfo(7, votesMethod)
      ), true)
    },
    { import SContextMethods._
      (SContext.typeId, Seq(
        MInfo(1, dataInputsMethod), MInfo(2, headersMethod), MInfo(3, preHeaderMethod),
        MInfo(4, inputsMethod), MInfo(5, outputsMethod), MInfo(6, heightMethod),
        MInfo(7, selfMethod), MInfo(8, selfBoxIndexMethod), MInfo(9, lastBlockUtxoRootHashMethod),
        MInfo(10, minerPubKeyMethod), MInfo(11, getVarMethod)
      ), true)
    },
    { import SGlobalMethods._
      (SGlobal.typeId, Seq(
        MInfo(1, groupGeneratorMethod), MInfo(2, xorMethod)
        ), true)
    },
    { import SCollectionMethods._
      (SCollection.typeId, Seq(
        MInfo(1, SizeMethod),
        MInfo(2, GetOrElseMethod),
        MInfo(3, MapMethod),
        MInfo(4, ExistsMethod),
        MInfo(5, FoldMethod),
        MInfo(6, ForallMethod),
        MInfo(7, SliceMethod),
        MInfo(8, FilterMethod),
        MInfo(9, AppendMethod),
        MInfo(10, ApplyMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        BitShiftLeftMethod,
        BitShiftRightMethod,
        BitShiftRightZeroedMethod,
        */
        MInfo(14, IndicesMethod),
        MInfo(15, FlatMapMethod),
        MInfo(19, PatchMethod),
        MInfo(20, UpdatedMethod),
        MInfo(21, UpdateManyMethod),
        /*TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        UnionSetsMethod,
        DiffMethod,
        IntersectMethod,
        PrefixLengthMethod,
        */
        MInfo(26, IndexOfMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        LastIndexOfMethod,
        FindMethod,
        */
        MInfo(29, ZipMethod)
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        DistinctMethod,
        StartsWithMethod,
        EndsWithMethod,
        MapReduceMethod,
        */
      ), true)
    },
    { import SOptionMethods._
      (SOption.typeId, Seq(
        MInfo(2, IsDefinedMethod),
        MInfo(3, GetMethod),
        MInfo(4, GetOrElseMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        FoldMethod,
        */
        MInfo(7, MapMethod),
        MInfo(8, FilterMethod)
      ), true)
    }
  )

  property("MethodCall Codes") {
    forAll(methods) { (typeId, methods, canHaveMethods) =>
      SType.types.get(typeId) match {
        case Some(tyDesc) =>
          assert(canHaveMethods, s"Type $tyDesc should NOT have methods")

          val mc = MethodsContainer(tyDesc.typeId)
          mc.methods.length shouldBe methods.length
          for (expectedMethod <- methods) {
            if (expectedMethod.isResolvableFromIds) {

              // the following line is used in MethodCall deserializer to resolve SMethod
              val resolvedMethod = SMethod.fromIds(typeId, expectedMethod.methodId)

              resolvedMethod.objType.typeId shouldBe typeId
              resolvedMethod.name shouldBe expectedMethod.method.name
              resolvedMethod.irInfo shouldBe expectedMethod.method.irInfo
            } else {
              // declared, but not supported
              assertExceptionThrown(
                SMethod.fromIds(typeId, expectedMethod.methodId),
                { case _: ValidationException => true
                  case _ => false },
                s"MethodCall shouldn't resolve for typeId = $typeId and $expectedMethod"
              )
            }
          }
        case None =>
          assert(!canHaveMethods, s"Type with code $typeId can have methods")
      }
    }

  }

  property("MethodCall on numerics") {
    forAll(Table[STypeCompanion]("type", SByte, SShort, SInt, SLong, SBigInt)) { t =>
      // this methods are expected to fail resolution in v3.x (but may change in future)
      (1 to 7).foreach { methodId =>
        assertExceptionThrown(
          SMethod.fromIds(t.typeId, methodId.toByte),
          {
            case _: ValidationException => true
            case _ => false
          },
          s"SMethod mustn't resolve for typeId = ${t.typeId} and methodId = $methodId"
        )
      }
    }
  }

  implicit def IR: TestingIRContext = new TestingIRContext
  implicit def cs: CompilerSettings = compilerSettingsInTests
  implicit def es: EvalSettings = evalSettings

  property("Apply with 0 arguments") {
    val expr = Apply(FuncValue(Vector(), IntConstant(1)), IndexedSeq())

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        val newF = funcJitFromExpr[Int, Int]("({ (x: Int) => 1 })()", expr)
        assertExceptionThrown(
          {
            val x = 100 // any value which is not used anyway
            val _ = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
              newF.apply(x)
            }
          },
          exceptionLike[InterpreterException]("Function application must have 1 argument, but was:")
        )
      }
    }
  }


  property("Apply with one argument") {
    val expr = Apply(
      FuncValue(Vector((1, SInt)), Negation(ValUse(1, SInt))),
      IndexedSeq(IntConstant(1)))
    val script = "({ (x: Int) => -x })(1)"

    val x = 1

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        val newF = funcJitFromExpr[Int, Int](script, expr)
        val (y, _) = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
          newF.apply(x)
        }
        y shouldBe -1
      }
    }
  }

  property("Apply with 2 and more arguments") {
    val expr = Apply(
      FuncValue(Vector((1, SInt), (2, SInt)), Plus(ValUse(1, SInt), ValUse(2, SInt))),
      IndexedSeq(IntConstant(1), IntConstant(1))
    )
    val script = "{ (x: Int, y: Int) => x + y }"

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        val newF = funcJitFromExpr[(Int, Int), Int](script, expr)
        assertExceptionThrown(
          {
            val _ = VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
              newF.apply((1, 1))
            }
          },
          exceptionLike[InterpreterException]("Function application must have 1 argument, but was:")
        )
      }
    }
  }

  /** Deeply nested maps which creates deeply nested collections.
    * @return lambda like `(xs: Coll[Byte]) => xs.map(_ => xs.map(... xs.map(_ => xs)...))`
    */
  def mkFuncValue(nDepth: Int, level: Int): SValue = {
    def mkCollection(nDepth: Int, level: Int): SValue = {
      if (level < nDepth)
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((level + 1, SByte)),
            mkCollection(nDepth, level + 1)
          )
        )
      else
        ValUse(1, SByteArray)
    }

    FuncValue(
      Array((1, SByteArray)),
      mkCollection(nDepth, level))
  }

  property("Building deeply nested expression") {

    mkFuncValue(1, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        ValUse(1, SByteArray))

    mkFuncValue(2, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((2, SByte)),
            ValUse(1, SByteArray)
          )
        ))

    mkFuncValue(3, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((2, SByte)),
            MapCollection(
              ValUse(1, SByteArray),
              FuncValue(
                Array((3, SByte)),
                ValUse(1, SByteArray)
              )
            )
          )
        ))
  }

  def exprCostForSize(size: Int, oldF: CompiledFunc[Coll[Byte], AnyRef], expected: Option[Coll[Byte] => AnyRef]) = {
    val xs = Coll(Array.tabulate[Byte](size)(i => i.toByte):_*)

    val (y, details) = oldF(xs)
    assert(details.actualTimeNano.get < 1000000000 /* 1 sec */)

    if (expected.isDefined) {
      val e = expected.get(xs)
      y shouldBe e
    }
    details.cost
  }

  def mkCompiledFunc(depth: Int): CompiledFunc[Coll[Byte], AnyRef] = {
    val expr = Apply(
      mkFuncValue(depth, 1),
      Array(OptionGet(GetVar(1.toByte, SOption(SByteArray))))
    )
    val script = "{ just any string }"
    implicit val tRes: RType[AnyRef] = asType[AnyRef](Evaluation.stypeToRType(expr.tpe))
    val oldF = funcJitFromExpr[Coll[Byte], AnyRef](script, expr)
    oldF
  }

  property("Spam: Building large nested collection") {

    forEachScriptAndErgoTreeVersion(
       activatedVers = Array(JitActivationVersion),
       ergoTreeVers = ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {

        { // depth 3
          val cf = mkCompiledFunc(3)
          val expected = (xs: Coll[Byte]) => xs.map(_ => xs.map(_ => xs))
          exprCostForSize(10, cf, Some(expected)) shouldBe JitCost(1456)
          exprCostForSize(100, cf, Some(expected)) shouldBe JitCost(104605)

          assertExceptionThrown(
            exprCostForSize(1000, cf, Some(expected)),
            exceptionLike[CostLimitException]("Estimated execution cost JitCost(10000005) exceeds the limit JitCost(10000000)")
          )
        }

        { // depth 60
          val cf = mkCompiledFunc(60)
          exprCostForSize(1, cf, None) shouldBe JitCost(2194)

          assertExceptionThrown(
            exprCostForSize(2, cf, None),
            exceptionLike[CostLimitException]("Estimated execution cost JitCost(10000001) exceeds the limit JitCost(10000000)")
          )
        }

      }
    }
  }

  // Test vectors for https://github.com/ScorexFoundation/sigmastate-interpreter/issues/828
  property("nested BoolToSigmaProp") {
    val addr = ErgoAddressEncoder.Mainnet.fromString("Fo6oijFP2JM87ac7w").getOrThrow
    val tree = addr.script
    tree shouldBe new ErgoTree(
      HeaderType @@ 16.toByte,
      Vector(TrueLeaf),
      Right(BoolToSigmaProp(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)).asBoolValue))
    )

    def createCtx: ErgoLikeContext = ErgoLikeContextTesting
        .dummy(fakeSelf, VersionContext.current.activatedVersion)
        .withErgoTreeVersion(tree.version)

    VersionContext.withVersions(activatedVersion = 1, tree.version) {
      // v4.x behavior
      val res = CErgoTreeEvaluator.evalToCrypto(createCtx, tree, evalSettings)
      res shouldBe ReductionResult(TrivialProp(true), 3)
    }

    VersionContext.withVersions(activatedVersion = 2, tree.version) {
      // v5.0 behavior
      assertExceptionThrown(
        CErgoTreeEvaluator.evalToCrypto(createCtx, tree, evalSettings),
        exceptionLike[ClassCastException]()
      )
    }
  }

  object BlockValueWithInvalidBoolToSigmaProp {
    def unapply(prop: SigmaPropValue): Option[BoolToSigmaProp] = prop.asValue[SType] match {
      case BlockValue(_, b @ BoolToSigmaProp(x)) if x.tpe == SSigmaProp => Some(b)
      case _ => None
    }
  }

  property("BoolToSigmaProp with SigmaProp argument should be deserializable") {
    { // Transaction: 5fe235558bd37328c5cc65711e5aff12b6f96d6f5abf062b7e7b994f7981f2ec
      val addr = ErgoAddressEncoder.Mainnet.fromString("Fo6oijFP2JM87ac7w").getOrThrow
      val tree = addr.script
      tree shouldBe new ErgoTree(
        HeaderType @@ 16.toByte,
        Vector(TrueLeaf),
        Right(BoolToSigmaProp(BoolToSigmaProp(ConstantPlaceholder(0, SBoolean)).asBoolValue))
      )
    }

    { // Transaction: 1b878563f198f622360d88f61a6258aa67dd40875e1b18d1e6ea013cd32afb40
      val addr = ErgoAddressEncoder.Mainnet.fromString("3ehvg9kjzVt2ep6VkYbsNZBXVoFjQA3b26LTmCwvE9vTUEU2TG1uKdqh7sb1a23G1HLkXa6rv4NUcR5Ucghp8CTuBnUxJB4qYq61GYNDFmCHdoZJq32vUVJ7Jsq4cFE7zp9ddFnchb8EN2Qkaa9rqzmruj4iKjLu8MMJ3V8ns1tpRF8eSp2KSjPuMYt6ZHysFNGoMt4dQ2P45YoCXbgiJtzcADgjMnr5bkpqKMx2ZEaAUWoZfHN8DUwvNawSCr2yHieHKbWujxeGuPUuGPAdJHQRcC47xpBj7rKExxGE6T117vAAzSwc98UG3CC8Lb8UeoE7WWi9LCTdXqJpJFrwb8Zqc9HnqSVRvAxeaKgcueX36absXAxpqpAGUcH8YwYoeVmSYLsQKQbUAVrFe73eJyRtgxpcVEqrs4rBZ3KeDJUe5J2NJTNYKoUFcruqqu4N1XUFCECWXANsE9TLoQNyqDgNRcnHE4t8nw6THPJXQWCTBHK6mHvkVcj6SvGinvVGfMpeuA8MF1FFtZJTMnM31cuMBexK3m5mDxsbamJngQiPrcyVqK4smDpdiqhds7APGJbwKTHgst2u1P6").getOrThrow
      val tree = addr.script
      tree match {
        case ErgoTree(_, _, Right(BlockValueWithInvalidBoolToSigmaProp(_)), _, _, _) =>
      }
    }

    { // Transactions:
      // 49ede8ab7b5a8ed00ba045f1ec9315de60566e149de5f35987a63c9fe481a13c
      // d3f35272c9e32f49b1723450fa6106032bcd1ad2b69dd199a68dbcd069be4baf
      // 1040deda322b4187e987b580b9df24c04804ad095dfc6a7e0e08db8e7b51dbfb
      // d6ca9f6760ff8d67ed0618c1f293574764813ceda31c51ce17ee10dc9743649b
      // 186afd8e39228c5b6e55a6b0d17eb08c62e03b0e82c7d3cc3a11bd51a96d2b92
      val addr = ErgoAddressEncoder.Mainnet.fromString("bpn7HXccD8wzY3x8XbymfBCh71aeci1FyM7JJgVx6mVY3vxznf2Pom7gtEgQBxr9UZb8cn9Z6GiXEjV7mB7ypRfH9Qpf3eCsd8qoEGmvsFqW2GyEvxgYWnJGd8QYTYZLBgGkSFcAeerLLDZwS1bos5oV4atMLnPDUJ6bH2EFFwVRdTAQKxaVMAhNtNYZxAcYtWhaYBJqYZM8ne3hGFrkWNTgNy7NxEDQ5LpBEM3rq6EqUENAGhH6THDL7RyU8AMkpr2vhosqEeqp4yXJmK887vU4qbnGGrMLX4w5GrgL9zLk41Vm6vzEUVLvp8XQJ8CULwkHNiKkNHRLeTk6BG4hwJsFoUSJJNWgsfputm8pEbaTuKfNG5u4NFmZ3YLfGsrqpr62c95QuNuD3g3FaoBUhxigkfNhKwFFtpKKsYerJvvp2Suh2eVRptFsCN15pjdkveUakhFsAo9j9YwhYVjjPpCWZgB8Wme8iZLRVfopSuDVdiqGsnmpJcuccmPsebgXPz3XvyjQazYE7WnR3Jrr1mRWHVEnJb3UU89JVPDpJPP1jUaSqmkKCkrPj3WMMQTDg17WW2DkhGKsb").getOrThrow
      val tree = addr.script
      tree match {
        case ErgoTree(_, _, Right(BlockValueWithInvalidBoolToSigmaProp(_)), _, _, _) =>
      }
    }

    { // Transactions:
      // d41cbc4ad342067e2f8945bfde4e04b15eafa200f868a223d131047bc46c52b3
      // 4503b5d77cb74b4354771b835cd61e9d5257022a8efff0fddfac249e0c25b492
      val addr = ErgoAddressEncoder.Mainnet.fromString("28JURWHTHwTnXJt5F38").getOrThrow
      val tree = addr.script
      tree shouldBe new ErgoTree(HeaderType @@ 16.toByte, Vector(),
        Right(BoolToSigmaProp(
          CreateProveDlog(OptionGet(ExtractRegisterAs(Self, ErgoBox.R4, SOption(SGroupElement)))).asBoolValue)
        ))
    }

    { // Transactions:
      // 263a441ecb4c2794711d7add8de59a0b0dc9b99e7a2dd3ff949cbafc958157f7
      // 5fd5027a70c2d45a853e16eb7a4909cced5d127ba3f87d0fe908f950c2084ec1
      // 4503b5d77cb74b4354771b835cd61e9d5257022a8efff0fddfac249e0c25b492
      // d4b522f88ff28d9bb686b3e2b346b91f78830279963432fbfbdebfb9e0d43fc6
      // d41cbc4ad342067e2f8945bfde4e04b15eafa200f868a223d131047bc46c52b3
      val addr = ErgoAddressEncoder.Mainnet.fromString("9drgw9DeDxHwdwYWT11yrYD3qCN3fzgnjA3eS5ZXEkR2Rrpd2x58R3HFViGAs93hYFqroBGShW5DG6h2tU7PqDURegny88hEhNKfLpWKBHpLMmfDwZwTM75MuYKXkmtyUmiMRAfGAVVjaSqThddra5ykZ4UrG8kgqkHSBTxtibVPmNff8Fx7Fy5q82xsZ95RFpjGkAX1QgtirHc3JP6QDZfHUFPx2gnyT9pDpYrKWEhjK8Ake779PoYqGzrL5gtuHyLqsC3WtLFMvP3QNfVtwPaqeCLEmjFmWYU6MV1A8fTJxGWDByiCMnva9wDgZgLh8wrSwttEGE9XHiZaWxWNWqVM8Ypn8aW8x8mKSjxQjF1BNxV41JTeGEMFDvY2HaNYgGbQhBbrTXFq9cvxuzPgXHtfwcbb2kiytr3YBCz8eNmbzp73LSKzRk4AFaTiTdFSSWJe72uLAurQTQBTzAzVgPptGWfrhMWmHCkN5qXQMpUQWNsCzwqRHeZzpSMVtWTyrBCGsbyuPitbukdLHZ8Wee7DtCy8j4Gkhewrwn23jVQu1ApN4uGAFEa29AL26bsMGD7tdu1StE9CKRVzbfEknaReqv6").getOrThrow
      val tree = addr.script
      tree match {
        case ErgoTree(_, _, Right(BlockValueWithInvalidBoolToSigmaProp(_)), _, _, _) =>
      }
      val lines = SigmaPPrint.tokenize(tree, 150, 300)
      if (printDebugInfo) {
        lines.foreach(print); println()
      }
    }

  }

}
