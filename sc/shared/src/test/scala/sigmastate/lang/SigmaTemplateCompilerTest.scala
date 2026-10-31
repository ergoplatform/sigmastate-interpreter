package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.sdk.Parameter
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.VersionContext
import sigma.ast.ErgoTree.HeaderType
import java.math.BigInteger
import org.scalacheck.Gen
import sigma.ast.{AtLeast, BinAnd, BoolToSigmaProp, ByteArrayConstant, ConcreteCollection, Constant, ConstantPlaceholder, EQ, ErgoTree, ExtractAmount, FalseLeaf, GE, GT, Height, IntConstant, LT, LongConstant, SBoolean, SInt, SLong, SSigmaProp, SString, SType, Self, SigmaAnd, SigmaOr, SigmaPropConstant, SizeOf, TrueLeaf}
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.{SValue, SigmaPropValue}
import sigma.serialization.ErgoTreeSerializer
import sigma.compiler.SigmaTemplateCompiler
import sigma.exceptions.TyperException
import sigmastate.CompilerTestsBase
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.interpreter.Interpreter.ScriptEnv

class SigmaTemplateCompilerTest extends AnyPropSpec with ScalaCheckPropertyChecks with CompilerTestsBase {
  val templateCompiler = SigmaTemplateCompiler(ErgoAddressEncoder.MainnetNetworkPrefix)

  property("compiles full contract template") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |* lines to describe functions
        |* @param param3 the final parameter
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string", param3: Long) = {
        |  sigmaProp(true)
        |}""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1),
      Parameter("param3", "the final parameter", 2)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString, SLong)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string"),
      None
    )

    checkEquals(template.expressionTree, BoolToSigmaProp(TrueLeaf))
  }

  property("compiles contract template without braces") {
    val source =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |
        |* lines to describe functions
        |* @return
        |*/
        |@contract def contractName(p1: Int = 5, p2: String = "default string") = sigmaProp(true)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.name shouldBe "contractName"
    template.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p1", "describe p1", 0),
      Parameter("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SString)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(5),
      Some("default string")
    )

    checkEquals(template.expressionTree, BoolToSigmaProp(TrueLeaf))
  }

  property("uses default value from parameter definition") {
    val source =
      """/**/
        |@contract def contractName(p: Boolean = true) = sigmaProp(p && HEIGHT > 1000)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("p", "", 0)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SBoolean)
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(true)
    )

    val expectedExpr = BoolToSigmaProp(BinAnd(ConstantPlaceholder(0, SBoolean), GT(Height, IntConstant(1000))))
    checkEquals(template.expressionTree, expectedExpr)

    val expectedTree = new ErgoTree(
      HeaderType @@ 26.toByte,   // use ErgoTreeUtils to get explanation
      Vector(TrueLeaf),
      Right(BoolToSigmaProp(BinAnd(ConstantPlaceholder(0, SBoolean), GT(Height, IntConstant(1000))))))

    expectedTree.version shouldBe VersionContext.JitActivationVersion
    expectedTree.hasSize shouldBe true
    expectedTree.isConstantSegregation shouldBe true

    // apply using default values declared in the parameters
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map.empty
      ),
      expectedTree
    )

    // apply overriding the default values
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("p" -> FalseLeaf)
      ),
      expectedTree.copy(constants = Vector(FalseLeaf))
    )
  }

  property("uses given environment when provided (overriding default value)") {
    val source =
      """/**/
        |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high)
        |""".stripMargin
    val template = templateCompiler.compile(source)

    template.parameters should contain theSameElementsInOrderAs IndexedSeq(
      Parameter("low", "", 0),
      Parameter("high", "", 1)
    )
    template.constTypes should contain theSameElementsInOrderAs Seq(SInt, SInt)
    // check parsed default values
    template.constValues.get should contain theSameElementsInOrderAs IndexedSeq(
      Some(0),
      None
    )
    checkEquals(
      template.expressionTree,
      BoolToSigmaProp(
        BinAnd(
          LT(ConstantPlaceholder(0, SInt), Height),
          LT(Height, ConstantPlaceholder(1, SInt))
        )
      )
    )

    // incomplete application (missing `high` parameter)
    assertExceptionThrown(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map.empty
      ),
      exceptionLike[IllegalArgumentException](
        "requirement failed: value for parameter `high` was not provided while it does not have a default value.")
    )

    val expectedTree = new ErgoTree(
      HeaderType @@ 26.toByte,
      Vector(IntConstant(0), IntConstant(100)),
      Right(
        BoolToSigmaProp(
          BinAnd(LT(ConstantPlaceholder(0, SInt), Height), LT(Height, ConstantPlaceholder(1, SInt)))
        )
      )
    )

    // apply providing the parameter without default value
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("high" -> IntConstant(100))
      ),
      expectedTree
    )

    // apply providing all parameters overriding the default values
    checkEquals(
      template.applyTemplate(
        version = Some(VersionContext.JitActivationVersion),
        paramValues = Map("low" -> IntConstant(10), "high" -> IntConstant(100))
      ),
      expectedTree.copy(constants = Vector(IntConstant(10), IntConstant(100)))
    )
  }

  property("fails when the parameter is not provided") {
    // NOTE: parameter `condition` is not provided */
    val source      =
      """/**/
       |@contract def contractName(low: Int = 0, high: Int) = sigmaProp(low < HEIGHT && HEIGHT < high) && condition
       |""".stripMargin
    assertExceptionThrown(
      templateCompiler.compile(source),
      exceptionLike[TyperException]("Cannot assign type for variable 'condition' because it is not found in env")
    )
  }

  private def pkConst(x: Int): Constant[SSigmaProp.type] =
    SigmaPropConstant(DLogProverInput(BigInteger.valueOf(x.toLong)).publicImage)

  private def collectPlaceholders(node: Any): Seq[ConstantPlaceholder[SType]] = node match {
    case ph: ConstantPlaceholder[_] =>
      Seq(ph.asInstanceOf[ConstantPlaceholder[SType]])
    case it: Iterable[_] =>
      it.toSeq.flatMap(collectPlaceholders)
    case arr: Array[_] =>
      arr.toSeq.flatMap(collectPlaceholders)
    case p: Product =>
      p.productIterator.toSeq.flatMap(collectPlaceholders)
    case _ =>
      Seq.empty
  }

  private def collectPlaceholderIds(tree: SValue): Seq[Int] =
    collectPlaceholders(tree).map(_.id)

  property("compiles 5-parameter template with placeholders matching parameter constantIndex (#1205)") {
    val source =
      """/** five parameters
        |* @return
        |*/
        |@contract def t(bob: SigmaProp, dave: SigmaProp, alice: SigmaProp, carol: SigmaProp, erin: SigmaProp) = {
        |  (atLeast(2, Coll(bob, dave, alice)) && carol) || erin
        |}""".stripMargin
    val ct = templateCompiler.compile(source)

    ct.parameters.map(p => p.name -> p.constantIndex) should contain theSameElementsInOrderAs Seq(
      "bob" -> 0,
      "dave" -> 1,
      "alice" -> 2,
      "carol" -> 3,
      "erin" -> 4
    )
    collectPlaceholderIds(ct.expressionTree) should contain theSameElementsInOrderAs Seq(0, 1, 2, 3, 4)

    val bob = pkConst(2)
    val dave = pkConst(4)
    val alice = pkConst(1)
    val carol = pkConst(3)
    val erin = pkConst(5)
    val applied = ct.applyTemplate(
      Some(0.toByte),
      Map("bob" -> bob, "dave" -> dave, "alice" -> alice, "carol" -> carol, "erin" -> erin)
    )
    val expectedProp = SigmaOr(
      SigmaAnd(
        AtLeast(
          IntConstant(2),
          ConcreteCollection(
            Array[SigmaPropValue](
              bob,
              dave,
              alice
            ),
            SSigmaProp
          )
        ),
        carol
      ),
      erin
    )
    checkEquals(applied.toProposition(replaceConstants = true), expectedProp)
  }

  property("applies 5+ parameter template with mixed types and deserializes applied ErgoTree bytes (#1205)") {
    val source =
      """/** mixed types
        |* @return
        |*/
        |@contract def t(pk: SigmaProp, deadline: Int, hash: Coll[Byte], amount: Long, flag: Boolean) = {
        |  sigmaProp(HEIGHT > deadline && hash.size == 32 && SELF.value >= amount && flag) && pk
        |}""".stripMargin
    val ct = templateCompiler.compile(source)
    collectPlaceholderIds(ct.expressionTree) should contain theSameElementsInOrderAs Seq(1, 2, 3, 4, 0)

    val pk = pkConst(7)
    val hashBytes = Array.fill(32)(1.toByte)
    val applied = ct.applyTemplate(
      Some(0.toByte),
      Map(
        "pk" -> pk,
        "deadline" -> IntConstant(1000),
        "hash" -> ByteArrayConstant(hashBytes),
        "amount" -> LongConstant(1000000L),
        "flag" -> TrueLeaf
      )
    )
    val deserialized = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(applied.bytes)
    deserialized.root shouldBe a[Right[_, _]]
    checkEquals(deserialized, applied)

    val expectedProp = SigmaAnd(
      BoolToSigmaProp(
        BinAnd(
          BinAnd(
            BinAnd(
              GT(Height, IntConstant(1000)),
              EQ(SizeOf(ByteArrayConstant(hashBytes)), IntConstant(32))
            ),
            GE(ExtractAmount(Self), LongConstant(1000000L))
          ),
          TrueLeaf
        )
      ),
      pk
    )
    checkEquals(deserialized.toProposition(replaceConstants = true), expectedProp)
  }

  property("placeholder index and type match constantIndex and constTypes for random 5..20 parameter names (#1205)") {
    val typeSpecs: IndexedSeq[(String, SType, (String, Int) => String)] = IndexedSeq(
      ("Int", SInt, (name, idx) => s"$name == $idx"),
      ("Long", SLong, (name, idx) => s"$name == ${idx}L"),
      ("Boolean", SBoolean, (name, _) => s"$name == true"),
      ("Coll[Byte]", SByteArray, (name, idx) => s"$name.size == $idx"),
      ("SigmaProp", SSigmaProp, (name, idx) => s"$name.propBytes.size == $idx")
    )
    val identGen: Gen[String] = for {
      len <- Gen.choose(4, 10)
      chars <- Gen.listOfN(len, Gen.alphaLowerChar)
      suffix <- Gen.choose(0, 999)
    } yield s"param_${chars.mkString}_$suffix"

    val paramNamesGen: Gen[Seq[String]] = for {
      count <- Gen.choose(5, 20)
      rawNames <- Gen.listOfN(count * 2, identGen)
      distinctNames = rawNames.distinct.take(count)
      if distinctNames.size == count
    } yield distinctNames

    forAll(paramNamesGen) { names =>
      val paramsWithTypes = names.zipWithIndex.map { case (name, idx) =>
        val (typeName, sType, mkCond) = typeSpecs(idx % typeSpecs.length)
        (name, idx, typeName, sType, mkCond(name, idx))
      }
      val sig = paramsWithTypes.map { case (name, _, typeName, _, _) => s"$name: $typeName" }.mkString(", ")
      val body = paramsWithTypes.map(_._5).mkString(" && ")
      val source =
        s"""/** random parameters
           |* @return
           |*/
           |@contract def t($sig) = {
           |  sigmaProp($body)
           |}""".stripMargin
      val ct = templateCompiler.compile(source)
      ct.parameters.zipWithIndex.foreach { case (p, idx) =>
        p.name shouldBe names(idx)
        p.constantIndex shouldBe idx
        ct.constTypes(p.constantIndex) shouldBe paramsWithTypes(idx)._4
      }
      val placeholders = collectPlaceholders(ct.expressionTree)
      placeholders.length shouldBe names.length
      ct.parameters.foreach { p =>
        val ph = placeholders(p.constantIndex)
        ph.id shouldBe p.constantIndex
        ph.tpe shouldBe ct.constTypes(p.constantIndex)
      }
    }
  }

  property("applies default values to their own parameters when 5+ parameters are declared (#1205)") {
    val source =
      """/** defaults with 6 parameters
        |* @return
        |*/
        |@contract def t(bob: Int = 10, dave: Long, alice: Int = 20, carol: Boolean = true, erin: Int, frank: Long = 99L) = {
        |  sigmaProp(bob < erin && alice > 0 && dave < frank && carol)
        |}""".stripMargin
    val ct = templateCompiler.compile(source)
    collectPlaceholderIds(ct.expressionTree) should contain theSameElementsInOrderAs Seq(0, 4, 2, 1, 5, 3)

    val applied = ct.applyTemplate(
      version = Some(VersionContext.JitActivationVersion),
      paramValues = Map(
        "dave" -> LongConstant(50L),
        "erin" -> IntConstant(30)
      )
    )
    applied.constants should contain theSameElementsInOrderAs IndexedSeq(
      IntConstant(10),
      LongConstant(50L),
      IntConstant(20),
      TrueLeaf,
      IntConstant(30),
      LongConstant(99L)
    )
    val expectedProp = BoolToSigmaProp(
      BinAnd(
        BinAnd(
          BinAnd(
            LT(IntConstant(10), IntConstant(30)),
            GT(IntConstant(20), IntConstant(0))
          ),
          LT(LongConstant(50L), LongConstant(99L))
        ),
        TrueLeaf
      )
    )
    checkEquals(applied.toProposition(replaceConstants = true), expectedProp)
  }

}


