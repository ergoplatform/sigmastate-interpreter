package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.ergoplatform._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.{Colls, VersionContext}
import sigma.ast.SCollection._
import sigma.ast._
import sigma.ast.syntax.{SValue, SigmaPropValue, SigmaPropValueOps}
import sigma.crypto.CryptoConstants
import sigma.data.ProveDlog
import sigmastate._
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.interpreter.Interpreter.ScriptEnv
import SigmaPredef._
import sigmastate.lang.parsers.ParserException
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.generators.ObjectGenerators
import sigma.ast.Select
import sigma.compiler.phases.{SigmaBinder, SigmaTyper}
import sigma.exceptions.TyperException
import sigmastate.exceptions.MethodNotFound
import sigmastate.helpers.SigmaPPrint

class SigmaTyperTest extends AnyPropSpec
  with ScalaCheckPropertyChecks with Matchers with LangTests with ObjectGenerators {

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  import predefFuncRegistry._

  /** Checks that parsing, binding and typing of `x` results in the given expected value. */
  def typecheck(env: ScriptEnv, x: String, expected: SValue = null): SType = {
    try {
      val builder = TransformingSigmaBuilder
      val parsed = SigmaParser(x).get.value
      val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
      val binder = new SigmaBinder(env, builder, TestnetNetworkPrefix, predefinedFuncRegistry)
      val bound = binder.bind(parsed)
      val typeEnv = env.collect { case (k, v: SType) => k -> v }
      val typer = new SigmaTyper(builder, predefinedFuncRegistry, typeEnv, lowerMethodCalls = true)
      val typed = typer.typecheck(bound)
      assertSrcCtxForAllNodes(typed)
      if (expected != null) {
        if (expected != typed) {
          SigmaPPrint.pprintln(typed, width = 100)
        }
        typed shouldBe expected
      }
      typed.tpe
    } catch {
      case e: Exception => throw e
    }
  }

  def typefail(env: ScriptEnv, x: String, expectedLine: Int, expectedCol: Int): Unit = {
    val builder = TransformingSigmaBuilder
    assertExceptionThrown({
      val parsed = SigmaParser(x).get.value
      val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
      val binder = new SigmaBinder(env, builder, TestnetNetworkPrefix, predefinedFuncRegistry)
      val bound = binder.bind(parsed)
      val typeEnv = env.collect { case (k, v: SType) => k -> v }
      val typer = new SigmaTyper(builder, predefinedFuncRegistry, typeEnv, lowerMethodCalls = true)
      typer.typecheck(bound)
    }, {
      case te: TyperException =>
        withClue(s"Exception: $te, is missing source context:") { te.source shouldBe defined }
        val sourceContext = te.source.get
        sourceContext.line shouldBe expectedLine
        sourceContext.column shouldBe expectedCol
        true
      case _: ParserException => true
      case t => throw t
    })
  }

  property("simple expressions") {
    typecheck(env, "x", IntConstant(10)) shouldBe SInt   // constants are substituted from env
    typecheck(env, "x + y", Plus(10, 11)) shouldBe SInt
    typecheck(env, "x + height1", Plus(Upcast(10, SLong), 100L)) shouldBe SLong
    typecheck(env, "x - y") shouldBe SInt
    typecheck(env, "x / y") shouldBe SInt
    typecheck(env, "x % y") shouldBe SInt
    typecheck(env, "c1 && c2", BinAnd(TrueLeaf, FalseLeaf)) shouldBe SBoolean
    typecheck(env, "arr1", ByteArrayConstant(Array[Byte](1,2))) shouldBe SByteArray
    typecheck(env, "HEIGHT", Height) shouldBe SInt
    typecheck(env, "HEIGHT + 1") shouldBe SInt
    typecheck(env, "INPUTS", Inputs) shouldBe SCollection(SBox)
    typecheck(env, "INPUTS.size") shouldBe SInt
    typecheck(env, "INPUTS.size > 1", GT(Select(Inputs, "size", Some(SInt)), 1)) shouldBe SBoolean
    typecheck(env, "xor(arr1, arr2)", Xor(ByteArrayConstant(arr1), ByteArrayConstant(arr2))) shouldBe SByteArray
    typecheck(env, "arr1 ++ arr2", Append(ByteArrayConstant(arr1), ByteArrayConstant(arr2))) shouldBe SByteArray
    typecheck(env, "col1 ++ col2") shouldBe SCollection(SLong)
    typecheck(env, "g1.exp(n1)") shouldBe SGroupElement
    typecheck(env, "g1 * g2") shouldBe SGroupElement
    typecheck(env, "p1 || p2") shouldBe SSigmaProp
    typecheck(env, "p1 && p2") shouldBe SSigmaProp
    typecheck(env, "b1 < b2") shouldBe SBoolean
    typecheck(env, "b1 > b2") shouldBe SBoolean
    typecheck(env, "b1 <= b2") shouldBe SBoolean
    typecheck(env, "b1 >= b2") shouldBe SBoolean
    typecheck(env, "n1 < n2") shouldBe SBoolean
    typecheck(env, "n1 > n2") shouldBe SBoolean
    typecheck(env, "n1 <= n2") shouldBe SBoolean
    typecheck(env, "n1 >= n2") shouldBe SBoolean
    typecheck(env, "n1 == n2") shouldBe SBoolean
    typecheck(env, "n1 != n2") shouldBe SBoolean
  }

  property("predefined functions") {
    typecheck(env, "allOf") shouldBe AllOfFunc.declaration.tpe
    typecheck(env, "allOf(Coll(c1, c2))") shouldBe SBoolean
    typecheck(env, "getVar[Byte](10).get") shouldBe SByte
    typecheck(env, "getVar[Coll[Byte]](10).get") shouldBe SByteArray
    typecheck(env, "getVar[SigmaProp](10).get") shouldBe SSigmaProp
    typecheck(env, "p1 && getVar[SigmaProp](10).get") shouldBe SSigmaProp
    typecheck(env, "getVar[SigmaProp](10).get || p2") shouldBe SSigmaProp
    typecheck(env, "getVar[SigmaProp](10).get && getVar[SigmaProp](11).get") shouldBe SSigmaProp
    typecheck(env, "Coll(true, getVar[SigmaProp](11).get)") shouldBe SCollection(SBoolean)
    typecheck(env, "min(1, 2)") shouldBe SInt
    typecheck(env, "min(1L, 2)") shouldBe SLong
    typecheck(env, "min(HEIGHT, INPUTS.size)") shouldBe SInt
    typecheck(env, "max(1, 2)") shouldBe SInt
    typecheck(env, "max(1L, 2)") shouldBe SLong
    typecheck(env, """bigInt("1111")""") shouldBe SBigInt
    typecheck(env, """fromBase16("1111")""") shouldBe SByteArray
    typecheck(env, """fromBase58("111")""") shouldBe SByteArray
    typecheck(env, """fromBase64("111")""") shouldBe SByteArray

    typecheck(env, {
      implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(TestnetNetworkPrefix)
      val pk = ProveDlog(CryptoConstants.dlogGroup.generator)
      val addr = P2PKAddress(pk)
      val str = addr.toString
      s"""PK("${str}")"""
    }) shouldBe SSigmaProp

    typecheck(env, "sigmaProp(HEIGHT > 1000)") shouldBe SSigmaProp
    typecheck(env, "ZKProof { sigmaProp(HEIGHT > 1000) }") shouldBe SBoolean
  }

  property("val constructs") {
    typecheck(env, "{val X = 10; X > 2}") shouldBe SBoolean
    typecheck(env, """{val X = 10; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env, """{val X = 10 + 1; X >= X}""".stripMargin) shouldBe SBoolean
    typecheck(env,
      """{val X = 10
        |val Y = X + 1
        |X < Y}
      """.stripMargin) shouldBe SBoolean
    typecheck(env, "{val X = (10, true); X._1 > 2 && X._2}") shouldBe SBoolean
    typecheck(env, "{val X = (Coll(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
  }

  property("generic methods of arrays") {
    val minToRaise = LongConstant(1000)
    val env = this.env ++ Map(
      "minToRaise" -> minToRaise
    )
    typecheck(env, "OUTPUTS.map({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Coll[Boolean]")
    typecheck(env, "OUTPUTS.exists({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "OUTPUTS.forall({ (out: Box) => out.value >= minToRaise })") shouldBe SBoolean
    typecheck(env, "{ val arr = Coll(1,2,3); arr.fold(0, { (i1: Int, i2: Int) => i1 + i2 })}") shouldBe SInt
    typecheck(env, "OUTPUTS.slice(0, 10)") shouldBe ty("Coll[Box]")
    typecheck(env, "OUTPUTS.filter({ (out: Box) => out.value >= minToRaise })") shouldBe ty("Coll[Box]")
  }

  property("tuple constructor") {
    typecheck(env, "()") shouldBe SUnit
    typecheck(env, "(1)") shouldBe SInt
    typecheck(env, "(1, 2)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, x + 1)") shouldBe STuple(SInt, SInt)
    typecheck(env, "(1, 2, 3)") shouldBe STuple(SInt, SInt, SInt)
    typecheck(env, "(1, 2 + 3, 4)") shouldBe STuple(SInt, SInt, SInt)

    typecheck(env, "(1, 2L)._1") shouldBe SInt
    typecheck(env, "(1, 2L)._2") shouldBe SLong
    typecheck(env, "(1, 2L, 3)._3") shouldBe SInt

    typefail(env, "(1, 2L)._3", 1, 1)

    // tuple as collection
    typecheck(env, "(1, 2L).size") shouldBe SInt
    typecheck(env, "(1, 2L)(0)") shouldBe SInt
    typecheck(env, "(1, 2L)(1)") shouldBe SLong
    typecheck(env, "{ (a: Int) => (1, 2L)(a) }") shouldBe SFunc(IndexedSeq(SInt), SAny)
  }

  // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ignore("tuple advanced operations") {
    typecheck(env, "(1, 2L).getOrElse(2, 3)") shouldBe SAny
    typecheck(env, "(1, 2L).slice(0, 2)") shouldBe SCollection(SAny)
  }

  property("types") {
    typecheck(env, "{val X: Int = 10; 3 > 2}") shouldBe SBoolean
    typecheck(env, "{val X: (Int, Boolean) = (10, true); 3 > 2}") shouldBe SBoolean
    typecheck(env, "{val X: Coll[Int] = Coll(1,2,3); X.size}") shouldBe SInt
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), 1); X}") shouldBe STuple(SCollection(SInt), SInt)
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), x); X._1}") shouldBe SCollection(SInt)
    typecheck(env, "{val X: (Coll[Int], Int) = (Coll(1,2,3), x); X._1}") shouldBe SCollection(SInt)
  }

  property("if") {
    typecheck(env, "if(true) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) 1 else 2") shouldBe SInt
    typecheck(env, "if(c1) x else y") shouldBe SInt
    typecheck(env,
      """if (true) {
        |  val A = 10; A
        |} else
        |  if ( x == y) 2 else 3""".stripMargin) shouldBe SInt
  }

  property("array literals") {
    typefail(env, "Coll()", 1, 1)
    typefail(env, "Coll(Coll())", 1, 6)
    typefail(env, "Coll(Coll(Coll()))", 1, 11)

    typecheck(env, "Coll(1)") shouldBe SCollection(SInt)
    typecheck(env, "Coll(1, x)") shouldBe SCollection(SInt)
    typecheck(env, "Coll(Coll(x + 1))") shouldBe SCollection(SCollection(SInt))

    typefail(env, "Coll(1, x + 1, Coll())", 1, 16)
    typefail(env, "Coll(1, false)", 1, 1)
  }

  property("methods returning Option") {
    typecheck(env, "getVar[Int](10)") shouldBe SOption(SInt)
    typecheck(env, "{ val v = getVar[Int](1); v.get }") shouldBe SInt
  }

  property("array indexed access") {
    typefail(env, "Coll()(0)", 1, 1)
    typecheck(env, "Coll(0)(0)") shouldBe SInt
    typefail(env, "Coll(0)(0)(0)", 1, 1)
  }

  property("array indexed access with evaluation") {
    typecheck(env, "Coll(0)(1 - 1)") shouldBe SInt
    typecheck(env, "Coll(0)((1 - 1) + 0)") shouldBe SInt
    typefail(env, "Coll(0)(0 == 0)", 1, 9)
    typefail(env, "Coll(0)(1,1,1)", 1, 1)
  }

  property("array indexed access with default value") {
    typecheck(env, "Coll(0).getOrElse(0, 1)") shouldBe SInt
    typefail(env, "Coll(0).getOrElse(true, 1)", 1, 1)
    typefail(env, "Coll(true).getOrElse(0, 1)", 1, 1)
    typefail(env, "Coll(0).getOrElse(0, Coll(1))", 1, 1)
  }

  property("array indexed access with default value with evaluation") {
    typecheck(env, "Coll(0).getOrElse(0, (2 - 1) + 0)") shouldBe SInt
  }

  property("lambdas") {
    typecheck(env, "{ (a: Int) => a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => a + 1 }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => { a + 1 } }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int) => { val b = a + 1; b } }") shouldBe SFunc(IndexedSeq(SInt), SInt)
    typecheck(env, "{ (a: Int, box: Box) => a + box.value }") shouldBe
      SFunc(IndexedSeq(SInt, SBox), SLong)
    /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    typecheck(env, "{ (p: (Int, GroupElement), box: Box) => p._1 > box.value && p._2.isIdentity }") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SGroupElement), SBox), SBoolean)
      */
    typecheck(env, "{ (p: (Int, SigmaProp), box: Box) => p._1 > box.value && p._2.isProven }") shouldBe
      SFunc(IndexedSeq(STuple(SInt, SSigmaProp), SBox), SBoolean)

    typefail(env, "{ (a) => a + 1 }", 1, 3)
  }

  property("function definitions via val") {
    typecheck(env, "{ val f = { (x: Int) => x + 1 }; f }") shouldBe SFunc(IndexedSeq(SInt), SInt)
  }

  property("function definitions") {
    typecheck(env, "{ def f(x: Int) = { x + 1 }; f }") shouldBe SFunc(IndexedSeq(SInt), SInt)
  }

  property("recursive function definitions") {
    an[TyperException] should be thrownBy {
      typecheck(env,
        s"""{
          |  def f(x: Int) = if (x / 2 == 0) g(x) else x
          |  def g(x: Int) = f(x + 1)
          |  g(1)
          |}
      """.stripMargin)
    }
  }

  property("predefined primitives") {
    typecheck(env, "{ (box: Box) => box.value }") shouldBe SFunc(IndexedSeq(SBox), SLong)
    typecheck(env, "{ (box: Box) => box.propositionBytes }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "{ (box: Box) => box.bytes }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "{ (box: Box) => box.id }") shouldBe SFunc(IndexedSeq(SBox), SByteArray)
    typecheck(env, "{ (box: Box) => box.creationInfo }") shouldBe
      SFunc(IndexedSeq(SBox), ExtractCreationInfo.ResultType)
  }

  property("type parameters") {
    typecheck(env, "SELF.R1[Int]") shouldBe SOption(SInt)
    typecheck(env, "SELF.R1[Int].isDefined") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].isEmpty") shouldBe SBoolean
    typecheck(env, "SELF.R1[Int].get") shouldBe SInt
    // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    //  typecheck(env, "SELF.getReg[Int](1)") shouldBe SOption.SIntOption
    typefail(env, "x[Int]", 1, 1)
    typefail(env, "arr1[Int]", 1, 1)
    typecheck(env, "SELF.R1[(Int,Boolean)]") shouldBe SOption(STuple(SInt, SBoolean))
    typecheck(env, "SELF.R1[(Int,Boolean)].get") shouldBe STuple(SInt, SBoolean)
    typefail(env, "SELF.R1[Int,Boolean].get", 1, 6)
    typecheck(env, "Coll[Int]()") shouldBe SCollection(SInt)
  }

  property("compute unifying type substitution: prim types") {
    forAll { t: SPredefType =>
      unifyTypes(t, t) shouldBe Some(EmptySubst)
      unifyTypes(SAny, t) shouldBe Some(EmptySubst)
      unifyTypes(SAny, SCollection(t)) shouldBe Some(EmptySubst)
      unifyTypes(SCollection(SAny), SCollection(t)) shouldBe Some(EmptySubst)
      unifyTypes(SCollection(SAny), STuple(t, t, t)) shouldBe Some(EmptySubst)
      unifyTypes(SCollection(SAny), STuple(t, STuple(t, t))) shouldBe Some(EmptySubst)
    }
  }

  property("compute unifying type substitution") {
    def checkTypes(t1: SType, t2: SType, exp: Option[STypeSubst]): Unit = {
      unifyTypes(t1, t2) shouldBe exp
      exp match {
        case Some(subst) =>
          unifyTypes(applySubst(t1, subst), t2) shouldBe Some(EmptySubst)
        case None =>
      }
    }
    def check(s1: String, s2: String, exp: Option[STypeSubst] = Some(EmptySubst)): Unit = {
      val t1 = ty(s1); val t2 = ty(s2)
      checkTypes(t1, t2, exp)
    }
    def unify(s1: String, s2: String, subst: (STypeVar, SType)*): Unit =
      check(s1, s2, Some(subst.toMap))

    unifyTypes(NoType, NoType) shouldBe None
    unifyTypes(SLong, SBoolean) shouldBe None

    check("(Int, Boolean)", "Int", None)
    check("(Int, Boolean)", "(Int, Boolean)")
    check("(Int, Boolean)", "(Int, Int)", None)
    check("(Int, Box)", "(Int, Box)")
    check("(Int, Box)", "(Int, Box, Boolean)", None)

    check("Coll[Any]", "(Int, Long)")  // tuple as array
    check("Coll[Coll[Any]]", "Coll[(Int, Long)]")

    check("Coll[Int]", "Coll[Boolean]", None)
    check("Coll[Int]", "Coll[Int]")
    check("Coll[(Int,Box)]", "Coll[Int]", None)
    check("Coll[(Int,Box)]", "Coll[(Int,Box)]")
    check("Coll[Coll[Int]]", "Coll[Coll[Int]]")
    
    check("Option[Int]", "Option[Boolean]", None)
    check("Option[Int]", "Option[Int]")
    check("Option[(Int,Box)]", "Option[Int]", None)
    check("Option[(Int,Box)]", "Option[(Int,Box)]")
    check("Option[Option[Int]]", "Option[Option[Int]]")

    check("Int => Int", "Int => Boolean", None)
    check("Int => Int", "Int => Int")
    check("(Int, Boolean) => Int", "Int => Int", None)
    check("(Int, Boolean) => Int", "(Int,Boolean) => Int")

    unify("A", "A")
    check("A", "B", None)

    check("(Int, A)", "Int", None)
    unify("(Int, A)", "(Int, A)")
    unify("(Int, A)", "(Int, Int)", ("A", SInt))
    unify("(A, B)", "(A, B)")
    unify("(A, B)", "(Int, Boolean)", ("A", SInt), ("B", SBoolean))
    check("(A, B)", "(Int, Boolean, Box)", None)
    check("(A, Boolean)", "(Int, B)", None)
    check("(A, Int)", "(B, Int)", None)

    unify("A", "Coll[Boolean]", ("A", ty("Coll[Boolean]")))
    unify("Coll[A]", "Coll[Int]", ("A", SInt))
    unify("Coll[A]", "Coll[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Coll[(Int, A)]", "Coll[(Int, Box)]", ("A", SBox))
    unify("Coll[Coll[A]]", "Coll[Coll[Int]]", ("A", SInt))
    unify("Coll[Coll[A]]", "Coll[Coll[A]]")
    check("Coll[Coll[A]]", "Coll[Coll[B]]", None)

    unify("A", "Option[Boolean]", ("A", ty("Option[Boolean]")))
    unify("Option[A]", "Option[Int]", ("A", SInt))
    unify("Option[A]", "Option[(Int, Box)]", ("A", ty("(Int, Box)")))
    unify("Option[(Int, A)]", "Option[(Int, Box)]", ("A", SBox))
    unify("Option[Option[A]]", "Option[Option[Int]]", ("A", SInt))
    unify("Option[Option[A]]", "Option[Option[A]]")
    check("Option[Option[A]]", "Option[Option[B]]", None)

    unify("A => Int", "Int => Int", ("A", SInt))
    check("A => Int", "Int => Boolean", None)
    unify("Int => A", "Int => Int", ("A", SInt))
    check("Int => A", "Boolean => Int", None)
    unify("(Int, A) => B", "(Int, Boolean) => Box", ("A", SBoolean), ("B", SBox))
    check("(Int, A) => A", "(Int, Boolean) => Box", None)
    unify("(Int, A) => A", "(Int, Boolean) => Boolean", ("A", SBoolean))

    unify(
      "((A,Int), Coll[B] => Coll[(Coll[C], B)]) => A",
      "((Int,Int), Coll[Boolean] => Coll[(Coll[C], Boolean)]) => Int",
      ("A", SInt), ("B", SBoolean))

    unifyTypes(SBoolean, SSigmaProp) shouldBe Some(EmptySubst)
    unifyTypes(SSigmaProp, SBoolean) shouldBe None
    check("(Int, Boolean)", "(Int, SigmaProp)")
    check("(Int, Boolean, Boolean)", "(Int, SigmaProp, SigmaProp)")
    check("Coll[Boolean]", "Coll[SigmaProp]")
    check("Coll[(Int,Boolean)]", "Coll[(Int,SigmaProp)]")
    check("Coll[Coll[Boolean]]", "Coll[Coll[SigmaProp]]")
    check("Option[Boolean]", "Option[SigmaProp]")
    check("Option[(Int,Boolean)]", "Option[(Int,SigmaProp)]")
    check("Option[Option[Boolean]]", "Option[Option[SigmaProp]]")
    check("Int => Boolean", "Int => SigmaProp")
    check("(Int, Boolean) => Int", "(Int, SigmaProp) => Int")
  }

  property("most specific general (MSG) type") {
    def checkTypes(t1: SType, t2: SType, exp: Option[SType]): Unit = {
      msgType(t1, t2) shouldBe exp
    }
    def checkAllTypes(ts: Seq[SType], exp: Option[SType]): Unit = {
      msgTypeOf(ts) shouldBe exp
    }
    def check(s1: String, s2: String, exp: Option[SType]): Unit = {
      val t1 = ty(s1); val t2 = ty(s2)
      checkTypes(t1, t2, exp)
    }
    def checkAll(ts: Seq[String], exp: Option[SType]): Unit = {
      val types = ts.map(ty(_))
      checkAllTypes(types, exp)
    }

    checkTypes(NoType, NoType, None)
    checkTypes(NoType, SInt, None)
    checkTypes(SInt, SInt, Some(SInt))
    checkTypes(SBoolean, SSigmaProp, Some(SBoolean))
    checkTypes(SSigmaProp, SBoolean, Some(SBoolean))

    check("(Int, Boolean)", "(Int, SigmaProp)", Some(ty("(Int, Boolean)")))
    check("(Int, SigmaProp)", "(Int, Boolean)", Some(ty("(Int, Boolean)")))
    check("Coll[Boolean]", "Coll[SigmaProp]", Some(ty("Coll[Boolean]")))
    check("Coll[SigmaProp]", "Coll[Boolean]", Some(ty("Coll[Boolean]")))
    check("Coll[(Int,Boolean)]", "Coll[(Int,SigmaProp)]", Some(ty("Coll[(Int,Boolean)]")))
    check("Coll[(Int,SigmaProp)]", "Coll[(Int,Boolean)]", Some(ty("Coll[(Int,Boolean)]")))
    check("Coll[Coll[Boolean]]", "Coll[Coll[SigmaProp]]", Some(ty("Coll[Coll[Boolean]]")))
    check("Coll[Coll[SigmaProp]]", "Coll[Coll[Boolean]]", Some(ty("Coll[Coll[Boolean]]")))
    check("Option[(Int,Boolean)]", "Option[(Int,SigmaProp)]", Some(ty("Option[(Int,Boolean)]")))
    check("Option[(Int,SigmaProp)]", "Option[(Int,Boolean)]", Some(ty("Option[(Int,Boolean)]")))
    check("Option[Option[Boolean]]", "Option[Option[SigmaProp]]", Some(ty("Option[Option[Boolean]]")))
    check("Option[Option[SigmaProp]]", "Option[Option[Boolean]]", Some(ty("Option[Option[Boolean]]")))
    check("Int => Boolean", "Int => SigmaProp", Some(ty("Int => Boolean")))
    check("Int => SigmaProp", "Int => Boolean", Some(ty("Int => Boolean")))

    checkAll(Seq("Boolean", "SigmaProp"), Some(SBoolean))
    checkAll(Seq("Boolean", "SigmaProp", "Boolean"), Some(SBoolean))
    checkAll(Seq("Boolean", "SigmaProp", "Int"), None)
    checkAll(Seq("Int", "Int", "Int"), Some(SInt))
    checkAll(Seq("(Int, Boolean)", "(Int,SigmaProp)", "(Int,Boolean)"), Some(ty("(Int,Boolean)")))
  }

  property("invalid binary operations type check") {
    typefail(env, "1 == false", 1, 1)
    typefail(env, "1 != false", 1, 1)
    typefail(env, "1 > false", 1, 1)
    typefail(env, "1 < false", 1, 1)
    typefail(env, "1 + false", 1, 5)
    typefail(env, "1 - false", 1, 1)
    typefail(env, "1 / false", 1, 1)
    typefail(env, "1 % false", 1, 1)
    typefail(env, "min(1, false)", 1, 5)
    typefail(env, "max(1, false)", 1, 5)
    typefail(env, "1 * false", 1, 5)
    typefail(env, "1 + \"a\"", 1, 5)
    typefail(env, "1 || 1", 1, 1)
    typefail(env, "col1 || col2", 1, 1)
    typefail(env, "g1 || g2", 1, 1)
    typefail(env, "true ++ false", 1, 1)
    typefail(env, "\"a\" ++ \"a\"", 1, 1)
  }

  property("upcast for binary operations with numeric types") {
    typecheck(env, "1 == 1L") shouldBe SBoolean
    typecheck(env, "1 > 1L") shouldBe SBoolean
    typecheck(env, "1 >= 1L") shouldBe SBoolean
    typecheck(env, "1 < 1L") shouldBe SBoolean
    typecheck(env, "1 <= 1L") shouldBe SBoolean
    typecheck(env, "1 + 1L") shouldBe SLong
    typecheck(env, "1 - 1L") shouldBe SLong
    typecheck(env, "1 * 1L") shouldBe SLong
    typecheck(env, "1 / 1L") shouldBe SLong
    typecheck(env, "1 % 1L") shouldBe SLong
  }

  property("casts for numeric types") {
    typecheck(env, "1.toByte") shouldBe SByte
    typecheck(env, "1.toShort") shouldBe SShort
    typecheck(env, "1L.toInt") shouldBe SInt
    typecheck(env, "1.toLong") shouldBe SLong
    typecheck(env, "1.toBigInt") shouldBe SBigInt
    // in an expression
    typecheck(env, "1L * 1.toLong") shouldBe SLong
  }

  property("invalid cast method for numeric types") {
    typefail(env, "1.toSuperBigInteger", 1, 1)
  }

  property("toBytes method for numeric types") {
    typecheck(env, "1.toByte.toBytes",
      expected = MethodCall.typed[Value[SCollection[SByte.type]]](
        Select(IntConstant(1), "toByte", Some(SByte)),
        SNumericTypeMethods.ToBytesMethod.withConcreteTypes(Map(STypeVar("TNum") -> SByte)),
        Vector(),
        Map()
      )) shouldBe SByteArray

    typecheck(env, "1.toShort.toBytes",
      expected = MethodCall.typed[Value[SCollection[SByte.type]]](
        Select(IntConstant(1), "toShort", Some(SShort)),
        SNumericTypeMethods.ToBytesMethod.withConcreteTypes(Map(STypeVar("TNum") -> SShort)),
        Vector(),
        Map()
      )) shouldBe SByteArray

    typecheck(env, "1.toBytes",
      expected = MethodCall.typed[Value[SCollection[SByte.type]]](
        IntConstant(1),
        SNumericTypeMethods.ToBytesMethod.withConcreteTypes(Map(STypeVar("TNum") -> SInt)),
        Vector(),
        Map()
      )) shouldBe SByteArray

    typecheck(env, "1.toLong.toBytes",
      expected = MethodCall.typed[Value[SCollection[SByte.type]]](
        Select(IntConstant(1), "toLong", Some(SLong)),
        SNumericTypeMethods.ToBytesMethod.withConcreteTypes(Map(STypeVar("TNum") -> SLong)),
        Vector(),
        Map()
      )) shouldBe SByteArray

    typecheck(env, "1.toBigInt.toBytes",
      expected = MethodCall.typed[Value[SCollection[SByte.type]]](
        Select(IntConstant(1), "toBigInt", Some(SBigInt)),
        SNumericTypeMethods.ToBytesMethod.withConcreteTypes(Map(STypeVar("TNum") -> SBigInt)),
        Vector(),
        Map()
      )) shouldBe SByteArray
  }

  property("string concat") {
    typecheck(env, """ "a" + "b" """) shouldBe SString
  }

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("modular arith ops") {
    typecheck(env, "10.toBigInt.modQ") shouldBe SBigInt
    typecheck(env, "10.toBigInt.plusModQ(2.toBigInt)") shouldBe SBigInt
    typecheck(env, "10.toBigInt.minusModQ(2.toBigInt)") shouldBe SBigInt
    typefail(env, "10.modQ", 1, 1)
    typefail(env, "10.toBigInt.plusModQ(1)", 1, 1)
    typefail(env, "10.toBigInt.minusModQ(1)", 1, 1)
  }

  property("byteArrayToLong") {
    typecheck(env, "byteArrayToLong(Coll[Byte](1.toByte))") shouldBe SLong
    typefail(env, "byteArrayToLong(Coll[Int](1))", 1, 1)
  }

  property("decodePoint") {
    typecheck(env, "decodePoint(Coll[Byte](1.toByte))") shouldBe SGroupElement
    typefail(env, "decodePoint(Coll[Int](1))", 1, 1)
  }

  property("xorOf") {
    typecheck(env, "xorOf(Coll[Boolean](true, false))") shouldBe SBoolean
    typefail(env, "xorOf(Coll[Int](1))", 1, 1)
  }

  property("outerJoin") {
    typecheck(env,
      """outerJoin[Byte, Short, Int, Long](
        | Coll[(Byte, Short)]((1.toByte, 2.toShort)),
        | Coll[(Byte, Int)]((1.toByte, 3.toInt)),
        | { (b: Byte, s: Short) => (b + s).toLong },
        | { (b: Byte, i: Int) => (b + i).toLong },
        | { (b: Byte, s: Short, i: Int) => (b + s + i).toLong }
        | )""".stripMargin) shouldBe SCollection(STuple(SByte, SLong))
  }

  property("AtLeast (invalid parameters)") {
    typefail(env, "atLeast(2, 2)", 1, 1)
  }

  property("substConstants") {
    typecheck(env, "substConstants[Long](Coll[Byte](1.toByte), Coll[Int](1), Coll[Long](1L))") shouldBe SByteArray
  }

  property("executeFromVar") {
    typecheck(env, "executeFromVar[Boolean](1)") shouldBe SBoolean
  }

  property("LogicalNot") {
    typecheck(env, "!true") shouldBe SBoolean
    typefail(env, "!getVar[SigmaProp](1).get", 1, 2)
  }

  property("Negation") {
    typecheck(env, "-HEIGHT") shouldBe SInt
    typefail(env, "-true", 1, 2)
  }

  property("BitInversion") {
    typecheck(env, "~1") shouldBe SInt
    typefail(env, "~true", 1, 2)
  }

  property("LogicalXor") {
    typecheck(env, "true ^ false") shouldBe SBoolean
  }

  property("BitwiseOr") {
    typecheck(env, "1 | 2") shouldBe SInt
    typefail(env, "true | false", 1, 1)
  }

  property("BitwiseAnd") {
    typecheck(env, "1 & 2") shouldBe SInt
    typefail(env, "true & false", 1, 1)
  }

  property("BitwiseXor") {
    typecheck(env, "1 ^ 2") shouldBe SInt
  }

  property("BitShiftRight") {
    typecheck(env, "1 >> 2") shouldBe SInt
    typefail(env, "true >> false", 1, 1)
  }

  property("BitShiftLeft") {
    typecheck(env, "1 << 2") shouldBe SInt
    typefail(env, "true << false", 1, 1)
  }

  property("BitShiftRightZeroed") {
    typecheck(env, "1 >>> 2") shouldBe SInt
    typefail(env, "true >>> false", 1, 1)
  }

  property("SCollection.indices") {
    typecheck(env, "Coll(1).indices") shouldBe SCollection(SInt)
    typecheck(env, "INPUTS.indices") shouldBe SCollection(SInt)
  }

  property("SCollection.flatMap") {
    typecheck(env, "OUTPUTS.flatMap({ (out: Box) => Coll(out.value >= 1L) })") shouldBe SCollection(SBoolean)
  }

  property("SBox.tokens") {
    typecheck(env, "SELF.tokens") shouldBe ErgoBox.STokensRegType
  }

// TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
//  property("SOption.toColl") {
//    typecheck(env, "getVar[Int](1).toColl") shouldBe SIntArray
//  }

  property("SContext.dataInputs") {
    typecheck(env, "CONTEXT.dataInputs") shouldBe SCollection(SBox)
  }

  property("SContext.getVar") {
    typecheck(env, "CONTEXT.getVar[Int](1.toByte).get") shouldBe SInt
  }

  property("SAvlTree.digest") {
    typecheck(env, "getVar[AvlTree](1).get.digest") shouldBe SByteArray
  }

  property("SGroupElement.exp") {
    typecheck(env, "g1.exp(1.toBigInt)") shouldBe SGroupElement
  }

  property("substConst") {
    def script(pk: ProveDlog): SigmaPropValue =
      AND(EQ(IntConstant(1), IntConstant(1)), SigmaPropIsProven(SigmaPropConstant(pk))).toSigmaProp

    val pk1 = DLogProverInput.random().publicImage
    val pk2 = DLogProverInput.random().publicImage
    val script1 = script(pk1)
    val script2 = script(pk2)
    val inputBytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(mkTestErgoTree(script1))
    val positions = IntArrayConstant(Array[Int](2))
    val newVals = ConcreteCollection(Array[SigmaPropValue](SigmaPropConstant(pk2)), SSigmaProp)

    val expectedBytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(mkTestErgoTree(script2))

    val customEnv: ScriptEnv = Map(
      "scriptBytes" -> Colls.fromArray(inputBytes),
      "positions" -> positions,
      "newVals" -> newVals,
      "expectedBytes" -> Colls.fromArray(expectedBytes)
    )
    typecheck(customEnv, "substConstants(scriptBytes, positions, newVals)") shouldBe SByteArray
  }

  property("Global.serialize") {
    runWithVersion(VersionContext.V6SoftForkVersion) {
      typecheck(env, "Global.serialize(1)",
        MethodCall.typed[Value[SCollection[SByte.type]]](
          Global,
          SGlobalMethods.getMethodByName("serialize").withConcreteTypes(Map(STypeVar("T") -> SInt)),
          Array(IntConstant(1)),
          Map()
        )) shouldBe SByteArray
    }

    runWithVersion((VersionContext.V6SoftForkVersion - 1).toByte) {
      assertExceptionThrown(
        typecheck(env, "Global.serialize(1)"),
        exceptionLike[MethodNotFound]("Cannot find method 'serialize' in in the object Global")
      )
    }
  }

  property("predefined serialize") {
    runWithVersion(VersionContext.V6SoftForkVersion) {
      typecheck(env, "serialize((1, 2L))",
        expected = MethodCall.typed[Value[SCollection[SByte.type]]](
          Global,
          SGlobalMethods.getMethodByName("serialize").withConcreteTypes(Map(STypeVar("T") -> SPair(SInt, SLong))),
          Array(Tuple(Vector(IntConstant(1), LongConstant(2L)))),
          Map()
        )) shouldBe SByteArray
    }
  }

}
