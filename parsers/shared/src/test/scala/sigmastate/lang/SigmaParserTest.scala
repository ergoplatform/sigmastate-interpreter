package sigmastate.lang

import fastparse.Parsed
import fastparse.Parsed.Failure
import org.ergoplatform.ErgoBox
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.SCollection.{SByteArray, SLongArray}
import sigma.ast._
import sigma.ast.syntax.SValue
import sigmastate._
import SigmaPredef.PredefinedFuncRegistry
import sigma.ast.syntax._
import sigmastate.lang.parsers.ParserException
import sigma.serialization.OpCodes
import sigmastate.helpers.SigmaPPrint

class SigmaParserTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with LangTests {
  import StdSigmaBuilder._

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  import predefFuncRegistry._

  def parse(x: String): SValue = {
    SigmaParser(x) match {
      case Parsed.Success(v, _) =>
        v.sourceContext.isDefined shouldBe true
        assertSrcCtxForAllNodes(v)
        v
      case f: Failure =>
        val traced = f.extra.trace()
        println(s"\nTRACE: ${traced.msg}")
        f.get // force show error diagnostics
    }
  }

  /** Checks parsing result, printing the actual value as a test vector if expected value
    * is not equal to actual.
    */
  def checkParsed(x: String, expected: SValue) = {
    val parsed = parse(x)
    if (expected != parsed) {
      SigmaPPrint.pprintln(parsed, width = 100)
    }
    parsed shouldBe expected
  }

  def parseWithException(x: String): SValue = {
    SigmaParser(x) match {
      case Parsed.Success(v, _) => v
      case f: Parsed.Failure =>
        throw new ParserException(s"Syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  def parseType(x: String): SType = {
    SigmaParser.parseType(x)
  }

  def fail(x: String, expectedLine: Int, expectedCol: Int): Unit = {
    val exception = the[ParserException] thrownBy parseWithException(x)
    withClue(s"Exception: $exception, is missing source context:") { exception.source shouldBe defined }
    val sourceContext = exception.source.get
    sourceContext.line shouldBe expectedLine
    sourceContext.column shouldBe expectedCol
  }

  def and(l: SValue, r: SValue) = MethodCallLike(l, "&&", IndexedSeq(r))
  def or(l: SValue, r: SValue) = MethodCallLike(l, "||", IndexedSeq(r))
  def xor(l: SValue, r: SValue) = MethodCallLike(l, "^", IndexedSeq(r))

  property("simple expressions") {
    parse("10") shouldBe IntConstant(10)
    parse("-10") shouldBe IntConstant(-10)
    parse("10L") shouldBe LongConstant(10)
    parse("10l") shouldBe LongConstant(10)
    parse("-10L") shouldBe LongConstant(-10)
    parse("0x10") shouldBe IntConstant(0x10)
    parse("0x10L") shouldBe LongConstant(0x10)
    parse("0x10l") shouldBe LongConstant(0x10)
    parse("10L-11L") shouldBe Minus(10L, 11L)
    parse("(10-11)") shouldBe Minus(10, 11)
    parse("(-10-11)") shouldBe Minus(-10, 11)
    parse("(10+11)") shouldBe plus(10, 11)
    parse("(10-11) - 12") shouldBe Minus(Minus(10, 11), 12)
    parse("10   - 11 - 12") shouldBe Minus(Minus(10, 11), 12)
    parse("10   + 11 + 12") shouldBe plus(plus(10, 11), 12)
    parse("1-2-3-4-5") shouldBe Minus(Minus(Minus(Minus(1, 2), 3), 4), 5)
    parse("10 - 11") shouldBe Minus(10, 11)
    parse("1 / 2") shouldBe Divide(1, 2)
    parse("5 % 2") shouldBe Modulo(5, 2)
    parse("1==1") shouldBe EQ(1, 1)
    parse("true && true") shouldBe and(TrueLeaf, TrueLeaf)
    parse("true || false") shouldBe or(TrueLeaf, FalseLeaf)
    parse("true ^ false") shouldBe xor(TrueLeaf, FalseLeaf)
    parse("true || (true && false)") shouldBe or(TrueLeaf, and(TrueLeaf, FalseLeaf))
    parse("true || (true ^ false)") shouldBe or(TrueLeaf, xor(TrueLeaf, FalseLeaf))
    parse("false || false || false") shouldBe or(or(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("false ^ false ^ false") shouldBe xor(xor(FalseLeaf, FalseLeaf), FalseLeaf)
    parse("(1>= 0)||(3L >2L)") shouldBe or(GE(1, 0), GT(3L, 2L))
    parse("arr1 ++ arr2") shouldBe MethodCallLike(Ident("arr1"), "++", IndexedSeq(Ident("arr2")))
    parse("col1 ++ col2") shouldBe MethodCallLike(Ident("col1"), "++", IndexedSeq(Ident("col2")))
    parse("ge.exp(n)") shouldBe Apply(Select(GEIdent("ge"), "exp"), Vector(BigIntIdent("n")))
    parse("g1 * g2") shouldBe MethodCallLike(Ident("g1"), "*", IndexedSeq(Ident("g2")))
    parse("g1 + g2") shouldBe MethodCallLike(Ident("g1"), "+", IndexedSeq(Ident("g2")))
  }

  property("precedence of binary operations") {
    parse("1 - 2 - 3") shouldBe Minus(Minus(1, 2), 3)
    parse("1 + 2 + 3") shouldBe plus(plus(1, 2), 3)
    parse("1 - 2 - 3 - 4") shouldBe Minus(Minus(Minus(1, 2), 3), 4)
    parse("1 + 2 + 3 + 4") shouldBe plus(plus(plus(1, 2), 3), 4)
    parse("1 == 0 || 3 == 2") shouldBe or(EQ(1, 0), EQ(3, 2))
    parse("3 - 2 > 2 - 1") shouldBe GT(Minus(3, 2), Minus(2, 1))
    parse("3 + 2 > 2 + 1") shouldBe GT(plus(3, 2), plus(2, 1))
    parse("1 - 2 - 3 > 4 - 5 - 6") shouldBe GT(Minus(Minus(1, 2), 3), Minus(Minus(4, 5), 6))
    parse("1 + 2 + 3 > 4 + 5 + 6") shouldBe GT(plus(plus(1, 2), 3), plus(plus(4, 5), 6))
    parse("1 >= 0 || 3 > 2") shouldBe or(GE(1, 0), GT(3, 2))
    parse("2 >= 0 - 1 || 3 - 1 >= 2") shouldBe or(GE(2, Minus(0, 1)), GE(Minus(3, 1), 2))
    parse("2 >= 0 + 1 || 3 + 1 >= 2") shouldBe or(GE(2, plus(0, 1)), GE(plus(3, 1), 2))
    parse("x1 || x2 > x3 - x4 - x5 || x6") shouldBe
      or(
        or(BoolIdent("x1"),
           GT(IntIdent("x2"),
              Minus(Minus(IntIdent("x3"), IntIdent("x4")), IntIdent("x5")))),
        BoolIdent("x6"))
    parse("x1 || x2 > x3 - x4") shouldBe
      or(BoolIdent("x1"),
        GT(IntIdent("x2"),
          Minus(IntIdent("x3"), IntIdent("x4"))))
    parse("x1 || x2 > x3 + x4 + x5 || x6") shouldBe
      or(
        or(BoolIdent("x1"),
          GT(IntIdent("x2"),
            plus(plus(IntIdent("x3"), IntIdent("x4")), IntIdent("x5")))),
        BoolIdent("x6"))
    parse("x1 || x2 > x3 + x4") shouldBe
      or(BoolIdent("x1"),
        GT(IntIdent("x2"),
          plus(IntIdent("x3"), IntIdent("x4"))))
  }

  property("tuple operations") {
    parse("()") shouldBe UnitConstant()
    parse("(1)") shouldBe IntConstant(1)
    parse("(1, 2)") shouldBe Tuple(IntConstant(1), IntConstant(2))
    parse("(1, X - 1)") shouldBe Tuple(IntConstant(1), mkMinus(IntIdent("X"), 1))
    parse("(1, X + 1)") shouldBe Tuple(IntConstant(1), plus(IntIdent("X"), 1))
    parse("(1, 2, 3)") shouldBe Tuple(IntConstant(1), IntConstant(2), IntConstant(3))
    parse("(1, 2 - 3, 4)") shouldBe Tuple(IntConstant(1), Minus(2, 3), IntConstant(4))
    parse("(1, 2 + 3, 4)") shouldBe Tuple(IntConstant(1), plus(2, 3), IntConstant(4))

    parse("(1, 2L)._1") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "_1")
    parse("(1, 2L)._2") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "_2")
    parse("(1, 2L, 3)._3") shouldBe Select(Tuple(IntConstant(1), LongConstant(2), IntConstant(3)), "_3")

    // tuple as collection
    parse("(1, 2L).size") shouldBe Select(Tuple(IntConstant(1), LongConstant(2)), "size")
    parse("(1, 2L)(0)") shouldBe Apply(Tuple(IntConstant(1), LongConstant(2)), IndexedSeq(IntConstant(0)))
    parse("(1, 2L).getOrElse(2, 3)") shouldBe Apply(Select(Tuple(IntConstant(1), LongConstant(2)), "getOrElse"), IndexedSeq(IntConstant(2), IntConstant(3)))
    parse("{ (a: Int) => (1, 2L)(a) }") shouldBe Lambda(IndexedSeq("a" -> SInt), Apply(Tuple(IntConstant(1), LongConstant(2)), IndexedSeq(Ident("a"))))
  }

  property("val constructs") {
    parse(
      """{val X = 10
        |3 > 2}
      """.stripMargin) shouldBe Block(Val("X", IntConstant(10)), GT(3, 2))

    parse("{val X = 10; 3 > 2}") shouldBe Block(Val("X", IntConstant(10)), GT(3, 2))
    parse("{val X = 3 - 2; 3 > 2}") shouldBe Block(Val("X", Minus(3, 2)), GT(3, 2))
    parse("{val X = 3 + 2; 3 > 2}") shouldBe Block(Val("X", plus(3, 2)), GT(3, 2))
    parse("{val X = if (true) true else false; false}") shouldBe Block(Val("X", If(TrueLeaf, TrueLeaf, FalseLeaf)), FalseLeaf)

    val expr = parse(
      """{val X = 10
        |val Y = 11
        |X > Y}
      """.stripMargin)

    expr shouldBe Block(Seq(Val("X", IntConstant(10)),Val("Y", IntConstant(11))), GT(IntIdent("X"), IntIdent("Y")))
  }

  property("types") {
    parse("{val X: Byte = 10; 3 > 2}") shouldBe Block(Seq(Val("X", SByte, IntConstant(10))), GT(3, 2))
    parse("{val X: Int = 10; 3 > 2}") shouldBe Block(Seq(Val("X", SInt, IntConstant(10))), GT(3, 2))
    parse("""{val X: (Int, Boolean) = (10, true); 3 > 2}""") shouldBe
      Block(Seq(Val("X", STuple(SInt, SBoolean), Tuple(IntConstant(10), TrueLeaf))), GT(3, 2))
    parse("""{val X: Coll[Int] = Coll(1,2,3); X.size}""") shouldBe
      Block(Seq(Val("X", SCollection(SInt), Apply(Ident("Coll"), IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))))),
            Select(Ident("X"), "size"))
    parse("""{val X: (Coll[Int], Box) = (Coll(1,2,3), INPUT); X._1}""") shouldBe
        Block(Seq(Val("X", STuple(SCollection(SInt), SBox), Tuple(Apply(Ident("Coll"), IndexedSeq(IntConstant(1), IntConstant(2), IntConstant(3))), Ident("INPUT")))),
          Select(Ident("X"), "_1"))
  }

  property("multiline") {
    parse(
      """
        |
        |false
        |
        |
      """.stripMargin) shouldBe FalseLeaf

    parse(
      """{val X = 10;
        |
        |true}
      """.stripMargin) shouldBe Block(Seq(Val("X", IntConstant(10))), TrueLeaf)
    parse(
      """{val X = 11
        |true}
      """.stripMargin) shouldBe Block(Seq(Val("X", IntConstant(11))), TrueLeaf)
  }

  property("comments") {
    parse(
      """{
       |// line comment
       |val X = 12
       |/* comment // nested line comment
       |*/
       |3 - // end line comment
       |  2
       |}
      """.stripMargin) shouldBe Block(Seq(Val("X", IntConstant(12))), Minus(3, 2))
  }

  property("if") {
    parse("if(true) 1 else 2") shouldBe If(TrueLeaf, IntConstant(1), IntConstant(2))
    parse("if(true) 1 else if(X==Y) 2 else 3") shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))
    parse(
      """if ( true )
        |1
        |else if(X== Y)
        |     2
        |     else 3""".stripMargin) shouldBe If(TrueLeaf, IntConstant(1), If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3)))

    parse("if (true) false else false==false") shouldBe If(TrueLeaf, FalseLeaf, EQ(FalseLeaf, FalseLeaf))

    parse(
      """if

             (true)
        |{ val A = 10;
        |  1 }
        |else if ( X == Y) 2 else 3""".stripMargin) shouldBe
        If(TrueLeaf,
          Block(Seq(Val("A", IntConstant(10))), IntConstant(1)),
          If(EQ(Ident("X"), Ident("Y")), IntConstant(2), IntConstant(3))
    )

  }

  property("array literals") {
    val emptyColl = Apply(Ident("Coll"), IndexedSeq.empty)
    parse("Coll()") shouldBe emptyColl
    val emptyColl2 = Apply(Ident("Coll"), IndexedSeq(emptyColl))
    parse("Coll(Coll())") shouldBe emptyColl2
    parse("Coll(Coll(Coll()))") shouldBe Apply(Ident("Coll"), IndexedSeq(emptyColl2))

    parse("Coll(1)") shouldBe Apply(Ident("Coll"), IndexedSeq(IntConstant(1)))
    parse("Coll(1, X)") shouldBe Apply(Ident("Coll"), IndexedSeq(IntConstant(1), Ident("X")))
    parse("Coll(1, X - 1, Coll())") shouldBe
    Apply(Ident("Coll"),
      IndexedSeq(
        IntConstant(1),
        mkMinus(Ident("X").asValue[SLong.type], IntConstant(1)),
        Apply(Ident("Coll"), IndexedSeq.empty)))
    parse("Coll(1, X + 1, Coll())") shouldBe
      Apply(Ident("Coll"),
        IndexedSeq(
          IntConstant(1),
          plus(Ident("X").asValue[SLong.type], IntConstant(1)),
          Apply(Ident("Coll"), IndexedSeq.empty)))
    parse("Coll(Coll(X - 1))") shouldBe Apply(Ident("Coll"),
      IndexedSeq(Apply(Ident("Coll"), IndexedSeq(
                  mkMinus(Ident("X").asValue[SLong.type], IntConstant(1))))))
    parse("Coll(Coll(X + 1))") shouldBe Apply(Ident("Coll"),
      IndexedSeq(Apply(Ident("Coll"), IndexedSeq(
        plus(Ident("X").asValue[SLong.type], IntConstant(1))))))
  }

  property("Option constructors") {
    parse("None") shouldBe Ident("None")
    parse("Some(None)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("None")))
    parse("Some(10)") shouldBe Apply(Ident("Some"), IndexedSeq(IntConstant(10)))
    parse("Some(X)") shouldBe Apply(Ident("Some"), IndexedSeq(Ident("X")))
    parse("Some(Some(X - 1))") shouldBe Apply(Ident("Some"),
      IndexedSeq(Apply(Ident("Some"), IndexedSeq(
                  mkMinus(Ident("X").asValue[SLong.type], IntConstant(1))))))
    parse("Some(Some(X + 1))") shouldBe Apply(Ident("Some"),
      IndexedSeq(Apply(Ident("Some"), IndexedSeq(
        plus(Ident("X").asValue[SLong.type], IntConstant(1))))))
  }

  property("array indexed access") {
    parse("Coll()") shouldBe Apply(Ident("Coll"), IndexedSeq.empty)
    parse("Array()(0)") shouldBe Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0)))
    parse("Array()(0)(0)") shouldBe Apply(Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0))), IndexedSeq(IntConstant(0)))
  }

  property("array indexed access with default values") {
    parse("Array()(0, 1)") shouldBe
      Apply(Apply(Ident("Array"), IndexedSeq.empty), IndexedSeq(IntConstant(0), IntConstant(1)))
    parse("Array()(0, 1)(0)") shouldBe
      Apply(Apply(Apply(Ident("Array"), IndexedSeq.empty),
        IndexedSeq(IntConstant(0), IntConstant(1))),
        IndexedSeq(IntConstant(0)))
  }

  property("generic methods of arrays") {
    parse("OUTPUTS.map({ (out: Box) => out.value })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "map"),
            Vector(Lambda(Vector(("out",SBox)), Select(Ident("out"),"value"))))
    parse("OUTPUTS.exists({ (out: Box) => out.value > 0 })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "exists"),
            Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
    parse("OUTPUTS.forall({ (out: Box) => out.value > 0 })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "forall"),
            Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
    parse("Array(1,2).fold(0, { (n1: Int, n2: Int) => n1 - n2 })") shouldBe
      Apply(
        Select(Apply(Ident("Array"), Vector(IntConstant(1), IntConstant(2))), "fold"),
        Vector(IntConstant(0), Lambda(Vector(("n1",SInt), ("n2",SInt)), Minus(IntIdent("n1"), IntIdent("n2"))))
      )
    parse("Array(1,2).fold(0, { (n1: Int, n2: Int) => n1 + n2 })") shouldBe
      Apply(
        Select(Apply(Ident("Array"), Vector(IntConstant(1), IntConstant(2))), "fold"),
        Vector(IntConstant(0), Lambda(Vector(("n1",SInt), ("n2",SInt)), plus(IntIdent("n1"), IntIdent("n2"))))
      )
    parse("OUTPUTS.slice(0, 10)") shouldBe
        Apply(Select(Ident("OUTPUTS"), "slice"), Vector(IntConstant(0), IntConstant(10)))
    parse("OUTPUTS.filter({ (out: Box) => out.value > 0 })") shouldBe
        Apply(Select(Ident("OUTPUTS"), "filter"),
          Vector(Lambda(Vector(("out",SBox)), GT(Select(Ident("out"),"value").asIntValue, 0))))
  }

  property("global functions") {
    parse("f(x)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x")))
    parse("f((x, y))") shouldBe Apply(Ident("f"), IndexedSeq(Tuple(IndexedSeq(Ident("x"), Ident("y")))))
    parse("f(x, y)") shouldBe Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y")))
    parse("f(x, y).size") shouldBe Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "size")
    parse("f(x, y).get(1)") shouldBe Apply(Select(Apply(Ident("f"), IndexedSeq(Ident("x"), Ident("y"))), "get"), IndexedSeq(IntConstant(1)))
    parse("{val y = f(x); y}") shouldBe Block(Seq(Val("y", Apply(Ident("f"), IndexedSeq(Ident("x"))))), Ident("y"))
    parse("getVar[Coll[Byte]](10).get") shouldBe Select(Apply(ApplyTypes(Ident("getVar"), Seq(SByteArray)), IndexedSeq(IntConstant(10))), "get")
    parse("min(x, y)") shouldBe Apply(Ident("min"), IndexedSeq(Ident("x"), Ident("y")))
    parse("min(1, 2)") shouldBe Apply(Ident("min"), IndexedSeq(IntConstant(1), IntConstant(2)))
    parse("max(x, y)") shouldBe Apply(Ident("max"), IndexedSeq(Ident("x"), Ident("y")))
    parse("max(1, 2)") shouldBe Apply(Ident("max"), IndexedSeq(IntConstant(1), IntConstant(2)))
  }

  property("lambdas") {
    parse("{ (x) => x - 1 }") shouldBe
      Lambda(IndexedSeq("x" -> NoType), mkMinus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int) => x - 1 }") shouldBe
      Lambda(IndexedSeq("x" -> SInt), mkMinus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int) => x + 1 }") shouldBe
      Lambda(IndexedSeq("x" -> SInt), plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int) => x - 1 }") shouldBe
      Lambda(IndexedSeq("x" -> SInt), NoType, mkMinus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int) => x + 1 }") shouldBe
      Lambda(IndexedSeq("x" -> SInt), NoType, plus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int, box: Box) => x - box.value }") shouldBe
        Lambda(IndexedSeq("x" -> SInt, "box" -> SBox), NoType,
               Minus(Ident("x").asValue[SInt.type], Select(Ident("box"), "value").asValue[SLong.type]))
    parse("{ (p: (Int, GroupElement), box: Box) => p._1 > box.value && p._2.isIdentity }") shouldBe
        Lambda(IndexedSeq("p" -> STuple(SInt, SGroupElement), "box" -> SBox), NoType,
          and(
            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SLong.type]),
            Select(Select(Ident("p"), "_2"), "isIdentity").asValue[SBoolean.type]
            )
        )
    parse("{ (p: (Int, SigmaProp), box: Box) => p._1 > box.value && p._2.isProven }") shouldBe
        Lambda(IndexedSeq("p" -> STuple(SInt, SSigmaProp), "box" -> SBox), NoType,
          and(
            GT(Select(Ident("p"), "_1").asValue[SInt.type], Select(Ident("box"), "value").asValue[SLong.type]),
            Select(Select(Ident("p"), "_2"), "isProven").asValue[SBoolean.type]
            )
        )

    parse("{ (x) => x - 1 }") shouldBe
        Lambda(IndexedSeq("x" -> NoType), mkMinus(Ident("x").asValue[SInt.type], IntConstant(1)))
    parse("{ (x: Int) => { x - 1 } }") shouldBe
        Lambda(IndexedSeq("x" -> SInt), Block(Seq(), mkMinus(Ident("x").asValue[SInt.type], IntConstant(1))))
    parse("{ (x: Int) =>  val y = x - 1; y }") shouldBe
      Lambda(IndexedSeq("x" -> SInt),
        Block(Val("y", mkMinus(IntIdent("x"), 1)), Ident("y")))
    parse("{ (x: Int) => { val y = x - 1; y } }") shouldBe
        Lambda(IndexedSeq("x" -> SInt),
          Block(Val("y", mkMinus(IntIdent("x"), 1)), Ident("y")))
    parse(
      """{ (x: Int) =>
        |val y = x - 1
        |y
        |}""".stripMargin) shouldBe
      Lambda(IndexedSeq("x" -> SInt),
        Block(Val("y", mkMinus(IntIdent("x"), 1)), Ident("y")))
  }

  property("passing a lambda argument") {
    val tree = Apply(Select(Ident("arr"), "exists"),
      IndexedSeq(Lambda(IndexedSeq("a" -> SInt), GE(Ident("a"), IntConstant(1)))))
    // both parens and curly braces
    parse("arr.exists ({ (a: Int) => a >= 1 })") shouldBe tree
    // only curly braces (one line)
    parse("arr.exists { (a: Int) => a >= 1 }") shouldBe tree
    // only curly braces (multi line)
    parse(
      """arr.exists { (a: Int) =>
        |a >= 1 }""".stripMargin) shouldBe tree

    val tree1 = Apply(Ident("f"), IndexedSeq(
      Lambda(IndexedSeq("a" -> SInt),
        Block(Val("b", mkMinus(IntIdent("a"), IntConstant(1))), Minus(IntIdent("a"), IntIdent("b"))))
    ))
    // single line block
    parse("f { (a: Int) => val b = a - 1; a - b }") shouldBe tree1
    // multi line block
    parse(
      """f { (a: Int) =>
        |val b = a - 1
        |a - b
        |}""".stripMargin) shouldBe tree1

    // nested lambda
    parse(
      """f { (a: Int) =>
        |def g(c: Int) = c - 1
        |a - g(a)
        |}""".stripMargin) shouldBe Apply(Ident("f"), IndexedSeq(
      Lambda(IndexedSeq("a" -> SInt),
        Block(
          Val("g",
            Lambda(IndexedSeq("c" -> SInt), mkMinus(IntIdent("c"), IntConstant(1)))),
          Minus(IntIdent("a"), Apply(IntIdent("g"), IndexedSeq(IntIdent("a"))).asValue[SLong.type]))
      )))
  }

  property("function definitions via val") {
    parse("{val f = { (x: Int) => x - 1 }; f}") shouldBe
      Block(Val("f", Lambda(IndexedSeq("x" -> SInt), mkMinus(IntIdent("x"), 1))), Ident("f"))
    parse(
      """{val f = { (x: Int) => x - 1 }
       |f}
      """.stripMargin) shouldBe
        Block(Val("f", Lambda(IndexedSeq("x" -> SInt), mkMinus(IntIdent("x"), 1))), Ident("f"))
  }

  property("function (one arg) definition expr body") {
    parse("{ def f(x: Int): Int = x - 1 }") shouldBe Block(List(),
      Val("f", SInt, Lambda(IndexedSeq("x" -> SInt), SInt, mkMinus(IntIdent("x"), 1))))
  }

  property("function (one arg) definition with no res type, expr body") {
    parse("{ def f(x: Int) = x - 1 }") shouldBe Block(List(),
      Val("f", NoType, Lambda(IndexedSeq("x" -> SInt), NoType, mkMinus(IntIdent("x"), 1))))
  }

  property("function (one arg) definition brackets body") {
    val expectedTree = Block(List(),
      Val("f", SInt, Lambda(IndexedSeq("x" -> SInt), SInt, Block(List(), mkMinus(IntIdent("x"), 1)))))
    parse("{ def f(x: Int): Int = { x - 1 } }") shouldBe expectedTree
    parse(
      """{
         def f(x: Int): Int = {
           x - 1
         }
        }
      """.stripMargin) shouldBe expectedTree
  }

  property("function(two arg) definition expr body") {
    parse("{ def f(x: Int, y: Int): Int = x - y }") shouldBe Block(List(),
      Val("f", SInt,
        Lambda(IndexedSeq("x" -> SInt, "y" -> SInt), SInt, mkMinus(IntIdent("x"), IntIdent("y")))))
  }

  property("function definition and application") {
    parse(
      """{
         def f(x: Int): Int = {
           x - 1
         }
         f(5)
        }
      """.stripMargin) shouldBe Block(
      Val("f", SInt,
        Lambda(IndexedSeq("x" -> SInt), SInt, Block(List(), mkMinus(IntIdent("x"), 1)))),
      Apply(Ident("f"), Vector(IntConstant(5)))
    )
  }

  property("function with type args") {
    val tA = STypeVar("A")
    val tB = STypeVar("B")
    parse("{ def f[A, B](x: A, y: B): (A, B) = (x, y) }") shouldBe Block(List(),
      Val("f",
        STuple(tA, tB),
        Lambda(IndexedSeq("x" -> tA, "y" -> tB),
          STuple(tA, tB),
          Tuple(Ident("x"), Ident("y"))
        )
      )
    )
  }

  property("function (no args) definition expr body") {
    parse("{ def f: Int = 1 }") shouldBe Block(List(),
      Val("f", SInt, Lambda(IndexedSeq(), SInt, IntConstant(1))))
  }

  property("method extension(dotty)(no args) with type args") {
    val tA = STypeVar("A")
    val tB = STypeVar("B")
    parse("{ def (pairs: Coll[(A,B)]) f[A, B]: Coll[(B, A)] = pairs.magicSwap }") shouldBe Block(List(),
      Val("f",
        SCollection(STuple(tB, tA)),
        Lambda(IndexedSeq("pairs" -> SCollection(STuple(tA, tB))),
          SCollection(STuple(tB, tA)),
          Select(Ident("pairs"), "magicSwap")
        )
      )
    )
  }

  property("method extension(dotty)(one arg) with type args") {
    val tA = STypeVar("A")
    val tB = STypeVar("B")
    parse("{ def (pairs: Coll[(A,B)]) take[A, B](i: Int): Coll[(A, B)] = pairs.drop(i) }") shouldBe Block(List(),
      Val("take",
        SCollection(STuple(tA, tB)),
        Lambda(IndexedSeq("pairs" -> SCollection(STuple(tA, tB)), "i" -> SInt),
          SCollection(STuple(tA, tB)),
          Apply(Select(Ident("pairs"), "drop"), Vector(Ident("i")))
        )
      )
    )
  }

  property("get field of ref") {
    parse("XXX.YYY") shouldBe Select(Ident("XXX"), "YYY")
    parse("""
        |
        | X.Y
        |
      """.stripMargin) shouldBe Select(Ident("X"), "Y")
  }

  property("Box properties") {
    parse("{ (box: Box) => box.value }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "value"))
    parse("{ (box: Box) => box.propositionBytes }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), SBoxMethods.PropositionBytes))
    parse("{ (box: Box) => box.bytes }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "bytes"))
    parse("{ (box: Box) => box.id }") shouldBe Lambda(IndexedSeq("box" -> SBox), NoType, Select(Ident("box"), "id"))
  }

  property("type parameters") {
    parse("X[Byte]") shouldBe ApplyTypes(Ident("X"), Seq(SByte))
    parse("X[Int]") shouldBe ApplyTypes(Ident("X"), Seq(SInt))
    parse("X[Int].isDefined") shouldBe Select(ApplyTypes(Ident("X"), Seq(SInt)), "isDefined")
    parse("X[Int].isEmpty") shouldBe Select(ApplyTypes(Ident("X"), Seq(SInt)), "isEmpty")
    parse("X[(Int, Boolean)]") shouldBe ApplyTypes(Ident("X"), Seq(STuple(SInt, SBoolean)))
    parse("X[Int, Boolean]") shouldBe ApplyTypes(Ident("X"), Seq(SInt, SBoolean))
    parse("SELF.R1[Int]") shouldBe ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SInt))
    parse("SELF.getReg[Int](1)") shouldBe Apply(ApplyTypes(Select(Ident("SELF"), "getReg"), Seq(SInt)), IndexedSeq(IntConstant(1)))
    parse("SELF.R1[Int].isDefined") shouldBe Select(ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SInt)),"isDefined")
    parse("SELF.R1[Int].isEmpty") shouldBe Select(ApplyTypes(Select(Ident("SELF"), "R1"), Seq(SInt)),"isEmpty")
    parse("f[Int](10)") shouldBe Apply(ApplyTypes(Ident("f"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    parse("INPUTS.map[Int]") shouldBe ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SInt))
    parse("INPUTS.map[Int](10)") shouldBe Apply(ApplyTypes(Select(Ident("INPUTS"), "map"), Seq(SInt)), IndexedSeq(IntConstant(10)))
    parse("Coll[Int]()") shouldBe Apply(ApplyTypes(Ident("Coll"), Seq(SInt)), IndexedSeq.empty)
  }

  property("type tests") {
    parseType("Int") shouldBe SInt
    parseType("(Int, Long)") shouldBe STuple(SInt, SLong)
    parseType("Coll[(Int, Long)]") shouldBe SCollection(STuple(SInt, SLong))
    parseType("Coll[(Coll[Byte], Long)]") shouldBe ErgoBox.STokensRegType
    parseType("Coll[(Coll[Byte], Coll[Long])]") shouldBe SCollection(STuple(SByteArray, SLongArray))
    parseType("Coll[(Coll[Byte], (Coll[Long], Long))]") shouldBe SCollection(STuple(SByteArray, STuple(SLongArray, SLong)))
  }

  property("negative tests") {
    fail("(10", 1, 4)
    fail("10)", 1, 3)
    fail("X)", 1, 2)
    fail("(X", 1, 3)
    fail("{ X", 1, 4)
    fail("{ val X", 1, 8)
    fail("\"str", 1, 5)
  }

  property("not(yet) supported lambda syntax") {
    // passing a lambda without curly braces is not supported yet :)
    fail("arr.exists ( (a: Int) => a >= 1 )", 1, 16)
    // no argument type
    an[ParserException] should be thrownBy parse("arr.exists ( a => a >= 1 )")
    an[ParserException] should be thrownBy parse("arr.exists { a => a >= 1 }")
  }

  property("numeric casts") {
    parse("1.toByte") shouldBe Select(IntConstant(1), "toByte")
    parse("1.toShort") shouldBe Select(IntConstant(1), "toShort")
    parse("1L.toInt") shouldBe Select(LongConstant(1), "toInt")
    parse("1.toLong") shouldBe Select(IntConstant(1), "toLong")
    parse("1.toBigInt") shouldBe Select(IntConstant(1), "toBigInt")
  }

  property("string literals") {
    parse("\"hello\"") shouldBe StringConstant("hello")
    // triple double quotes
    parse("\"\"\"hello\"\"\"") shouldBe StringConstant("hello")
    // triple double quotes with newline and a backslash
    parse("\"\"\"hel\nlo\"\"\"") shouldBe StringConstant("hel\nlo")
    assertExceptionThrown(
      parse("\"\"\"h\\el\nlo\"\"\""),
      exceptionLike[Exception]("Parse Error, Position 1:5")
    )
    // in expression
    parse(""" "hello" == "hello" """) shouldBe EQ(StringConstant("hello"), StringConstant("hello"))
  }

  property("string concat") {
    parse(""" "hello" + "hello" """) shouldBe
      MethodCallLike(StringConstant("hello"), "+", IndexedSeq(StringConstant("hello")))
  }

  property("bigInt string decoding") {
    parse("""bigInt("32667486267383620946248345338628674027033885928301927616853987602485119134400")""") shouldBe
      Apply(BigIntFromStringFunc.symNoType, IndexedSeq(StringConstant("32667486267383620946248345338628674027033885928301927616853987602485119134400")))
  }

  property("fromBaseX string decoding") {
    parse("""fromBase16("1111")""") shouldBe Apply(FromBase16Func.symNoType, IndexedSeq(StringConstant("1111")))
    parse("""fromBase58("111")""") shouldBe Apply(FromBase58Func.symNoType, IndexedSeq(StringConstant("111")))
    parse("""fromBase64("111")""") shouldBe Apply(FromBase64Func.symNoType, IndexedSeq(StringConstant("111")))
  }

  property("PK") {
    parse("""PK("111")""") shouldBe Apply(Ident("PK"), IndexedSeq(StringConstant("111")))
  }

  property("deserialize") {
    parse("""deserialize[GroupElement]("12345")""") shouldBe
      Apply(ApplyTypes(DeserializeFunc.symNoType, Seq(SGroupElement)), IndexedSeq(StringConstant("12345")))
    parse("""deserialize[(GroupElement, Coll[(Int, Byte)])]("12345")""") shouldBe
      Apply(ApplyTypes(DeserializeFunc.symNoType, Seq(STuple(SGroupElement, SCollection(STuple(SInt, SByte))))), IndexedSeq(StringConstant("12345")))
  }

  property("ZKProof") {
    parse("ZKProof { condition }") shouldBe Apply(ZKProofFunc.sym, IndexedSeq(Ident("condition")))
    parse("ZKProof { sigmaProp(HEIGHT > 1000) }") shouldBe
      Apply(ZKProofFunc.sym,
        IndexedSeq(Apply(SigmaPropFunc.symNoType, IndexedSeq(GT(Ident("HEIGHT"), IntConstant(1000))))))
  }

  property("invalid ZKProof (non block parameter)") {
    fail("ZKProof 1 > 1", 1, 9)
  }

  property("sigmaProp") {
    parse("sigmaProp(HEIGHT > 1000)") shouldBe Apply(SigmaPropFunc.symNoType,
      IndexedSeq(GT(Ident("HEIGHT"), IntConstant(1000))))
  }

  property("SBigInt.toBytes") {
    parse("10.toBigInt.toBytes") shouldBe Select(Select(IntConstant(10), "toBigInt"), "toBytes")
  }

  property("SBigInt.modQ") {
    parse("10.toBigInt.modQ") shouldBe Select(Select(IntConstant(10), "toBigInt"), "modQ")
  }

  property("SBigInt.plusModQ") {
    parse("10.toBigInt.plusModQ(1.toBigInt)") shouldBe
      Apply(Select(Select(IntConstant(10), "toBigInt"), "plusModQ"), Vector(Select(IntConstant(1), "toBigInt")))
  }

  property("SBigInt.minusModQ") {
    parse("10.toBigInt.minusModQ(1.toBigInt)") shouldBe
      Apply(Select(Select(IntConstant(10), "toBigInt"), "minusModQ"), Vector(Select(IntConstant(1), "toBigInt")))
  }

  property("SBigInt.multModQ") {
    parse("10.toBigInt.multModQ(1.toBigInt)") shouldBe
      Apply(Select(Select(IntConstant(10), "toBigInt"), "multModQ"), Vector(Select(IntConstant(1), "toBigInt")))
  }

  property("byteArrayToLong") {
    parse("byteArrayToLong(Coll[Byte](1.toByte))") shouldBe
      Apply(ByteArrayToLongFunc.symNoType, Vector(
        Apply(
          ApplyTypes(Ident("Coll", NoType), Vector(SByte)),
          Vector(Select(IntConstant(1), "toByte", None)))
      ))
  }

  property("decodePoint") {
    parse("decodePoint(Coll[Byte](1.toByte))") shouldBe
      Apply(DecodePointFunc.symNoType, Vector(
        Apply(
          ApplyTypes(Ident("Coll", NoType), Vector(SByte)),
          Vector(Select(IntConstant(1), "toByte", None)))
      ))
  }

  property("xorOf") {
    parse("xorOf(Coll[Boolean](true, false))") shouldBe
      Apply(XorOfFunc.symNoType, Vector(
        Apply(
          ApplyTypes(Ident("Coll", NoType), Vector(SBoolean)),
          Vector(TrueLeaf, FalseLeaf))
      ))
  }

  property("SBoolean.toByte") {
    parse("true.toByte") shouldBe Select(TrueLeaf, "toByte")
  }

  property("SOption.map") {
    parse("Some(1).map { (b: Int) => b}") shouldBe
      Apply(Select(Apply(Ident("Some", NoType), Vector(IntConstant(1))), "map", None),
        Vector(Lambda(List(), Vector(("b", SInt)), NoType, Some(Ident("b", NoType)))))
  }

  property("SCollection.zip") {
    parse("OUTPUTS.zip(Coll(1, 2))") shouldBe
      Apply(Select(Ident("OUTPUTS"), "zip"),
        Vector(Apply(Ident("Coll"), Vector(IntConstant(1), IntConstant(2)))))
  }

  property("SCollection.zipWith") {
    parse("OUTPUTS.zipWith(Coll(1, 2), { (box: Box, i: Int) => i })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "zipWith"),
        Vector(Apply(Ident("Coll"), Vector(IntConstant(1), IntConstant(2))),
          Lambda(List(), Vector(("box", SBox), ("i", SInt)), NoType, Some(Ident("i", NoType)))))
  }

  property("SCollection.flatMap") {
    parse("OUTPUTS.flatMap({ (box: Box) => Coll(box) })") shouldBe
      Apply(Select(Ident("OUTPUTS"), "flatMap"),
        Vector(Lambda(List(), Vector(("box", SBox)), NoType,
          Some(Apply(Ident("Coll"), Vector(Ident("box", NoType)))))))
  }

  property("SGroupElement.exp") {
    parse("{ (g: GroupElement) => g.exp(1.toBigInt) }") shouldBe
      Lambda(List(), Vector(("g", SGroupElement)), NoType,
        Some(Apply(Select(Ident("g", NoType), "exp", None),
          Vector(Select(IntConstant(1), "toBigInt", None)))))
  }

  property("SNumeric.toBytes") {
    parse("1.toBytes") shouldBe Select(IntConstant(1), "toBytes")
    parse("1L.toBytes") shouldBe Select(LongConstant(1), "toBytes")
  }

  property("SNumeric.toBits") {
    parse("1.toBits") shouldBe Select(IntConstant(1), "toBits")
    parse("1L.toBits") shouldBe Select(LongConstant(1), "toBits")
  }

  property("SNumeric.compare") {
    parse("1.compare(2)") shouldBe Apply(Select(IntConstant(1), "compare", None), Vector(IntConstant(2)))
  }

  property("numeric constant negation") {
    parse("-3") shouldBe IntConstant(-3)
    parse("-3.toByte") shouldBe Select(IntConstant(-3), "toByte")
  }

  property("numeric negation unary op") {
    parse("-OUTPUTS.size") shouldBe Negation(Select(Ident("OUTPUTS"), "size").asIntValue)
    parse("- (3 - 2)") shouldBe Negation(Minus(IntConstant(3), IntConstant(2)))
  }

  property("bitwise inversion unary op ~") {
    parse("~OUTPUTS.size") shouldBe BitInversion(Select(Ident("OUTPUTS"), "size").asIntValue)
  }

  property("HEADERS and SHeader methods") {
    parse("HEADERS") shouldBe Ident("HEADERS")
    parse("HEADERS(0).version") shouldBe Select(Apply(Ident("HEADERS"), Vector(IntConstant(0))), "version")
    parse("HEADERS(0).parentId") shouldBe Select(Apply(Ident("HEADERS"), Vector(IntConstant(0))), "parentId")
  }

  property("logical not unary op") {
    parse("!true") shouldBe LogicalNot(TrueLeaf)
    parse("! (1 == 0)") shouldBe LogicalNot(EQ(IntConstant(1), IntConstant(0)))
  }

  property("logical XOR") {
    parse("true ^ false") shouldBe MethodCallLike(TrueLeaf, "^", Vector(FalseLeaf))
  }

  property("BitAnd: bitwise AND for numeric types") {
    parse("1 & 2") shouldBe BitOp(IntConstant(1), IntConstant(2), OpCodes.BitAndCode)
  }

  property("BitOr: bitwise OR for numeric types") {
    parse("1 | 2") shouldBe BitOp(IntConstant(1), IntConstant(2), OpCodes.BitOrCode)
  }

  property("BitXor: bitwise XOR for numeric types") {
    parse("1 ^ 2") shouldBe MethodCallLike(IntConstant(1), "^", Vector(IntConstant(2)))
  }

  property("BitShiftRight: bit-shifted right for numeric types") {
    parse("128 >> 2") shouldBe MethodCallLike(IntConstant(128), ">>", Vector(IntConstant(2)))
  }

  property("BitShiftLeft: bit-shifted left for numeric types") {
    parse("128 << 2") shouldBe MethodCallLike(IntConstant(128), "<<",  Vector(IntConstant(2)))
  }

  property("BitShiftRightZeroed: bit-shifted right(zeroed) for numeric types") {
    parse("128 >>> 2") shouldBe MethodCallLike(IntConstant(128), ">>>", Vector(IntConstant(2)))
  }

  property("CollShiftRight: shift collection right") {
    parse("Coll(true, false) >> 2") shouldBe
      MethodCallLike(Apply(Ident("Coll"), Vector(TrueLeaf, FalseLeaf)), ">>", Vector(IntConstant(2)))
  }

  property("CollShiftLeft: shift collection left") {
    parse("Coll(true, false) << 2") shouldBe
      MethodCallLike(Apply(Ident("Coll"), Vector(TrueLeaf, FalseLeaf)), "<<", Vector(IntConstant(2)))
  }

  property("CollShiftRightZeroed: shift collection right(zeroed)") {
    parse("Coll(true, false) >>> 2") shouldBe
      MethodCallLike(Apply(Ident("Coll"), Vector(TrueLeaf, FalseLeaf)), ">>>", Vector(IntConstant(2)))
  }

  property("Coll.rotateLeft: shift collection left") {
    parse("Coll(true, false).rotateLeft(2)") shouldBe
      Apply(Select(Apply(Ident("Coll"), Vector(TrueLeaf, FalseLeaf)), "rotateLeft", None), Vector(IntConstant(2)))
  }

  property("Coll.rotateRight: shift collection right") {
    parse("Coll(true, false).rotateRight(2)") shouldBe
      Apply(Select(Apply(Ident("Coll"), Vector(TrueLeaf, FalseLeaf)), "rotateRight", None), Vector(IntConstant(2)))
  }

  property("outerJoin") {
    parse("""outerJoin[Byte, Short, Int, Long](
        | Coll[(Byte, Short)]((1.toByte, 2.toShort)),
        | Coll[(Byte, Int)]((1.toByte, 3.toInt)),
        | { (b, s) => (b + s).toLong },
        | { (b, i) => (b + i).toLong },
        | { (b, s, i) => (b + s + i).toLong }
        | )""".stripMargin) shouldBe Apply(
      ApplyTypes(OuterJoinFunc.symNoType, Vector(SByte, SShort, SInt, SLong)),
      Vector(
        Apply(
          ApplyTypes(Ident("Coll", NoType), Vector(STuple(SByte, SShort))),
          Vector(Tuple(Vector(Select(IntConstant(1), "toByte", None), Select(IntConstant(2), "toShort", None))))
        ),
        Apply(
          ApplyTypes(Ident("Coll", NoType), Vector(STuple(SByte, SInt))),
          Vector(Tuple(Vector(Select(IntConstant(1), "toByte", None), Select(IntConstant(3), "toInt", None))))
        ),
        Lambda(List(),
          Vector(("b", NoType), ("s", NoType)),
          NoType,
          Some(Select(MethodCallLike(Ident("b", NoType), "+", Vector(Ident("s", NoType)), NoType), "toLong", None))
        ),
        Lambda(List(),
          Vector(("b", NoType), ("i", NoType)),
          NoType,
          Some(Select(MethodCallLike(Ident("b", NoType), "+", Vector(Ident("i", NoType)), NoType), "toLong", None))
        ),
        Lambda(List(),
          Vector(("b", NoType), ("s", NoType), ("i", NoType)),
          NoType,
          Some(
            Select(
              MethodCallLike(
                MethodCallLike(Ident("b", NoType), "+", Vector(Ident("s", NoType)), NoType),
                "+",
                Vector(Ident("i", NoType)), NoType),
              "toLong",
              None)
          )
        )
      )
    )
  }

  property("substConstants") {
    parse("substConstants[Long](Coll[Byte](1.toByte), Coll[Int](1), Coll[Long](1L))") shouldBe Apply(
      ApplyTypes(SubstConstantsFunc.symNoType, Vector(SLong)),
      Vector(
        Apply(ApplyTypes(Ident("Coll", NoType), Vector(SByte)), Vector(Select(IntConstant(1), "toByte", None))),
        Apply(ApplyTypes(Ident("Coll", NoType), Vector(SInt)), Vector(IntConstant(1))),
        Apply(ApplyTypes(Ident("Coll", NoType), Vector(SLong)), Vector(LongConstant(1)))
      )
    )
  }

  property("executeFromVar") {
    parse("executeFromVar[Boolean](1)") shouldBe Apply(
      ApplyTypes(ExecuteFromVarFunc.symNoType, Vector(SBoolean)), Vector(IntConstant(1))
    )
  }

  property("serialize") {
    checkParsed("serialize(1)", Apply(Ident("serialize", NoType), Array(IntConstant(1))))
    checkParsed("serialize((1, 2L))",
      Apply(Ident("serialize", NoType), Array(Tuple(Vector(IntConstant(1), LongConstant(2L))))))
    checkParsed("serialize(Coll(1, 2, 3))",
      Apply(
        Ident("serialize", NoType),
        Array(Apply(Ident("Coll", NoType), Array(IntConstant(1), IntConstant(2), IntConstant(3))))
      ))
  }

  property("single name pattern fail") {
    fail("{val (a,b) = (1,2)}", 1, 6)
  }

  property("unknown prefix in unary op") {
    fail("+1", 1, 2)
  }

  property("empty lines before invalid op") {
    fail(
      """
        |
        |
        |+1""".stripMargin, 4, 2)
  }

  property("unknown binary op") {
    fail("1**1", 1, 1)
  }

  property("compound types not supported") {
    fail("Coll[Int with Sortable](1)", 1, 6)
  }

  property("path types not supported") {
    fail("Coll[Int.A](1)", 1, 10)
  }

  property("block contains non-Val binding before expression") {
    fail("{1 ; 1 == 1}", 1, 2)
  }
}