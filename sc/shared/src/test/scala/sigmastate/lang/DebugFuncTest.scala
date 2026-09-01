package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigma.ast.SCollection._
import sigma.ast._
import sigma.ast.syntax.SValue
import sigmastate._
import SigmaPredef._
import sigma.compiler.phases.{SigmaBinder, SigmaTyper}
import sigmastate.interpreter.Interpreter.ScriptEnv

class DebugFuncTest extends AnyPropSpec
  with ScalaCheckPropertyChecks with Matchers with LangTests {

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  import predefFuncRegistry._

  /** Checks that parsing, binding and typing of `x` results in the expected type. */
  def typecheck(env: ScriptEnv, x: String): SType = {
    val builder = TransformingSigmaBuilder
    val parsed = SigmaParser(x).get.value
    val predefinedFuncRegistry = new PredefinedFuncRegistry(builder)
    val binder = new SigmaBinder(env, builder, TestnetNetworkPrefix, predefinedFuncRegistry)
    val bound = binder.bind(parsed)
    val typeEnv = env.collect { case (k, v: SType) => k -> v }
    val typer = new SigmaTyper(builder, predefinedFuncRegistry, typeEnv, lowerMethodCalls = true)
    val typed = typer.typecheck(bound)
    typed.tpe
  }

  property("debug() with primitive types") {
    typecheck(env, "debug(10, \"myInt\")") shouldBe SInt
    typecheck(env, "debug(10L, \"myLong\")") shouldBe SLong
    typecheck(env, "debug(true, \"myBool\")") shouldBe SBoolean
    typecheck(env, "debug(1.toByte, \"myByte\")") shouldBe SByte
    typecheck(env, "debug(1.toShort, \"myShort\")") shouldBe SShort
  }

  property("debug() without label parameter") {
    typecheck(env, "debug(10, \"\")") shouldBe SInt
    typecheck(env, "debug(HEIGHT, \"\")") shouldBe SInt
  }

  property("debug() pass-through semantics") {
    // Value should be returned unchanged
    typecheck(env, "{ val dVal = debug(42, \"test\"); dVal }") shouldBe SInt
    typecheck(env, "{ val dVal = debug(42, \"test\"); dVal + 1 }") shouldBe SInt
    typecheck(env, "debug(10, \"value\") + 5") shouldBe SInt
  }

  property("debug() with complex types") {
    typecheck(env, "debug(Coll(1,2,3), \"collection\")") shouldBe SCollection(SInt)
    typecheck(env, "debug(Coll[Byte](1.toByte), \"bytes\")") shouldBe SByteArray
    typecheck(env, "debug(getVar[Int](1), \"option\")") shouldBe SOption(SInt)
  }

  property("debug() with register access") {
    typecheck(env, "debug(SELF.R4[Int], \"register\")") shouldBe SOption(SInt)
    typecheck(env, "debug(SELF.R5[Long].get, \"price\")") shouldBe SLong
  }

  property("debug() with HEIGHT and context") {
    typecheck(env, "debug(HEIGHT, \"current height\")") shouldBe SInt
    typecheck(env, "debug(INPUTS.size, \"inputs count\")") shouldBe SInt
    typecheck(env, "debug(OUTPUTS.size, \"outputs count\")") shouldBe SInt
  }

  property("debug() with tuples") {
    typecheck(env, "debug((1, 2L), \"tuple\")") shouldBe STuple(SInt, SLong)
    typecheck(env, "debug((1, true), \"pair\")") shouldBe STuple(SInt, SBoolean)
  }

  property("debug() in expressions") {
    typecheck(env, "debug(HEIGHT, \"h\") > 100") shouldBe SBoolean
    typecheck(env, "{ val h = debug(HEIGHT, \"height\"); h > 100 }") shouldBe SBoolean
  }

  property("debug() with BigInt") {
    typecheck(env, "debug(1.toBigInt, \"bigint\")") shouldBe SBigInt
  }

  property("debug() with GroupElement") {
    typecheck(env, "debug(g1, \"group\")") shouldBe SGroupElement
  }

  property("debug() with SigmaProp") {
    typecheck(env, "debug(p1, \"sigmaprop\")") shouldBe SSigmaProp
  }

  property("debug() chaining") {
    typecheck(env, "{ val dVal = debug(10, \"first\"); val dVal2 = debug(dVal + 5, \"second\"); dVal2 }") shouldBe SInt
  }

  property("debug() in conditional") {
    typecheck(env, "if (debug(HEIGHT, \"h\") > 100) 1 else 2") shouldBe SInt
  }

  property("debug() with collection operations") {
    typecheck(env, "debug(Coll(1,2,3), \"coll\").size") shouldBe SInt
    typecheck(env, "debug(Coll(1,2,3), \"coll\")(0)") shouldBe SInt
  }

  property("debug() type preservation in complex scenarios") {
    // Ensure type is preserved through various operations
    typecheck(env, "{ val arr = debug(Coll(1,2,3), \"array\"); arr.map({ (x: Int) => x + 1 }) }") shouldBe SCollection(SInt)
    typecheck(env, "{ val opt = debug(getVar[Int](1), \"var\"); opt.isDefined }") shouldBe SBoolean
  }

  property("debug() with nested calls") {
    typecheck(env, "debug(debug(10, \"inner\"), \"outer\")") shouldBe SInt
    typecheck(env, "{ val dVal = debug(debug(5, \"a\") + debug(3, \"b\"), \"sum\"); dVal }") shouldBe SInt
  }

  property("debug() with empty label") {
    typecheck(env, "debug(42, \"\")") shouldBe SInt
    typecheck(env, "debug(HEIGHT, \"\")") shouldBe SInt
  }

  property("debug() in map/filter operations") {
    typecheck(env, "Coll(1,2,3).map({ (x: Int) => debug(x, \"item\") })") shouldBe SCollection(SInt)
    typecheck(env, "Coll(1,2,3).filter({ (x: Int) => debug(x > 1, \"check\") })") shouldBe SCollection(SInt)
  }

  property("debug() with ergoTree properties") {
    typecheck(env, "debug(SELF.value, \"box value\")") shouldBe SLong
    typecheck(env, "debug(SELF.propositionBytes, \"script bytes\")") shouldBe SByteArray
    typecheck(env, "debug(SELF.tokens.size, \"tokens count\")") shouldBe SInt
  }

  property("debug() type preservation in lambda") {
    typecheck(env, "{ val f = { (x: Int) => debug(x * 2, \"doubled\") }; f(5) }") shouldBe SInt
    typecheck(env, "{ val g = { (x: Long) => debug(x + 1L, \"incremented\") }; g(10L) }") shouldBe SLong
  }
}
