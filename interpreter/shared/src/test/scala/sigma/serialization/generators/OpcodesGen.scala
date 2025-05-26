package sigma.serialization.generators

import org.scalacheck.Gen
import sigma.serialization.OpCodes._
import sigma.serialization.ValueCodes.OpCode

trait OpcodesGen {

  val arithmeticCodeGen: Gen[OpCode] = Gen.oneOf(MinusCode, PlusCode, MultiplyCode, DivisionCode, ModuloCode, MinCode, MaxCode)


}
