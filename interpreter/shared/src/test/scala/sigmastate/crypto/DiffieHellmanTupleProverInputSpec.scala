package sigmastate.crypto

import java.math.BigInteger

import org.scalatest.propspec.AnyPropSpec
import sigma.crypto.CryptoConstants
import sigma.data.ProveDHTuple
import sigmastate.TestsBase

class DiffieHellmanTupleProverInputSpec extends AnyPropSpec with TestsBase {
  private val dlogGroup = CryptoConstants.dlogGroup

  private def freshGenerator() = dlogGroup.createRandomGenerator()

  property("isValid accepts a consistent (w, g, h, g^w, h^w) tuple") {
    val honest = DiffieHellmanTupleProverInput.random()
    DiffieHellmanTupleProverInput.isValid(honest.w, honest.commonInput) shouldBe true
    honest.isValidSecret shouldBe true
  }

  property("isValid rejects an inconsistent secret (random w against fixed tuple)") {
    val honest = DiffieHellmanTupleProverInput.random()
    val bogus = honest.w.add(BigInteger.ONE).mod(dlogGroup.order)
    DiffieHellmanTupleProverInput.isValid(bogus, honest.commonInput) shouldBe false
  }

  property("isValid rejects when only one of g^w, h^w matches") {
    val g = dlogGroup.generator
    val h = freshGenerator()
    val w = DiffieHellmanTupleProverInput.random().w
    val u = dlogGroup.exponentiate(g, w)
    // v is bogus — exponent differs from w
    val v = dlogGroup.exponentiate(h, w.add(BigInteger.ONE).mod(dlogGroup.order))
    DiffieHellmanTupleProverInput.isValid(w, ProveDHTuple(g, h, u, v)) shouldBe false
  }

  property("create throws on inconsistent secret") {
    val honest = DiffieHellmanTupleProverInput.random()
    val bogus = honest.w.add(BigInteger.ONE).mod(dlogGroup.order)
    assertThrows[IllegalArgumentException] {
      DiffieHellmanTupleProverInput.create(bogus, honest.commonInput)
    }
  }

  property("create succeeds and returns equivalent input for a valid secret") {
    val honest = DiffieHellmanTupleProverInput.random()
    val viaFactory = DiffieHellmanTupleProverInput.create(honest.w, honest.commonInput)
    viaFactory.w shouldBe honest.w
    viaFactory.commonInput shouldBe honest.commonInput
    viaFactory.isValidSecret shouldBe true
  }
}
