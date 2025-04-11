package sigmastate.utxo

import sigmastate.CompilerCrossVersionProps
import sigma.Extensions.ArrayOps
import sigma.Context
import sigmastate.helpers.CompilerTestingCommons
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, TestContractSpec}
import org.ergoplatform.ErgoBox
import scorex.crypto.hash.Blake2b256
import sigma.compiler.ir.IRContext
import sigma.data.{CSigmaProp, TrivialProp}

class UsingContextPropertiesSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps { suite =>
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val prover                         = spec.ProvingParty("Alice")
  private implicit lazy val IR: IRContext = spec.IR

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)

  def hash(str: String) = Blake2b256(str)
  val id = hash("some id")

  property("Accessing context properties") {
    case class ContextContract[Spec <: ContractSpec]
        (prover: Spec#ProvingParty)
        (implicit val spec: Spec) extends SigmaContractSyntax
    {
      def pkProver = prover.pubKey
      lazy val contractEnv = Env("pkProver" -> pkProver)

      lazy val dataInputsProp = proposition("dataInputsProp",
      { CONTEXT: Context => import CONTEXT._
        sigmaProp(CONTEXT.dataInputs.size == 1 && INPUTS.size == 2)
      },
      "{ sigmaProp(CONTEXT.dataInputs.size == 1 && INPUTS.size == 2) } ")

      lazy val dataBoxProp = proposition("dataBoxProp",
      { CONTEXT: Context => import CONTEXT._
        sigmaProp(CONTEXT.dataInputs(0).value > INPUTS(0).value)
      },
      "{ sigmaProp(CONTEXT.dataInputs(0).value > INPUTS(0).value) } ")

      lazy val headerProp = proposition("headerProp",
      { CONTEXT: Context => import CONTEXT._
        sigmaProp(CONTEXT.headers(0).version == 0)
      },
      "{ sigmaProp(CONTEXT.headers(0).version == 0) } ")

      lazy val proverSig = proposition("proverSig", { _ => pkProver }, "pkProver")
    }

    val contract = ContextContract[spec.type](prover)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()
    val d = mockTx
        .outBox(100, contract.proverSig)
        .withRegs(reg1 -> id.toColl)
    val s1 = mockTx
        .outBox(20, contract.dataInputsProp)
    val s2 = mockTx
        .outBox(20, contract.dataBoxProp)

    val spendingTx = candidateBlock(1).newTransaction()
        .withDataInputs(d)
        .spending(s1, s2)

    val newBox1 = spendingTx.outBox(10, contract.proverSig)

    {
      val in = spendingTx.inputs(0)
      val res = in.runDsl()
      res shouldBe CSigmaProp(TrivialProp.TrueProp)

      val pr = prover.prove(in).get
      contract.verifier.verify(in, pr) shouldBe true
    }

    {
      val in = spendingTx.inputs(1)
      val res = in.runDsl()
      res shouldBe CSigmaProp(TrivialProp.TrueProp)
      val pr = prover.prove(in).get
      contract.verifier.verify(in, pr) shouldBe true
    }
  }

}
