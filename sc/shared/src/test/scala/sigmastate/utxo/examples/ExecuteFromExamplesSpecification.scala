package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import sigmastate.helpers.CompilerTestingCommons
import sigma.Context
import sigma.ast.ByteArrayConstant

class ExecuteFromExamplesSpecification extends CompilerTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  case class OracleContract[Spec <: ContractSpec]
      (alice: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    def pkA = alice.pubKey

    lazy val contractEnv = Env("pkA" -> pkA)

    lazy val prop = proposition("test_deserialize", { ctx: Context =>
      import ctx._
      // trying to map the following code here (taken from ErgoLikeInterpreterSpecification.scala
      //val scriptIsCorrect = DeserializeContext(1, SSigmaProp)
      //SigmaAnd(scriptIsCorrect, scriptIsCorrect)
      pkA
    },
    """{
     |      val script = executeFromVar[Boolean](1) // gives error
     |      // also how to use script in code?
     |      pkA
     |}
    """.stripMargin)

    lazy val aliceSignature  = proposition("aliceSignature", _ => pkA, "pkA")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val alice = spec.ProvingParty("Alice")

  property("Execute from var example (ErgoDsl)") {
    val contract = OracleContract[spec.type](alice)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = candidateBlock(0).newTransaction()
    val sAlice = mockTx.outBox(10, contract.prop).withTokens()

    val tx = candidateBlock(50).newTransaction().spending(sAlice)

    tx.outBox(20, contract.aliceSignature)

    val in = tx.inputs(0)
    val vars = Map(1.toByte -> ByteArrayConstant(alice.pubKey.propBytes))
    val res = in.runDsl(vars)
    res shouldBe alice.pubKey

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
