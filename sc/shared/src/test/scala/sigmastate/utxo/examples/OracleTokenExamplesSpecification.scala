package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import scorex.crypto.hash.Blake2b256
import sigma.data.Digest32Coll
import sigmastate.helpers.CompilerTestingCommons
import sigma.Coll
import sigma.Context

class OracleTokenExamplesSpecification extends CompilerTestingCommons { suite =>
  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)

  private lazy val tokenId = Digest32Coll @@@ spec.Coll(Blake2b256("token1"))

  case class OracleContract[Spec <: ContractSpec]
      ( temperature: Long,
        tokenID: Coll[Byte],
        alice: Spec#ProvingParty, bob: Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    def pkA = alice.pubKey
    def pkB = bob.pubKey
    def inRegId = reg1.asIndex

    lazy val contractEnv = Env("pkA" -> pkA, "pkB" -> pkB, "tokenId" -> tokenId)

    lazy val prop = proposition("tokenContract", { ctx: Context =>
      import ctx._
      val inReg = INPUTS(0).R4[Long].get
      val inToken = INPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId
      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
      inToken && okContractLogic
    },
    """{
     |      val inReg = INPUTS(0).R4[Long].get
     |      val inToken = INPUTS(0).R2[Coll[(Coll[Byte], Long)]].get(0)._1 == tokenId
     |      val okContractLogic = (inReg > 15L && pkA) || (inReg <= 15L && pkB)
     |      inToken && okContractLogic
     |}
    """.stripMargin)

    lazy val aliceSignature  = proposition("aliceSignature", _ => pkA, "pkA")

    lazy val dummySignature  = proposition("dummySignature", _ => pkA, "pkA")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")

  property("lightweight oracle token example (ErgoDsl)") {
    val temperature: Long = 18
    val contract = OracleContract[spec.type](temperature, tokenId, alice, bob)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = candidateBlock(0).newTransaction()
    val sOracle = mockTx
        .outBox(value = 1L, contract.dummySignature)
        .withRegs(reg1 -> temperature)
        .withTokens(Token(tokenId, 1))

    val sAlice = mockTx.outBox(10, contract.prop)
    val sBob   = mockTx.outBox(10, contract.prop)

    val tx = candidateBlock(50).newTransaction().spending(sOracle, sAlice, sBob)
    tx.outBox(20, contract.aliceSignature)
    val in = tx.inputs(1)
    val res = in.runDsl()
    res shouldBe alice.pubKey

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
