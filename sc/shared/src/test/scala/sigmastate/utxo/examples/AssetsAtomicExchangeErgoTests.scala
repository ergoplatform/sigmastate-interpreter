package sigmastate.utxo.examples

import sigmastate.helpers.CompilerTestingCommons
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.ErgoContractSpec
import sigma.Coll
import scorex.crypto.hash.Blake2b256
import sigma.data.Digest32Coll

class AssetsAtomicExchangeErgoTests extends CompilerTestingCommons { suite =>
  lazy val spec = new ErgoContractSpec()(new TestingIRContext)
  private lazy val tokenId = Digest32Coll @@@ spec.Coll(Blake2b256("token1"))
  lazy val buyer = spec.ProvingParty("Alice")
  lazy val seller = spec.ProvingParty("Bob")
  val ergAmt = 100
  val tAmt = 60
  lazy val buyerBoxId: Coll[Byte] = spec.Coll(Blake2b256("BBox"))
  lazy val sellerBoxId: Coll[Byte] = spec.Coll(Blake2b256("SBox"))

  // TODO should be enabled after ErgoContractSpec is implemented
  ignore("atomic exchange spec") {
    val contract = AssetsAtomicExchange[spec.type](70, tokenId, buyer, seller)(spec)
    import contract.spec._

    // ARRANGE
//    val startBlock = getBlock(50)
//    val txs = startBlock.getTransactions()
//    val buyerBox = txs(0).outputs(0)
//    val sellerBox = getBoxesByParty(seller).collectFirst { case b if b.value > ergAmt => b }.get
    val buyerBox = getBoxById(buyerBoxId)
    val sellerBox = getBoxById(sellerBoxId)
    val txCtx = newTransactionContext
    // ACT


    // start exchange protocol
    val (ergHolder, tokenHolder) = contract.startExchange(txCtx.block, buyerBox, sellerBox, 100, Token(contract.tokenId, 60))

    // setup spending transaction
    val (buyerTokens, _) = contract.finishExchange(txCtx.block, ergHolder, tokenHolder)

    // ASSERT
    val input0 = buyerTokens.tx.inputs(0)
//    val res = input0.runDsl()
//    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0).get

    txCtx.attachProof(input0 -> buyerProof)
    try {
      txCtx.submit()
    } catch {
      case t: Throwable =>
    }
  }
}
