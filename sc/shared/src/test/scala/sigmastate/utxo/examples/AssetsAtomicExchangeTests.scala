package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.R4
import sigmastate.helpers.CompilerTestingCommons
import org.ergoplatform.dsl.ContractSyntax.Token
import org.ergoplatform.dsl.TestContractSpec
import scorex.crypto.hash.Blake2b256
import sigma.ast.SCollection.SByteArray
import sigma.ast._
import sigma.ast.syntax._
import sigma.data.{CSigmaProp, Digest32Coll, TrivialProp}
import sigma.ast.{BlockValue, ByteArrayConstant, LongConstant, ValDef, ValUse, Value}
import sigmastate.eval.Extensions._
import sigma.ast.syntax.ValueOps

/** An example of an atomic ergo <=> asset exchange.
  * Let's assume that Alice is willing to buy 60 assets of type "token1" for 100 ergo coins, and Bob
  * is willing to be a counterparty in this deal.
  *
  * Alice is creating a box of 100 coins protected by the script "I demand a spending transaction to create a box
  * which is protected by my public key pubkey_A and contains at least 60 assets of type "token1" and also 1 ergo coin"
  * (the last condition ensures that the box is easily spendable).
  *
  * Similarly, Bob is creating a box protected by a script like "I demand a spending transaction to create a box
  * which is protected by my public ket pubkey_B and contains at least 100 ergo tokens".
  *
  * (Please note that we are skipping some practically important details, for example, order cancellation conditions).
  *
  * Please note that once both box are on the blockchain, a correct exchange transaction could be created and
  * posted by anyone.
  *
  * Please note that more complex protocols could be build on top of the atomic exchange. For example, transactions
  * creating both boxes could be sent off-chain to a matching service, and be posted on the blockchain along with
  * the exchange transaction.
  *
  * //todo: make an example of multiple orders being matched
  */
class AssetsAtomicExchangeTests extends CompilerTestingCommons { suite =>
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  private lazy val tokenId = Digest32Coll @@@ spec.Coll(Blake2b256("token1"))
  lazy val buyer = spec.ProvingParty("Alice")
  lazy val seller = spec.ProvingParty("Bob")

  property("atomic exchange spec") {
    val contract = new AssetsAtomicExchange[spec.type](70, tokenId, buyer, seller)(spec) {
      import spec._

      def extractToken(box: Value[SBox.type]) = ByIndex(
        ExtractRegisterAs(box, ErgoBox.TokensRegId)(ErgoBox.STokensRegType).get, 0)

      val expectedBuyerProp = BlockValue(
        Vector(
          ValDef(1, ByIndex(Outputs, 0)),
          // token
          ValDef(2, extractToken(ValUse(1, SBox)))
        ),
        SigmaOr(List(
          SigmaAnd(List(
            GT(Height, deadline).toSigmaProp,
            tokenBuyer.pubKey.toTreeData.asSigmaProp
          )),
          AND(
            // extract toked id
            EQ(SelectField(ValUse(2, STuple(SByteArray, SLong)), 1), ByteArrayConstant(tokenId.toArray)),
            // extract token amount
            GE(SelectField(ValUse(2, STuple(SByteArray, SLong)), 2), LongConstant(60)),
            // right protection buyer
            EQ(ExtractScriptBytes(ValUse(1, SBox)), tokenBuyer.pubKey.toTreeData.asSigmaProp.propBytes),
            EQ(ExtractRegisterAs(ValUse(1, SBox), R4, SOption(SCollection(SByte))).get, ExtractId(Self))
          ).toSigmaProp
        ))
      ).asSigmaProp
// TODO update buyerProp to satisfy the condition
//      buyerProp.ergoTree shouldBe mkTestErgoTree(expectedBuyerProp)
    }
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which we will spend
    val mockTx = candidateBlock(0).newTransaction()
    // setup buyer's box from which we will transfer Ergs to holder box
    val mockBuyerBox = mockTx
        .outBox(100, contract.buyerSignature)
    // setup seller's box from which we will transfer tokens to holder boxes
    val mockSellerBox = mockTx
        .outBox(MinErgValue * 2, contract.sellerSignature)
        .withTokens(Token(contract.tokenId, 60))

    // ACT
    val startBlock = candidateBlock(50)
    // start exchange protocol
    val (ergHolder, tokenHolder) = contract.startExchange(startBlock, mockBuyerBox, mockSellerBox, 100, Token(contract.tokenId, 60))
    // setup spending transaction
    val (buyerTokens, sellerErgs) = contract.finishExchange(startBlock, ergHolder, tokenHolder)

    // ASSERT
    val input0 = buyerTokens.tx.inputs(0)
    val res = input0.runDsl()
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0).get
    contract.verifier.verify(input0, buyerProof) shouldBe true

    val input1 = buyerTokens.tx.inputs(1)
    val res1 = input1.runDsl()
    res1 shouldBe CSigmaProp(TrivialProp.TrueProp)
    val sellerProof = contract.tokenSeller.prove(input1).get
    contract.verifier.verify(input1, sellerProof) shouldBe true
  }

  property("partial filling") {
    val contract = AssetsPartialFilling[spec.type](70, tokenId, buyer, seller)(spec)
    import contract.spec._

    // ARRANGE
    // block, tx, and output boxes which will be spent
    val mockTx = candidateBlock(0).newTransaction()
    // setup buyer's box from which we will transfer Ergs to holder box
    val mockBuyerBox = mockTx
        .outBox(10000, contract.buyerSignature)
    // setup seller's box from which we will transfer tokens to holder boxes
    val mockSellerBox = mockTx
        .outBox(MinErgValue * 2, contract.sellerSignature)
        .withTokens(Token(contract.token1, 60))

    // ACT
    val startBlock = candidateBlock(0)
    // start exchange protocol
    val (buyerHolder, sellerHolder) = contract.startExchange(startBlock, mockBuyerBox, mockSellerBox, 10000, Token(contract.token1, 60))

    // setup spending transaction
    val spendingTx = candidateBlock(0).newTransaction().spending(buyerHolder, sellerHolder)
    spendingTx.outBox(5050, contract.buyerSignature)
        .withTokens(Token(contract.token1, 10))
        .withRegs(R4 -> buyerHolder.id)
    spendingTx.outBox(4950 + sellerHolder.value, contract.sellerSignature)
        .withTokens(Token(contract.token1, 50))
        .withRegs(R4 -> sellerHolder.id)

    // ASSERT
    val input0 = spendingTx.inputs(0)
    val buyerExt = Map(Byte.MaxValue -> ShortConstant(0.toShort))
    val res = input0.runDsl(buyerExt)
    res shouldBe CSigmaProp(TrivialProp.TrueProp)

    val buyerProof = contract.tokenBuyer.prove(input0, buyerExt).get
    contract.verifier.verify(input0, buyerProof) shouldBe true

    val input1 = spendingTx.inputs(1)
    val sellerExt = Map(Byte.MaxValue -> ShortConstant(1.toShort))
    val res1 = input1.runDsl(sellerExt)
    res1 shouldBe CSigmaProp(TrivialProp.TrueProp)
    val sellerProof = contract.tokenSeller.prove(input1, sellerExt).get
    contract.verifier.verify(input1, sellerProof) shouldBe true
  }

}
