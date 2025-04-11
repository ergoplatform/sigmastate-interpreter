package sigmastate.utxo.examples

import org.ergoplatform._
import org.ergoplatform.ErgoBox.{R4, R5}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.Colls
import sigma.data.{AvlTreeData, AvlTreeFlags, Digest32Coll, TrivialProp}
import sigmastate.CompilerCrossVersionProps
import sigma.ast.{AvlTreeConstant, ByteArrayConstant, LongConstant, SigmaPropConstant}
import sigma.Extensions.ArrayOps
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestProvingInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigma.serialization.ErgoTreeSerializer
import sigma.ast.syntax._
import sigma.compiler.ir.IRContext
import sigma.eval.SigmaDsl

import scala.util.Random

/**
  * A Local Exchange Trading System On Top Of Ergo
==============================================

 A local exchange trading system (LETS) is a local mutual credit association which members are allowed to create common
 credit money individually, with all the deals in the system written into a common ledger.
 As an example, assume that Alice with zero balance is willing to buy a liter of raw milk from Bob.
 First, they agree on a price, for example, assume that the price is about 2 Euro (as Alice and Bob
 are living in Ireland). After the deal being written into a ledger, Alice's balance becomes -2 (minus
 two) Euro, and Bob's balance becomes 2 Euro. Then Bob may spend his 2 Euro, for example, on
 home-made beer from Charlie. Often, such systems impose limits on negative balances, and sometimes
 even on positive ones, in order to promote exchange in the community.

 Historically, such systems become popular during crisis times. The first system was established by Michael Linton in
 a Canadian town stuck in depression back in 1981. Local exchange trading systems were extremely popular during
 1998-2002 Argentine Great Depression. Most LETS groups range from 50 to 250 members, with paper-based credit notes and
 ledger maintained by a core committee. However, paper-based LETS currencies have shown some problems, such as
 counterfeit notes, possible rogue behavior of system managers, and so on. Therefore, blockchain-based LETS could be superior
 to the old systems. More information on LETS could be found in
 ["The Ecology of Money" book (by Richard Douthwaite)](http://feasta.org/documents/moneyecology/chaptertwo.htm) and
 [Wikipedia](https://en.wikipedia.org/wiki/Local_exchange_trading_system).

 In this article we show how LETS could be implemented on top of Ergo. To the best of our knowledge, this is
 the first implementation of such kind of community currency on top of a blockchain.
 Our reference implementation
 is simple and consists of two contracts, namely, a management contract and an exchange contract.
 We skip Ergo preliminaries, so please read
 [the ICO article](https://github.com/ergoplatform/ergo/wiki/An-ICO-Example-On-Top-Of-Ergo) and
 ErgoScript tutorials([basic](https://docs.ergoplatform.com/ErgoScript.pdf) and
 [advanced](https://docs.ergoplatform.com/sigmastate_protocols.pdf)) for starters.
 Nevertheless, we are going to introduce a couple of new terms in following sentences.
 If a token is issued with amount equal to one, we call it the singleton token. Similarly,
 a box which contains the singleton token is called the singleton box.

 The management contract is controlling a singleton box which holds members of the LETS system.
 The contract enables the adding of new members at the pace of one member per one transaction. The box
 is not storing members, but only a small digest of authenticated data structure built on top of
 the members' directory. A member is associated with a singleton token issued in a transaction which
 is adding the member to the directory. The transaction creates a new member's box which contains
 the member's singleton token. The member's box is protected by the exchange contract. Also, the newly
 created member's box has initial balance written down into the R4 register, and the balance is
 equal to zero in our example. The transaction creating a new member must provide a proof of correctness for
 directory transformation.

 The management contract box is controlled usually by a committee, and the committee could evolve over time. To support
 that, we allow committee logic to reside in the register R5.
 For example, assume that a new committee member has been added along with a new LETS member,
 the input management contract box is requiring 2-out-of-3 signatures, and the output box requires 3-out-of-4 signatures.
 In this case contents of the R5 register in the input and the output box would differ.

 The management contract code in ErgoScript with comments is provided below. Please note that
 "userContractHash" is about exchange contract hash.

    val selfOut = OUTPUTS(0)

    // Management script
    val managementScript = selfOut.R5[SigmaProp].get

    // The management script template is replicating self, and management script is satisfied
    val scriptCorrect = (selfOut.propositionBytes == SELF.propositionBytes) && managementScript

    // A spending transaction is creating boxes for directory, user, fee.
    val outsSizeCorrect = OUTPUTS.size == 3

    // Checks that the management label token is replicating self
    val outTokenCorrect = (selfOut.tokens.size == 1) && (selfOut.tokens(0)._1 == letsToken)

    // Checks that new token is issued, and its amount is correct
    // OUTPUTS(0) tokens already checked via outtokenCorrect
    val issuedTokenId = INPUTS(0).id
    val userOut = OUTPUTS(1)
    val correctTokenAmounts =
      (userOut.tokens.size == 1 &&
        userOut.tokens(0)._1 == issuedTokenId &&
        userOut.tokens(0)._2 == 1 &&
        OUTPUTS(2).tokens.size == 0 &&
        outTokenCorrect)

    // Checks that the new user has been created with the zero balance
    val zeroUserBalance  = userOut.R4[Long].get == 0

    val properUserScript = blake2b256(userOut.propositionBytes) == userContractHash

    // Checks that the new token identifier has been added to the directory
    val selfTree = SELF.R4[AvlTree].get
    val toAdd: Coll[(Coll[Byte], Coll[Byte])] = Coll((issuedTokenId, Coll[Byte]()))
    val proof = getVar[Coll[Byte]](1).get
    val modifiedTree = selfTree.insert(toAdd, proof).get
    val expectedTree = selfOut.R4[AvlTree].get
    val treeCorrect = modifiedTree == expectedTree

    correctTokenAmounts && scriptCorrect && treeCorrect && zeroUserBalance && properUserScript


 The exchange contract script is fairly straightforward and provided below along with comments describing its logic. In the
 contract, it is assumed that a spending transaction for an exchange contract box is receiving at least two inputs,
 and the first two inputs should be protected by the exchange contract script and contain LETS member tokens. To check
 that singleton member tokens in the inputs do indeed belong to the LETS system, a spending transaction provides the management
 contract box as the first read-only data input, and also should provide a proof that the member tokens do belong to
 the directory authenticated via the R4 register of the management contract box. "letsToken" in the script is about
 the singleton token of the management box.

    // Minimal balance allowed for LETS trader
    val minBalance = -20000

    val lookupProof = getVar[Coll[Byte]](1).get

    // The read-only box which contains directory of LETS members
    val treeHolderBox = CONTEXT.dataInputs(0)
    val properLetsToken = treeHolderBox.tokens(0)._1 == letsToken
    val membersTree = treeHolderBox.R4[AvlTree].get

    // A spending transaction is taking two boxes of LETS members willing to make a deal,
    // and returns boxes with modified balances.
    val participant0 = INPUTS(0)
    val participant1 = INPUTS(1)
    val participantOut0 = OUTPUTS(0)
    val participantOut1 = OUTPUTS(1)

    //Check that members do indeed belong to the LETS
    val token0 = participant0.tokens(0)._1
    val token1 = participant1.tokens(0)._1
    val memberTokens = Coll(token0, token1)
    val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })

    // Check that LETS member balance changes during the deal are correct
    val initialBalance0 = participant0.R4[Long].get
    val initialBalance1 = participant1.R4[Long].get
    val finishBalance0  = participantOut0.R4[Long].get
    val finishBalance1  = participantOut1.R4[Long].get
    val diff0 = finishBalance0 - initialBalance0
    val diff1 = finishBalance1 - initialBalance1
    val diffCorrect = diff0 == -diff1
    val balancesCorrect = (finishBalance0 > minBalance) && (finishBalance1 > minBalance) && diffCorrect

    // Check that member boxes save their scripts.
    // todo: optimization could be made here
    val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
    val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
    val scriptsSaved = script0Saved && script1Saved

    // Member-specific box protection
    val selfPubKey = SELF.R5[SigmaProp].get

    selfPubKey && properLetsToken && membersExist && diffCorrect && scriptsSaved

 Note that both contracts could be modified in many ways to get new systems with different properties. So hopefully
 some day this article will be continued!
  */

class LetsSpecification extends CompilerTestingCommons with CompilerCrossVersionProps { suite =>
  // Not mixed with TestContext since it is not possible to call compiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new TestingIRContext

  lazy val project = new ErgoLikeTestProvingInterpreter()

  val letsTokenId = Digest32Coll @@ Colls.fromArray(Array.fill(32)(Random.nextInt(100).toByte))

  val env = Map(ScriptNameProp -> "withdrawalScriptEnv", "letsToken" -> ByteArrayConstant(letsTokenId))

  private val miningRewardsDelay = 720
  private val feeProp = ErgoTreePredef.feeProposition(miningRewardsDelay) // create ErgoTree v0

  lazy val exchangeScript = compile(env,
    """{
      |
      |  // Minimal balance allowed for LETS trader
      |  val minBalance = -20000
      |
      |  val lookupProof = getVar[Coll[Byte]](1).get
      |
      |  // The read-only box which contains directory of LETS members
      |  val treeHolderBox = CONTEXT.dataInputs(0)
      |  val properLetsToken = treeHolderBox.tokens(0)._1 == letsToken
      |  val membersTree = treeHolderBox.R4[AvlTree].get
      |
      |  // A spending transaction is taking two boxes of LETS members willing to make a deal,
      |  // and returns boxes with modified balances.
      |  val participant0 = INPUTS(0)
      |  val participant1 = INPUTS(1)
      |  val participantOut0 = OUTPUTS(0)
      |  val participantOut1 = OUTPUTS(1)
      |
      |  //Check that members do indeed belong to the LETS
      |  val token0 = participant0.tokens(0)._1
      |  val token1 = participant1.tokens(0)._1
      |  val memberTokens = Coll(token0, token1)
      |  val membersExist = membersTree.getMany(memberTokens, lookupProof).forall({ (o: Option[Coll[Byte]]) => o.isDefined })
      |
      |  // Check that LETS member balance changes during the deal are correct
      |  val initialBalance0 = participant0.R4[Long].get
      |  val initialBalance1 = participant1.R4[Long].get
      |  val finishBalance0  = participantOut0.R4[Long].get
      |  val finishBalance1  = participantOut1.R4[Long].get
      |  val diff0 = finishBalance0 - initialBalance0
      |  val diff1 = finishBalance1 - initialBalance1
      |  val diffCorrect = diff0 == -diff1
      |  val balancesCorrect = (finishBalance0 > minBalance) && (finishBalance1 > minBalance) && diffCorrect
      |
      |  // Check that member boxes save their scripts.
      |  // todo: optimization could be made here
      |  val script0Saved = participantOut0.propositionBytes == participant0.propositionBytes
      |  val script1Saved = participantOut1.propositionBytes == participant1.propositionBytes
      |  val scriptsSaved = script0Saved && script1Saved
      |
      |  // Member-specific box protection
      |  val selfPubKey = SELF.R5[SigmaProp].get
      |
      |  selfPubKey && properLetsToken && membersExist && diffCorrect && scriptsSaved
      |}""".stripMargin
  ).asSigmaProp
  def exchangeTree = mkTestErgoTree(exchangeScript)

  def userContractHash = Blake2b256(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(exchangeTree))

  def managementScript = compile(env.updated("userContractHash", userContractHash),
    """{
      |
      | val selfOut = OUTPUTS(0)
      |
      | // Management script
      | val managementScript = selfOut.R5[SigmaProp].get
      |
      | // The management script template is replicating self, and management script is satisfied
      | val scriptCorrect = (selfOut.propositionBytes == SELF.propositionBytes) && managementScript
      |
      | // A spending transaction is creating boxes for directory, user, fee.
      | val outsSizeCorrect = OUTPUTS.size == 3
      |
      | // Checks that the management label token is replicating self
      | val outTokenCorrect = (selfOut.tokens.size == 1) && (selfOut.tokens(0)._1 == letsToken)
      |
      | // Checks that new token is issued, and its amount is correct
      | // OUTPUTS(0) tokens already checked via outtokenCorrect
      | val issuedTokenId = INPUTS(0).id
      | val userOut = OUTPUTS(1)
      | val correctTokenAmounts =
      |   (userOut.tokens.size == 1 &&
      |     userOut.tokens(0)._1 == issuedTokenId &&
      |     userOut.tokens(0)._2 == 1 &&
      |     OUTPUTS(2).tokens.size == 0 &&
      |     outTokenCorrect)
      |
      |  // Checks that the new user has been created with the zero balance
      |  val zeroUserBalance  = userOut.R4[Long].get == 0
      |
      |  val properUserScript = blake2b256(userOut.propositionBytes) == userContractHash
      |
      |  // Checks that the new token identifier has been added to the directory
      |  val selfTree = SELF.R4[AvlTree].get
      |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = Coll((issuedTokenId, Coll[Byte]()))
      |  val proof = getVar[Coll[Byte]](1).get
      |  val modifiedTree = selfTree.insert(toAdd, proof).get
      |  val expectedTree = selfOut.R4[AvlTree].get
      |  val treeCorrect = modifiedTree == expectedTree
      |
      |  correctTokenAmounts && scriptCorrect && treeCorrect && zeroUserBalance && properUserScript
      |}""".stripMargin
  ).asSigmaProp

//  printDebug(exchangeScript)
//  printDebug(managementScript)

  property("adding new member") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val initTreeData = new AvlTreeData(digest.toColl, AvlTreeFlags.InsertOnly, 32, None)

    val managementTree = mkTestErgoTree(managementScript)
    val projectBoxBefore = testBox(10, managementTree, 0,
      Seq(letsTokenId -> 1L),
      Map(
        R4 -> AvlTreeConstant(SigmaDsl.avlTree(initTreeData)),
        R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val userTokenId = Digest32Coll @@@ Colls.fromArray(projectBoxBefore.id)

    avlProver.performOneOperation(Insert(ADKey @@@ userTokenId.toArray, ADValue @@ Array.emptyByteArray))

    val proof = avlProver.generateProof()
    val endTree = new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.InsertOnly, 32, None)

    val projectBoxAfter = testBox(9, managementTree, 0,
      Seq(letsTokenId -> 1L),
      Map(
        R4 -> AvlTreeConstant(SigmaDsl.avlTree(endTree)),
        R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val feeBox = testBox(1, feeProp, 0, Seq(), Map())
    val userBox = testBox(1, exchangeTree, 0, Seq(userTokenId -> 1L), Map(R4 -> LongConstant(0)))

    val issuanceTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter, userBox, feeBox))

    val fundingContext = ErgoLikeContextTesting(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = issuanceTx,
      self = projectBoxBefore, activatedVersionInTests)

    val managementProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = managementProver.prove(env, managementTree, fundingContext, fakeMessage).get
    printDebug("new user script cost: " + res.cost)
  }

  property("exchange") {

    val userTokenId0 = Digest32Coll @@ Colls.fromArray(Array.fill(32)(Random.nextInt(100).toByte))
    val userTokenId1 = Digest32Coll @@ Colls.fromArray(Array.fill(32)(Random.nextInt(100).toByte))

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    avlProver.performOneOperation(Insert(ADKey @@@ userTokenId0.toArray, ADValue @@ Array.emptyByteArray))
    avlProver.performOneOperation(Insert(ADKey @@@ userTokenId1.toArray, ADValue @@ Array.emptyByteArray))
    val digest = avlProver.digest
    avlProver.generateProof()
    val initTreeData = new AvlTreeData(digest.toColl, AvlTreeFlags.InsertOnly, 32, None)

    avlProver.performOneOperation(Lookup(ADKey @@@ userTokenId0.toArray))
    avlProver.performOneOperation(Lookup(ADKey @@@ userTokenId1.toArray))
    val proof = avlProver.generateProof()

    val managementTree = mkTestErgoTree(managementScript)
    val directoryBox = testBox(10, managementTree, 0,
      Seq(letsTokenId -> 1L),
      Map(
        R4 -> AvlTreeConstant(SigmaDsl.avlTree(initTreeData)),
        R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val directoryDataInput = DataInput(directoryBox.id)

    val exchangeTree = mkTestErgoTree(exchangeScript)
    val userBoxBefore0 = testBox(1, exchangeTree, 0,
      Seq(userTokenId0 -> 1L),
      Map(R4 -> LongConstant(0), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))
    val userBoxBefore1 = testBox(1, exchangeTree, 0,
      Seq(userTokenId1 -> 1L),
      Map(R4 -> LongConstant(0), R5 -> SigmaPropConstant(TrivialProp.TrueProp)))

    val userBoxAfter0 = testBox(1, exchangeTree, 0,
      Seq(userTokenId0 -> 1L), Map(R4 -> LongConstant(-5)))
    val userBoxAfter1 = testBox(1, exchangeTree, 0,
      Seq(userTokenId1 -> 1L), Map(R4 -> LongConstant(5)))

    val issuanceTx = new ErgoLikeTransaction(
      IndexedSeq(),
      IndexedSeq(directoryDataInput),
      IndexedSeq(userBoxAfter0, userBoxAfter1))

    val exchangeContext = ErgoLikeContextTesting(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      dataBoxes = IndexedSeq(directoryBox),
      boxesToSpend = IndexedSeq(userBoxBefore0, userBoxBefore1),
      spendingTransaction = issuanceTx,
      selfIndex = 0,
      activatedVersionInTests)

    val managementProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = managementProver.prove(env, exchangeTree, exchangeContext, fakeMessage).get
    printDebug("exchange script cost: " + res.cost)
  }

}
