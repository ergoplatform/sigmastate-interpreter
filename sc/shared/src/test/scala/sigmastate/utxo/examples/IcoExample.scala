package sigmastate.utxo.examples

import scorex.utils.Longs
import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform._
import org.ergoplatform.dsl.TestContractSpec
import org.scalatest.BeforeAndAfterAll
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.Colls
import sigma.ast.SByte
import sigma.data.{AvlTreeData, AvlTreeFlags, Digest32Coll}
import sigma.ast._
import sigmastate._
import sigma.Extensions.ArrayOps
import sigma.ast.syntax.{CollectionConstant, SigmaPropValue}
import sigma.crypto.CryptoConstants
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestProvingInterpreter}
import sigmastate.interpreter.Interpreter.ScriptNameProp
import sigmastate.interpreter.Interpreter
import sigma.ast.syntax._
import sigma.compiler.ir.IRContext
import sigma.eval.SigmaDsl
import sigma.serialization.ErgoTreeSerializer
import sigma.serialization.ErgoTreeSerializer.DefaultSerializer

import scala.util.Random

/**
  *
  * An ICO Example On Top Of Ergo
    ==============================

We cover such complex example as an ICO (Initial Coin Offering) here covering several important novel features of the Ergo Platform.

## Part 1. Preliminaries


A cryptocurrency protocol needs to specify what a spending transaction actually spends. There are two possibilities here.
In Bitcoin, a transaction is spending one-time digital coins and creates new ones. In Nxt, Ethereum, or Waves, a transaction is deducting
a transaction amount from long-living account or establishing a new one~(with possible side-effects on the way, like contract execution in Waves or Ethereum). Ergo is similar to Bitcoin, it is spending one-time coins (Ergo transaction also can have data inputs which are not to be spent, rather, they are providing information from current set of unspent coins).

It is not trivial to do an ICO in model of one-time coins, as, in opposite to account-based cryptocurrencies, there is no explicit
persistent storage here. However, Ergo brings spending transaction into execution context of a script which is protecting a coin.
With this small change it becomes possible to express dependencies between transaction outputs and inputs. In turn, by setting dependencies we can execute even arbitrarily complex Turing-complete programs on top of blockchain (see ...). In this article we will define a concrete scenario of a multi-stage contract, as for ICO we need for different stages (funding, token issuance, withdrawal).

Now imagine an ICO for thousands of participants. Unlike Ethereum, Ergo does not provide possibility to store large sets of data and carry them over throughout contract execution. Rather, it allows to store only about 40-bytes header of a data
structure, represented as key -> value dictionary, authenticated similarly to Merkle tree. To access some elements
in the dictionary, or to modify it, a spending transaction which is triggering protecting script execution should
provide lookup or modification proofs. This gives possibility for a contract to authenticate potentially huge datasets without requiring much memory to store contract state. However, storing space in the state (of active contracts) would mean bigger transactions, but this problem is easier from scalability point of view, and scalability is a top priority for Ergo.


## Part 2. The ICO Contract

There could be many possible scenarios associated with the Initial Coin Offering (ICO). In this article we are working with the ICO contract steps briefly described below with details provided further:
	- first, funding epoch takes place. It starts with a project's coin authenticating an empty dictionary. The dictionary is intended for holding (investor, balance) pairs, where the investor is associated with script of its withdrawing coin. For the balance, we assume that 1 token is equal to 1 Ergo during the ICO. During the funding epoch, it is possible to put Ergs into the project's coin.
	For that, a spending transaction for the project's coin provides inputs which holding investor withdrawing scripts. Investor scripts and input values should be added to the tree. There could be many chained funding transactions.
	- second, the funding period should be finished with closing the tree holding investors data. An authenticated tree could have different modification operations allowed individually: inserts, deletes, updates, or all the operations could be disallowed (so the tree could be in the read-only mode). Also, this transaction creates tokens of the ICO project which will be withdrawn in the next stage. The project can withdraw Ergs at this stage.
	- third, investors are withdrawing their tokens. For that, a spending transaction creates outputs with guarding conditions and token values from the tree. The tree should be cleared from withdrawn pairs. There could be many chained funding transactions.

This three stages should be linked together, and form logical order. To fulfill these goals, we are using the same coin,

## Part 3. The ICO Contract Details

Now it is the time to provide details and ErgoScript code on the ICO contract stages.

### The Funding Stage

First, the funding stage. We assume that initially a project creates a coin which is committing to an empty dictionary
(stored in the register R5) and also the scripts. This stage is lasts for height 2,000 at least, more concretely, the
first transaction with height of 2,000 at least should change the coin's script.

The project's coin is considering and checking that it is always input number zero, and also output number zero. The
other inputs are considered investors' inputs. An investor's input contains hash of a withdrawing coin guarding script
in the register R4. The hashes as well as a monetary values of investing inputs should be added to the dictionary. The
spending transaction should provide a proof that investor data are indeed added to the dictionary,
and the proof is checked in the contract.

It is not checked in the funding sub-contract, rather, investors should check that the dictionary allows insertions
only, not updating existing values or removals (it is not hard to add an explicit check though).

The spending transaction should pay a fee, otherwise, it it unlikely that it would be included in a block. Thus the
funding contract is checking that the spending transaction has two outputs (one for itself, another to pay a fee),
the fee is to be no more than a certain limit (just one nanoErg in our example), and the guarding proposition should
be such that only a miner can spend the output (we use just a variable "feeProp" from compilation environment in our
example without providing any details, but this "feeProp" is corresponding to standard though not required by
consensus guarding script).

The code below basically checks all that described above, in the form of compilable code. Please note that the
"nextStageScriptHash" environment variable contains hash of the issuance stage serialized script.

    val selfIndexIsZero = INPUTS(0).id == SELF.id

    val proof = getVar[Coll[Byte]](1).get

    val inputsCount = INPUTS.size

    val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
        val pk = b.R4[Coll[Byte]].get
        val value = longToByteArray(b.value)
        (pk, value)
    })

    val modifiedTree = SELF.R5[AvlTree].get.insert(toAdd, proof).get

    val expectedTree = OUTPUTS(0).R5[AvlTree].get

    val properTreeModification = modifiedTree == expectedTree

    val outputsCount = OUTPUTS.size == 2

    val selfOutputCorrect = if(HEIGHT < 2000) {
        OUTPUTS(0).propositionBytes == SELF.propositionBytes
    } else {
        blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
    }

    val feeOutputCorrect = (OUTPUTS(1).value <= 1) && (OUTPUTS(1).propositionBytes == feeBytes)

    val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect

    selfIndexIsZero && outputsCorrect && properTreeModification

### The Issuance Stage

This stage is about only one spending transaction to get to the next stage, which is the withdrawal stage. At the
issuance stage, the main things should happen. As in the previous case, the issuance sub-contract assumes

In the first place, the tree should change a list of allowed operations from "inserts only" to "removals only", as the
next stage, the withdrawal one, is about removing from the dictionary.

In the second place, the contract is checking that proper amount of ICO tokens are issued. In Ergo, it is allowed to
issue one new kind of token per transaction, and the identifier of the token should be equal to the (unique)
identifier of the first input coin. The issuance sub-contract is checking that a new token has been issued, and the
amount of it is equal to the amount of nanoErgs collected by the ICO contract coin to the moment.

In the third place, the contract is checking that a spending transaction is indeed re-creating the coin with a
guarding script corresponding to the next stage, the withdrawal stage.

At this stage a project can withdraw collected Ergs. And, of course, the spending transaction should pay a fee. Thus
the sub-contract is checking that the spending transaction has indeed 3 outputs (for the contract itself, for the
project withdrawal, and for the fee), and that the only first output is carrying the tokens issued. As we do not
specify project money withdrawal details, we require a project signature on the spending transaction.

    val openTree = SELF.R5[AvlTree].get

    val closedTree = OUTPUTS(0).R5[AvlTree].get

    val digestPreserved = openTree.digest == closedTree.digest
    val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
    val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
    val treeIsClosed = closedTree.enabledOperations == 4

    val tokenId: Coll[Byte] = INPUTS(0).id

    val tokensIssued = OUTPUTS(0).tokens(0)._2

    val outputsCountCorrect = OUTPUTS.size == 3
    val secondOutputNoTokens = OUTPUTS(0).tokens.size == 1 && OUTPUTS(1).tokens.size == 0 && OUTPUTS(2).tokens.size == 0

    val correctTokensIssued = SELF.value == tokensIssued

    val correctTokenId = OUTPUTS(0).R4[Coll[Byte]].get == tokenId && OUTPUTS(0).tokens(0)._1 == tokenId

    val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
    val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash

    val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed

    projectPubKey && treeIsCorrect && valuePreserved && stateChanged

### The Withdrawal Stage

At this stage, it is allowed for an investor to withdraw money to predefined guarding script (which hash is written down
into the dictionary). A withdrawing transaction thus is having N + 2 outputs, where the first output should carry over the
withdrawal sub-contract, the following N outputs have guarding scripts and token values according to the dictionary,
and for the the last output there are no any conditions, aside of that it is not allowed to carry away tokens with it
(supposedly, this output pays a fee). The contract is requiring two proofs for the dictionary elements: one proof
is showing that values to be withdrawn are indeed in the dictionary, and the second proof is proving that a resulting
dictionary is free of the withdrawn values. The sub-contract is below.

    val removeProof = getVar[Coll[Byte]](2).get
    val lookupProof = getVar[Coll[Byte]](3).get
    val withdrawIndexes = getVar[Coll[Int]](4).get

    val out0 = OUTPUTS(0)

    val tokenId: Coll[Byte] = SELF.R4[Coll[Byte]].get

    val withdrawals = withdrawIndexes.map({(idx: Int) =>
        val b = OUTPUTS(idx)
        if(b.tokens(0)._1 == tokenId) {
            (blake2b256(b.propositionBytes), b.tokens(0)._2)
        } else {
            (blake2b256(b.propositionBytes), 0L)
        }
    })

    val withdrawValues = withdrawals.map({(t: (Coll[Byte], Long)) => t._2})

    val withdrawTotal = withdrawValues.fold(0L, { (l1: Long, l2: Long) => l1 + l2 })

    val toRemove = withdrawals.map({(t: (Coll[Byte], Long)) => t._1})

    val initialTree = SELF.R5[AvlTree].get

    val removedValues = initialTree.getMany(toRemove, lookupProof).map({(o: Option[Coll[Byte]]) => byteArrayToLong(o.get)})
    val valuesCorrect = removedValues == withdrawValues

    val modifiedTree = initialTree.remove(toRemove, removeProof).get

    val expectedTree = out0.R5[AvlTree].get

    val selfTokensCorrect = SELF.tokens(0)._1 == tokenId
    val selfOutTokensAmount = SELF.tokens(0)._2
    val soutTokensCorrect = out0.tokens(0)._1 == tokenId
    val soutTokensAmount = out0.tokens(0)._2

    val tokensPreserved = selfTokensCorrect && soutTokensCorrect && (soutTokensAmount + withdrawTotal == selfOutTokensAmount)

    val properTreeModification = modifiedTree == expectedTree

    val selfOutputCorrect = out0.propositionBytes == SELF.propositionBytes

    properTreeModification && valuesCorrect && selfOutputCorrect && tokensPreserved

## Possible Enhancements

Please note that there are many nuances our example contract is ignoring. For example, it is allowed to execute the
contract to anyone who is able to construct proper spending transactions (in out example, anyone listening to the
blockchain) during funding and withdrawal stages. In the real-world cases, additional signature from the project or
a trusted arbiter could be needed.
Also, there is no self-destruction case considered in the withdrawal contract, so it will live before being destroyed
by miners via storage rent mechanism, potentially for decades or even centuries. For the funding stage, it would be
reasonable to have an additional input from the project with the value equal to the value of the fee output. And so on.
  */

class IcoExample extends CompilerTestingCommons
  with CompilerCrossVersionProps with BeforeAndAfterAll { suite =>

  // Not mixed with TestContext since it is not possible to call commpiler.compile outside tests if mixed
  implicit lazy val IR: IRContext = new IRContext {}

  lazy val spec = TestContractSpec(suite)
  lazy val project = new ErgoLikeTestProvingInterpreter()

  private val miningRewardsDelay = 720
  private val feeProp = ErgoTreePredef.feeProposition(miningRewardsDelay) // create ErgoTree v0
  private val feeBytes = feeProp.bytes

  val env = Map(
    "feeBytes" -> Colls.fromArray(feeBytes),
    "projectPubKey" -> project.secrets.head.publicImage
  )
  lazy val withdrawalScript: SigmaPropValue = compile(env,
    """{
      |  val removeProof = getVar[Coll[Byte]](2).get
      |  val lookupProof = getVar[Coll[Byte]](3).get
      |  val withdrawIndexes = getVar[Coll[Int]](4).get
      |
      |  val out0 = OUTPUTS(0)
      |
      |  val tokenId: Coll[Byte] = SELF.R4[Coll[Byte]].get
      |
      |  val withdrawals = withdrawIndexes.map({(idx: Int) =>
      |     val b = OUTPUTS(idx)
      |     if(b.tokens(0)._1 == tokenId) {
      |       (blake2b256(b.propositionBytes), b.tokens(0)._2)
      |     } else {
      |       (blake2b256(b.propositionBytes), 0L)
      |     }
      |  })
      |
      |  //val withdrawals = OUTPUTS.slice(1, OUTPUTS.size-1).map(...)
      |
      |  val withdrawValues = withdrawals.map({(t: (Coll[Byte], Long)) => t._2})
      |
      |  val withdrawTotal = withdrawValues.fold(0L, { (l1: Long, l2: Long) => l1 + l2 })
      |
      |  val toRemove = withdrawals.map({(t: (Coll[Byte], Long)) => t._1})
      |
      |  val initialTree = SELF.R5[AvlTree].get
      |
      |  val removedValues = initialTree.getMany(toRemove, lookupProof).map({(o: Option[Coll[Byte]]) => byteArrayToLong(o.get)})
      |  val valuesCorrect = removedValues == withdrawValues
      |
      |  val modifiedTree = initialTree.remove(toRemove, removeProof).get
      |
      |  val expectedTree = out0.R5[AvlTree].get
      |
      |  val selfTokensCorrect = SELF.tokens(0)._1 == tokenId
      |  val selfOutTokensAmount = SELF.tokens(0)._2
      |  val soutTokensCorrect = out0.tokens(0)._1 == tokenId
      |  val soutTokensAmount = out0.tokens(0)._2
      |
      |  val tokensPreserved = selfTokensCorrect && soutTokensCorrect && (soutTokensAmount + withdrawTotal == selfOutTokensAmount)
      |
      |  val properTreeModification = modifiedTree == expectedTree
      |
      |  val selfOutputCorrect = out0.propositionBytes == SELF.propositionBytes
      |
      |  selfOutputCorrect
      | // properTreeModification && valuesCorrect && selfOutputCorrect && tokensPreserved
      |}""".stripMargin
  ).asBoolValue.toSigmaProp
  def withdrawalTree = mkTestErgoTree(withdrawalScript)

  def wsHash = {
    Blake2b256(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(withdrawalTree))
  }

  def issuanceScript: SigmaPropValue = compile(env.updated("nextStageScriptHash", Colls.fromArray(wsHash)),
    """{
      |  val openTree = SELF.R5[AvlTree].get
      |
      |  val closedTree = OUTPUTS(0).R5[AvlTree].get
      |
      |  val digestPreserved = openTree.digest == closedTree.digest
      |  val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
      |  val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
      |  val treeIsClosed = closedTree.enabledOperations == 4
      |
      |  val tokenId: Coll[Byte] = INPUTS(0).id
      |
      |  val tokensIssued = OUTPUTS(0).tokens(0)._2
      |
      |  val outputsCountCorrect = OUTPUTS.size == 3
      |  val secondOutputNoTokens = OUTPUTS(0).tokens.size == 1 && OUTPUTS(1).tokens.size == 0 && OUTPUTS(2).tokens.size == 0
      |
      |  val correctTokensIssued = SELF.value == tokensIssued
      |
      |  val correctTokenId = OUTPUTS(0).R4[Coll[Byte]].get == tokenId && OUTPUTS(0).tokens(0)._1 == tokenId
      |
      |  val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
      |  val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
      |
      |  val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed
      |
      |  projectPubKey && treeIsCorrect && valuePreserved && stateChanged
      |}""".stripMargin
  ).asSigmaProp

  def issuanceHash = {
    val tree = mkTestErgoTree(issuanceScript)
    Blake2b256(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree))
  }

  def fundingScript: SigmaPropValue = compile(env.updated("nextStageScriptHash", Colls.fromArray(issuanceHash)),
    """{
      |
      |  val selfIndexIsZero = INPUTS(0).id == SELF.id
      |
      |  val proof = getVar[Coll[Byte]](1).get
      |
      |  val inputsCount = INPUTS.size
      |
      |  val toAdd: Coll[(Coll[Byte], Coll[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      |     val pk = b.R4[Coll[Byte]].get
      |     val value = longToByteArray(b.value)
      |     (pk, value)
      |  })
      |
      |  val modifiedTree = SELF.R5[AvlTree].get.insert(toAdd, proof).get
      |
      |  val expectedTree = OUTPUTS(0).R5[AvlTree].get
      |
      |  val properTreeModification = modifiedTree == expectedTree
      |
      |  val outputsCount = OUTPUTS.size == 2
      |
      |  val selfOutputCorrect = if(HEIGHT < 2000) {
      |    OUTPUTS(0).propositionBytes == SELF.propositionBytes
      |  } else {
      |    blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
      |  }
      |
      |  val feeOutputCorrect = (OUTPUTS(1).value <= 1) && (OUTPUTS(1).propositionBytes == feeBytes)
      |
      |  val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect
      |
      |  selfIndexIsZero && outputsCorrect && properTreeModification
      |}""".stripMargin
  ).asBoolValue.toSigmaProp

  property("simple ico example - fundraising stage only") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest.toColl
    val initTreeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None))

    val fundingTree = mkTestErgoTree(fundingScript)

    val projectBoxBefore = testBox(10, fundingTree, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(initTreeData)))

    val funderBoxCount = 2000

    val funderBoxes = (1 to funderBoxCount).map { _ =>
      testBox(10, TrueTree, 0, Seq(),
        Map(R4 -> ByteArrayConstant(Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte))))
    }

    val inputBoxes = IndexedSeq(projectBoxBefore) ++ funderBoxes

    inputBoxes.tail.foreach { b =>
      val k = b.get(R4).get.asInstanceOf[CollectionConstant[SByte.type]].value.toArray
      val v = Longs.toByteArray(b.value)
      avlProver.performOneOperation(Insert(ADKey @@ k, ADValue @@ v))
    }

    val proof = avlProver.generateProof()
    val endTree = SigmaDsl.avlTree(new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.AllOperationsAllowed, 32, None))

    val projectBoxAfter = testBox(funderBoxCount * 10 - 1, fundingTree, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.fill(1)(0: Byte)), R5 -> AvlTreeConstant(endTree)))
    val feeBox = testBox(1, feeProp, 0, Seq(), Map())

    val fundingTx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(projectBoxAfter, feeBox))

    val fundingContext = ErgoLikeContextTesting(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = inputBoxes,
      spendingTransaction = fundingTx,
      self = projectBoxBefore, activatedVersionInTests)

    val projectProver = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(1, ByteArrayConstant(proof))

    val res = projectProver.prove(env, fundingTree, fundingContext, fakeMessage).get
    printDebug("funding script cost: " + res.cost)
    printDebug("lookup proof size: " + proof.length)

    //todo: test switching to fixing stage
  }

  property("simple ico example - issuance stage") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val openTreeData = SigmaDsl.avlTree(new AvlTreeData(digest.toColl, AvlTreeFlags.AllOperationsAllowed, 32, None))

    val issuanceTree = mkTestErgoTree(issuanceScript)
    val projectBoxBeforeClosing = testBox(10, issuanceTree, 0, Seq(),
      Map(R4 -> ByteArrayConstant(Array.emptyByteArray), R5 -> AvlTreeConstant(openTreeData)))

    val tokenId = Digest32Coll @@@ Colls.fromArray(projectBoxBeforeClosing.id)
    val closedTreeData = SigmaDsl.avlTree(new AvlTreeData(digest.toColl, AvlTreeFlags.RemoveOnly, 32, None))

    val projectBoxAfterClosing = testBox(1, withdrawalTree, 0,
      Seq(tokenId -> projectBoxBeforeClosing.value),
      Map(R4 -> ByteArrayConstant(tokenId), R5 -> AvlTreeConstant(closedTreeData)))

    val ergoWithdrawalBox = testBox(8, TrueTree, 0, Seq(), Map())
    val feeBox = testBox(1, feeProp, 0, Seq(), Map())

    val issuanceTx = ErgoLikeTransaction(
      IndexedSeq(), IndexedSeq(projectBoxAfterClosing, ergoWithdrawalBox, feeBox))

    val issuanceContext = ErgoLikeContextTesting(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBeforeClosing),
      spendingTransaction = issuanceTx,
      self = projectBoxBeforeClosing, activatedVersionInTests)

    val res = project.prove(env, issuanceTree, issuanceContext, fakeMessage).get
    printDebug("token issuance script cost: " + res.cost)
  }

  property("simple ico example - withdrawal stage") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    val funderBoxCount = 2000
    val funderProps = (1 to funderBoxCount).map { _ =>
      val keyPoint = CryptoConstants.dlogGroup.createRandomElement()
      val tree = mkTestErgoTree(CreateProveDlog(GroupElementConstant(keyPoint)).asSigmaProp)
      val propBytes = DefaultSerializer.serializeErgoTree(tree)
      propBytes -> Longs.toByteArray(Random.nextInt(Int.MaxValue).toLong)
    }
    val funderKvs = funderProps.map { case (prop, v) =>
      val k = Blake2b256(prop)
      k -> v
    }

    funderKvs.foreach { case (k, v) =>
      avlProver.performOneOperation(Insert(ADKey @@@ k, ADValue @@ v))
    }
    val digest = avlProver.digest
    val fundersTree = new AvlTreeData(digest.toColl, AvlTreeFlags.RemoveOnly, 32, None)

    val withdrawalsCount = 8
    val withdrawals = funderKvs.take(withdrawalsCount)

    avlProver.generateProof()

    withdrawals.foreach { case (k, _) =>
      avlProver.performOneOperation(Lookup(ADKey @@@ k))
    }
    val lookupProof = avlProver.generateProof()

    withdrawals.foreach { case (k, _) =>
      avlProver.performOneOperation(Remove(ADKey @@@ k))
    }
    val removalProof = avlProver.generateProof()

    val finalTree = new AvlTreeData(avlProver.digest.toColl, AvlTreeFlags.RemoveOnly, 32, None)

    val tokenId = Digest32Coll @@ Colls.fromArray(Array.fill(32)(Random.nextInt(100).toByte))

    val withdrawalAmounts = funderProps.take(withdrawalsCount).map { case (prop, v) =>
      val tv = Longs.fromByteArray(v)
      prop -> tv
    }

    val withdrawBoxes = withdrawalAmounts.map { case (prop, tv) =>
      testBox(1, DefaultSerializer.deserializeErgoTree(prop), 0, Seq(tokenId -> tv))
    }

    val totalTokenAmount = withdrawalAmounts.map(_._2).sum + 1

    val projectBoxBefore = testBox(11, withdrawalTree, 0,
      Seq(tokenId -> totalTokenAmount),
      Map(R4 -> ByteArrayConstant(tokenId), R5 -> AvlTreeConstant(fundersTree)))
    val projectBoxAfter = testBox(1, withdrawalTree, 0,
      Seq(tokenId -> 1),
      Map(R4 -> ByteArrayConstant(tokenId), R5 -> AvlTreeConstant(finalTree)))
    val feeBox = testBox(1, feeProp, 0, Seq(), Map())

    val outputs = IndexedSeq(projectBoxAfter) ++ withdrawBoxes ++ IndexedSeq(feeBox)
    val fundingTx = ErgoLikeTransaction(IndexedSeq(), outputs)

    val fundingContext = ErgoLikeContextTesting(
      currentHeight = 1000,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(projectBoxBefore),
      spendingTransaction = fundingTx,
      self = projectBoxBefore, activatedVersionInTests)

    val projectProver =
      new ContextEnrichingTestProvingInterpreter()
        .withContextExtender(2, ByteArrayConstant(removalProof))
        .withContextExtender(3, ByteArrayConstant(lookupProof))
        .withContextExtender(4, IntArrayConstant((1 to withdrawalsCount).toArray))

    val res = projectProver.prove(env, withdrawalTree, fundingContext, fakeMessage).get
    printDebug("withdrawal script cost: " + res.cost)
    printDebug("remove proof size: " + removalProof.length)
    printDebug("lookup proof size: " + lookupProof.length)
  }

  /** This is the last executed test suite, so this method is executed after all tests.
    * We output statistics of how PrecompiledScriptProcessor cache was used. */
  override protected def afterAll(): Unit = {
    printDebug("verifySignatureProfiler ==========================================")
    printDebug(Interpreter.verifySignatureProfiler.generateReport)
  }

}
