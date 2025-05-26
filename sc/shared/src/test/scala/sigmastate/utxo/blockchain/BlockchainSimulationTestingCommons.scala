package sigmastate.utxo.blockchain

import org.ergoplatform.ErgoBox.Token
import org.ergoplatform._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Remove}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigma.ast.{ErgoTree, LongConstant, TrueLeaf}
import sigmastate.eval._
import sigmastate.helpers.{BlockchainState, CompilerTestingCommons, ErgoLikeContextTesting, ErgoLikeTestProvingInterpreter, ErgoTransactionValidator}
import sigmastate.helpers.TestingHelpers._
import sigmastate.utils.Helpers._

import scala.collection.mutable
import scala.util.{Random, Try}
import scorex.util._
import sigma.Colls
import sigma.data.{AvlTreeData, AvlTreeFlags}
import ErgoTree.ZeroHeader
import sigma.compiler.ir.IRContext
import sigma.eval.Extensions.SigmaBooleanOps
import sigma.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons.{FullBlock, ValidationState}

import scala.annotation.tailrec

trait BlockchainSimulationTestingCommons extends CompilerTestingCommons {

  @tailrec
  protected final def checkState(state: ValidationState,
                                 miner: ErgoLikeTestProvingInterpreter,
                                 currentLevel: Int,
                                 limit: Int,
                                 propOpt: Option[ErgoTree] = None,
                                 extension: ContextExtension = ContextExtension.empty): Unit = currentLevel match {
    case i if i >= limit => ()
    case _ =>
      val block = generateBlock(state, miner, currentLevel, propOpt, extension)
      val updStateTry = state.applyBlock(block)
      updStateTry.getOrThrow
      checkState(updStateTry.get, miner, currentLevel + 1, limit, propOpt, extension)
  }

  protected def generateBlock(state: ValidationState,
                              prover: ErgoLikeTestProvingInterpreter,
                              height: Int,
                              propOpt: Option[ErgoTree] = None,
                              extension: ContextExtension = ContextExtension.empty): FullBlock = {
    val prop: ErgoTree = propOpt.getOrElse(
      mkTestErgoTree(prover.dlogSecrets.head.publicImage.toSigmaPropValue))
    val minerPubkey = prover.dlogSecrets.head.publicImage.pkBytes

    val boxesToSpend = state.boxesReader.randomBoxes(30 + height)

    val txs = boxesToSpend.map { box =>
      val newBoxCandidate =
        new ErgoBoxCandidate(10, prop, height, Colls.emptyColl[Token], Map())
      val unsignedInput = new UnsignedInput(box.id)
      val tx = UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq(newBoxCandidate))
      val context = ErgoLikeContextTesting(height + 1,
        state.state.lastBlockUtxoRoot,
        minerPubkey,
        IndexedSeq(box),
        tx,
        box,
        activatedVersionInTests,
        extension)
      val env = emptyEnv + (ScriptNameProp -> s"height_${state.state.currentHeight}_prove")
      val proverResult = prover.prove(env, box.ergoTree, context, tx.messageToSign).get
      proverResult.extension shouldBe extension

      tx.toSigned(IndexedSeq(proverResult))
    }.toIndexedSeq.ensuring(_.nonEmpty, s"Failed to create txs from boxes $boxesToSpend at height $height")

    FullBlock(txs, minerPubkey)
  }

  protected def randomDeepness: Int = 10 + Random.nextInt(10)

}

object BlockchainSimulationTestingCommons extends CompilerTestingCommons {

  private val MaxBlockCost = 1000000

  case class FullBlock(txs: IndexedSeq[ErgoLikeTransaction], minerPubkey: Array[Byte])

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private type KeyType = mutable.WrappedArray.ofByte

    private def getKey(id: Array[Byte]): KeyType = new mutable.WrappedArray.ofByte(id)

    private val boxes = mutable.Map[KeyType, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = byId(getKey(boxId))

    def byId(boxId: KeyType): Try[ErgoBox] = Try(boxes(boxId))

    def randomBoxes(howMany: Int): Iterable[ErgoBox] = {
      scala.util.Random.shuffle(boxes.values).take(howMany)
    }

    def byTwoInts(r1Id: ErgoBox.RegisterId, int1: Int,
                  r2Id: ErgoBox.RegisterId, int2: Int): Option[ErgoBox] =
      boxes.values.find { box =>
        box.get(r1Id).getOrElse(LongConstant(int1 + 1)) == LongConstant(int1) &&
          box.get(r2Id).getOrElse(LongConstant(int2 + 1)) == LongConstant(int2)
      }

    def allIds: Iterable[KeyType] = boxes.keys

    def applyBlock(block: FullBlock): Unit = {
      val toRemove = block.txs.flatMap(_.inputs).map(_.boxId)
      toRemove.foreach(k => prover.performOneOperation(Remove(k)))
      toRemove.foreach(k => boxes.remove(getKey(k)))

      val toAdd = block.txs.flatMap(_.outputs)
      toAdd.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes)))
      toAdd.foreach(b => boxes.put(getKey(b.id), b))

      prover.generateProof()
    }

    def digest: ADDigest = prover.digest
  }


  case class ValidationState(state: BlockchainState, boxesReader: InMemoryErgoBoxReader, activatedVersion: Byte)(implicit IR: IRContext) {
    val validator = new ErgoTransactionValidator(activatedVersion)

    def applyBlock(block: FullBlock, maxCost: Int = MaxBlockCost): Try[ValidationState] = Try {
      val height = state.currentHeight + 1

      val blockCost = block.txs.foldLeft(0L) { case (accCost, tx) =>
        validator.validate(tx, state.copy(currentHeight = height),
          block.minerPubkey,
          boxesReader) match {
          case Left(throwable) => throw throwable
          case Right(cost) => accCost + cost
        }
      }

      assert(blockCost <= maxCost, s"Block cost $blockCost exceeds limit $maxCost")

      boxesReader.applyBlock(block)
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(digest = Colls.fromArray(boxesReader.digest)))
      ValidationState(newState, boxesReader, activatedVersion)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

    def initBlock(scriptVersion: Byte) = FullBlock(
      (0 until 10).map { i =>
        val txId = Blake2b256.hash(i.toString.getBytes ++ scala.util.Random.nextString(12).getBytes).toModifierId
        val boxes = (1 to 50).map(_ =>
          testBox(10,
            ErgoTree.fromProposition(
              ErgoTree.headerWithVersion(ZeroHeader, scriptVersion),
              TrueLeaf.toSigmaProp),
            i, Seq(), Map(), txId))
        createTransaction(boxes)
      },
      ErgoLikeContextTesting.dummyPubkey
    )

    def initialState(activatedVersion: Byte, block: FullBlock)(implicit IR: IRContext): ValidationState = {
      val keySize = 32
      val prover = new BatchProver(keySize, None)

      val digest = Colls.fromArray(prover.digest)
      val utxoRoot = AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, keySize)

      val bs = BlockchainState(currentHeight = -2, utxoRoot)

      val boxReader = new InMemoryErgoBoxReader(prover)

      ValidationState(bs, boxReader, activatedVersion).applyBlock(block).get
    }
  }

}