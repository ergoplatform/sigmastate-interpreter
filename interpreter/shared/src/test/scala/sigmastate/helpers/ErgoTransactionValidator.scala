package sigmastate.helpers

import org.ergoplatform._
import sigma.eval.EvalSettings
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}

import scala.util.{Failure, Success}

/** Base class for interpreters used in tests.
  * @see derived classes */
class ErgoLikeTestInterpreter extends ErgoLikeInterpreter {
  override type CTX = ErgoLikeContext
  override val evalSettings: EvalSettings = DefaultEvalSettings.copy(
    isMeasureOperationTime = true,
    isDebug = true,
    isTestRun = true)
}

class ErgoTransactionValidator(activatedVersion: Byte) {
  val verifier = new ErgoLikeTestInterpreter()

  def validate(tx: ErgoLikeTransaction,
               blockchainState: BlockchainState,
               minerPubkey: Array[Byte],
               boxesReader: ErgoBoxReader): Either[Throwable, Long] = {

    val msg = tx.messageToSign
    val inputs = tx.inputs

    val boxes: IndexedSeq[ErgoBox] = tx.inputs.map(_.boxId).map{id =>
      boxesReader.byId(id) match {
        case Success(box) => box
        case Failure(e) => return Left[Throwable, Long](e)
      }
    }

    val txCost = boxes.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)
      val proof = input.spendingProof

      val proverExtension = tx.inputs(idx).spendingProof.extension

      val context =
        ErgoLikeContextTesting(blockchainState.currentHeight, blockchainState.lastBlockUtxoRoot, minerPubkey, boxes,
          tx, box, activatedVersion, proverExtension)
      val verificationResult = verifier.verify(
        emptyEnv + (ScriptNameProp -> s"height_${blockchainState.currentHeight }_verify"),
        box.ergoTree, context, proof, msg)
      val scriptCost: Long = verificationResult match {
        case Success((res, cost)) =>
          if(!res) return Left[Throwable, Long](new Exception(s"Validation failed for input #$idx"))
          else cost
        case Failure(e) =>
          return Left[Throwable, Long](e)
      }
      accCost + scriptCost
    }
    Right(txCost)
  }
}