package sigmastate.helpers

import scorex.utils.Random
import sigma.ast._
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.crypto.DiffieHellmanTupleProverInput

class ContextEnrichingTestProvingInterpreter
  extends ErgoLikeTestProvingInterpreter with ContextEnrichingProverInterpreter {

  override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = (1 to 10).map { i =>
    val ba = Random.randomBytes(75)
    i.toByte -> ByteArrayConstant(ba)
  }.toMap

  def withContextExtender(tag: Byte, value: EvaluatedValue[_ <: SType]): ContextEnrichingTestProvingInterpreter = {
    val s = secrets
    val ce = contextExtenders

    new ContextEnrichingTestProvingInterpreter {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ce + (tag -> value)
    }
  }

  def withSecrets(additionalSecrets: Seq[DLogProverInput]): ContextEnrichingTestProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new ContextEnrichingTestProvingInterpreter {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ce
    }
  }

  def withDHSecrets(additionalSecrets: Seq[DiffieHellmanTupleProverInput]): ContextEnrichingTestProvingInterpreter = {
    val ce = contextExtenders
    val s = secrets ++ additionalSecrets

    new ContextEnrichingTestProvingInterpreter {
      override lazy val secrets = s
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = ce
    }
  }
}
