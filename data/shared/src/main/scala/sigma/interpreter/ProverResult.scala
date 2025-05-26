package sigma.interpreter

import scorex.util.encode.Base16
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}

/**
  * Proof of correctness of tx spending
  *
  * @param proof     - proof that satisfies final sigma proposition
  * @param extension - user-defined variables to be put into context
  */
class ProverResult(val proof: Array[Byte], val extension: ContextExtension) {
  override def hashCode(): Int = java.util.Arrays.hashCode(proof) * 31 + extension.hashCode()

  override def equals(obj: scala.Any): Boolean =
  (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case obj: ProverResult =>
      java.util.Arrays.equals(proof, obj.proof) && extension == obj.extension
    case _ => false
  })

  override def toString = s"ProverResult(${Base16.encode(proof)},$extension)"
}

object ProverResult {
  val empty: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)

  def apply(proof: Array[Byte], extension: ContextExtension): ProverResult =
    new ProverResult(proof, extension)

  object serializer extends SigmaSerializer[ProverResult, ProverResult] {

    override def serialize(obj: ProverResult, w: SigmaByteWriter): Unit = {
      w.putUShort(obj.proof.length)
      w.putBytes(obj.proof)
      ContextExtension.serializer.serialize(obj.extension, w)
    }

    override def parse(r: SigmaByteReader): ProverResult = {
      val sigBytesCount = r.getUShort()
      val proofBytes = if (sigBytesCount == 0)
        Array.emptyByteArray // this allows to avoid hundreds of thousands of allocations
      else
        r.getBytes(sigBytesCount)
      val ce = ContextExtension.serializer.parse(r)
      ProverResult(proofBytes, ce)
    }
  }

}


case class CostedProverResult(override val proof: Array[Byte],
                              override val extension: ContextExtension,
                              cost: Long) extends ProverResult(proof, extension)
