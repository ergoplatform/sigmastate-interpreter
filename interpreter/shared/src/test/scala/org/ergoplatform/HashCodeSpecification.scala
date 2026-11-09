package org.ergoplatform

import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import scorex.utils.Ints
import sigma.data.CHeader
import sigma.interpreter.ProverResult
import sigma.SigmaTestingData

/** Tests that classes wrapping cryptographic hashes derive their hashCode from
  * the hash bytes instead of hashing the whole byte array (see issue #197).
  */
class HashCodeSpecification extends SigmaTestingData {

  /** Several 32-byte values usable as box ids / digests. */
  private def boxIds: Seq[BoxId] = Seq(
    Array.tabulate[Byte](32)(i => (i + 1).toByte),
    Array.fill[Byte](32)(0x7f.toByte),
    Array.tabulate[Byte](32)(i => (i * 31 + 5).toByte)
  ).map(bytes => ADKey @@ bytes)

  property("Input, DataInput and UnsignedInput hashCode is derived from boxId") {
    for (boxId <- boxIds) {
      DataInput(boxId).hashCode() shouldBe Ints.fromByteArray(boxId)
      new UnsignedInput(boxId).hashCode() shouldBe Ints.fromByteArray(boxId)
      // Input extends UnsignedInput and inherits its hashCode
      Input(boxId, ProverResult.empty).hashCode() shouldBe Ints.fromByteArray(boxId)
    }

    // different box ids must produce different hash codes
    val hashCodes = boxIds.map(Ints.fromByteArray)
    hashCodes.distinct.length shouldBe hashCodes.length
  }

  property("Inputs with the same boxId are equal and have equal hash codes") {
    val boxId = boxIds.head
    val in1 = Input(boxId, ProverResult.empty)
    val in2 = Input(boxId, ProverResult(Array[Byte](1, 2, 3), sigma.interpreter.ContextExtension.empty))
    in1 shouldBe in2
    in1.hashCode() shouldBe in2.hashCode()
  }

  property("ErgoHeader and CHeader hashCode is derived from the block id") {
    val ch = TestData.h1.asInstanceOf[CHeader]
    val headerIdBytes = ch.ergoHeader.serializedId

    ch.ergoHeader.hashCode() shouldBe Ints.fromByteArray(headerIdBytes)
    ch.hashCode() shouldBe Ints.fromByteArray(headerIdBytes)
  }

  property("Headers with different ids have different hash codes") {
    val ch1 = TestData.h1.asInstanceOf[CHeader]
    val ch2 = TestData.h2.asInstanceOf[CHeader]
    ch1.id should not be ch2.id
    ch1.hashCode() should not be ch2.hashCode()
  }
}
