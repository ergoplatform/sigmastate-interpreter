package sigma.serialization

import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{Serializer, VLQByteBufferReader, VLQByteBufferWriter}
import sigma.data.SigmaConstants

import java.nio.ByteBuffer

/** Implementation of [[Serializer]] provided by `sigma-core` module. */
abstract class CoreSerializer[TFamily, T <: TFamily] extends Serializer[TFamily, T, CoreByteReader, CoreByteWriter] {

  def error(msg: String) = throw SerializerException(msg)

  /** Serializes the given 'obj' to a new array of bytes using this serializer. */
  final def toBytes(obj: T): Array[Byte] = {
    val w = CoreSerializer.startWriter()
    serialize(obj, w)
    w.toBytes
  }

  /** Deserializes `bytes` to an object of this [[TFamily]] using this serializer.
    * The actual class of the returned object is expected to be descendant of [[TFamily]].
    */
  final def fromBytes(bytes: Array[Byte]): TFamily = {
    parse(CoreSerializer.startReader(bytes))
  }
}

object CoreSerializer {
  /** Max length of Box.propositionBytes collection */
  val MaxPropositionSize: Int = SigmaConstants.MaxPropositionBytes.value

  /** Max tree depth should not be greater then provided value */
  val MaxTreeDepth: Int = SigmaConstants.MaxTreeDepth.value

  /** Helper function to be use in serializers.
    * Starting position is marked and then used to compute number of consumed bytes.
    * val r = Serializer.startReader(bytes, pos)
    * val obj = r.getValue()
    * obj -> r.consumed
    */
  def startReader(bytes: Array[Byte],
      pos: Int = 0): CoreByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(pos)
    val r = new CoreByteReader(
      new VLQByteBufferReader(buf),
      maxTreeDepth = MaxTreeDepth
    ).mark()
    r
  }

  /** Helper function to be use in serializers. */
  def startReader(bytes: Array[Byte]): CoreByteReader = {
    val buf = ByteBuffer.wrap(bytes)
    val r = new CoreByteReader(new VLQByteBufferReader(buf),
      maxTreeDepth = MaxTreeDepth).mark()
    r
  }

  /** Helper function to be use in serializers.
    * val w = Serializer.startWriter()
    * w.putLong(l)
    * val res = w.toBytes
    * res */
  def startWriter(): CoreByteWriter = {
    val b = new ByteArrayBuilder()
    val wi = new VLQByteBufferWriter(b)
    val w = new CoreByteWriter(wi)
    w
  }

}

