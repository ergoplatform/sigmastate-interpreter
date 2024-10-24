package sigma.serialization

import scorex.util.Extensions._
import scorex.util.serialization.Reader
import sigma.ast.SType
import sigma.validation.ValidationRules.CheckPositionLimit

/** Reader used in the concrete implementations of [[SigmaSerializer]].
  * It decorates the given reader, delegates most of the methods to it, but also adds new
  * methods.
  *
  * @param r                              the underlying reader this reader reads from
  * @param maxTreeDepth                   limit on the tree depth (recursive invocations)
  *                                       of the deserializer
  */
class CoreByteReader(val r: Reader, val maxTreeDepth: Int = CoreSerializer.MaxTreeDepth)
  extends Reader {

  /** Checks that the current reader position is <= positionLimit.
    * NOTE, since v5.0 the same check is done via validation rule, which wraps the
    * original exception into ValidationException. This makes this condition soft-forkable,
    * other than that both v4.x and v5.x will work and fail at the same time, so the
    * change is consensus-safe.
    */
  @inline protected def checkPositionLimit(): Unit = {
    CheckPositionLimit(position, positionLimit)
  }

  override type CH = r.CH

  @inline
  override def newReader(chunk: CH): Reader.Aux[CH] = r.newReader(chunk)

  @inline override def getChunk(size: Int): CH = r.getChunk(size)

  @inline override def getShortString(): String = {
    checkPositionLimit()
    r.getShortString()
  }

  @inline override def peekByte(): Byte = r.peekByte()

  @inline override def getByte(): Byte = {
    checkPositionLimit()
    r.getByte()
  }

  @inline override def getUByte(): Int = {
    checkPositionLimit()
    r.getUByte()
  }

  @inline override def getShort(): Short = {
    checkPositionLimit()
    r.getShort()
  }

  @inline override def getUShort(): Int = {
    checkPositionLimit()
    r.getUShort()
  }

  @inline override def getInt(): Int = {
    checkPositionLimit()
    r.getInt()
  }

  @inline override def getUInt(): Long = {
    checkPositionLimit()
    r.getUInt()
  }

  @inline def getUIntExact: Int = getUInt().toIntExact

  @inline override def getLong(): Long = {
    checkPositionLimit()
    r.getLong()
  }

  @inline override def getULong(): Long = {
    checkPositionLimit()
    r.getULong()
  }

  @inline override def getBytes(size: Int): Array[Byte] = {
    checkPositionLimit()
    r.getBytes(size)
  }

  /** Reads either the given of remaining number of bytes from this reader.
    * This method is `unsafe` because it may return less than requested number of bytes.
    * @param numRequestedBytes
    */
  @inline final def getBytesUnsafe(numRequestedBytes: Int): Array[Byte] = {
    checkPositionLimit()
    val bytesToRead = Math.min(numRequestedBytes, remaining)
    r.getBytes(bytesToRead)
  }

  @inline override def getBits(size: Int): Array[Boolean] = {
    checkPositionLimit()
    r.getBits(size)
  }

  @inline override def getOption[T](getValue: => T): Option[T] = {
    checkPositionLimit()
    r.getOption(getValue)
  }

  @inline override def consumed: Int = r.consumed

  @inline override def position: Int = r.position

  @inline override def position_=(p: Int): Unit = r.position_=(p)

  @inline override def remaining: Int = r.remaining

  @inline override def mark(): this.type = {
    r.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)

  private var lvl: Int = 0
  @inline def level: Int = lvl
  @inline def level_=(v: Int): Unit = {
    if (v > maxTreeDepth)
      throw new DeserializeCallDepthExceeded(s"nested value deserialization call depth($v) exceeds allowed maximum $maxTreeDepth")
    lvl = v
  }

  private var positionLmt: Int = r.position + r.remaining
  @inline final def positionLimit: Int = positionLmt
  @inline final def positionLimit_=(v: Int): Unit = {
    positionLmt = v
  }

  private var _wasDeserialize: Boolean = false
  /** Helper property which is used to track deserialization operations during parsing. */
  @inline final def wasDeserialize: Boolean = _wasDeserialize
  @inline final def wasDeserialize_=(v: Boolean): Unit = {
    _wasDeserialize = v
  }

  private var _wasUsingBlockchainContext: Boolean = false
  /** Helper property which is used to track operations using the blockchain context during parsing. */
  @inline final def wasUsingBlockchainContext: Boolean = _wasUsingBlockchainContext

  @inline final def wasUsingBlockchainContext_=(v: Boolean): Unit = {
    _wasUsingBlockchainContext = v
  }
}
