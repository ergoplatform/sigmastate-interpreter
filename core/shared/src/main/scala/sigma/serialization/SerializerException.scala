package sigma.serialization

import sigma.SigmaException

import scala.collection.compat.immutable.ArraySeq

/** Exception thrown during serialization.
  *
  * @param message the error message
  * @param cause   an optional cause for the exception
  */
case class SerializerException(
    override val message: String,
    override val cause: Option[Throwable] = None,
    override val args: Seq[Any] = ArraySeq.empty
)
    extends SigmaException(message, cause, args)

/** Thrown by TypeSerializer when type prefix <= 0. */
final class InvalidTypePrefix(message: String, cause: Option[Throwable] = None)
    extends SerializerException(message, cause)

/** Thrown when the current reader position > positionLimit which is set in the Reader.
  * @see [[org.ergoplatform.validation.ValidationRules.CheckPositionLimit]]
  */
final class ReaderPositionLimitExceeded(
    message: String,
    val position: Int,
    val positionLimit: Int,
    cause: Option[Throwable] = None)
    extends SerializerException(message, cause)

/** Thrown when the current depth level > maxDepthLevel which is set in the Reader. */
final class DeserializeCallDepthExceeded(message: String, cause: Option[Throwable] = None)
    extends SerializerException(message, cause)

/** Thrown by [[org.ergoplatform.validation.ValidationRules.CheckValidOpCode]] validation rule. */
final class InvalidOpCode(message: String, cause: Option[Throwable] = None)
    extends SerializerException(message, cause)
