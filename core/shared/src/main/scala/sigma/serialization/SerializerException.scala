package sigma.serialization

import sigma.SigmaException

/** Exception thrown during serialization.
  *
  * @param message the error message
  * @param cause   an optional cause for the exception
  */
case class SerializerException(
    override val message: String,
    override val cause: Option[Throwable] = None
) extends SigmaException(message, cause)

/** Thrown by TypeSerializer when type prefix <= 0. */
final class InvalidTypePrefix(message: String)
    extends SerializerException(message)

/** Thrown when the current reader position > positionLimit which is set in the Reader.
  * @see [[org.ergoplatform.validation.ValidationRules.CheckPositionLimit]]
  */
final class ReaderPositionLimitExceeded(
    message: String,
    val position: Int,
    val positionLimit: Int
) extends SerializerException(message)

/** Thrown when the current depth level > maxDepthLevel which is set in the Reader. */
final class DeserializeCallDepthExceeded(message: String)
    extends SerializerException(message)

/** Thrown by [[org.ergoplatform.validation.ValidationRules.CheckValidOpCode]] validation rule. */
final class InvalidOpCode(message: String)
    extends SerializerException(message)
