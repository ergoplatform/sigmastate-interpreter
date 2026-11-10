package sigma

/** Base class for Sigma-related exceptions.
  *
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class SigmaException(
  val message: String,
  val cause: Option[Throwable] = None
) extends Exception(message, cause.orNull)

/** Thrown when the depth of a SigmaBoolean proposition tree exceeds the allowed
  * maximum (SigmaConstants.MaxTreeDepth). Mirrors the deserializer depth limit
  * (see DeserializeCallDepthExceeded).
  */
final class SigmaTreeDepthExceeded(message: String) extends SigmaException(message)
