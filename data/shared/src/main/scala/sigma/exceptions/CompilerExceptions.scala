package sigma.exceptions

import sigma.SigmaException
import sigma.ast.SourceContext

/** Base class for exceptions thrown by the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional underlying cause for the exception
  */
class CompilerException(
    message: String,
    val source: Option[SourceContext] = None,
    cause: Option[Throwable] = None
) extends SigmaException(message, cause) {

  def this(message: String) = this(message, None, None)

  def this(message: String, source: Option[SourceContext]) = this(message, source, None)

  override def getMessage: String = source.map { srcCtx =>
    val lineNumberStrPrefix = s"line ${srcCtx.line}: "
    "\n" + lineNumberStrPrefix +
      s"${srcCtx.sourceLine}\n${" " * (lineNumberStrPrefix.length + srcCtx.column - 1)}^\n" + message
  }.getOrElse(message)
}

/** Exception thrown during the binding phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional underlying cause for the exception
  */
class BinderException(
    message: String,
    source: Option[SourceContext] = None,
    cause: Option[Throwable] = None
) extends CompilerException(message, source, cause) {

  def this(message: String) = this(message, None, None)

  def this(message: String, source: Option[SourceContext]) = this(message, source, None)
}

/** Exception thrown during the type checking phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional underlying cause for the exception
  */
class TyperException(
    message: String,
    source: Option[SourceContext] = None,
    cause: Option[Throwable] = None
) extends CompilerException(message, source, cause) {

  def this(message: String) = this(message, None, None)

  def this(message: String, source: Option[SourceContext]) = this(message, source, None)
}

/** Exception thrown during the building phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional underlying cause for the exception
  */
class BuilderException(
    message: String,
    source: Option[SourceContext] = None,
    cause: Option[Throwable] = None
) extends CompilerException(message, source, cause) {

  def this(message: String) = this(message, None, None)

  def this(message: String, source: Option[SourceContext]) = this(message, source, None)
}

/** Exception thrown during graph building.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional underlying cause for the exception
  */
class GraphBuildingException(
    message: String,
    source: Option[SourceContext] = None,
    cause: Option[Throwable] = None
) extends CompilerException(message, source, cause) {

  def this(message: String) = this(message, None, None)

  def this(message: String, source: Option[SourceContext]) = this(message, source, None)

}






