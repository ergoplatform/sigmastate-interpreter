package sigma.exceptions

import sigma.ast.SourceContext

final class ConstraintFailed(message: String, source: Option[SourceContext] = None)
  extends BuilderException(message, source)

