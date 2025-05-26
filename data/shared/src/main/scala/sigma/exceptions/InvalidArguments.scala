package sigma.exceptions

import sigma.ast.SourceContext

final class InvalidArguments(message: String, source: Option[SourceContext] = None)
  extends BinderException(message, source)

