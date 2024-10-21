package sigma.compiler

/**
  * Can be thrown in staged method body to prevent body unfolding.
  * When this exception is thrown, the caller can catch it and reify
  * this invocation as MethodCall graph node.
  */
class DelayInvokeException extends Exception {
  override def fillInStackTrace(): Throwable = this  // to avoid spending time on recording stack trace
}

