package sigma.js

import scala.scalajs.js

/** Base trait for all JS wrappers over some Scala type T. */
trait JsWrapper[T] extends js.Object {
  def wrappedValue: T
}
