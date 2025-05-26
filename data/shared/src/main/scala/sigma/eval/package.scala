package sigma

import sigma.data.{CAnyValue, CSigmaDslBuilder, OverloadHack, RType}

package object eval {
  /** The primary reference to Global instance of SigmaDsl.
    * Besides operations of SigmaDslBuilder class, this instance also contains methods,
    * which are not available in Dsl code, and which are not in SigmaDslBuilder interface.
    * For example methods like `Box`, `toErgoBox` are available here, but not available in Dsl.
    *
    * @see SigmaDslBuilder
    */
  val SigmaDsl = CSigmaDslBuilder

  /** Encapsulate platform-specific logic of ensuring the value carries its precise type.
    * For JVM this is identity function.
    * For JS it can transform to AnyValue, if the type is numeric
    */
  def ensureTypeCarringValue(v: Any, tT: RType[Any]): Any =
    if (Environment.current.isJVM) v
    else { // JS
      v match {
        case _: Byte | _: Short | _: Int =>
          // this is necessary for JS where Byte, Short, Int have the same runtime class
          // and hence we need to pass the type information explicitly
          CAnyValue(v)(tT, OverloadHack.overloaded1)
        case _ => v
      }
    }
}
