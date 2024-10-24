package sigma

/** Describes the current execution environment. */
sealed abstract class Environment {
  /** Runtime platform for the current environment. */
  def runtimePlatform: RuntimePlatform

  /** Returns true when executed on JVM. */
  def isJVM: Boolean

  /** Returns true when executed on JS. */
  def isJS: Boolean
}

object Environment {
  /** Current runtime environment. */
  val current: Environment = new Environment {
    override def isJVM: Boolean = runtimePlatform == RuntimePlatform.JVM
    override def isJS: Boolean = runtimePlatform == RuntimePlatform.JS
    override def runtimePlatform: RuntimePlatform = sigma.reflection.Platform.runtimePlatform
  }
}