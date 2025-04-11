package sigma

import VersionContext.JitActivationVersion

import scala.util.DynamicVariable

/** Represent currently activated protocol version and currently executed ErgoTree version.
  *
  * This parameters, once set in DynamicVariable can be accessed everywhere on the current
  * thread.
  *
  * @param activatedVersion Currently activated script version == Block.headerVersion - 1
  * @param ergoTreeVersion  version of the currently executed ErgoTree
  *
  * @see
  */
case class VersionContext(activatedVersion: Byte, ergoTreeVersion: Byte) {
  require(ergoTreeVersion <= activatedVersion,
    s"In a valid VersionContext ergoTreeVersion must never exceed activatedVersion: $this")

  /** @return true, if the activated script version of Ergo protocol on the network is
   * greater than v1. */
  def isJitActivated: Boolean = activatedVersion >= JitActivationVersion
}

object VersionContext {
  /** Maximum version of ErgoTree supported by this interpreter release.
    * See version bits in `ErgoTree.header` for more details.
    * This value should be increased with each new protocol update via soft-fork.
    * The following values are used for current and upcoming forks:
    * - version 3.x this value must be 0
    * - in v4.0 must be 1
    * - in v5.x must be 2
    * etc.
    */
  val MaxSupportedScriptVersion: Byte = 2 // supported versions 0, 1, 2

  /** The first version of ErgoTree starting from which the JIT costing interpreter is used. */
  val JitActivationVersion: Byte = 2

  private val _defaultContext = VersionContext(
    activatedVersion = 1 /* v4.x */,
    ergoTreeVersion  = 1
  )

  /** Universally accessible version context which is used to version the code
    * across the whole repository.
    *
    * The default value represent activated Ergo protocol and highest ErgoTree version.
    */
  private val _versionContext: DynamicVariable[VersionContext] =
    new DynamicVariable[VersionContext](_defaultContext)

  /** Returns the current VersionContext attached to the current thread.
    * Each thread can have only one current version context at any time, which can be
    * changed using `withVersions` method.
    *
    * @see withVersions()
    */
  def current: VersionContext = {
    val ctx = _versionContext.value
    if (ctx == null)
      throw new IllegalStateException(
        s"VersionContext is not specified on thread ${Thread.currentThread().getId}")
    ctx
  }

  /** Executes the given block under the given version context attached to the current thread.
    *
    * The typical usage is to use `VersionContext.withVersions(activatedVersion,
    * treeVersion) {...}` when the block of code needs to be executed with the given
    * versions.
    *
    * For example, sigmastate.Interpreter uses it to execute operations according to the
    * necessary versions of Ergo protocol and ErgoTree.
    *
    * @param activatedVersion Currently activated script version == Block.headerVersion - 1
    * @param ergoTreeVersion  ErgoTree version to be set on the current thread
    * @param block            block of code to execute
    * @return result of block execution
    */
  def withVersions[T](activatedVersion: Byte, ergoTreeVersion: Byte)(block: => T): T =
    _versionContext.withValue(VersionContext(activatedVersion, ergoTreeVersion))(block)

  /** Checks the version context has the given versions*/
  def checkVersions(activatedVersion: Byte, ergoTreeVersion: Byte) = {
    val ctx = VersionContext.current
    if (ctx.activatedVersion != activatedVersion || ctx.ergoTreeVersion != ergoTreeVersion) {
      val expected = VersionContext(activatedVersion, ergoTreeVersion)
      throw new IllegalStateException(
        s"Global VersionContext.current = ${ctx} while expected $expected.")
    }
  }

}
