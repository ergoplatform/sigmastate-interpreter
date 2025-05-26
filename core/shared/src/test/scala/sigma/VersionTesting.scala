package sigma

import debox.cfor

import scala.util.DynamicVariable

trait VersionTesting {

  /** In v5.x we run test for only one activated version on the network (== 2).
    * In the branch for v6.0 the new version 3 should be added so that the tests run for both.
    */
  protected val activatedVersions: Seq[Byte] =
    (0 to VersionContext.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigma] val _currActivatedVersion = new DynamicVariable[Byte](2) // v5.x by default

  /** Current activated version used in tests. */
  def activatedVersionInTests: Byte = _currActivatedVersion.value

  val ergoTreeVersions: Seq[Byte] =
    (0 to VersionContext.MaxSupportedScriptVersion).map(_.toByte).toArray[Byte]

  private[sigma] val _currErgoTreeVersion = new DynamicVariable[Byte](0)

  /** Current ErgoTree version assigned dynamically. */
  def ergoTreeVersionInTests: Byte = _currErgoTreeVersion.value

  /** Executes the given block for each combination of _currActivatedVersion and
    * _currErgoTreeVersion assigned to dynamic variables.
    */
  def forEachScriptAndErgoTreeVersion
    (activatedVers: Seq[Byte], ergoTreeVers: Seq[Byte])
    (block: => Unit): Unit = {
    cfor(0)(_ < activatedVers.length, _ + 1) { i =>
      val activatedVersion = activatedVers(i)
      // setup each activated version
      _currActivatedVersion.withValue(activatedVersion) {

        cfor(0)(
          i => i < ergoTreeVers.length && ergoTreeVers(i) <= activatedVersion,
          _ + 1) { j =>
          val treeVersion = ergoTreeVers(j)
          // for each tree version up to currently activated, set it up and execute block
          _currErgoTreeVersion.withValue(treeVersion)(block)
        }

      }
    }
  }

  /** Helper method which executes the given `block` once for each `activatedVers`.
    * The method sets the dynamic variable activatedVersionInTests with is then available
    * in the block.
    */
  def forEachActivatedScriptVersion(activatedVers: Seq[Byte])(block: => Unit): Unit = {
    cfor(0)(_ < activatedVers.length, _ + 1) { i =>
      val activatedVersion = activatedVers(i)
      _currActivatedVersion.withValue(activatedVersion)(block)
    }
  }

  /** Helper method which executes the given `block` once for each `ergoTreeVers`.
    * The method sets the dynamic variable ergoTreeVersionInTests with is then available
    * in the block.
    */
  def forEachErgoTreeVersion(ergoTreeVers: Seq[Byte])(block: => Unit): Unit = {
    cfor(0)(_ < ergoTreeVers.length, _ + 1) { i =>
      val version = ergoTreeVers(i)
      _currErgoTreeVersion.withValue(version)(block)
    }
  }

  val printVersions: Boolean = false

  protected def testFun_Run(testName: String, testFun: => Any): Unit = {
    def msg = s"""property("$testName")(ActivatedVersion = $activatedVersionInTests; ErgoTree version = $ergoTreeVersionInTests)"""
    if (printVersions) println(msg)
    try testFun
    catch {
      case t: Throwable =>
        if (!printVersions) {
          // wasn't printed, print it now
          println(msg)
        }
        throw t
    }
  }

}
