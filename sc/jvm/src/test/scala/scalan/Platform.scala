package scalan

import scalan.compilation.GraphVizExport
import sigma.util.FileUtil
import org.scalatest.Assertions
import sigma.compiler.ir.IRContext

object Platform {
  /** Output graph given by symbols in `sfs` to files.
    * @param ctx The Scalan context
    * @param prefix The prefix of the directory where the graphs will be stored.
    * @param testName The name of the test.
    * @param name The name of the graph.
    * @param sfs A sequence of functions that return symbols of the graph roots.
    */
  def stage[Ctx <: IRContext](ctx: Ctx)
      (prefix: String, testName: String, name: String, sfs: Seq[() => ctx.Sym]): Unit = {
    val directory = FileUtil.file(prefix, testName)
    val gv = new GraphVizExport(ctx)
    implicit val graphVizConfig = gv.defaultGraphVizConfig
    try {
      val ss = sfs.map(_.apply()).asInstanceOf[Seq[gv.ctx.Sym]]
      gv.emitDepGraph(ss, directory, name)(graphVizConfig)
    } catch {
      case e: Exception =>
        val graphMsg = gv.emitExceptionGraph(e, directory, name) match {
          case Some(graphFile) =>
            s"See ${graphFile.file.getAbsolutePath} for exception graph."
          case None =>
            s"No exception graph produced."
        }
        Assertions.fail(s"Staging $name failed. $graphMsg", e)
    }
  }

  /** On JVM it calls Thread.sleep. */
  def threadSleepOrNoOp(millis: Long): Unit =
    Thread.sleep(millis)

  /** Seed for a test whose random inputs end up in the compiled script (`BasicOpsSpecification`):
    * `SIGMA_TEST_SEED` replays a run; while the ErgoTree snapshot is recorded
    * (`SIGMA_TREE_SNAPSHOT_DIR`) the test's `fixedSeed` keeps the recorded trees reproducible;
    * otherwise the seed is fresh and printed so a failure can be replayed.
    */
  def testSeed(fixedSeed: Long): Long = sys.env.get("SIGMA_TEST_SEED") match {
    case Some(s) => s.toLong
    case None if sys.env.contains("SIGMA_TREE_SNAPSHOT_DIR") => fixedSeed
    case None =>
      val seed = new java.security.SecureRandom().nextLong()
      println(s"Random test inputs seeded with $seed (SIGMA_TEST_SEED=$seed replays this run)")
      seed
  }

  /** Records compiler output for cross-commit diffing. When the `SIGMA_TREE_SNAPSHOT_DIR`
    * environment variable is set, appends one line
    * `sha256(code) activatedVersion ergoTreeVersion hex(bytes)` to `<dir>/<suite>.txt`;
    * does nothing otherwise. Use a fresh directory per run, then `diff -r` two runs.
    */
  def recordTreeSnapshot(suite: String, code: String, bytes: => Array[Byte]): Unit = {
    sys.env.get("SIGMA_TREE_SNAPSHOT_DIR").foreach { dir =>
      val digest = java.security.MessageDigest.getInstance("SHA-256")
      val key = digest.digest(code.getBytes("UTF-8")).map("%02x".format(_)).mkString
      val vc = sigma.VersionContext.current
      val hex = bytes.map("%02x".format(_)).mkString
      val line = s"$key ${vc.activatedVersion} ${vc.ergoTreeVersion} $hex\n"
      Platform.synchronized {
        val d = new java.io.File(dir)
        d.mkdirs()
        val w = new java.io.FileWriter(new java.io.File(d, s"$suite.txt"), true)
        try w.write(line) finally w.close()
      }
    }
  }
}
