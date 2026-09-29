package scalan

import sigma.compiler.ir.IRContext

import scala.annotation.unused

object Platform {
  /** In JS tests do nothing. The corresponding JVM method outputs graphs into files. */
  def stage[Ctx <: IRContext](ctx: Ctx)(
      @unused prefix: String,
      @unused testName: String,
      @unused name: String,
      @unused sfs: Seq[() => ctx.Sym]): Unit = {
  }

  /** On JS it is no-operation. */
  def threadSleepOrNoOp(@unused millis: Long): Unit = {
  }

  /** On JS it is no-operation. The JVM version appends compiler output to a snapshot file. */
  def recordTreeSnapshot(@unused suite: String, @unused code: String, @unused bytes: => Array[Byte]): Unit = {
  }

  /** On JS there is no environment to read: the seed is always fresh and printed. */
  def testSeed(@unused fixedSeed: Long): Long = {
    val seed = scala.util.Random.nextLong()
    println(s"Random test inputs seeded with $seed")
    seed
  }
}
