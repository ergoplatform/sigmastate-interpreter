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
}
