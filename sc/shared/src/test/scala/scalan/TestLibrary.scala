package scalan

import sigma.compiler.ir.{GraphIRReflection, IRContext}

trait TestLibrary extends IRContext {
  import CollBuilder._
  import SigmaDslBuilder._
  val reflection = (GraphIRReflection)
}
