package scalan

import sigma.compiler.ir.{GraphIRReflection, IRContext}

trait TestLibrary extends IRContext {
  val reflection = GraphIRReflection
}
