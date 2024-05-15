package special.wrappers

import sigma.compiler.Scalan
import wrappers.scala.WOptionsModule
import wrappers.scalan.WRTypesModule
import wrappers.special.WSpecialPredefsModule

trait WrappersModule
  extends WSpecialPredefsModule
  with WOptionsModule
  with WRTypesModule { self: Scalan => }