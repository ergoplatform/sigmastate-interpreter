package sigma.compiler.ir

import sigma.compiler.ir.meta.ModuleInfo

/** Defines registration of IR modules.
  * @see ModuleInfo
  */
trait Modules extends Base { self: IRContext =>

  /** Whether staged modules should be registered when cake is constructed and initialized. */
  def okRegisterModules: Boolean = false

  /** Called once for each staged module during this cake initialization. */
  protected def registerModule(moduleInfo: ModuleInfo) = {
    if (okRegisterModules) {
      !!!(s"Cannot register module $moduleInfo: registerModule method is not overridden in IR cake $this. ")
    }
  }
}
