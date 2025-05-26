package sigma.compiler.ir.meta

/** Information about generated Special library module.
  * Instances are created in generated code.
  *
  * @see *Impl.scala files
  */
case class ModuleInfo(
    packageName: String,
    moduleName: String,
    extension: String = ".scalan") {
  val name = SSymName(packageName, moduleName)

  def getKey = name.mkFullName

  def sourceFileName = packageName.replace('.', '/') + s"/$moduleName$extension"
}
