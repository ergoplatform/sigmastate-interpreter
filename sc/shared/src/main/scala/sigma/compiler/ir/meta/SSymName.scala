package sigma.compiler.ir.meta

import sigma.util.StringUtil.StringUtilExtensions

case class SSymName(packageName: String, name: String) {
  import SSymName._
  def mkFullName = fullNameString(packageName, name)
}

object SSymName {
  def fullNameString(packageName: String, name: String): String =
    if (packageName.isNullOrEmpty) name else s"$packageName.$name"
}