package sigmastate.utils

import sigma.util.FileUtil

object GenPredefTypesApp extends SpecGen {
  import SpecGenUtils._

  def main(args: Array[String]) = {
    val table = printTypes(companions)
    val fPrimOps = FileUtil.file("docs/spec/generated/predeftypes.tex")
    FileUtil.write(fPrimOps, table)

    for (tc <- typesWithMethods) {
      val typeName = tc.typeName
      val methodsRows = printMethods(tc)
      val fMethods = FileUtil.file(s"docs/spec/generated/${typeName}_methods.tex")
      FileUtil.write(fMethods, methodsRows)

      println(s"\\input{generated/${typeName}_methods.tex}")
    }
  }
}
