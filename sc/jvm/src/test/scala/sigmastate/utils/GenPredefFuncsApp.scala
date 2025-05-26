package sigmastate.utils

import sigma.util.FileUtil
import sigma.util.PrintExtensions.IterableExtensions

object GenPredefFuncsApp extends SpecGen {

  def main(args: Array[String]) = {
    val rowsFile = FileUtil.file("docs/spec/generated/predeffunc_rows.tex")
    val sectionsFile = FileUtil.file(s"docs/spec/generated/predeffunc_sections.tex")

    val opsTable = collectOpsTable()
    val opInfos = opsTable.collect { case (d, m, optF @ Some(f)) =>
      val info = getOpInfo(d, m, optF)
      (d, f, info)
    }.sortBy(i => toDisplayCode(i._1.opCode))

    val funcRows = StringBuilder.newBuilder
    val sections = StringBuilder.newBuilder

    for ((d, f, info) <- opInfos) {
      val opCode = toDisplayCode(d.opCode)
      val mnemonic = d.typeName
      val opName = toTexName(f.name)
      val argsTpe = f.declaration.tpe.tDom.rep(_.toTermString)
      val resTpe = f.declaration.tpe.tRange.toTermString
      val serRef = s"\\hyperref[sec:serialization:operation:$mnemonic]{\\lst{$mnemonic}}"
      val desc = if (info.description.length > 150) "..." else info.description
      funcRows.append(
        s""" $opCode & $serRef & \\parbox{4cm}{\\lst{$opName:} \\\\ \\lst{($argsTpe)} \\\\ \\lst{  => $resTpe}} & $desc \\\\
          | \\hline
         """.stripMargin)

      val subsection = funcSubsection(f)
      sections.append(subsection)
    }
    println(s"Total ops: ${opInfos.length}")
    FileUtil.write(rowsFile, funcRows.result())
    FileUtil.write(sectionsFile, sections.result())
  }
}
