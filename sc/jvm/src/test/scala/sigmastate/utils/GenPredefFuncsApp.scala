package sigmastate.utils

import sigma.util.FileUtil
import sigma.util.PrintExtensions.IterableExtensions

object GenPredefFuncsApp extends SpecGen {

  private[utils] def shouldAbbreviateDescription(description: String): Boolean = {
    val normalized = description.replace("\r\n", "\n")
    normalized.length > 150
  }

  /** Generates the contents of `predeffunc_rows.tex` (the summary table rows) and
    * `predeffunc_sections.tex` (the per-function subsections), as a single pair so the
    * two files stay derived from the same ordered list of predefined functions. */
  def generatePredefFuncTables(): (String, String) = {
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
      val desc = if (shouldAbbreviateDescription(info.description)) "..." else info.description
      funcRows.append(
        s""" $opCode & $serRef & \\parbox{4cm}{\\lst{$opName:} \\\\ \\lst{($argsTpe)} \\\\ \\lst{  => $resTpe}} & $desc \\\\
          | \\hline
         """.stripMargin)

      val subsection = funcSubsection(f)
      sections.append(subsection)
    }
    (funcRows.result(), sections.result())
  }

  def main(args: Array[String]) = {
    val rowsFile = FileUtil.file("docs/spec/generated/predeffunc_rows.tex")
    val sectionsFile = FileUtil.file(s"docs/spec/generated/predeffunc_sections.tex")

    val (funcRows, sections) = generatePredefFuncTables()
    FileUtil.write(rowsFile, funcRows)
    FileUtil.write(sectionsFile, sections)
  }
}
