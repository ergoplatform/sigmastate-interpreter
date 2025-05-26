package sigmastate.utils

import sigma.util.Extensions.ByteOps
import sigma.util.FileUtil
import sigma.util.PrintExtensions._
import sigma.ast.{MethodCall, PropertyCall}
import sigma.serialization.ValueSerializer._

/** Generate contents of ErgoTree serializer format specification.
  * To generate serialization formats, it is necessary that all branches of serializers
  * are executed at least once. Those executions are then traced and the structure
  * of the serialized formats is recorded in `ValueSerializer.serializerInfo` map,
  * which lives in memory and is not stored anywhere.
  * NOTE: you need to set `ValueSerializer.collectSerInfo = true`,
  *       don't forget to to set it back to `false` before release.
  */
object GenSerializers extends SpecGen {

  def printDataScope(dataScope: DataScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val name = dataScope.name
    val fmt = dataScope.data.format
    val size = fmt.size
    val desc = dataScope.data.info.description
    val row =
      s"""    $prefix $$ $name $$ & \\lst{$fmt} & $size & $desc \\\\
         |    \\hline
      """.stripMargin
    sb.append(row)
    openRow = false
  }

  def printForScope(scope: ForScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val header =
      s"""    \\multicolumn{4}{l}{${prefix}\\lst{for}~$$i=1$$~\\lst{to}~$$${scope.limitVar}$$} \\\\
        |    \\hline
         """.stripMargin
    sb.append(header)

    for ((_, s) <- scope.children) {
      printScope(s, level + 1, sb)
    }

    val footer = s"    \\multicolumn{4}{l}{${prefix}\\lst{end for}} \\\\"
    sb.append(footer)
    openRow = true
  }

  def printOptionScope(scope: OptionScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val header =
      s"""    \\multicolumn{4}{l}{${prefix}\\lst{optional}~$$${scope.name}$$} \\\\
         |    \\hline
         |    ${prefix}~~$$tag$$ & \\lst{Byte} & 1 & 0 - no value; 1 - has value \\\\
         |    \\hline
         |    \\multicolumn{4}{l}{${prefix}~~\\lst{when}~$$tag == 1$$} \\\\
         |    \\hline
         """.stripMargin
    sb.append(header)

    for ((_, s) <- scope.children) {
      printScope(s, level + 2, sb)
    }

    val footer = s"    \\multicolumn{4}{l}{${prefix}\\lst{end optional}} \\\\"
    sb.append(footer)
    openRow = true
  }

  def printCasesScope(scope: CasesScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val header =
      s"""    \\multicolumn{4}{l}{${prefix}\\lst{match}~$$ ${scope.matchExpr} $$} \\\\
         """.stripMargin
    sb.append(header)

    for (when <- scope.cases) {
      val pattern =
        s"""
         |    \\multicolumn{4}{l}{${prefix}~~${ if(when.isOtherwise) s"\\lst{$otherwiseCondition}" else s"\\lst{with}~$$${when.condition}$$" } } \\\\
         |    \\hline
        """.stripMargin
      sb.append(pattern)
      for((_,s) <- when.children) {
        printScope(s, level + 2, sb)
      }
    }

    val footer = s"    \\multicolumn{4}{l}{${prefix}\\lst{end match}} \\\\"
    sb.append(footer)
    openRow = true
  }

  def printScope(scope: Scope, level: Int, sb: StringBuilder): Unit = {
    if (openRow) {
      sb.append(s"\\hline\n")
      openRow = false // close the table row with a horizontal line
    }
    scope match {
      case scope: DataScope =>
        printDataScope(scope, level, sb)
      case scope: ForScope =>
        printForScope(scope, level, sb)
      case scope: OptionScope =>
        printOptionScope(scope, level, sb)
      case scope: CasesScope =>
        printCasesScope(scope, level, sb)
      case _ =>
        sb.append(s"% skipped $scope\n")
    }
  }

  var openRow: Boolean = false

  def printSerScopeSlots(serScope: SerScope) = {
    val rows = StringBuilder.newBuilder
    openRow = false
    serScope.children.map { case (_, scope) =>
       printScope(scope, 0, rows)
    }
    rows.result()
  }

  def printSerializerSections() = {
    val opsTable = collectOpsTable()
    val opInfos = opsTable.map { case (d, m, f) => getOpInfo(d, m, f) }
    val scopes = serializerInfo
      .filter(_._2.children.nonEmpty).toSeq
      .sortBy(_._1).map(_._2)
    scopes.map { s =>
      val ser = getSerializer(s.opCode)
      val opCode = ser.opCode.toUByte
      val opName = ser.opDesc.typeName
      val rows = printSerScopeSlots(s)
      val opRow = opsTable.find(r => r._1.opCode == s.opCode)
      val opInfo = opInfos.find(i => i.opDesc.opCode == s.opCode)
      val desc = opInfo.map(_.description)
      val opRef = opRow
        .filterNot { case (d,_,_) => d == PropertyCall || d == MethodCall }
        .opt { case (_, m, f) =>
          m.fold(f.opt { f =>
            val refName = f.docInfo.opTypeName
            val opName = f.name.replace("%", "\\%")
            s"See~\\hyperref[sec:appendix:primops:$refName]{\\lst{${opName}}}"
          })({ m =>
            val typeName = m.objType.typeName
            s"See~\\hyperref[sec:type:$typeName:${m.name}]{\\lst{$typeName.${m.name}}}"
          })
        }

      s"""
        |\\subsubsection{\\lst{$opName} operation (OpCode $opCode)}
        |\\label{sec:serialization:operation:$opName}
        |
        |${desc.opt(_.toString)} $opRef
        |
        |\\noindent
        |\\(\\begin{tabularx}{\\textwidth}{| l | l | l | X |}
        |    \\hline
        |    \\bf{Slot} & \\bf{Format} & \\bf{\\#bytes} & \\bf{Description} \\\\
        |    \\hline
        |    $rows
        |\\end{tabularx}\\)
       """.stripMargin
    }.mkString("\n")
  }

  def generateSerSpec() = {
    val fileName = "ergotree_serialization1.tex"
    val formatsTex = printSerializerSections()
    val file = FileUtil.file(s"docs/spec/generated/$fileName")
    FileUtil.write(file, formatsTex)

    println(s"\\input{generated/$fileName}")
  }

}
