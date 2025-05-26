package sigmastate.utils

import sigma.ast.SMethod
import sigma.util.PrintExtensions._
import scala.util.Try
import Helpers._ // required for Scala 2.11

/** Generate as a console output all InfoObject objects.
  * Those provide stable identifiers to access metadata information.
  * This should be regenerated each time metadata is changed.
  * Example:
  * object AppendInfo extends InfoObject {
  *   private val method = SMethod.fromIds(12, 9)
  *   val thisArg = method.argInfo("this")
  *   val otherArg = method.argInfo("other")
  *   ...
  * }
  * The following consistency checks are performed:
  * 1) every func/method argument has attached ArgInfo
  * 2) method is resolvable by ids (e.g. as SMethod.fromIds(12, 9))
  * 3) func is resolvable by name (e.g. predefinedOps.funcs("sigmaProp"))
  */
object GenInfoObjects extends SpecGen {

  def main(args: Array[String]) = {
    val table = collectOpsTable()
    val infos = table.map { case (d, m, f) => getOpInfo(d, m, f) }

    val infoStrings = infos.sortBy(_.opDesc.typeName).map { info =>
      val opName = info.opDesc.typeName

      val res = info.op match {
        case Right(m) =>
          val typeId = m.objType.typeId
          assert(m.stype.tDom.length == info.args.length,
            s"Method $m has ${m.stype.tDom} arguments, but ${info.args} descriptions attached.")
          Try{SMethod.fromIds(typeId, m.methodId)}
            .fold(t => throw new RuntimeException(s"Cannot resolve method $m using SMethod.fromIds(${typeId}, ${m.methodId})"), _ => ())
          val args = info.args.map { a =>
            s"""val ${a.name}Arg: ArgInfo = method.argInfo("${a.name}")"""
          }
          s"""
            |  object ${opName}Info extends InfoObject {
            |    private val method = SMethod.fromIds(${typeId}, ${m.methodId})
            |    ${args.rep(sep = "\n    ")}
            |    val argInfos: Seq[ArgInfo] = Array(${info.args.rep(a => s"${a.name}Arg")})
            |  }
           """.stripMargin
        case Left(f) =>
          assert(f.declaration.args.length == info.args.length,
            s"Predefined function $f has ${f.declaration.args} arguments, but ${info.args} descriptions attached.")
          val isSpecialFunc = predefFuncRegistry.specialFuncs.get(f.name).isDefined
          Try{assert(predefFuncRegistry.funcs.get(f.name).isDefined || isSpecialFunc)}
              .fold(t => throw new RuntimeException(s"Cannot resolve func $f using predefFuncRegistry.funcs(${f.name})", t), _ => ())
          val args = info.args.map { a =>
            s"""val ${a.name}Arg: ArgInfo = func.argInfo("${a.name}")"""
          }
          s"""
            |  object ${opName}Info extends InfoObject {
            |    private val func = predefinedOps.${ if (isSpecialFunc) "specialFuncs" else "funcs" }("${f.name}")
            |    ${args.rep(sep = "\n    ")}
            |    val argInfos: Seq[ArgInfo] = Array(${info.args.rep(a => s"${a.name}Arg")})
            |  }
           """.stripMargin
      }
      res
    }
    val infoText = infoStrings.rep(sep = "")
    println(infoText)
    println(s"Total infos: ${infoStrings.length}")
  }

}
