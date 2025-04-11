package sigma.compiler

import fastparse.Parsed
import org.ergoplatform.sdk.{ContractTemplate, Parameter}
import sigma.ast.SourceContext
import sigma.ast.syntax.SValue
import sigma.compiler.ir.CompiletimeIRContext
import sigmastate.lang.{ContractParser, ParsedContractTemplate}
import sigmastate.lang.parsers.ParserException

/** Compiler which compiles Ergo contract templates into a [[ContractTemplate]]. */
class SigmaTemplateCompiler(networkPrefix: Byte) {
  val sigmaCompiler = new SigmaCompiler(networkPrefix)

  /**
   * Compiles the provided contract source code into a [[ContractTemplate]].
   *
   * @param source The ErgoScript contract source code.
   * @return The contract template.
   */
  def compile(source: String): ContractTemplate = {
    ContractParser.parse(source) match {
      case Parsed.Success(parsedTemplate, _) =>
        implicit val ir = new CompiletimeIRContext
        val parEnv = parsedTemplate.signature.params.map { p => p.name -> p.tpe }.toMap
        val result = sigmaCompiler.compileParsed(parEnv, parsedTemplate.body)
        assemble(parsedTemplate, result.buildTree)

      case f: Parsed.Failure =>
        throw new ParserException(s"Contract template syntax error: $f", Some(SourceContext.fromParserFailure(f)))
    }
  }

  private def assemble(parsed: ParsedContractTemplate, expr: SValue): ContractTemplate = {
    val (constTypes, constValueOpts) = parsed.signature.params.map(param => (param.tpe, param.defaultValue)).unzip
    val allConstValuesAreNone = constValueOpts.forall(_.isEmpty)
    val constValues =
      if (allConstValuesAreNone) None
      else Some(constValueOpts.toIndexedSeq)
    val contractParams = parsed.signature.params.zipWithIndex.map { case (param, idx) =>
      val desc: String = parsed.docs.params.find(docParam => docParam.name == param.name).map(_.description).getOrElse("")
      Parameter(param.name, desc, idx)
    }

    ContractTemplate(
      name = parsed.signature.name,
      description = parsed.docs.description,
      constTypes = constTypes.toIndexedSeq,
      constValues = constValues,
      parameters = contractParams.toIndexedSeq,
      expressionTree = expr.toSigmaProp
    )
  }
}

object SigmaTemplateCompiler {
  def apply(networkPrefix: Byte): SigmaTemplateCompiler = new SigmaTemplateCompiler(networkPrefix)
}
