package org.ergoplatform

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import sigma.ast.SType
import sigma.ast.syntax.SigmaPropValue
import sigma.ast.Value
import sigma.ast.syntax.ValueOps
import sigma.compiler.SigmaCompiler
import sigma.compiler.ir.IRContext

object ErgoScriptPredef {
  import sigmastate.interpreter.Interpreter._

  /** Compiles the given ErgoScript `code` into ErgoTree expression. */
  def compileWithCosting(env: ScriptEnv, code: String, networkPrefix: NetworkPrefix)(implicit IR: IRContext): Value[SType] = {
    val compiler = SigmaCompiler(networkPrefix)
    val res = compiler.compile(env, code)
    res.buildTree
  }

  /**
    * Proposition of the box that may be spent by a transaction
    * which inputs contains at least `thresholdAmount` of token with id `tokenId`.
    * The logic of this script is following
    * (v1) INPUTS.flatMap(box => box.tokens.filter(t => t._1 == tokenId).map(t => t._2)).sum >= thresholdAmount
    * (v2) INPUTS.flatMap(box => box.tokens).filter(t => t._1 == tokenId).sum >= thresholdAmount
    * (v3) INPUTS.map(box => box.tokens.find(t => t._1 == tokenId).map(t => t._2).getOrElse(0)).sum >= thresholdAmount
    */
  def tokenThresholdScript(
      tokenId: Array[Byte],
      thresholdAmount: Long,
      networkPrefix: NetworkPrefix)
      (implicit IR: IRContext): SigmaPropValue = {
    val env = emptyEnv +
        ("tokenId" -> tokenId, "thresholdAmount" -> thresholdAmount)
    val res = compileWithCosting(env,
      """{
       |  val sumValues = { (xs: Coll[Long]) => xs.fold(0L, { (acc: Long, amt: Long) => acc + amt }) }
       |
       |  val tokenAmounts = INPUTS.map({ (box: Box) =>
       |    sumValues(box.tokens.map { (tokenPair: (Coll[Byte], Long)) =>
       |      val ourTokenAmount = if (tokenPair._1 == tokenId) tokenPair._2 else 0L
       |      ourTokenAmount
       |    })
       |  })
       |  val total = sumValues(tokenAmounts)
       |  sigmaProp(total >= thresholdAmount)
       |}
      """.stripMargin, networkPrefix)
    res.asSigmaProp
  }
}
