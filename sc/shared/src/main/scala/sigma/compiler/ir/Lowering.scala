package sigma.compiler.ir

import sigma.ast.syntax.SValue

/** The reverse lowering table of the compiler: for a call node whose callee has a dedicated
  * ErgoTree node, the row rebuilds that node from the already built receiver and arguments.
  * Rows are keyed by callee identity ([[MethodCallee]] compares `(objType, methodId)`,
  * [[OpCallee]] is structural), never by name. Callees without a row are emitted as plain
  * `MethodCall` ErgoTree nodes by [[TreeBuilding]].
  */
trait Lowering { IR: IRContext =>

  /** Rebuilds an ErgoTree node from the IR call node, its built receiver and built arguments. */
  type Row = (MethodCall, SValue, Seq[SValue]) => SValue

  /** All callees that have a dedicated ErgoTree node. Populated entity by entity. */
  protected lazy val rows: Map[IRCallee, Row] = Map.empty

  /** The row for `callee`, if it has a dedicated ErgoTree node. */
  final def rowFor(callee: IRCallee): Option[Row] = rows.get(callee)
}
