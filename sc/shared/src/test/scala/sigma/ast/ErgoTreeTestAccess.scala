package sigma.ast

/** Access the constructor-scoped Scala 3 copy method without changing production visibility. */
object ErgoTreeTestAccess {
  def withConstants(tree: ErgoTree, constants: IndexedSeq[Constant[SType]]): ErgoTree =
    tree.copy(constants = constants)
}
