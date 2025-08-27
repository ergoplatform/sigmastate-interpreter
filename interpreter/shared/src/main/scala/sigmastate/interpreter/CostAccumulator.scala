package sigmastate.interpreter

import sigma.ast.JitCost
import sigma.exceptions.CostLimitException

/** Encapsulate simple monotonic (add only) counter with reset. */
class CostCounter(val initialCost: JitCost) {
  private var _currentCost: JitCost = initialCost

  @inline def += (n: JitCost) = {
    this._currentCost = this._currentCost + n
  }
  @inline def currentCost: JitCost = _currentCost
  @inline def resetCost() = { _currentCost = initialCost }
}

/** Implements finite state machine with stack of graph blocks (scopes),
  * which correspond to lambdas and thunks.
  * It accepts messages: startScope(), endScope(), add(), reset()
  * At any time `totalCost` is the currently accumulated cost. */
class CostAccumulator(initialCost: JitCost, costLimit: Option[JitCost]) {

  @inline private def initialStack() = List(new Scope(initialCost))
  private var _scopeStack: List[Scope] = initialStack

  @inline def currentScope: Scope = _scopeStack.head

  /** Represents a single scope during execution of the graph.
    * The lifetime of each instance is bound to scope execution.
    * When the evaluation enters a new scope (e.g. calling a lambda) a new Scope instance is created and pushed
    * to _scopeStack, then is starts receiving `add` method calls.
    * When the evaluation leaves the scope, the top is popped off the stack. */
  class Scope(initialCost: JitCost) extends CostCounter(initialCost) {


    @inline def add(opCost: JitCost): Unit = {
          this += opCost
    }

    /** Called by nested Scopes to communicate accumulated cost back to parent scope.
      * When current scope terminates, it communicates accumulated cost up to its parent scope.
      * This value is used at the root scope to obtain total accumulated scope.
      */
    private var _resultRegister: Int = 0
    @inline def childScopeResult: Int = _resultRegister
    @inline def childScopeResult_=(resultCost: Int): Unit = {
      _resultRegister = resultCost
    }

  }

  /** Called once for each operation of a scope (lambda or thunk).
    * @throws CostLimitException when current accumulated cost exceeds `costLimit`
    */
  def add(opCost: JitCost): Unit = {
    currentScope.add(opCost)

    // check that we are still withing the limit
    if (costLimit.isDefined) {
      val limit = costLimit.get
      // the cost we accumulated so far
      val accumulatedCost = currentScope.currentCost
      if (accumulatedCost > limit) {
        throw new CostLimitException(
          accumulatedCost.value,
          CostLimitException.msgCostLimitError(accumulatedCost, limit)
        )
      }
    }
  }

  /** Resets this accumulator into initial state to be ready for new graph execution. */
  @inline def reset() = {
    _scopeStack = initialStack()
  }

  /** Returns total accumulated cost */
  @inline def totalCost: JitCost = currentScope.currentCost
}
