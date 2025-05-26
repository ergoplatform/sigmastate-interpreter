package org.ergoplatform

import sigmastate.crypto.SigmaProtocolPrivateInput
import sigmastate.interpreter.ProverInterpreter

/** Prover which can reduce ErgoTrees and prove sigma propositions using provided secrets.
  *
  * @param secrets All secrets available for this prover.
  */
class SigmaPropProver(override val secrets: Seq[SigmaProtocolPrivateInput[_]])
    extends ErgoLikeInterpreter
        with ProverInterpreter {
  override type CTX = ErgoLikeContext
}
