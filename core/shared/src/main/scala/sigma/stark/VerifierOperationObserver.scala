/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Copyright 2026 A. Shannon.
 */
package sigma.stark

/** Allocation-free operation hook for verifier diagnostics.
  *
  * Operation identities are fixed integers and carry no proof-controlled
  * payload. Production verification passes `null`; diagnostic callers may
  * supply an observer whose exceptions intentionally propagate.
  */
private[sigma] trait VerifierOperationObserver {
  def onOperation(operationId: Int): Unit
}

private[sigma] object VerifierOperationObserver {
  final val MerkleTopPairHash: Int = 1
  final val MerkleQueryPairHash: Int = 2
  final val ContentHashCall: Int = 3
  final val ContentHashPermutation: Int = 4
  final val RngCommit: Int = 5
  final val RngElementDraw: Int = 6
  final val RngPermutation: Int = 7
}
