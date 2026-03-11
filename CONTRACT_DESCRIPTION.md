# Sigma OR Range Proof — ErgoScript Contract Description

## Overview

Verifies that a Pedersen Commitment `C = v·G + r·H` commits to a value `v ∈ [0, 2ⁿ−1]` using **bit-decomposition with native Sigma OR proofs** — each bit is proven individually via `proveDHTuple`.

Four contract variants exist for n = 8, 16, 32, 64 bits.

---

## Hardcoded Constants (Embedded in ErgoTree)

| Constant | Value | Purpose |
|---|---|---|
| `H` | `022975f1d28b92b6e84499b83b0797ef5235553eeb7edaa0cea243c1128c2fe739` | NUMS second generator, derived via `hash_to_curve(G)`. Nobody knows `h` where `H = h·G`. |
| `G_inv` | `0379BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798` | Additive inverse of secp256k1 generator G (negated y-coordinate). |
| `groupGenerator` | (ErgoScript built-in) | Standard secp256k1 generator G. |
| Bit width `n` | Compile-time | Each bit requires an explicit `proveDHTuple` block (no loops in ErgoScript). |
| Powers of 2 | `1, 2, 4, 8, ..., 2^(n-1)` as `BigInt` literals | Weights for homomorphic sum. |

---

## Box Registers (Prover-Supplied Data)

| Register | Type | Contents | Description |
|---|---|---|---|
| **R4** | `GroupElement` | `C` | The Pedersen Commitment: `C = v·G + r·H` |
| **R5** | `Coll[GroupElement]` | `bits[0..n-1]` | Bit commitments: `Cᵢ = bᵢ·G + rᵢ·H` where `bᵢ ∈ {0,1}` |
| **R6** | `Coll[GroupElement]` | `aux[0..n-1]` | Auxiliary public keys: `auxᵢ = rᵢ·G` |

---

## Context Extension

**Not used.** All data comes through registers R4–R6. The Sigma protocol proof bytes are carried in the standard `spendingProof.proofBytes` of the transaction input.

---

## Verification Logic

### Check 1: Homomorphic Sum (Boolean Guard → `sigmaProp`)

Verifies bit commitments correctly decompose `C`:

```
C == C₀ · C₁^2 · C₂^4 · ... · Cₙ₋₁^(2^(n-1))
```

Works because Pedersen Commitments are additively homomorphic:
`Σ 2ⁱ · Cᵢ = (Σ 2ⁱ · bᵢ)·G + (Σ 2ⁱ · rᵢ)·H = v·G + r·H = C`

Prover must choose `rᵢ` such that `Σ 2ⁱ · rᵢ = r`.

### Check 2: Per-Bit Sigma OR (Native Sigma Propositions)

For each bit `i`, a `proveDHTuple`-based OR proves `bᵢ ∈ {0, 1}`:

```scala
atLeast(1, Coll(
    proveDHTuple(G, H, aux(i), bits(i)),                  // bᵢ = 0: Cᵢ = rᵢ·H
    proveDHTuple(G, H, aux(i), bits(i).multiply(G_inv))   // bᵢ = 1: Cᵢ−G = rᵢ·H
))
```

- **Case bᵢ = 0:** `Cᵢ = 0·G + rᵢ·H = rᵢ·H`. Prover proves DH tuple `(G, H, rᵢ·G, rᵢ·H)` with secret `rᵢ`.
- **Case bᵢ = 1:** `Cᵢ = 1·G + rᵢ·H`. Then `Cᵢ · G⁻¹ = rᵢ·H`. Prover proves DH tuple with secret `rᵢ`.

Sigma OR ensures the prover satisfies exactly one branch without revealing which.

### Final Proposition

```scala
sigmaProp(sumOk) && b0 && b1 && b2 && ... && b(n-1)
```

---

## Off-Chain Prover (Scala Test)

The `BasicOpsSpecification.scala` test generates all proof data:

1. **Decompose** `v` into bits `b₀, b₁, ..., bₙ₋₁`
2. **For each bit:** pick random `rᵢ ∈ Zq`, compute `Cᵢ = bᵢ·G + rᵢ·H` and `auxᵢ = rᵢ·G`
3. **Adjust** last `rₙ₋₁` so that `Σ 2ⁱ·rᵢ = r` (matching the original blinding factor)
4. **Pack** `C` → R4, `[C₀..Cₙ₋₁]` → R5, `[aux₀..auxₙ₋₁]` → R6
5. **Sign** using `DHTupleProverInput(rᵢ, ProveDHTuple(G, H, auxᵢ, ...))` for each bit

---

## Complexity Per Variant

| Variant | Bits | `proveDHTuple` propositions | `GroupElement.exp` calls | Registers |
|---|---|---|---|---|
| 8-bit | 8 | 16 (2 per bit) | 7 | R4: 1, R5: 8, R6: 8 |
| 16-bit | 16 | 32 | 15 | R4: 1, R5: 16, R6: 16 |
| 32-bit | 32 | 64 | 31 | R4: 1, R5: 32, R6: 32 |
| 64-bit | 64 | 128 | 63 | R4: 1, R5: 64, R6: 64 |
