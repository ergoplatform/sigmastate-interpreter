# Ergo Intents — Venue-Side Settlement Spec (draft for ErgoScript implementation)

*Design note — reuses the single-chain atomic-exchange construction from the ErgoScript whitepaper
(`~/ergo/sigmastate-interpreter/docs/wpaper/sigma.tex` §"Example: atomic exchange on a single block
chain", lines 331–360) as the settlement layer, and the NEAR Intents off-chain solver auction as the
market layer. Distinct from `onramp-insurance.md` (which specifies a dispute-resolution vault for
crypto↔cash OTC deals) and from `~/ergo/p2pgate/specs/vault-contract.md` (which specifies the
collateral-insurance vault — cite it for the in-script Schnorr pattern, §5, and for the sigma-state 6
typing constraints, §4).*

**Status:** specification draft. No ErgoScript has been written or compiled. §10 lists the open
questions the implementing agent must resolve. Do not cite any figure or guarantee from this document
externally until the contracts compile and the test matrix in §11 is green.

---

## 1. Scope

This document specifies a **venue-side settlement primitive**: an on-chain, pre-funded,
deadline-bounded, permissionlessly-fillable swap offer, where the anti-double-fill guarantee is
enforced by consensus rather than by a contract-side ledger lookup.

It does **not** specify a pooled liquidity ledger, a taker-side intent signature scheme, or a
cross-chain leg. See §9 (non-goals) for why each is out of scope for v1 and what changes if we want them.

Terminology, used consistently below:

- **maker** — the party that posts the offer box (owns the collateral, sets the ask).
- **filler / solver** — the party that assembles the fill transaction. May be the taker acting for
  itself, or a third-party market maker acting for a taker.
- **taker** — the party that wants the asked-for asset and supplies the counter-asset.

---

## 2. Why not port NEAR Intents

NEAR Intents is two separable layers (verified against `near/intents` and the protocol gitbook,
`defuse-protocol/gitbook-docs`):

1. **Interface layer** — users sign off-chain messages (`MultiPayload`: NEP-413, ERC-191, raw Ed25519,
   BIP-322, TON Connect, WebAuthn) declaring desired balance changes. Anyone may submit them.
2. **Settlement layer** — the `intents.near` Verifier holds a **pooled internal ledger** and nets
   everyone's signed `TokenDiff` deltas in one call, via `TransferMatcher.finalize()`, which fails the
   whole transaction if any token's deltas go unmatched.

Only layer 1 is portable. Layer 2 requires a contract with mutable global state and arbitrary callers —
there is no UTXO analogue, and we do not want one: the pooled ledger is the main structural criticism of
the NEAR design (full custody in one contract).

What we keep from NEAR is the **market**: an off-chain quote auction in which solvers compete on price.
Nothing in that protocol is chain-specific.

What we take from `sigma.tex` is the settlement: a box whose spend condition is "deliver the counter-asset
to the taker, or wait out the deadline."

---

## 3. The whitepaper construction, restated

`sigma.tex` line 360 is the load-bearing sentence:

> *Once the two boxes are on the blockchain, **anyone can create such a transaction using the two boxes
> as inputs**, and thus effect the exchange between Alice and Bob. Unlike the cross-chain trading example
> above using hashing (which requires one side to go first), there are no potential problems with
> synchronization here, because the exchange will happen in a single transaction or will not happen at all.*

Read through an intents lens: Alice's box is a **posted intent**, already funded and already
deadline-bounded; the fill is **permissionless**; settlement is **atomic** because both legs are inputs
to one transaction.

The whitepaper's second load-bearing detail is line 341:

```
OUTPUTS(0).R4[Coll[Byte]].get == SELF.id
```

Line 348 gives the attack it prevents: without it, two outstanding offer boxes can both be spent
against a *single* output paying the counter-asset once — each input script passes individually, and
the maker is shortchanged. This is the **lineage tag**, and it is the UTXO answer to NEAR's salted-nonce
registry. It is strictly stronger: NEAR's nonce check is a storage *lookup* that must be got right by
the contract; this is a consensus-enforced requirement that every spent box produces exactly one
identifying output.

Divergences from the whitepaper, and why (both are deliberate):

| Whitepaper | This spec | Reason |
|---|---|---|
| `OUTPUTS(0)` / `OUTPUTS(1)` index-pinned | Filler supplies output indices via `getVar` | Index-pinning is brittle: inserting a change output shifts the indices. `vault-contract.md` §3.3 uses `getVar` path selection for the same reason. |
| `pkA` / `pkB` hardwired at compile time | Maker key in R9; taker key supplied per-fill | A public order book cannot hardwire the counterparty. |
| Fixed `deadline` constant | `deadlineHeight` in R8 | Maker must be able to set and vary it. |
| Two boxes, one taker | One maker box + arbitrary filler | Already permissionless in the paper; made explicit. |

---

## 4. Trust model

| Party | Trusted for | Not trusted for |
|---|---|---|
| Maker | Nothing. Can reclaim only after `deadlineHeight`, or sell at whatever price a filler offers. | Cannot cancel early, cannot move the collateral except to a fulfilling filler. |
| Filler | Nothing. May fill once per box, per the lineage tag. | Cannot double-fill. Cannot under-deliver — the amount floor is a spend condition. |
| Taker | Nothing. | — |
| Node / miner | Nothing (blocks are not censorship-resistant; see §9). | — |

**There is no pooled balance sheet and no custodian.** The only trust assumption is consensus: the lineage
tag and the token-conservation rule are enforced by every node. If nodes disagree, the scheme is broken
in the same way every other Ergo contract is broken — this spec inherits Ergo's security, it does not
improve on it.

For the **cross-chain** case this does not compose; see §9.

---

## 5. The OFFER box

### 5.1 Contents

- **Tokens:** exactly one secondary token kind — the maker's collateral. (Primary value in ERG is enough
  to cover box rent and the miner's fee on the eventual fill; surplus returns to the maker.)
- **Value (ERG):** 0.01 ERG default, matching `vault-contract.md` §3.1.

### 5.2 Registers

Registers R0–R3 are protocol-fixed (`value`, `propositionBytes`, `tokens`, `creationInfo`). Free registers
are R4–R9 — **six total**, and this box uses all six.

| Reg | Type | Content |
|---|---|---|
| R4 | `Coll[Byte]` | `orderId` — lineage tag. Outputs that fulfil this box must carry `R4 == SELF.id`. Also the box's external identity (quote lookup key). |
| R5 | `Coll[Byte]` | `collateralTokenId` — 32 B token id of the maker's own collateral. |
| R6 | `Coll[Byte]` | `wantedTokenId` — 32 B token id the maker wants delivered. |
| R7 | `Long` | `wantedMinAmount` — minimum acceptable amount of `wantedTokenId`. |
| R8 | `Long` | `deadlineHeight` — reclaim becomes available after `HEIGHT > deadlineHeight`. |
| R9 | `Coll[Byte]` | `makerPubKey` — 33 B compressed secp256k1 point. Stored raw and `decodePoint`ed in-script, per `vault-contract.md` §3.2 (sigma 6 cannot lift a `GroupElement` constant out of a `Coll[Byte]` register). |

### 5.3 Spending paths

Path selection discriminates on **context-variable-0 presence first**, then height — the inverse of the
obvious ordering. `vault-contract.md` §3.3 records the regression that motivates this: a height-first
discriminator there routed every post-timeout claim-open into the wrong branch, because the PAYMENT_PROVEN
output satisfied the same conservation check as a payout. Here, a fill racing the deadline must be
distinguishable from a reclaim, so:

```
if getVar[Coll[Byte]](0).isDefined  ->  PATH FILL  (fill attempt; checks its own height bound)
else                                ->  PATH RECLAIM (checks its own height bound)
```

Reclaim transactions therefore carry **no context variables at all**; fill transactions carry exactly one.
A fill submitted after `deadlineHeight` fails inside the fill branch rather than being silently rerouted
into reclaim, and a reclaim submitted early fails inside the reclaim branch. Both boundaries fail closed
and legibly.

**PATH RECLAIM — maker takes the collateral back.**
- `HEIGHT > SELF.R8[Long].get`
- `proveDlog(decodePoint(SELF.R9[Coll[Byte]].get))`
- Outputs: all of the box's secondary tokens to any maker-chosen address, in full.

**PATH FILL — anyone delivers the counter-asset.**
- `HEIGHT <= SELF.R8[Long].get` (the fill branch re-checks the bound explicitly)
- Output at index `fillOutIdx` (from `getVar[Byte](1)`) satisfies **all** of:
  - `OUTPUTS(fillOutIdx).R2` contains a pair whose `_1 == SELF.R6[wantedTokenId]` and whose `_2 >= SELF.R7[wantedMinAmount]`
  - `OUTPUTS(fillOutIdx).propositionBytes == takerKey.propBytes` — taker key from `getVar` (see §5.4)
  - `OUTPUTS(fillOutIdx).R4[Coll[Byte]].get == SELF.id` — **the lineage tag**
- No signature from the maker or the taker is required. The maker's terms are hardwired in the box; the
  taker's address is supplied by the filler.

**Token conservation (protocol rule, not a script condition).** Per `sigma.tex` line 319, for every token
id the sum across inputs must be ≥ the sum across outputs. Therefore the fill cannot mint the counter-asset,
and the maker's collateral must be carried to an output (conventionally the filler's). Note the paper's
line 319 claim "the maximum number of tokens in a box is set to 4" is **stale**: `SigmaConstants.MaxTokens`
in `~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/data/SigmaConstants.scala` is **255**
(constant id 3).

### 5.4 Context variables

| Var | Type | Path | Content |
|---|---|---|---|
| 0 | `Coll[Byte]` | FILL | Taker public key, 33 B compressed secp256k1 point. Its absence is the reclaim discriminator, so reclaim txs supply nothing. |
| 1 | `Byte` | FILL | `fillOutIdx` — index into `OUTPUTS` of the delivery box. |

The maker's collateral is not referenced by index: convention is that it goes to the filler, and the
filler is whoever built the transaction. If we need to pin it (see §10 Q4), add var 2.

---

## 6. The fill transaction

One transaction, atomic by construction. Input order is free.

```
INPUTS
  [0] maker's OFFER box            (triggered by either path)
  [1] filler's collateral box(es)  (token id == SELF.R5)

OUTPUTS
  [i]   wantedTokenId ≥ SELF.R7, propositionBytes == takerKey, R4 == SELF.id   ← the fill
  [j]   SELF.R5 collateral to the filler
  [k]   change / maker surplus
```

Neither leg can happen alone. If the filler does not supply `wantedTokenId ≥ wantedMinAmount`, the maker's
box's fill branch is unsatisfied and the whole transaction is invalid. That is the entire atomicity
argument — there is no netting engine because there is nothing to net: one maker, one filler.

**Sizing rule for the maker's collateral.** The maker should over-post slightly (collateral ≥ the ask) so
the filler's fee and the miner fee are payable in the counter-asset without a third output. If
`collateral < wantedMinAmount`, the token-conservation rule makes the fill impossible — the box can only be
reclaimed. Enforce the invariant at post time, off-chain; see §10 Q3.

---

## 7. Fees: the solver's spread is the fee

There is **no fee output and no protocol fee** on either path. This matches `vault-contract.md` §6, where
the in-contract `PROTOCOL_FEE_BPS` was removed on 2026-09-18 — every path pays the recipient in full, and
the miner fee was never part of the contract's token math.

The filler's revenue is the **spread**, and it is bounded by consensus rather than by contract arithmetic:

- the maker's ask is hardwired in the box;
- the taker's `minOut` is fixed by their off-chain intent (see §8), which the filler must satisfy;
- the filler delivers ≥ `minOut` and takes the collateral.

`makerAsk − minOut` is the filler's margin. The protocol takes none of it. **v1 asserts no protocol
revenue.** If we later want a cut, the honest mechanism is a third party (the payer) voluntarily routing a
dust amount to a treasury, not a contract-deducted fee — see §10 Q6.

---

## 8. Off-chain market layer (out of contract scope, specified for completeness)

Reused from NEAR Intents with **no chain-specific parts**:

1. Taker broadcasts a quote request (`assetIn`, `assetOut`, `exactAmountIn`/`exactAmountOut`,
   `minDeadlineMs`) to the solver relay over WebSocket JSON-RPC.
2. Solvers reply within a bounded window (NEAR uses 3000 ms) with `quoteHash`, amounts,
   `expirationTime`.
3. Taker accepts a quote. Their `minOut` is now fixed.
4. The winning filler builds and broadcasts the fill transaction (§6).

The relay, quote format, and solver registration are **NEAR Intents' protocol unchanged**. The only
adaptation: a quote here references a *maker box* rather than a pooled balance, so a quote is only
fillable while that box is unspent — fillers must re-check offer liveness (box unspent, `HEIGHT ≤
deadlineHeight`) before committing. NEAR's fill path has no such race because pooled balances are
re-read at submission.

**Taker protection is off-chain, not on-chain.** In v1 the taker signs nothing. Their protection is that
the fill transaction delivers `wantedTokenId ≥ minOut` to a key they control, and their solver only
broadcasts after constructing it. A hostile filler cannot under-deliver (the spend condition forbids it)
but can *withhold* — the taker simply does not get filled, and the offer stays live until `deadlineHeight`.
This is a market guarantee, not a cryptographic one. §9 covers the upgrade.

---

## 9. Non-goals for v1

**Not a pooled ledger.** No contract holds balances on a user's behalf, so there is no
`balance_of(signer_id, tokenId)`, no internal netting, and no "deposit once, trade repeatedly from an
internal balance" flow. Every fill references a real maker UTXO. This is intentional (§2) and it is the
main divergence from NEAR.

**Not a cross-chain bridge.** Every leg here is same-chain. The `sigma.tex` cross-chain variant (lines
283–310) is HTLC: Alice must go first in Bob's chain, and the paper devotes lines 308–310 to the
sequentiality race and the `deadlineAlice`/`deadlineBob` skew. On Ergo the cross-chain leg is Rosen
Bridge — a watcher/guard threshold multisig, i.e. an oracle, not a script. `onramp-insurance.md` §3.3
additionally records that trustless in-script verification of some chains is blocked outright because
ErgoTree has only `blake2b256` and `sha256` (opcodes 91, 92 in
`~/ergo/sigma-rust/ergotree-ir/src/serialization/op_code.rs`) — no Keccak-256. **Do not describe this spec
as a bridge.**

**No taker-side intent signature.** NEAR's `TokenDiff` is signed off-chain and verified in-contract against
a registered public key. We could do the same (secp256k1 Schnorr over a signed record, per
`vault-contract.md` §5), but the offer box would then have to pin the taker's key at post time — turning a
public order into a **bilateral** offer. That is a legitimate v2; it is not v1. See §10 Q1.

**No taker-side slippage protection on-chain.** Enforced by the taker's solver refusing to broadcast,
not by the contract.

**No batch/multi-maker fill.** `sigma.tex` settles exactly two boxes. N-way netting would require
generalizing the output-condition machinery (§10 Q2); defer.

---

## 10. Open questions for the implementing agent

Each of these must be resolved by compiling and testing, not by reading. Ordered by blast radius.

**Q1 — Does the taker key need pinning at post time, or is per-fill supply safe?**
v1 supplies the taker key per fill (`getVar[Byte]`-style var 0). Concern: a hostile filler could construct
a fill delivering to the filler's own key and broadcast it — burning the maker's offer at the maker's ask
for the filler's profit, with the maker having received nothing they wanted. The lineage tag prevents
double-fill but not this. Two candidate fixes: (a) pin the taker key in a register at post time, accepting
that the box becomes bilateral; (b) leave per-fill supply and rely on the taker's solver only ever
broadcasting fills to itself, which is a *griefing* vector (the offer is consumed) but not a theft. **(a)
is probably correct and should be treated as v1.1.** This is the single most important open question in
the document.

**Q2 — Can the fill condition be generalized to N makers?**
The two-box shape is what makes the construction obviously correct. Extending to N inputs means the fill
branch must verify N lineage-tagged outputs and N floor conditions in one script. Bounds to check against
`SigmaConstants`: `MaxChildrenCountForAtLeastOp` = 255, `MaxTupleLength` = 255, `MaxPropositionBytes` =
4096. Note there are **no loops** — the script is straight-line, so N-way support is unrolled or uses
`Coll` combinators (`map`/`forall`/`fold`, opcodes 61–64) over a register-supplied list. Confirm the sigma
6 typer accepts the chosen form.

**Q3 — What is the collateral-to-ask invariant, and can it be checked in-script?**
§6 says the maker must over-post. Can the box check it at fill time? R5 gives the token id but not the
amount in a form that reads cleanly; `SELF.tokens` is `Coll[(Coll[Byte], Long)]`. If the maker under-posts,
the fill is simply impossible and the box degenerates to a timed reclaim — annoying, not unsafe. Decide
whether to enforce at post time (wallet-side) or at fill time (script) and record the choice.

**Q4 — Should the filler's collateral output be index-pinned?**
Currently unconstrained. `sigma.tex` pins `OUTPUTS(1)` for the maker's box. Pinning it costs a context var
but makes the fill transaction's shape self-checking. Recommend pinning (var 2) if Q1 lands on (a).

**Q5 — Ordering/arithmetic on `byteArrayToBigInt`.**
`vault-contract.md` §4 records: `byteArrayToBigInt` typechecks only on a *direct expression* (bare
`getVar(...).get`, a `slice` of one, or `blake2b256(...)` thereof) — val references and arithmetic results
fail; and ordering/arithmetic on its result fails the typer, only `==` and method arguments work. The fill
branch compares `R2` token amounts as `Long`, so this may be moot — but if any token id or amount must be
recomputed from bytes, these constraints bind immediately.

**Q6 — Any protocol revenue, ever?**
§7 asserts none, consistent with `vault-contract.md` §6. If a cut is wanted later, the mechanism must be
voluntary (payer routes dust to a treasury), because a contract-deducted fee means the contract must know
the fill output index — coupling to Q4. Do not reintroduce a fee without an owner decision of the kind
recorded on 2026-09-18.

**Q7 — Reclaim to a mixer-derived address.**
`onramp-insurance.md` §2 proposes the service reclaim vaults through mixer outputs / stealth addresses so
routine reclaims do not link a maker's boxes. Should the OFFER path carry the same privacy property? Note
this is a *maker-side* flow, so linkage resistance matters less than in the on-ramp case, but the answer
should be recorded rather than assumed.

---

## 11. Constraints the implementing agent must respect

Verified against `~/ergo/sigma-rust/ergotree-ir/src/serialization/op_code.rs` and
`~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/data/SigmaConstants.scala`.

| Constant | Value | Consequence for this spec |
|---|---|---|
| `MaxPropositionBytes` (id 5) | 4096 | The compiled script must fit. Straight-line only. |
| `MaxSigmaPropSizeInBytes` (id 8) | 1024 | Bound on the assembled sigma-prop proof. |
| `MaxBoxSize` (id 1) | 4096 | R4–R9 contents must fit, including the maker's collateral token set. |
| `MaxRegisters` (id 4) | 10 | R0–R3 fixed; **6 free**. This spec uses all six. No room for a 7th field without dropping one. |
| `MaxTokens` (id 3) | 255 | Per-box secondary token kinds. (`sigma.tex` line 319's "4" is stale.) |
| `MaxChildrenCountForAtLeastOp` (id 11) | 255 | Ceiling on `atLeast` fan-in (relevant if Q2/Q4 use threshold multisig). |
| `MaxBigIntSizeInBytes` (id 7) | 32 | BigInt is 256-bit two's-complement. Affects any Schnorr `z` encoding (cf. `vault-contract.md` §5 — the signer grinds `z` to fit 254 bits). |
| `MaxHeaders` (id 10) | 10 | Only relevant if a path reads `headers`. v1 does not. |

Available and relevant: `HEIGHT` (51), `INPUTS` (52), `OUTPUTS` (53), `SELF` (55), `getVar` (115),
`blake2b256` (91), `decodePoint` (126), `proveDlog` (93), `proveDHTuple` (94), `atLeast` (40),
`SigmaAnd` (122) / `SigmaOr` (123), `sizeOf` (65), `byIndex` (66), `slice` (68).

**Not available:** Keccak-256 / SHA-3 (only `blake2b256`, 91, and `sha256`, 92). ECDSA verification. Ed25519
verification — the `gf2_192` twisted-Edwards code in sigma-rust is protocol-internal and is not exposed as
an ErgoTree primitive. Loops or unbounded iteration (hence "self-replicating scripts give
Turing-completeness *across* boxes", `sigma.tex` line 363 — each individual script is not
Turing-complete).

**Toolchain:** sigma-state 6.0.6, `.es` sources compiled and tested from Kotlin, per
`vault-contract.md` §7 (`./gradlew :contracts:test`). `VersionContext.MaxSupportedScriptVersion` is 3
(`~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/VersionContext.scala`).

---

## 12. Test matrix to implement

Baseline (per-box, one filler):

1. Fill before deadline, exact `wantedMinAmount` — passes; lineage-tagged output created.
2. Fill with `wantedTokenId` amount **below** the floor — fails.
3. Fill with the **wrong token id** — fails.
4. Fill to an output whose `R4 != SELF.id` — fails.
5. Fill to an output whose `propositionBytes` is not the supplied taker key — fails.
6. Fill with a `fillOutIdx` pointing at a nonexistent output index — fails.
7. Fill with `HEIGHT > deadlineHeight` — fails (in the fill branch, per §5.3).
8. Reclaim with `HEIGHT <= deadlineHeight` — fails.
9. Reclaim after `deadlineHeight` with `makerPubKey` — passes; all collateral to maker-chosen address.
10. Reclaim with the wrong key — fails.

The lineage-tag attack, which is the reason this construction exists (whitepaper line 348):

11. **Two OFFER boxes with the same terms, one fill, one delivery output** — fails. Without the R4
    condition both input scripts pass individually and the maker is shortchanged. This test is the
    specification; if it passes without the R4 check, the check is broken.

Robustness of the discriminator:

12. Reclaim tx supplying var 0 (misuse) — behavior recorded; must not silently route into fill.
13. Fill tx after the deadline — must fail in the fill branch, not be rerouted into reclaim (the
    `vault-contract.md` §3.3 regression).

Q1 mitigation, once chosen:

14. Fill delivering to a key other than the pinned taker (if Q1 = (a)) — fails.

## 13. Sources

- `~/ergo/sigmastate-interpreter/docs/wpaper/sigma.tex` — §"Example: atomic transactions and cross-chain
  trading" (lines 283–310), §"Example: atomic exchange on a single block chain" (331–360), lineage-tag
  attack analysis (line 348), token-conservation rule (line 319), register layout (316–329),
  self-replication/Turing-completeness-across-boxes (line 363).
- `~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/data/SigmaConstants.scala` — all size
  limits in §11.
- `~/ergo/sigma-rust/ergotree-ir/src/serialization/op_code.rs` — opcode inventory; the
  blake2b256/sha256-only hash set; absence of ECDSA/Ed25519/loop primitives.
- `~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/VersionContext.scala` — script version 3.
- `near/intents` — `contracts/defuse/core/src/engine/mod.rs` (`execute_signed_intent` flow),
  `.../intents/token_diff.rs` (`TokenDiff`, fee-on-negative-delta, `closure_deltas`),
  `.../engine/state/deltas.rs` (`TransferMatcher::finalize`, unmatched-delta invariant).
- `defuse-protocol/gitbook-docs` — `market-makers/verifier/introduction.md`,
  `.../intent-types-and-execution.md`, `.../bus/solver-relay.md`, `fees.md`.
- `ergoforum.org/t/3407` — verifying Schnorr signatures in ErgoScript (used if Q1 → (a) needs a pinned
  key, or v2 taker signatures).
- `~/ergo/chaincash/contracts/onchain/note.es`, `reserve.es` — working reference for
  lineage/history-tagging and multi-path contracts on Ergo.
- `onramp-insurance.md` §2 (mixer-derived reclaims), §3.3 (Keccak absence), §5 (limitations).
- `~/ergo/p2pgate/specs/vault-contract.md` §3.3 (path-selection discipline), §4 (sigma-state 6 typing
  constraints), §5 (in-script Schnorr), §6 (fee removal).
