# Ergo Intents — Cross-Chain Settlement Layer (draft for ErgoScript implementation)

*Companion to `ergo-intents.md`, not a replacement. That document specifies the **same-chain** venue-side
OFFER box and is authoritative for it; nothing here changes it. This document adds the cross-chain leg,
the market-maker-facing contract, and the oracle service that both depend on.*

*Design origin — kushti's early draft, "Ergo Intents": UTXO is already intent-friendly (the ErgoScript
whitepaper's atomic exchange is an intent at the ErgoTree level); the open questions are cross-chain,
market-maker participation, and UX. The draft's central move — "for cross-chain events, we can use
contracts from oracle pools, but make the oracle operator group chosen from pools of RSN holders" — is
adopted in §3 and sharpened in §3.4, where it turns out to be partly off-chain.*

**Status:** specification draft. No ErgoScript written or compiled for the MM contract or the attestation
contract. §11 lists the open questions. §12 is the test matrix. Do not cite any figure or guarantee from
this document externally until the contracts compile and the matrix is green.

---

## 1. Scope

Three settlement cases, distinguished only by whether the counter-leg lives on Ergo:

| Case | Leg A | Leg B | v1? |
|---|---|---|---|
| **A — same-chain** | on Ergo | on Ergo | **specified in `ergo-intents.md`**, not repeated |
| **B — one Ergo leg** | on Ergo | off Ergo (e.g. rsBTC for BNB on BSC) | **this document, §5** |
| **C — no Ergo leg** | off Ergo | off Ergo (e.g. BNB on BSC for ETH on Ethereum) | **analysed and deferred, §6** |

What this document specifies: the **MM collateral contract** for case B (§5), the **oracle attestation
contract** it depends on (§3.2), the **note format** (§5.2), the trust model (§7), and the constraints
(§10). The off-chain market layer is specified only as far as §8 needs it for the auction to make sense.

**Case A is deliberately not restated.** `ergo-intents.md` already fixes the same-chain box, its register
layout, its context-var-first path discriminator and its 14-case test matrix. Case B shares case A's
lineage tag and its no-custodian property; everything else differs.

Terminology, following the draft:

- **maker** — the party posting the collateral box. In case B this is a market maker (MM). The draft's
  convention is adopted: a *user's* order is called a "maker order" even when the user is not an MM,
  because from the chain's perspective it is posted and committed, not filled. Where this document means
  the market maker specifically it says **MM**.
- **filler** — the party assembling the fill transaction. Permissionless; need not be the taker.
- **taker / recipient** — the party receiving the Ergo leg, named in the note.
- **attestor** — an oracle that has staked and posts an event attestation. Distinct from the guards in
  `rosen-bridge` (§3.4).

---

## 2. Why a separate document

`ergo-intents.md` §9 commits, in writing, to these non-goals: **not a cross-chain bridge**, **no pooled
ledger**, **no taker-side intent signature**. This document keeps the no-pooled-ledger and
no-custodian properties — they are the reason the construction is worth doing — but it necessarily gives
up the "same-chain only" non-goal.

The draft is explicit that case C "is maybe not required at the moment", so §6 is analysis, not a
specification. Do not read §6 as a commitment.

---

## 3. The oracle service

### 3.1 What EIP-0023 gives us, and what it does not

The oracle pool (EIP-0023, `~/ergo/oracle-core`, `eips@eip23`) is a **price consensus**, not an event
attestation service. `refresh_contract.es` collects ≥ `minDataPoints` posted oracle boxes, requires them
sorted and within `maxDeviationPercent`, and writes their **average** into the pool box's R4.

An event attestation ("BNB transfer T on BSC happened") is a different problem. Averaging is wrong for it:
51% saying "yes" and 49% saying "no" must not yield a rate. Two options:

- **(i) Reuse the price machinery with `maxDeviationPercent = 0`.** All attestors must publish the same
  value, so a disagreement stalls the epoch instead of averaging out. This works, but it changes the
  liveness property: one disagreeing attestor blocks progress until they are slashed or the epoch rolls.
- **(ii) Count attestations, don't aggregate them.** Require ≥ k attestation boxes *for the same event*,
  each from a distinct attestor. Disagreement stalls nothing — k co-signing attestors suffice regardless of
  how many dissent.

**(ii) is specified here.** It is closer to `rosen-bridge`'s `Commitment.es` than to `refresh_contract.es`,
and it matches the draft's phrasing ("when oracle pool also reports ... "). Option (i) is noted as a
fallback if epoch-based attestation is ever wanted.

### 3.2 The attestation contract (`Attestation.es`) — new work

Modelled on `Commitment.es`, with the datapoint replaced by an event identifier.

Each attestor holds a **singleton attestor NFT** (this is `oracle_token` in EIP-0023, `X-RWT` in
`rosen-bridge`). To attest event `E`, the attestor spends its attestor box and creates an attestation
box that **carries the attestor NFT forward**, with:

| Field | Where | Content |
|---|---|---|
| attestor identity | `tokens(0)._1` | attestor NFT id (amount 1) |
| event id | R4 | `blake2b256(canonicalEventPayload)`, 32 B |
| chain id | R5 | foreign chain id, 32 B — **v2: drop if single-chain deployment** |
| event height | R6 | foreign block height, `Long` |

The NFT-forward is what makes distinctness free. A second attestation from the same attestor would need a
second copy of a token whose amount is 1; token conservation forbids it. **No `distinct` operation is
required and none exists in ErgoScript** — the NFT is the proof. This is exactly why EIP-0023 works.

Spend paths: `attest` (owner, creates the attestation box) and `topUp`/`return` for the attestor NFT
itself, mirroring `oracle_contract.es`'s `isSimpleCopy || collection` shape.

### 3.3 What the MM contract reads

The MM contract (§5) requires, among `INPUTS`, at least `R6` (minAttestations) boxes that satisfy **all**:

- `propositionBytes == ATTEST_SCRIPT_HASH` — compile-time constant, pinning the attestation script.
- `tokens(0)._1 == ATTESTOR_POOL_NFT` — the agreed pool's attestor NFT. Compile-time constant in v1
  (**open question Q1**: this couples each MM script to one pool, exactly as `vault-contract.md` §8.3
  couples `vault_payment_proven.es` to one oracle NFT via `%%ORACLE_NFT_ID%%`).
- `R4[Coll[Byte]].get == eventId` — the event named in the note (§5.2).
- `R5[Coll[Byte]].get == chainId` — agreed at note-signing time.

Pinning **both** script hash and NFT id is deliberate belt-and-braces: `rosen-bridge`'s `Collateral.es`
checks both the repo NFT and the collateral script, and each alone has a known weakness — NFT-only accepts
a foreign-script box (`vault-contract.md` §7 test 20, an honest semantic not a hole), script-only accepts
an attestor who has not staked.

### 3.4 "Guards chosen from pools of RSN holders" — half on-chain, half off

The draft proposes reusing oracle-pool contracts with the operator group drawn from RSN holders. Taken
literally this is **not implementable on-chain**, and the spec should say so rather than paper over it.

The blocker: "RSN holders" is a *global set of addresses*. There is no on-chain way to ask it. Enumerating
token holders requires scanning the UTXO set, which no script can do, and approximating it requires a
mutable registry box — which is pooled global state, the thing `ergo-intents.md` §2 exists to avoid.

What **is** implementable, and is what EIP-0023 and `rosen-bridge` already do:

- **On-chain:** a fixed attestor set, as `proveDlog` keys in a register, threshold-checked. `Lock.es` is
  15 lines and does this in production; `vault-contract.md` §3.3/§8.4 adopt it as the phase-2 upgrade for
  the very same purpose.
- **Off-chain:** the *eligibility and election* of that set by RSN holders. Guardians propose, RSN holders
  vote, the elected set is written into the box by the current quorum.

**So the draft's idea is sound but must be split.** Record it as: *the attestor set is a governance
artifact, not a chain computation; its membership is decided by RSN holders off-chain and enforced
on-chain by a threshold check.* That is the honest formulation, and it is what `rosen-bridge`'s
`GuardSign.es` + `RepoConfig.es` update path already implements in production.

**Consequence for this document:** the MM box needs the attestor keys only if §3.3's quorum is
signature-based. It is not (§5.4) — attestors post boxes, they do not sign fills. So the MM box does
**not** carry the attestor key list. That is what keeps it inside six registers.

---

## 4. Why case B needs no custodian

`ergo-intents.md` §4 commits to "no pooled balance sheet and no custodian". `rosen-port` was studied as
the counter-example: it is a custodial batch bridge whose "container" is a Postgres row, whose guarantees
are all off-chain DB state checks, and whose double-spend protection is a TOCTOU race
(`PortBridger.onBeforeExecute` reads status, `onAfterExecute` writes it, nothing in between).

Case B does not inherit that. The MM's collateral sits in a **script-governed box on Ergo**, and the fill
condition is a spend condition evaluated by consensus. Nobody can move it. The oracle service is trusted
only to *report facts about foreign chains* — never to direct funds. That is a strictly weaker trust
assumption than "trusted with the money", and it is the reason this construction is worth doing even
though it looks structurally like `rosen-port` with a contract stapled on.

---

## 5. Case B — the MM collateral contract

### 5.1 The box

**Tokens:** exactly one secondary token kind — the MM's collateral (rsBTC, or any Ergo token).
**Value:** 0.01 ERG default, matching `vault-contract.md` §3.1.

Registers R0–R3 are protocol-fixed. R4–R9 are free — six total, and this box uses **six**:

| Reg | Type | Content |
|---|---|---|
| R4 | `Coll[Byte]` | `noteCommitment` = `blake2b256(noteBytes)`. Binds the box to exactly one note (§5.2). Also the tag carried into the fill output. |
| R5 | `Long` | `redeemDeadlineHeight`. Fill is valid only while `HEIGHT <= R5`. |
| R6 | `Long` | `minAttestations` — k. Quorum required among the input attestation boxes. |
| R7 | `Long` | `refundUnlockHeight`. `0` = no refund announced. Set by the announce action to `HEIGHT + COOL_OFF`. |
| R8 | `Coll[Byte]` | `mmPubKey` — 33 B compressed secp256k1 point, stored raw and `decodePoint`ed in-script (`vault-contract.md` §3.2: sigma 6 cannot lift a `GroupElement` constant out of a `Coll[Byte]` register). |
| R9 | — | free. Candidate for a runtime `ATTESTOR_POOL_NFT` if Q1 resolves away from the compile-time pin. |

The collateral token id is **not** a register: it is `SELF.tokens(0)._1`, and `Collateral.es:30-36`
shows the defensive form (`if (SELF.tokens.size > 1) { check } else { true }`) for tolerating a
zero-token box rather than throwing on `tokens(1)`.

`COOL_OFF` is a compile-time constant. §5.5 argues the draft's 12-hour value is an
economic commitment rather than a safety property, and says what changes if that is rejected.

### 5.2 The note

The note is an **off-chain byte blob signed by the MM with Schnorr**. It is not stored on-chain; its
32-byte commitment is (R4). Layout, fixed offsets:

```
offset  size  field
0       32    eventId        blake2b256(canonicalEventPayload)
32       8    amount         collateral units owed to the recipient, Long big-endian
40      var   recipient      the raw `propositionBytes` of the intended recipient
```

`canonicalEventPayload` is defined by the attestation service (§3.2) and is opaque to the MM contract —
the contract never parses it. Only `eventId` crosses the boundary, and the attestors are the party that
computed it from the real foreign-chain event.

`redeemDeadlineHeight` and `minAttestations` are deliberately **not** in the note. They live in the box
(R5, R6) so the chain, not the note, is the authority on timing and quorum. Keeping them in one place
removes a class of note/box desync bug.

The recipient bytes are compared with `==` against `OUTPUTS(fillIdx).propositionBytes` — no decoding, no
`byteArrayToBigInt`. v1 constrains `recipient` to 33 bytes (a compressed P2PK point). Wider recipient
types are **open question Q4**.

### 5.3 Order of operations

1. MM and taker agree terms off-chain, including **which oracle pool** will attest (draft: "before
   creating an order, user and market-maker agree on oracles being used, from the active pool").
2. MM chooses `redeemDeadlineHeight` (§5.5 for how) and `minAttestations` (pool's policy).
3. MM signs the note: Schnorr over `noteBytes`, challenge binding `mmPubKey`.
4. MM publishes the box with R4 = `blake2b256(noteBytes)`, R8 = `mmPubKey`, the collateral tokens.

Because R4 commits to the note, **one box serves exactly one note**. A large order that wants streaming
fills is served by **many boxes** — the draft's "if order is big a lot of order contracts could be created
on-chain for streaming swaps". Collapsing many notes into one box requires per-note spent-state, i.e. an
AVL tree, and is deferred (**open question Q5**, `basis.es` is the precedent, §9).

### 5.4 Context variables

| Var | Type | Path | Content |
|---|---|---|---|
| 0 | `Int` | all non-reclaim | `action`: `0` = fill, `1` = announce refund. Absent on reclaim txs. |
| 1 | `Int` | fill | `fillOutIdx` — index into `OUTPUTS` of the delivery box. |
| 2 | `Int` | fill | `selfOutIdx` — index into `OUTPUTS` of the continuing MM box (the collateral remainder). |
| 3 | `Coll[Byte]` | fill | `noteBytes` (§5.2). |
| 4 | `Coll[Byte]` | fill | `noteSig` — MM's Schnorr signature: `a(33) ‖ z(33..)` over `noteBytes`. |
| 5 | `Coll[Byte]` | fill | `attestationBoxesCount` is **not** carried — the contract counts them from `INPUTS`. |

The **presence of var 0** is the path discriminator, per `vault-contract.md` §3.3's regression: the proof
reducer evaluates every `val` of every block it enters, so a reclaim transaction must be able to reach its
branch without materialising var 3/var 4. Reclaim and announce txs therefore supply **no context variables
at all** — announce is a pure `proveDlog(R8)` spend.

Attestors **do not sign the fill**. Their attestation is a box in `INPUTS`. This is deliberate: it means
the fill is permissionless to assemble and the attestors need not be online per-fill. The cost is that the
attestation binds to `eventId` only, not to the recipient or amount — a second fill against the same
event with a different recipient would need a second box, which the NFT makes impossible (§3.2), and the
MM's Schnorr signature binds the recipient and amount on the other side of the boundary. **Open question
Q2** asks whether that split is acceptable or whether §5.4's signature-based alternative (attestors sign
`eventId ‖ amount ‖ recipient`, contract verifies k via `atLeast`) is worth the added online requirement.

### 5.5 Spending paths

**Discriminator: var 0 presence first, height second.** This is the `vault-contract.md` §3.3 ordering and
it is load-bearing here too.

```
if getVar[Int](0).isDefined  ->  var0 == 0 ? PATH FILL
                                 var0 == 1 ? PATH ANNOUNCE
else                          ->  PATH RECLAIM
```

**PATH FILL — anyone delivers the counter-leg's proof.**
- `HEIGHT <= SELF.R5[Long].get` (fill window still open)
- MM's Schnorr signature valid over `noteBytes` under `decodePoint(SELF.R8[Coll[Byte]].get)` (§5.6)
- ≥ `SELF.R6[Long].get` attestation boxes in `INPUTS` satisfying §3.3, all with the note's `eventId`
- output at `fillOutIdx`:
  - `tokens(0)._1 == SELF.tokens(0)._1` and `tokens(0)._2 >= byteArrayToLong(noteBytes.slice(32, 40))`
  - `propositionBytes == noteBytes.slice(40, noteBytes.size)`
  - `R4[Coll[Byte]].get == SELF.id` — **the lineage tag**
- output at `selfOutIdx`: the MM box, continuing, with script preserved, R5/R6/R8 preserved,
  `tokens(0)._1` preserved, `tokens(0)._2 == SELF.tokens(0)._2 - delivered`, and **R7 preserved exactly**
  (`refundPreserved`, the `basis.es` idiom — a fill must not cancel a refund the MM already announced)
- `selfOutIdx` box may be absent only if the collateral is fully consumed; the contract checks
  `selfOutIdx` output existence against `SELF.tokens(0)._2 - delivered == 0` (**open question Q3** — how
  to express "this output need not exist" without an `isDefined` check on a `Box`, which sigma 6's typer
  may reject)

No signature from the MM beyond the Schnorr on the note, and none from the recipient or attestors.

**PATH ANNOUNCE — MM signals intent to reclaim.** No collateral moves; R7 is set.
- `HEIGHT > SELF.R5[Long].get` — the fill window is already closed, so announcing can never shorten it.
  This is the constraint that makes the announcement safe; it is not in the draft and should be.
- `selfOut.R7[Long].get >= HEIGHT + COOL_OFF` — no backdating; `>=` tolerates delayed block inclusion
- script, R4, R5, R6, R8, tokens all preserved exactly (the AVL-free R5 problem does not arise here, but
  R5 holds a deadline that must not silently move)
- `SELF.R7[Long].getOrElse(0L) == 0L` — **one-shot**; re-announcing cannot reset the clock
- `proveDlog(decodePoint(SELF.R8[Coll[Byte]].get))`

**PATH RECLAIM — MM takes the collateral back.**
- `SELF.R7[Long].get > 0L` — a refund was announced
- `HEIGHT >= SELF.R7[Long].get` — the cooling-off period has elapsed
- `proveDlog(decodePoint(SELF.R8[Coll[Byte]].get))`

**On the 12-hour cooling-off period.** The draft's intent is preserved, but its security value should be
stated accurately rather than oversold. It is **not** an on-chain safety property: by the time ANNOUNCE
is reachable the fill window has already closed (R5), so no fill can be blocked by it. What it buys is
(a) an on-chain, publicly observable commitment that makes the MM's promise costly to break, and (b) a
window in which off-chain watchers — who monitor R7, as `basis.es` instructs wallets to do — can react to
a pending off-chain settlement before the MM's capital moves. That is an economic and social device, not a
consensus-enforced one. **If the owner decides 12h is not earning its complexity, the simplification is
available: drop ANNOUNCE and R7 entirely, and let RECLAIM fire on `HEIGHT > R5` alone.** That version has
one timer instead of two and one fewer path. Recorded as **open question Q6**; the spec keeps the
two-timer version because the draft asked for it.

### 5.6 The Schnorr check

Same construction as `vault-contract.md` §5 and `basis.es:335-346`, with the challenge binding the public
key (the strong Fiat-Shamir variant from ergoforum.org/t/3407):

```
a = decodePoint(noteSig.slice(0, 33))
z = byteArrayToBigInt(noteSig.slice(33, noteSig.size))
e = byteArrayToBigInt(blake2b256(noteSig.slice(0,33) ++ noteBytes ++ mmPubKey.getEncoded))
check: groupGenerator.exp(z) == a.multiply(mmPubKey.exp(e))      // z = r + e*x mod n
```

The signer must grind the nonce until `z` fits 254 bits, so its two's-complement interpretation via
`byteArrayToBigInt` is always positive (`MaxBigIntSizeInBytes` = 32, `vault-contract.md` §11). `basis.es`
uses 32-byte `z`; 33-byte `z` is the safer encoding and `MaxBigIntSizeInBytes` permits it, but the two must
agree exactly — recorded in the test matrix.

**A concrete typing hazard, unresolved.** `vault-contract.md` §4 records that `byteArrayToBigInt` only
typechecks when its argument is a *direct expression* — a bare `getVar(...).get`, a slice of one, or
`blake2b256(...)` thereof — and that **val references as arguments fail**. The snippet above passes
slices of vals (`noteSig.slice(...)`, `noteBytes.slice(...)`). `basis.es:336-346` does exactly this and is
deployed, so one of the two documents is wrong about the current typer. Two mitigations, both to be tried
at implementation time: write the slices inline in the argument position where cheap, and if the inline
form fights the typer, use slice-of-val and record the discrepancy against `vault-contract.md` §4.
**Open question Q7** — resolve by compiling, not by reading.

---

## 6. Case C — both legs off Ergo

The draft flags this as "hardest case ... maybe not required atm". The analysis says the draft is right,
and for a stronger reason than cost.

If neither leg lives on Ergo, the MM box has nothing to hold. The oracle service *can* attest both
foreign events — that is the one thing it is good at — but an attestation is a **fact, not an obligation**.
Nothing on Ergo is harmed when the MM walks away, so the box's spend conditions enforce nothing about
either leg.

What a case-C box could still do:

- record the agreed terms and the agreed attestor set, so both parties have a shared, hash-committed
  artifact;
- act as a **single consumption point** — one-shot, gated on dual attestation, so the two attestations are
  consumed together and cannot be replayed independently;
- serve as an RSN-bonded commitment, giving the attestor community a slashing hook.

None of those is user protection. Protection in case C has to come from the MM's foreign-chain collateral
and its reputation in the RSN community — i.e. from **governance and slashing, not from a script**. That
is a different product with a different trust model, and it should not be smuggled in under a contract
spec.

**Recommendation:** case C requires at least one leg to have an Ergo representation. Note that rsBTC, USE
and RSN all exist on Ergo, so most real pairs (rsBTC↔USE, rsBTC↔BNB) *do* qualify. The genuinely
excluded set is pairs with no Ergo-side asset at all (e.g. BNB↔ETH), which for v1 should be routed to an
existing bridge or refused. If a case-C commitment box is ever wanted, it is a small addition on top of §5
and should be specced separately.

---

## 7. Trust model

Stated plainly, in the style of `vault-contract.md` §9.

| Party | Trusted for | Not trusted for |
|---|---|---|
| **MM** | Nothing. Can reclaim only after `redeemDeadlineHeight` and an announced cooling-off period. | Cannot move collateral except to a fulfilling filler. Cannot cancel early. Cannot present a note twice. |
| **Filler** | Nothing. | Cannot double-fill (box is consumed; lineage tag). Cannot under-deliver (`amount` floor is a spend condition). Cannot forge an attestation (NFT singletons). |
| **Taker / recipient** | Nothing, but must still sign the tx — `proveDlog(recipient)` is a spend condition, so the recipient cannot be forced into a delivery they did not want. | — |
| **Attestor** | **Reporting facts about foreign chains.** This is the entire trust assumption, and it is real: a compromised attestor set can fabricate an `eventId` for an event that never happened. | Cannot move funds. Cannot present a note twice (NFT singleton). Cannot change quorum. |
| **Attestor-set membership** | Decided off-chain by RSN holders (§3.4), enforced on-chain by the compile-time `ATTEST_SCRIPT_HASH` / `ATTESTOR_POOL_NFT` pins. | Cannot be changed without a new MM script (Q1). |
| **Node / miner** | Nothing. Blocks are not censorship-resistant. | — |

**No custodian.** The MM's collateral is in a script-governed box and only the MM's key (for reclaim) or a
fulfilling fill (for delivery) can move it.

**The honest residual.** A compromised attestor quorum fabricates an `eventId`; a filler presents the
fabricated attestation against a genuine MM note; the MM's collateral is paid out for a foreign transfer
that never happened. The MM chose to trust that quorum when it accepted the note, which is why §3.1
chooses counting over averaging and why the quorum is a pinned, governed set rather than "whoever answers
first". This is the same shape as `vault-contract.md`'s phase-1 oracle trust, and the same mitigation
applies: attestor operator ≠ MM operator, per-deal size caps while centralised, and the phase-2 threshold
upgrade.

**Deliberate narrowing, stated as a choice.** `ergo-intents.md` §4 lists the miner as trusted for nothing
and claims no custodian. That safety is *earned by narrowness* — one offer, one filler, no pooled state —
and it is a choice, not a free consequence of the UTXO model. Rosen (`Collateral.es`, `Fraud.es`,
`Emission.es`, `Permit.es`, `X-AWC` — roughly 250 of its 587 script lines) buys its security with a
custodian plus a full staking and slashing apparatus. This design buys the same anti-double-spend property
as Rosen's quorum checks with ~40 lines and no slashing, because there is nothing pooled to protect. The
trade is that the moment this design grows shared or pooled state, it will need a fraction of Rosen's
apparatus. Recorded here so the choice is on the record rather than implied.

---

## 8. Market layer (out of contract scope)

From the draft: users do orders through a UX; after a round of off-chain exchange with market makers,
on-chain orders are created; there is a **separate MM-facing surface** in the form of API + MCP +
dashboard.

The auction mechanics are **NEAR Intents' protocol unchanged** — solvers compete on price over a WebSocket
JSON-RPC relay, taker accepts a quote, the winning filler broadcasts. `ergo-intents.md` §8 specifies this
and it applies verbatim here, with one adaptation that matters more in case B than in case A:

**A quote is only fillable while its MM box is unspent and `HEIGHT <= redeemDeadlineHeight`.** Case A has
the same race, but in case B the MM's *terms* depend on an oracle attestation that may not exist yet at
quote time, so a quote is really a promise about a future attestation. Fillers must re-check, at assembly
time: box unspent, fill window open, **and attestation boxes for the agreed `eventId` already on-chain and
pinned in the agreed pool**. The relay's `expirationTime` must be shorter than the residual fill window,
or the auction will routinely produce quotes that expire unfillable.

The MM-facing API surface (register a box, publish terms, watch attestations, request reclaim) is out of
scope for this document. It should be specced when there is a client.

---

## 9. Deferred: multiple notes per box (AVL)

One box per note is a real constraint on streaming liquidity: each fill is a box, so rent and the R5
deadline are per-fill. Collapsing N notes into one box requires the contract to remember which notes have
been spent, which needs per-note state — an **AVL tree in a register**, keyed by `eventId` (or by
`blake2b256(eventId ‖ amount ‖ recipient)` to bind the whole note), with an `insertOrUpdate` proof supplied
as a context variable.

`basis.es` is the working precedent: it keeps `hash(ownerKey ‖ receiverKey) -> (timestamp,
cumulativeRedeemed)` in R5, reads a lookup proof from context var 7, and requires
`nextTree == selfOut.R5[AvlTree].get`. Register pressure is the obstacle — R9 is the only free register
and the tree needs its own, so this would mean dropping `minAttestations` to a compile-time constant
(Q1's runtime-pin option becomes unavailable).

This is **open question Q5**, and it is the one place where `ergo-intents.md`'s register budget (all six
used) is genuinely binding.

---

## 10. Constraints

Verified against `~/ergo/sigma-rust/ergotree-ir/src/serialization/op_code.rs` and
`~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/data/SigmaConstants.scala`.

| Constant | Value | Consequence here |
|---|---|---|
| `MaxRegisters` (id 4) | 10 | R0–R3 fixed; **6 free**. This box uses **all six** (R4–R8). R9 free. No room for the AVL tree (§9). |
| `MaxPropositionBytes` (id 5) | 4096 | Straight-line only. The attestation-quorum `filter`/`forall` over `INPUTS` must fit; see Q8. |
| `MaxSigmaPropSizeInBytes` (id 8) | 1024 | Bounds the assembled sigma-prop proof. Two `proveDlog`s (MM on reclaim/announce, recipient on fill) is well inside it. |
| `MaxBoxSize` (id 1) | 4096 | R4 (32 B) + R8 (33 B) + two `Long`s + longs — trivially fine. |
| `MaxTokens` (id 3) | 255 | One collateral kind on the MM box; k attestation boxes are separate boxes, not tokens. |
| `MaxBigIntSizeInBytes` (id 7) | 32 | Bounds the Schnorr `z` encoding (§5.6). |
| `MaxTupleLength` (id 9) | 255 | Not binding. |

**Available and used:** `HEIGHT` (51), `INPUTS` (52), `OUTPUTS` (53), `SELF` (55), `filter` (69),
`exists` (62), `forall` (63), `sizeOf` (65), `byIndex` (66), `slice` (68), `blake2b256` (91),
`proveDlog` (93), `getVar` (115), `SigmaAnd` (122) / `SigmaOr` (123), `decodePoint` (126).

> **Correction to `ergo-intents.md` §10/Q2 and §11.** Both list the `Coll` combinators as opcodes 61–64
> (`map`/`exists`/`forall`/`fold`) but omit **`filter` (69)**. `filter` is the operator that actually
> makes an N-party quorum work in one straight-line script — `Commitment.es:53-74` and
> `EventTrigger.es:33-45` use it in production for exactly that. `map` is 61, `exists` 62, `forall` 63,
> `fold` 64, `sizeOf` 65, `byIndex` 66, `append` 67, `slice` 68, **`filter` 69**, `AVL_TREE` 70,
> `AVT_TREE_GET` 71. Also note `getVar[Long]` and `longToByteArray`/`byteArrayToLong` are used heavily by
> `basis.es` and are available.

**Not available:** Keccak-256 / SHA-3 (only `blake2b256`, 91, and `sha256`, 92) — so the canonical event
payload hash must be blake2b256-compatible; any chain event whose natural digest is Keccak (Ethereum logs)
needs a blake2b256 commitment computed by the attestor over its own re-encoding, which is an attestor-side
convention and **open question Q9**. No ECDSA, no Ed25519, no loops, no `distinct`, no `Option.isDefined`
on a `Box` (Q3).

**Toolchain:** sigma-state 6.0.6, `.es` sources compiled and tested from Kotlin, per
`vault-contract.md` §7 (`./gradlew :contracts:test`). `VersionContext.MaxSupportedScriptVersion` is 3.

---

## 11. Open questions

Ordered by blast radius. Each must be resolved by compiling and testing.

**Q1 — Compile-time `ATTESTOR_POOL_NFT` pin, or a register?** v1 pins the pool NFT and the attestation
script hash as compile-time constants, which means **one MM script per oracle pool**. The alternative is a
runtime pin, but R4–R8 are all spent and R9 alone cannot hold a pool NFT *and* be free for the AVL tree
(§9). If a single multi-pool script is wanted, something has to be dropped — most likely `minAttestations`
to a compile-time constant. Decide before writing the script; it changes the deployment story.
*Recommendation: keep the compile-time pin, exactly as `vault-contract.md` §8.3 does, and document the
coupling as a deployment error rather than a contract hole.*

**Q2 — Is `eventId`-only attestation binding sufficient?** §5.4 has attestors post boxes binding only
`eventId`; the MM's Schnorr binds recipient and amount. A hostile filler who obtains k attestations for a
real event cannot redirect them to another recipient (the MM's note fixes it), so the split appears sound —
but prove it with tests 12–14 rather than by argument. The stronger alternative (attestors sign
`eventId ‖ amount ‖ recipient`, verified via `atLeast` over mapped `proveDlog`) binds everything at the
cost of requiring the attestor set online per fill and a key list in the MM box. **This is the single most
important question in the document**, because it decides whether the attestors are a *reporting service*
or a *co-signing service*.

**Q3 — Expressing an absent optional output.** When a fill consumes the collateral completely, the
continuing MM box at `selfOutIdx` need not exist. The natural form is an `isDefined` check on the `Box`,
which sigma 6's typer is unlikely to accept (boxes are not `Option`). Alternatives: require the continuing
output always (creating a dust box that must then be reclaimed), or require the filler to supply
`selfOutIdx` pointing at any output when the remainder is zero and check the token delta instead.
Measure, do not guess.

**Q4 — Recipient type.** v1 requires the note's `recipient` to be 33 B of `propositionBytes` (P2PK). P2SH,
multi-sig and contract recipients would need the note to carry a script digest and the contract to compare
`blake2b256(propositionBytes)` instead of raw bytes — cheap, but it changes the note format and the test
matrix. Note that `propositionBytes == SELF.propositionBytes` on the continuing box already establishes
script preservation, so the pattern is proven.

**Q5 — Multiple notes per box (AVL).** §9. Real constraint on register budget, and the main scaling limit.

**Q6 — Is the 12h cooling-off earning its place?** §5.5. If no, drop ANNOUNCE and R7 and reclaim on
`HEIGHT > R5`. Simpler by one path and one register. **Owner decision**, of the same kind as the
2026-09-18 fee removal recorded in `vault-contract.md` §6.

**Q7 — `byteArrayToBigInt` argument form.** §5.6. `vault-contract.md` §4 says val references fail;
`basis.es` ships code that does it. Resolve by compiling; record the answer against `vault-contract.md` §4
either way.

**Q8 — Quorum filter cost.** The attestation check filters `INPUTS` and applies four conditions to each
surviving box. With `k` up to the pool's watcher count, does the compiled script stay inside
`MaxPropositionBytes` (4096) and produce a `SigmaProp` under `MaxSigmaPropSizeInBytes` (1024)? Also: the
filter must not evaluate registers on boxes of other scripts, or the reducer will evaluate them anyway
(`vault-contract.md` §4, last bullet) — filter on `propositionBytes == ATTEST_SCRIPT_HASH` **first**, then
touch registers. Measure with the real k from the active pool.

**Q9 — Keccak chains.** §10. For an Ethereum-leg event, what exactly does the attestor hash? Options:
commit over the attestor's own re-encoding of the log (attestor-side convention, chain-agnostic), or hash
only the fields the MM contract cares about (txId ‖ blockHeight ‖ to ‖ amount). The second is simpler and
sufficient, since the contract never parses the payload.

**Q10 — Schnorr `z` encoding.** 32 or 33 bytes (§5.6). Must match the signer exactly; pick one and pin it
in the test vectors.

---

## 12. Test matrix

Staged. **Stage 0** must be green before §5 is written, because stage 0 tests the typing assumptions the
whole design rests on.

**Stage 0 — typing assumptions (§5.6, Q7, Q10)**
0.1. `byteArrayToBigInt` on a slice of a val reference — compiles and evaluates. (Resolves Q7. If it
     fails, rewrite inline and re-run 0.1 as the inline form.)
0.2. `byteArrayToLong` on a slice of a val reference.
0.3. `byteArrayToLong` on a val holding an `AvlTree.get` result.
0.4. Schnorr over the note verifies under 32-byte `z` and under 33-byte `z`; both agree with the signer.
0.5. `z` with the sign bit set — rejected.
0.6. `z` = group order `n` as 32 bytes — rejected.
0.7. A 33-byte `a` that is not a valid curve point — `decodePoint` throws during evaluation, spend
     rejected.
0.8. Honest `(a, z)` with one flipped bit in the carried `noteBytes` — rejected (challenge recomputes over
     var bytes).

**Stage 1 — attestation contract (§3.2)**
1.1. Attestor publishes an attestation for `eventId`, carrying its attestor NFT forward — passes.
1.2. Attestation box missing the attestor NFT — fails.
1.3. Attestation box carrying a different attestor NFT — fails.
1.4. Attestation at a stale event height — fails (if the service bounds freshness; **open: does it?**).
1.5. Two attestations from the same attestor in one transaction — fails (NFT singleton; the distinctness
     property of §3.2, explicitly tested).
1.6. Attestor spends its box without creating an attestation — fails.

**Stage 2 — fill path (§5.5)**
2.1. Fill with a valid MM signature, `k` valid attestations, deadline open, exact `amount` — passes;
     lineage-tagged delivery output created; collateral conserved.
2.2. Fill with `amount` **below** the note's — fails.
2.3. Fill with `tokens(0)._1` != the MM box's collateral id — fails.
2.4. Fill to an output whose `R4 != SELF.id` — fails.
2.5. Fill to an output whose `propositionBytes` != the note's `recipient` — fails.
2.6. Fill with an MM signature from a key other than R8 — fails.
2.7. Fill with the note's `eventId` not matching any attestation — fails.
2.8. Fill with `k - 1` valid attestations — fails. **This is the quorum test; it and 2.7 together are the
     specification of §3.**
2.9. Fill with `k` attestations, one of them carrying a foreign script but the right NFT — fails
     (`propositionBytes == ATTEST_SCRIPT_HASH`).
2.10. Fill with `k` attestations from the right pool's NFT but with `R5` (chain id) not the agreed one —
     fails.
2.11. Fill with `HEIGHT > redeemDeadlineHeight` — fails **in the fill branch**, not rerouted to reclaim
      (`vault-contract.md` §3.3 regression).
2.12. Fill with the continuing box's R7 altered (cancelling a pending refund) — fails.
2.13. Partial fill (collateral remains) — passes; continuing box carries the exact remainder.
2.14. Full fill (collateral exhausted, no continuing box) — passes or fails per **Q3**; the test records
      whichever the implementation settles on and is marked `@Disabled` until Q3 is answered.

**Stage 3 — refund paths**
3.1. Announce with `HEIGHT <= redeemDeadlineHeight` — fails (cannot shorten the fill window).
3.2. Announce with R7 set into the past — fails (no backdating).
3.3. Re-announce when R7 is already set — fails (one-shot; cannot reset the clock).
3.4. Reclaim before `R7` — fails.
3.5. Reclaim at/after `R7` with the MM key — passes; all collateral to an MM-chosen address.
3.6. Reclaim with the wrong key — fails.
3.7. Announce and reclaim in the same transaction — **must fail**; otherwise the cooling-off period is
      bypassable in one tx. This is the test that proves §5.5's two-path split is real.
3.8. Reclaim tx supplying context variables (var 0 mis-supplied) — behaviour recorded; must not silently
      route into fill.

**Stage 4 — cross-property**
4.1. Fill racing the deadline: `HEIGHT == redeemDeadlineHeight` — passes (window is inclusive).
4.2. Two fills of the same box in one transaction — fails (R7/… self-index collision; see below).
4.3. Attestation reused across two different MM notes for the same event — passes if the event genuinely
      occurred once and both notes are distinct claims on it; **the behaviour must be recorded and
      consciously accepted or forbidden**, because it is a double-claim surface that the NFT does *not*
      close (the NFT closes double-*attestation*, not double-*claim*). Flagged as a design question for
      the owner.
4.4. MM note signed for event `E1`, presented against attestations for event `E2` — fails.
4.5. Filler supplies `fillOutIdx` pointing at a nonexistent output index — fails.

---

## 13. Sources

- `~/ergo/p2pgate/specs/vault-contract.md` — §3.2 (raw-`Coll[Byte]` key storage), §3.3 (path-selection
  discipline and the post-timeout regression), §4 (sigma-6 typing constraints), §5 (in-script Schnorr,
  freshness binding), §6 (2026-09-18 fee removal), §7 (test-matrix style), §8.3 (compile-time NFT pin and
  its deployment-coupling semantic), §8.4, §9 (honest trust-model statements).
- `~/ergo/p2pgate/onramp-insurance.md` — §2 (mixer-derived reclaims; relevant to §13 note below), §3.3
  (Keccak absence), §5.
- `intents/ergo-intents.md` — §3 (lineage tag, divergences from `sigma.tex`), §4 (trust model),
  §5 (same-chain box), §8 (market layer), §9 (non-goals this document revises), §10 (open questions),
  §11 (constraint table, corrected here), §12 (test-matrix style).
- `~/ergo/oracle-core` (`develop`) — attestor box progression and the singleton-NFT design:
  `core/src/box_kind/oracle_box.rs`, `core/src/contracts/oracle.rs`,
  `core/src/default_parameters.rs`.
- `~/ergo/eips@eip23` — EIP-0023 `.es` sources: `pool_contract.es`, `oracle_contract.es`,
  `refresh_contract.es`, `ballot_contract.es`, `update_contract.es`. `Commitment.es:53-74` and
  `EventTrigger.es:33-45` are the quorum precedents; `oracle_contract.es:43` is the register-erasure
  anti-replay idiom.
- `rosen-bridge/contract` (`develop`, v7.1.1) — `Lock.es` and `GuardSign.es` (the `atLeast` over mapped
  `proveDlog` threshold and the data-input guard box, i.e. `vault-contract.md`'s phase-2 target);
  `Collateral.es:13,30-36` (delta arithmetic and the zero-token defensive form, relevant to Q3);
  `RwtRepo.es:13` (token-identity path discriminator); `RepoConfig.es`, `Emission.es` (guard-gated config
  updates).
- `~/ergo/basis-tracker` — `contract/basis.es` and `contract/basis-token.es`: in-script Schnorr with
  strong Fiat-Shamir (`basis.es:335-346`), AVL-tree redemption state and `insertOrUpdate` proof checking
  (§9 precedent), the two-phase refund with `refundPreserved` on the redemption path
  (`basis.es:360`), the `getVar`-packed action/index idiom (`basis.es:213-215`), and the security
  analysis framing in the contract header.
- `~/ergo/chaincash` — `contracts/onchain/note.es`, `reserve.es` (lineage/history tagging, multi-path
  contracts).
- `kii-dot/rosen-port` — the custodial batch-bridge counter-example discussed in §4: pooled balance,
  DB-as-ledger, TOCTOU double-bridge race.
- `~/ergo/sigma-rust/ergotree-ir/src/serialization/op_code.rs` — opcode inventory, including `filter` (69),
  omitted from `ergo-intents.md` §10/§11.
- `~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/data/SigmaConstants.scala` — all size
  limits in §10.
- `~/ergo/sigmastate-interpreter/core/shared/src/main/scala/sigma/VersionContext.scala` — script version 3.
- `docs/wpaper/sigma.tex` — §"Example: atomic exchange on a single block chain" (331–360), lineage-tag
  attack (line 348), token-conservation rule (line 319).
- ergoforum.org/t/3407 — verifying Schnorr signatures in ErgoScript.

**Open cross-reference, not resolved here:** `onramp-insurance.md` §2 proposes reclaiming through
mixer outputs / stealth addresses so routine reclaims do not link a maker's boxes. The same question
applies to the MM's RECLAIM path (§5.5) and `ergo-intents.md` §10/Q7 asks it for the maker. The answer is
the same in both cases and is not yet written down: the payee is unconstrained on every path, so a
mixer-derived address is a **wallet-side convention, not a contract property** — the contract cannot
enforce it and does not need to. Recorded here so it is not asked twice.
