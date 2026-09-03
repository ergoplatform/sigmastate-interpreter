/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains material adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark.circuit

import sigma.stark.{BabyBear, Ext4}

/** Interpreter for the recursion circuit's constraint program — faithful
  * port of risc0-zkp 3.0.4 `src/adapter.rs` `PolyExtStepDef::step`
  * (`PolyExtExecutor::run`).
  *
  * The program is a straight-line pass over [[PolyExtTable]] ops driving two
  * append-only stacks: `fp` of Ext4 values and `mix` of `(tot, mul)` Ext4
  * pairs. The result is `mix(ret)` — its `tot` component is the mixed
  * constraint-polynomial evaluation `verify_validity` compares against the
  * recombined check polynomial.
  *
  * Correctness is pinned by the compact external-oracle KAT extracted from
  * the real devnet receipt (`stark-kats/polyext_transcript_oracle.tsv`
  * checkpoints `poly_mix`,
  * `out`, `mix`, `eval_u` → `result`), per the oracle-parity rule.
  *
  * Kept allocation-light for the consensus path: three preallocated arrays
  * and an index-based while loop; the only per-op allocations are the
  * immutable [[Ext4]] results themselves.
  */
object PolyExtInterpreter {

  /** Mirror of risc0-zkp's `MixState`: the running `(tot, mul)` pair of a
    * constraint chain.
    */
  final case class MixState(tot: Ext4, mul: Ext4)

  /** Run the program. Mirrors the Rust signature
    * `step(mix: &ExtElem, u: &[ExtElem], args: &[&[Elem]]) -> MixState`:
    *
    * @param table   the parsed op table
    * @param polyMix the transcript-drawn constraint mixer (`poly_mix`)
    * @param u       the tapped evaluations `eval_u`, one Ext4 per tap, in
    *                canonical tap order; coefficients must be canonical
    *                (`[0, P)`)
    * @param args    the global buffers, canonical base-field values;
    *                `args(0)` = out globals (OUT_SIZE = 32),
    *                `args(1)` = accum-mix globals (MIX_SIZE = 20) — exactly
    *                what `verify` passes as `&[out, &mix]`
    * @return `Right(MixState)` on success; `Left` for any malformed access
    *         (tap index outside `u`, global outside `args`, non-canonical
    *         global value, stack-shape mismatch) — never throws.
    */
  def step(
      table: PolyExtTable,
      polyMix: Ext4,
      u: Array[Ext4],
      args: Array[Array[Int]]
  ): Either[String, MixState] = {
    if (table == null) return Left("constraint table is null")
    if (polyMix == null) return Left("polyMix is null")
    if (u == null) return Left("tap evaluations are null")
    if (args == null) return Left("global buffers are null")
    if (table.fpVars < 0 || table.mixVars < 0)
      return Left("constraint table has negative stack dimensions")

    // Validate every proof-independent operation and proof-dependent access
    // before entering the shared arithmetic kernel. Production reaches that
    // same kernel only through Risc0RawSealVerifier.ProgramSnapshot, whose
    // construction performs these checks while taking an owned copy.
    val ops = table.ops
    var fpN = 0
    var mixN = 0

    var i = 0
    while (i < ops.length) {
      val op = ops(i)
      if (op == null) return Left(s"op $i is null")
      op match {
        case PolyExtOp.Const(v) =>
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          if (v < 0 || v >= BabyBear.P.toLong)
            return Left(s"op $i: Const value $v is not a canonical BabyBear element")
          fpN += 1
        case PolyExtOp.ConstExt(c0, c1, c2, c3) =>
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          if (c0 < 0 || c0 >= BabyBear.P.toLong ||
              c1 < 0 || c1 >= BabyBear.P.toLong ||
              c2 < 0 || c2 >= BabyBear.P.toLong ||
              c3 < 0 || c3 >= BabyBear.P.toLong)
            return Left(s"op $i: ConstExt contains a non-canonical BabyBear element")
          fpN += 1
        case PolyExtOp.Get(tap) =>
          if (tap < 0 || tap >= u.length)
            return Left(s"op $i: Get($tap) outside u (${u.length} taps)")
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          val value = u(tap)
          if (value == null) return Left(s"op $i: Get($tap) resolved to null")
          fpN += 1
        case PolyExtOp.GetGlobal(arg, offset) =>
          if (arg < 0 || arg >= args.length)
            return Left(s"op $i: GetGlobal arg $arg outside args (${args.length})")
          val buffer = args(arg)
          if (buffer == null) return Left(s"op $i: GetGlobal arg $arg is null")
          if (offset < 0 || offset >= buffer.length)
            return Left(s"op $i: GetGlobal($arg, $offset) outside buffer (${buffer.length})")
          val v = buffer(offset)
          if (v < 0 || v >= BabyBear.P)
            return Left(s"op $i: GetGlobal($arg, $offset) value $v not a canonical field element")
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          fpN += 1
        case PolyExtOp.Add(a, b) =>
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          if (a < 0 || a >= fpN || b < 0 || b >= fpN)
            return Left(s"op $i: Add operands ($a,$b) outside fp stack ($fpN)")
          fpN += 1
        case PolyExtOp.Sub(a, b) =>
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          if (a < 0 || a >= fpN || b < 0 || b >= fpN)
            return Left(s"op $i: Sub operands ($a,$b) outside fp stack ($fpN)")
          fpN += 1
        case PolyExtOp.Mul(a, b) =>
          if (fpN >= table.fpVars) return Left(s"op $i: fp stack overflow")
          if (a < 0 || a >= fpN || b < 0 || b >= fpN)
            return Left(s"op $i: Mul operands ($a,$b) outside fp stack ($fpN)")
          fpN += 1
        case PolyExtOp.True =>
          if (mixN >= table.mixVars) return Left(s"op $i: mix stack overflow")
          mixN += 1
        case PolyExtOp.AndEqz(chain, inner) =>
          if (mixN >= table.mixVars) return Left(s"op $i: mix stack overflow")
          if (chain < 0 || chain >= mixN || inner < 0 || inner >= fpN)
            return Left(s"op $i: AndEqz operands ($chain,$inner) outside mix/fp stacks ($mixN,$fpN)")
          mixN += 1
        case PolyExtOp.AndCond(chain, cond, inner) =>
          if (mixN >= table.mixVars) return Left(s"op $i: mix stack overflow")
          if (chain < 0 || chain >= mixN || cond < 0 || cond >= fpN ||
              inner < 0 || inner >= mixN)
            return Left(s"op $i: AndCond operands ($chain,$cond,$inner) outside mix/fp stacks ($mixN,$fpN)")
          mixN += 1
      }
      i += 1
    }

    // Mirror upstream's post-run stack-shape assertions (as Left, not panic).
    if (fpN != table.fpVars)
      Left(s"fp stack ended at $fpN, expected ${table.fpVars}")
    else if (mixN != table.mixVars)
      Left(s"mix stack ended at $mixN, expected ${table.mixVars}")
    else if (table.ret < 0 || table.ret >= mixN)
      Left(s"ret ${table.ret} outside mix stack ($mixN)")
    else
      Right(runValidated(
        ops,
        table.ret,
        table.fpVars,
        table.mixVars,
        polyMix,
        u,
        args))
  }

  /** Execute one already-validated constraint program.
    *
    * This is the sole implementation of the ten PolyExt arithmetic cases.
    * It is package-bounded because callers must first establish that every
    * stack/global/tap operand and every constant is valid. The production
    * verifier does so in its immutable `ProgramSnapshot`; [[step]] retains
    * the defensive public validation contract above.
    *
    * Keep this loop's allocation and expression order aligned with
    * risc0-zkp's `PolyExtExecutor::run`: three preallocated stacks and only
    * immutable [[Ext4]] arithmetic values inside the loop.
    */
  private[stark] def runValidated(
      ops: Array[PolyExtOp],
      ret: Int,
      fpVars: Int,
      mixVars: Int,
      polyMix: Ext4,
      u: Array[Ext4],
      args: Array[Array[Int]]
  ): MixState = {
    val fp = new Array[Ext4](fpVars)
    val mixTot = new Array[Ext4](mixVars)
    val mixMul = new Array[Ext4](mixVars)
    var fpN = 0
    var mixN = 0
    var i = 0
    while (i < ops.length) {
      ops(i) match {
        case PolyExtOp.Const(value) =>
          fp(fpN) = Ext4.fromBase(value.toInt)
          fpN += 1
        case PolyExtOp.ConstExt(a, b, c, d) =>
          fp(fpN) = Ext4(
            a.toInt,
            b.toInt,
            c.toInt,
            d.toInt)
          fpN += 1
        case PolyExtOp.Get(tap) =>
          fp(fpN) = u(tap)
          fpN += 1
        case PolyExtOp.GetGlobal(arg, offset) =>
          fp(fpN) = Ext4.fromBase(args(arg)(offset))
          fpN += 1
        case PolyExtOp.Add(a, b) =>
          fp(fpN) = fp(a) + fp(b)
          fpN += 1
        case PolyExtOp.Sub(a, b) =>
          fp(fpN) = fp(a) - fp(b)
          fpN += 1
        case PolyExtOp.Mul(a, b) =>
          fp(fpN) = fp(a) * fp(b)
          fpN += 1
        case PolyExtOp.True =>
          mixTot(mixN) = Ext4.Zero
          mixMul(mixN) = Ext4.One
          mixN += 1
        case PolyExtOp.AndEqz(chain, inner) =>
          mixTot(mixN) = mixTot(chain) + mixMul(chain) * fp(inner)
          mixMul(mixN) = mixMul(chain) * polyMix
          mixN += 1
        case PolyExtOp.AndCond(chain, condition, inner) =>
          mixTot(mixN) = mixTot(chain) +
            fp(condition) * mixTot(inner) * mixMul(chain)
          mixMul(mixN) = mixMul(chain) * mixMul(inner)
          mixN += 1
      }
      i += 1
    }
    MixState(mixTot(ret), mixMul(ret))
  }
}
