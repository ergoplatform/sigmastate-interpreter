/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains generated-data handling adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark.circuit

/** One step of the RISC0 recursion-circuit constraint program.
  *
  * Mirror of risc0-zkp 3.0.4 `src/adapter.rs` `PolyExtStep`: a program over
  * two append-only stacks — `fp` of Ext4 values and `mix` of `(tot, mul)`
  * pairs — where every operand is a position in one of those stacks
  * (append-only, so an operand can only reference an already-pushed value).
  *
  * `Const`/`ConstExt` operands keep the raw `u32` values from the table
  * (as `Long`) so the canonical integrity serialization
  * ([[PolyExtTable.canonicalBytes]]) reproduces the generator's byte stream
  * exactly. The EIP-0045 B2 boundary requires every such value to be a
  * canonical BabyBear element; the interpreter rejects rather than reducing a
  * non-canonical value.
  */
sealed abstract class PolyExtOp

object PolyExtOp {
  /** Push fp: base-field constant lifted to Ext4. */
  final case class Const(value: Long) extends PolyExtOp

  /** Push fp: explicit Ext4 constant (0 occurrences in the recursion circuit). */
  final case class ConstExt(c0: Long, c1: Long, c2: Long, c3: Long) extends PolyExtOp

  /** Push fp: `u(tap)` — a tapped trace-polynomial evaluation. */
  final case class Get(tap: Int) extends PolyExtOp

  /** Push fp: `args(arg)(offset)` lifted to Ext4; arg 0 = out globals (32),
    * arg 1 = accum-mix globals (20).
    */
  final case class GetGlobal(arg: Int, offset: Int) extends PolyExtOp

  /** Push fp: `fp(a) + fp(b)`. */
  final case class Add(a: Int, b: Int) extends PolyExtOp

  /** Push fp: `fp(a) - fp(b)`. */
  final case class Sub(a: Int, b: Int) extends PolyExtOp

  /** Push fp: `fp(a) * fp(b)`. */
  final case class Mul(a: Int, b: Int) extends PolyExtOp

  /** Push mix: `(tot = 0, mul = 1)` — the empty constraint chain. */
  case object True extends PolyExtOp

  /** Push mix: `(chain.tot + chain.mul * fp(inner), chain.mul * POLY_MIX)`. */
  final case class AndEqz(chain: Int, inner: Int) extends PolyExtOp

  /** Push mix: `(chain.tot + fp(cond) * mix(inner).tot * chain.mul,
    * chain.mul * mix(inner).mul)`.
    */
  final case class AndCond(chain: Int, cond: Int, inner: Int) extends PolyExtOp
}

/** The RISC0 recursion circuit's constraint system as data — the parsed
  * `PolyExtStepDef` op table (risc0-circuit-recursion 4.0.4 `poly_ext.rs`,
  * extracted to `stark-kats/circuit_polyext_ops.tsv`; interpreter contract
  * in risc0-zkp 3.0.4 `src/adapter.rs`).
  *
  * Instances only come out of [[PolyExtTable.parse]], which validates the
  * table structurally (sequential indices, in-range stack operands against
  * the append-only discipline, final stack sizes, `ret` in range, opcode
  * histogram vs the recorded meta). The Blake2b-256 integrity hash over
  * [[canonicalBytes]] is asserted by the JVM spec (Blake2b lives in core's
  * JVM-only dependency set), not here.
  *
  * The operation table is owned by this instance. Public review access returns
  * a defensive copy; the indexed interpreter uses a package-bounded accessor.
  */
final class PolyExtTable private (
    sourceOps: Array[PolyExtOp],
    val ret: Int,
    val fpVars: Int,
    val mixVars: Int,
    val opcodeHistogram: Map[String, Int],
    val blake2b256Hex: String
) {
  private val opsSnapshot = sourceOps.clone()

  /** Defensive review snapshot. */
  def ops: Array[PolyExtOp] = opsSnapshot.clone()

  def opsCount: Int = opsSnapshot.length

  private[stark] def opAt(index: Int): PolyExtOp = opsSnapshot(index)

  /** The generator's canonical serialization of the parsed table — UTF-8 of
    * `"<mnemonic>:<comma-operands>\n"` per op in order (see
    * `circuit_tables.md`). Recomputed from the PARSED ops (not file bytes)
    * so hashing it re-proves parse fidelity.
    */
  def canonicalBytes: Array[Byte] = {
    val sb = new java.lang.StringBuilder(opsSnapshot.length * 16)
    var i = 0
    while (i < opsSnapshot.length) {
      val (name, operands) = PolyExtTable.serializeOp(opsSnapshot(i))
      sb.append(name).append(':')
      var j = 0
      while (j < operands.length) {
        if (j > 0) sb.append(',')
        sb.append(operands(j))
        j += 1
      }
      sb.append('\n')
      i += 1
    }
    sb.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
  }
}

object PolyExtTable {

  private val NoOperandsSentinel = "-"

  /** Construction boundary for the authenticated EIP-0045 B2 decoder.
    *
    * The binary decoder has already enforced the append-only operand
    * discipline, exact stack sizes, opcode census, canonical constants, and
    * decode/re-encode identity. Keeping this factory package-bounded prevents
    * production code from routing authenticated binary data through the
    * classpath/TSV parser while preserving the private primary constructor.
    */
  private[stark] def fromValidatedBinary(
      ops: Array[PolyExtOp],
      ret: Int,
      fpVars: Int,
      mixVars: Int,
      opcodeHistogram: Map[String, Int]): PolyExtTable =
    new PolyExtTable(
      ops,
      ret,
      fpVars,
      mixVars,
      opcodeHistogram,
      "authenticated-b2")

  /** Mnemonic + operand list of an op, matching the generator's
    * `serialize_op` exactly (stark-kat `circuit_extract.rs`).
    */
  private[circuit] def serializeOp(op: PolyExtOp): (String, Array[Long]) = op match {
    case PolyExtOp.Const(v)              => ("Const", Array(v))
    case PolyExtOp.ConstExt(a, b, c, d)  => ("ConstExt", Array(a, b, c, d))
    case PolyExtOp.Get(tap)              => ("Get", Array(tap.toLong))
    case PolyExtOp.GetGlobal(arg, off)   => ("GetGlobal", Array(arg.toLong, off.toLong))
    case PolyExtOp.Add(a, b)             => ("Add", Array(a.toLong, b.toLong))
    case PolyExtOp.Sub(a, b)             => ("Sub", Array(a.toLong, b.toLong))
    case PolyExtOp.Mul(a, b)             => ("Mul", Array(a.toLong, b.toLong))
    case PolyExtOp.True                  => ("True", Array.empty[Long])
    case PolyExtOp.AndEqz(c, i)          => ("AndEqz", Array(c.toLong, i.toLong))
    case PolyExtOp.AndCond(c, cond, i)   => ("AndCond", Array(c.toLong, cond.toLong, i.toLong))
  }

  /** Parse `circuit_polyext_ops.tsv` content. Total: any malformed input —
    * bad numbers, out-of-range stack operands, meta/structure mismatches —
    * yields `Left`, never throws. The caller supplies raw lines (`#`
    * comments and blank lines are skipped here) so shared code stays
    * platform-neutral; the JVM side feeds it from `getResourceAsStream`.
    */
  def parse(lines: Iterator[String]): Either[String, PolyExtTable] =
    if (lines == null) Left("circuit_polyext_ops: input iterator is null")
    else try parseChecked(lines)
    catch {
      case e: NumberFormatException => Left(s"circuit_polyext_ops: bad number: ${e.getMessage}")
    }

  private def parseChecked(lines: Iterator[String]): Either[String, PolyExtTable] = {
    val meta = scala.collection.mutable.HashMap.empty[String, String]
    val ops = scala.collection.mutable.ArrayBuffer.empty[PolyExtOp]
    // Append-only stack discipline: operand validity is decidable during the
    // single parse pass by tracking how many values each stack holds so far.
    var fpCount = 0
    var mixCount = 0
    val histogram = scala.collection.mutable.HashMap.empty[String, Int]

    while (lines.hasNext) {
      val line = lines.next()
      if (line == null) return Left("circuit_polyext_ops: null input row")
      if (line.nonEmpty && !line.startsWith("#")) {
        // Data rows are whitespace-free tokens separated only by tabs. In
        // particular, a zero-arity True op uses the explicit `-` sentinel;
        // an empty final TSV cell must not be normalized away by a reader.
        val f = line.split("\t", -1)
        if (f.exists(_.isEmpty))
          return Left(s"circuit_polyext_ops: empty field in row: $line")
        if (f.exists(_.exists(ch => java.lang.Character.isWhitespace(ch))))
          return Left(s"circuit_polyext_ops: whitespace in field in row: $line")
        f(0) match {
          case "meta" =>
            if (f.length != 3) return Left(s"circuit_polyext_ops: bad meta row: $line")
            if (meta.contains(f(1)))
              return Left(s"circuit_polyext_ops: duplicate meta '${f(1)}'")
            meta.put(f(1), f(2))
          case "op" =>
            if (f.length != 4) return Left(s"circuit_polyext_ops: bad op row: $line")
            val idx = f(1).toInt
            if (idx != ops.length)
              return Left(s"circuit_polyext_ops: op index $idx out of order (expected ${ops.length})")
            if (f(3) == NoOperandsSentinel && f(2) != "True")
              return Left(s"circuit_polyext_ops: op $idx: '$NoOperandsSentinel' is only valid for True")
            if (f(2) == "True" && f(3) != NoOperandsSentinel)
              return Left(s"circuit_polyext_ops: op $idx: True requires '$NoOperandsSentinel'")
            val operands: Array[Long] =
              if (f(3) == NoOperandsSentinel) Array.empty[Long]
              else f(3).split(",", -1).map(_.toLong)

            def fpIdx(v: Long): Int = {
              if (v < 0 || v >= fpCount)
                throw new NumberFormatException(s"op $idx: fp operand $v not in [0, $fpCount)")
              v.toInt
            }
            def mixIdx(v: Long): Int = {
              if (v < 0 || v >= mixCount)
                throw new NumberFormatException(s"op $idx: mix operand $v not in [0, $mixCount)")
              v.toInt
            }
            def u32(v: Long): Long = {
              if (v < 0 || v > 0xFFFFFFFFL)
                throw new NumberFormatException(s"op $idx: operand $v not a u32")
              v
            }
            def nonNeg(v: Long): Int = {
              if (v < 0 || v > Int.MaxValue)
                throw new NumberFormatException(s"op $idx: operand $v out of range")
              v.toInt
            }
            def arity(n: Int): Unit =
              if (operands.length != n)
                throw new NumberFormatException(
                  s"op $idx: ${f(2)} expects $n operands, got ${operands.length}")

            val op: PolyExtOp = f(2) match {
              case "Const"    => arity(1); PolyExtOp.Const(u32(operands(0)))
              case "ConstExt" =>
                arity(4)
                PolyExtOp.ConstExt(u32(operands(0)), u32(operands(1)), u32(operands(2)), u32(operands(3)))
              case "Get"       => arity(1); PolyExtOp.Get(nonNeg(operands(0)))
              case "GetGlobal" => arity(2); PolyExtOp.GetGlobal(nonNeg(operands(0)), nonNeg(operands(1)))
              case "Add"       => arity(2); PolyExtOp.Add(fpIdx(operands(0)), fpIdx(operands(1)))
              case "Sub"       => arity(2); PolyExtOp.Sub(fpIdx(operands(0)), fpIdx(operands(1)))
              case "Mul"       => arity(2); PolyExtOp.Mul(fpIdx(operands(0)), fpIdx(operands(1)))
              case "True"      => arity(0); PolyExtOp.True
              case "AndEqz"    => arity(2); PolyExtOp.AndEqz(mixIdx(operands(0)), fpIdx(operands(1)))
              case "AndCond" =>
                arity(3)
                PolyExtOp.AndCond(mixIdx(operands(0)), fpIdx(operands(1)), mixIdx(operands(2)))
              case other => return Left(s"circuit_polyext_ops: op $idx: unknown mnemonic '$other'")
            }
            op match {
              case PolyExtOp.True | _: PolyExtOp.AndEqz | _: PolyExtOp.AndCond => mixCount += 1
              case _                                                           => fpCount += 1
            }
            histogram.put(f(2), histogram.getOrElse(f(2), 0) + 1)
            ops += op
          case other => return Left(s"circuit_polyext_ops: unknown row kind '$other'")
        }
      }
    }

    def metaInt(key: String): Either[String, Int] = meta.get(key) match {
      case Some(v) => Right(v.toInt)
      case None    => Left(s"circuit_polyext_ops: missing meta '$key'")
    }

    metaInt("ops_count") match {
      case Left(e) => Left(e)
      case Right(opsCount) =>
        metaInt("ret") match {
          case Left(e) => Left(e)
          case Right(ret) =>
            metaInt("fp_vars") match {
              case Left(e) => Left(e)
              case Right(fpVars) =>
                metaInt("mix_vars") match {
                  case Left(e) => Left(e)
                  case Right(mixVars) =>
                    validate(ops.toArray, opsCount, ret, fpVars, mixVars,
                      fpCount, mixCount, histogram.toMap, meta.toMap)
                }
            }
        }
    }
  }

  private def validate(
      ops: Array[PolyExtOp],
      opsCount: Int,
      ret: Int,
      fpVars: Int,
      mixVars: Int,
      fpCount: Int,
      mixCount: Int,
      histogram: Map[String, Int],
      meta: Map[String, String]
  ): Either[String, PolyExtTable] = {
    if (ops.length != opsCount)
      Left(s"circuit_polyext_ops: ${ops.length} ops parsed but meta ops_count=$opsCount")
    else if (fpCount != fpVars)
      Left(s"circuit_polyext_ops: $fpCount fp vars but meta fp_vars=$fpVars")
    else if (mixCount != mixVars)
      Left(s"circuit_polyext_ops: $mixCount mix vars but meta mix_vars=$mixVars")
    else if (ret < 0 || ret >= mixCount)
      Left(s"circuit_polyext_ops: ret=$ret not in [0, $mixCount)")
    else if (mixVars != ret + 1)
      // Upstream sizes the mix stack as ret + 1 (`PolyExtExecutor::new`):
      // the result must be the LAST mix var pushed.
      Left(s"circuit_polyext_ops: mix_vars=$mixVars but ret=$ret requires ${ret + 1}")
    else {
      meta.get("opcode_histogram") match {
        case None => Left("circuit_polyext_ops: missing meta 'opcode_histogram'")
        case Some(h) =>
          val entries = h.split(",").map(_.split("=", 2))
          if (entries.exists(_.length != 2))
            Left(s"circuit_polyext_ops: bad histogram meta '$h'")
          else {
            val metaHist = entries.map(p => (p(0), p(1).toInt)).toMap
            if (histogram != metaHist)
              Left(s"circuit_polyext_ops: opcode histogram $histogram != meta $metaHist")
            else
              meta.get("blake2b256") match {
                case None => Left("circuit_polyext_ops: missing meta 'blake2b256'")
                case Some(hash) =>
                  Right(new PolyExtTable(ops, ret, fpVars, mixVars, histogram, hash))
              }
          }
      }
    }
  }
}
