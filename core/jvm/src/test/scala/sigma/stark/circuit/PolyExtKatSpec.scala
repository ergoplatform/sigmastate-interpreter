package sigma.stark.circuit

import org.bouncycastle.crypto.digests.Blake2bDigest
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sigma.stark.Ext4

import scala.io.Source

/** Known-Answer-Test parity for the EIP-0045 verifyStark constraint-system
  * data tables and the poly-ext interpreter.
  *
  * Expected values come from EXTERNAL oracles only — the recursion-circuit
  * tables extracted from risc0-circuit-recursion 4.0.4 / risc0-zkp 3.0.4 and
  * the six compact Fiat-Shamir checkpoints extracted verbatim from the REAL
  * devnet receipt accepted by the untouched Rust verifier
  * (`stark-kats/polyext_transcript_oracle.tsv`) — never
  * from the Scala code under test (oracle-parity rule).
  */
class PolyExtKatSpec extends AnyFunSuite with Matchers {

  // Raw lines, comments included — the loaders skip them themselves.
  private def rawLines(resource: String): Array[String] = {
    val is = getClass.getResourceAsStream(resource)
    require(is != null, s"missing KAT resource $resource — run stark-kat/ generator")
    try Source.fromInputStream(is, "UTF-8").getLines().toArray
    finally is.close()
  }

  private def load[A](what: String, r: Either[String, A]): A = r match {
    case Right(a) => a
    case Left(e)  => fail(s"$what loader rejected valid table: $e")
  }

  private lazy val opsTable: PolyExtTable =
    load("ops", PolyExtTable.parse(rawLines("/stark-kats/circuit_polyext_ops.tsv").iterator))
  private lazy val tapSet: CircuitTapSet =
    load("taps", CircuitTapSet.parse(rawLines("/stark-kats/circuit_taps.tsv").iterator))

  /** The six `ck` checkpoint lines retained for this KAT, by
    * label. `eval_u` carries a leading count field; the rest are plain
    * comma-joined standard-form u32 lists.
    */
  private lazy val checkpoints: Map[String, Array[Int]] = {
    val wanted = Set("out", "mix", "poly_mix", "eval_u", "result", "check_value")
    val rows = rawLines("/stark-kats/polyext_transcript_oracle.tsv").iterator
      .filter(l => l.startsWith("ck\t"))
      .map(_.split("\t", -1))
      .toArray
    rows.length shouldBe wanted.size
    rows.map(_(1)).toSet shouldBe wanted
    val parsed = rows.iterator
      .map { f =>
        val values = f.last.split(",", -1).map(java.lang.Long.parseLong).map { v =>
          v should be >= 0L
          v should be < 2013265921L
          v.toInt
        }
        if (f(1) == "eval_u") f(2).toInt shouldBe values.length / 4
        (f(1), values)
      }
      .toMap
    parsed.keySet shouldBe wanted
    parsed
  }

  private def ext(words: Array[Int], at: Int): Ext4 =
    Ext4(words(at), words(at + 1), words(at + 2), words(at + 3))

  private def blake2b256Hex(bytes: Array[Byte]): String = {
    val d = new Blake2bDigest(256)
    d.update(bytes, 0, bytes.length)
    val out = new Array[Byte](32)
    d.doFinal(out, 0)
    out.map(b => "%02x".format(b & 0xff)).mkString
  }

  // ----- loaders -----

  test("taps loader matches the extracted recursion TapSet (643 taps / 163 regs / 12,23,128 columns)") {
    tapSet.tapSize shouldBe 643
    tapSet.regCount shouldBe 163
    tapSet.regs.length shouldBe 163
    tapSet.groupNames.toSeq shouldBe Seq("accum", "code", "data")
    tapSet.groupBegin.toSeq shouldBe Seq(0, 16, 39, 643)
    tapSet.groupSize.toSeq shouldBe Seq(12, 23, 128)
    tapSet.groupTapCount(0) shouldBe 16
    tapSet.groupTapCount(1) shouldBe 23
    tapSet.groupTapCount(2) shouldBe 604
    tapSet.combosCount shouldBe 5
    tapSet.comboBegin.toSeq shouldBe Seq(0, 1, 3, 9, 15, 20)
    tapSet.comboTaps.toSeq shouldBe Seq(0, 0, 1, 0, 1, 2, 3, 4, 68, 0, 1, 2, 7, 15, 16, 0, 2, 7, 15, 16)
    tapSet.totComboBacks shouldBe 20
    // Registers cover the taps exactly, in order, sized by their skip.
    tapSet.regs.map(_.size).sum shouldBe 643
    // Register back-lists are exactly what the eval_u loop consumes; spot-pin
    // the first accum register (backs 0,1) and every code register (back 0).
    tapSet.regs(0).backs.toSeq shouldBe Seq(0, 1)
    tapSet.regs.filter(_.group == 1).foreach { r => r.backs.toSeq shouldBe Seq(0) }
  }

  test("ops loader matches the extracted table shape and its Blake2b-256 integrity hash") {
    opsTable.opsCount shouldBe 12359
    opsTable.ret shouldBe 1228
    opsTable.fpVars shouldBe 11130
    opsTable.mixVars shouldBe 1229
    opsTable.opcodeHistogram shouldBe Map(
      "Add" -> 4061, "AndCond" -> 152, "AndEqz" -> 1076, "Const" -> 284,
      "Get" -> 669, "GetGlobal" -> 52, "Mul" -> 4679, "Sub" -> 1385, "True" -> 1)
    // Integrity: Blake2b-256 over the canonical re-serialization of the
    // PARSED ops must equal the hash the generator recorded from the Rust
    // table (proves parse fidelity, not just file integrity).
    blake2b256Hex(opsTable.canonicalBytes) shouldBe opsTable.blake2b256Hex
    // Every Get stays inside the tapset; the table references its full width.
    val getTaps = opsTable.ops.collect { case PolyExtOp.Get(t) => t }
    getTaps.max should be < tapSet.tapSize
  }

  test("parsed operation tables own their backing array") {
    val canonical = opsTable.canonicalBytes
    val exposed = opsTable.ops
    exposed(0) = null
    opsTable.ops(0) should not be null
    opsTable.canonicalBytes.toSeq shouldBe canonical.toSeq
  }

  // ----- oracle parity -----

  test("interpreter reproduces the recorded constraint evaluation of the real devnet receipt") {
    val out = checkpoints("out")
    val mix = checkpoints("mix")
    out.length shouldBe 32
    mix.length shouldBe 20
    val polyMix = ext(checkpoints("poly_mix"), 0)
    val evalUWords = checkpoints("eval_u")
    evalUWords.length shouldBe tapSet.tapSize * 4
    val u = Array.tabulate(tapSet.tapSize)(i => ext(evalUWords, i * 4))

    val result = PolyExtInterpreter.step(opsTable, polyMix, u, Array(out, mix)) match {
      case Right(ms) => ms
      case Left(e)   => fail(s"interpreter rejected the recorded inputs: $e")
    }
    result.tot shouldBe ext(checkpoints("result"), 0)
    // The verifier accepted, so the recorded check polynomial equals the
    // recorded result — pin that consistency of the capture itself too.
    checkpoints("check_value").toSeq shouldBe checkpoints("result").toSeq
  }

  // ----- error paths -----

  private val tinyTable = Seq(
    "meta\tops_count\t5",
    "meta\tret\t1",
    "meta\tfp_vars\t3",
    "meta\tmix_vars\t2",
    "meta\topcode_histogram\tAndEqz=1,Const=2,Sub=1,True=1",
    "meta\tblake2b256\t00",
    "op\t0\tConst\t5",
    "op\t1\tConst\t5",
    "op\t2\tSub\t0,1",
    "op\t3\tTrue\t-",
    "op\t4\tAndEqz\t0,2"
  )

  test("ops loader accepts a well-formed tiny table and the interpreter evaluates it") {
    val t = load("tiny", PolyExtTable.parse(tinyTable.iterator))
    val polyMix = Ext4(7, 0, 3, 0)
    // (5 - 5) folded through True gives tot = 0, mul = polyMix.
    PolyExtInterpreter.step(t, polyMix, Array.empty[Ext4], Array(Array.empty[Int], Array.empty[Int])) match {
      case Right(ms) =>
        ms.tot shouldBe Ext4.Zero
        ms.mul shouldBe polyMix
      case Left(e) => fail(s"tiny table rejected: $e")
    }
  }

  test("interpreter rejects non-canonical Const and ConstExt values instead of reducing them") {
    def mutate(from: String, to: String): Seq[String] =
      tinyTable.map(line => if (line == from) to else line)

    val badConst = load("non-canonical Const", PolyExtTable.parse(
      mutate("op\t0\tConst\t5", "op\t0\tConst\t2013265921").iterator))
    PolyExtInterpreter.step(
      badConst,
      Ext4.One,
      Array.empty[Ext4],
      Array(Array.empty[Int], Array.empty[Int])).isLeft shouldBe true

    val constExtTable = Seq(
      "meta\tops_count\t2",
      "meta\tret\t0",
      "meta\tfp_vars\t1",
      "meta\tmix_vars\t1",
      "meta\topcode_histogram\tConstExt=1,True=1",
      "meta\tblake2b256\t00",
      "op\t0\tConstExt\t0,0,2013265921,0",
      "op\t1\tTrue\t-")
    val badConstExt = load("non-canonical ConstExt", PolyExtTable.parse(constExtTable.iterator))
    PolyExtInterpreter.step(
      badConstExt,
      Ext4.One,
      Array.empty[Ext4],
      Array(Array.empty[Int], Array.empty[Int])).isLeft shouldBe true
  }

  test("ops loader requires the explicit no-operands sentinel and rejects ambiguous whitespace") {
    def mutate(from: String, to: String): Seq[String] =
      tinyTable.map(l => if (l == from) to else l)

    PolyExtTable.parse(mutate("op\t3\tTrue\t-", "op\t3\tTrue\t").iterator).isLeft shouldBe true
    PolyExtTable.parse(mutate("op\t3\tTrue\t-", "op\t3\tTrue\t- ").iterator).isLeft shouldBe true
    PolyExtTable.parse(mutate("op\t3\tTrue\t-", "op\t3\tTrue\t0").iterator).isLeft shouldBe true
    PolyExtTable.parse(mutate("op\t0\tConst\t5", "op\t0\tConst\t-").iterator).isLeft shouldBe true
    PolyExtTable.parse(mutate("op\t0\tConst\t5", "op\t0\t\t5").iterator).isLeft shouldBe true
    PolyExtTable.parse(mutate("meta\tret\t1", "meta\tret\t").iterator).isLeft shouldBe true
  }

  test("ops loader rejects forward stack references, bad mnemonics, and shape mismatches") {
    def mutate(replace: (String, String)*): Seq[String] =
      tinyTable.map(l => replace.foldLeft(l) { case (acc, (from, to)) => if (acc == from) to else acc })
    // Sub referencing an fp var that is not pushed yet.
    PolyExtTable.parse(mutate("op\t2\tSub\t0,1" -> "op\t2\tSub\t0,2").iterator).isLeft shouldBe true
    // AndEqz referencing a mix var that is not pushed yet.
    PolyExtTable.parse(mutate("op\t4\tAndEqz\t0,2" -> "op\t4\tAndEqz\t1,2").iterator).isLeft shouldBe true
    // Unknown mnemonic.
    PolyExtTable.parse(mutate("op\t0\tConst\t5" -> "op\t0\tFrobnicate\t5").iterator).isLeft shouldBe true
    // Non-numeric operand.
    PolyExtTable.parse(mutate("op\t0\tConst\t5" -> "op\t0\tConst\tx").iterator).isLeft shouldBe true
    // Out-of-order op index.
    PolyExtTable.parse(mutate("op\t1\tConst\t5" -> "op\t9\tConst\t5").iterator).isLeft shouldBe true
    // Truncated table: meta counts no longer match.
    PolyExtTable.parse(tinyTable.dropRight(1).iterator).isLeft shouldBe true
    // ret not the last mix var.
    PolyExtTable.parse(mutate("meta\tret\t1" -> "meta\tret\t0").iterator).isLeft shouldBe true
    // Histogram mismatch.
    PolyExtTable.parse(
      mutate("meta\topcode_histogram\tAndEqz=1,Const=2,Sub=1,True=1" ->
        "meta\topcode_histogram\tAndEqz=1,Const=1,Sub=2,True=1").iterator).isLeft shouldBe true
  }

  test("interpreter rejects out-of-range taps and globals with Left, never throws") {
    val polyMix = Ext4(1, 2, 3, 4)
    val okArgs = Array(new Array[Int](32), new Array[Int](20))
    // Real table, u too short for its Get ops.
    PolyExtInterpreter.step(opsTable, polyMix, Array.empty[Ext4], okArgs).isLeft shouldBe true
    val u = Array.fill(tapSet.tapSize)(Ext4.Zero)
    // Missing mix-globals buffer.
    PolyExtInterpreter.step(opsTable, polyMix, u, Array(new Array[Int](32))).isLeft shouldBe true
    // Mix-globals buffer too short.
    PolyExtInterpreter.step(opsTable, polyMix, u, Array(new Array[Int](32), new Array[Int](19))).isLeft shouldBe true
    // Non-canonical global value.
    val badOut = new Array[Int](32); badOut(0) = 2013265921
    PolyExtInterpreter.step(opsTable, polyMix, u, Array(badOut, new Array[Int](20))).isLeft shouldBe true
    // All-zero canonical inputs are structurally fine and must evaluate.
    PolyExtInterpreter.step(opsTable, polyMix, u, okArgs).isRight shouldBe true
  }

  test("interpreter rejects every negative or mutated operand with Left") {
    def table(
        ops: Array[PolyExtOp],
        ret: Int,
        fpVars: Int,
        mixVars: Int): PolyExtTable =
      PolyExtTable.fromValidatedBinary(ops, ret, fpVars, mixVars, Map.empty)

    val globals = Array(Array(0), Array(0))
    val taps = Array(Ext4.Zero)
    val getNegative = table(Array(PolyExtOp.Get(-1), PolyExtOp.True), 0, 1, 1)
    PolyExtInterpreter.step(getNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val globalArgNegative = table(
      Array(PolyExtOp.GetGlobal(-1, 0), PolyExtOp.True), 0, 1, 1)
    PolyExtInterpreter.step(globalArgNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val globalOffsetNegative = table(
      Array(PolyExtOp.GetGlobal(0, -1), PolyExtOp.True), 0, 1, 1)
    PolyExtInterpreter.step(globalOffsetNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val fpNegative = table(
      Array(PolyExtOp.Const(1), PolyExtOp.Add(-1, 0), PolyExtOp.True), 0, 2, 1)
    PolyExtInterpreter.step(fpNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val mixNegative = table(
      Array(PolyExtOp.Const(1), PolyExtOp.True, PolyExtOp.AndEqz(-1, 0)),
      1,
      1,
      2)
    PolyExtInterpreter.step(mixNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val conditionNegative = table(
      Array(PolyExtOp.Const(1), PolyExtOp.True, PolyExtOp.AndCond(0, -1, 0)),
      1,
      1,
      2)
    PolyExtInterpreter.step(conditionNegative, Ext4.One, taps, globals).isLeft shouldBe true

    val nullOp = table(Array[PolyExtOp](null, PolyExtOp.True), 0, 0, 1)
    PolyExtInterpreter.step(nullOp, Ext4.One, taps, globals).isLeft shouldBe true
    PolyExtInterpreter.step(null, Ext4.One, taps, globals).isLeft shouldBe true
    PolyExtInterpreter.step(getNegative, null, taps, globals).isLeft shouldBe true
    PolyExtInterpreter.step(getNegative, Ext4.One, null, globals).isLeft shouldBe true
    PolyExtInterpreter.step(getNegative, Ext4.One, taps, null).isLeft shouldBe true
    val validGlobal = table(
      Array(PolyExtOp.GetGlobal(0, 0), PolyExtOp.True), 0, 1, 1)
    PolyExtInterpreter.step(
      validGlobal,
      Ext4.One,
      taps,
      Array[Array[Int]](null)).isLeft shouldBe true
  }

  test("taps loader rejects malformed rows with Left, never throw") {
    val tapLines = rawLines("/stark-kats/circuit_taps.tsv")
    def replaceMeta(key: String, value: String): Array[String] =
      tapLines.map { line =>
        if (line.startsWith("meta\t" + key + "\t")) "meta\t" + key + "\t" + value
        else line
      }

    // Offset outside the group's column count.
    CircuitTapSet.parse(tapLines.map(l =>
      if (l == "tap\t16\t1\t0\t0\t0\t1") "tap\t16\t1\t23\t0\t0\t1" else l).iterator).isLeft shouldBe true
    // Broken register run (skip overrun at the last tap).
    CircuitTapSet.parse(tapLines.map(l =>
      if (l.startsWith("tap\t642\t")) l.dropRight(1) + "9" else l).iterator).isLeft shouldBe true
    // Dropped tap: group_begin no longer matches.
    CircuitTapSet.parse(tapLines.filterNot(_.startsWith("tap\t642\t")).iterator).isLeft shouldBe true

    // Every range is checked before copyOfRange or allocation.
    CircuitTapSet.parse(
      replaceMeta("combo_begin", "0,-1,3,9,15,20").iterator).isLeft shouldBe true
    CircuitTapSet.parse(
      replaceMeta("combo_begin", "0,3,2,9,15,20").iterator).isLeft shouldBe true
    CircuitTapSet.parse(
      replaceMeta("group_begin", "0,16,10,643").iterator).isLeft shouldBe true
    CircuitTapSet.parse(
      replaceMeta("combos_count", "-1").iterator).isLeft shouldBe true

    val hugeSkip = tapLines.map { line =>
      if (line.startsWith("tap\t0\t")) {
        val fields = line.split("\t", -1)
        fields(6) = Int.MaxValue.toString
        fields.mkString("\t")
      }
      else line
    }
    CircuitTapSet.parse(hugeSkip.iterator).isLeft shouldBe true

    val negativeBack = tapLines.map { line =>
      if (line.startsWith("tap\t0\t")) {
        val fields = line.split("\t", -1)
        fields(4) = "-1"
        fields.mkString("\t")
      }
      else line
    }
    CircuitTapSet.parse(negativeBack.iterator).isLeft shouldBe true
    CircuitTapSet.parse(null).isLeft shouldBe true
    CircuitTapSet.parse(Iterator[String](null)).isLeft shouldBe true
    PolyExtTable.parse(null).isLeft shouldBe true
    PolyExtTable.parse(Iterator[String](null)).isLeft shouldBe true
    PolyExtTable.parse((tinyTable :+ "meta\tret\t1").iterator).isLeft shouldBe true
  }
}
