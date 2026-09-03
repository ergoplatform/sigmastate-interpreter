/*
 * SPDX-License-Identifier: MIT AND Apache-2.0
 *
 * Contains generated data structures adapted from RISC Zero.
 * Copyright 2025 RISC Zero, Inc.
 * Modified and translated for Sigma State in 2026.
 * See NOTICE and docs/eip-0045-risc0-source-map.json.
 */
package sigma.stark.circuit

/** One tap row of the recursion circuit's `TapSet` (risc0-zkp 3.0.4
  * `src/taps.rs` `TapData`): column `offset` of register group `group`,
  * read `back` rows behind the current row, belonging to DEEP-ALI combo
  * `combo`; `skip` = number of taps in this tap's register (repeated on
  * every tap of the register, exactly as upstream stores it).
  */
final case class CircuitTap(group: Int, offset: Int, back: Int, combo: Int, skip: Int)

/** One register — a run of taps sharing (group, offset, combo), differing
  * only in `back`. This is the granularity the verifier consumes:
  * `verify_validity` walks registers in order, evaluating the register's
  * coeff_u slice at `z * back_one^back(i)` for each of its `size` backs
  * (risc0-zkp 3.0.4 `src/verify/mod.rs`, eval_u loop).
  */
final class TapRegister(val group: Int, val offset: Int, val combo: Int, val backs: Array[Int]) {
  def size: Int = backs.length
  def back(i: Int): Int = backs(i)
}

/** The recursion circuit's `TapSet`, parsed from `stark-kats/circuit_taps.tsv`
  * (extracted via the public `CIRCUIT.get_taps()`; schema in
  * `circuit_tables.md`).
  *
  * Group ids follow upstream: 0 = accum, 1 = code, 2 = data
  * (risc0-zkp 3.0.4 `src/adapter.rs:27-29`). Group g's taps are rows
  * `[groupBegin(g), groupBegin(g+1))`; `groupSize(g)` is its column count
  * (the Merkle-leaf row width). Combo c's back-set is
  * `comboTaps(comboBegin(c) until comboBegin(c+1))`.
  *
  * Arrays are exposed for indexed consumption and must not be mutated.
  */
final class CircuitTapSet(
    val groupNames: Array[String],
    val groupBegin: Array[Int],
    val groupSize: Array[Int],
    val regCount: Int,
    val combosCount: Int,
    val comboBegin: Array[Int],
    val comboTaps: Array[Int],
    val totComboBacks: Int,
    val taps: Array[CircuitTap],
    val regs: Array[TapRegister]
) {
  def tapSize: Int = taps.length
  def groupCount: Int = groupNames.length

  /** Number of taps in group `g`. */
  def groupTapCount(g: Int): Int = groupBegin(g + 1) - groupBegin(g)
}

object CircuitTapSet {

  /** Parse `circuit_taps.tsv` content. Total: malformed input yields `Left`,
    * never throws. Validates the register structure (the `skip` walk), tap
    * ordering against `group_begin`, offsets against `group_size`, and each
    * register's back-list against its combo's back-set.
    */
  def parse(lines: Iterator[String]): Either[String, CircuitTapSet] =
    if (lines == null) Left("circuit_taps: input iterator is null")
    else try parseChecked(lines)
    catch {
      case e: NumberFormatException => Left(s"circuit_taps: bad number: ${e.getMessage}")
    }

  private def parseChecked(lines: Iterator[String]): Either[String, CircuitTapSet] = {
    val meta = scala.collection.mutable.HashMap.empty[String, String]
    val taps = scala.collection.mutable.ArrayBuffer.empty[CircuitTap]

    while (lines.hasNext) {
      val line = lines.next()
      if (line == null) return Left("circuit_taps: null input row")
      if (line.nonEmpty && !line.startsWith("#")) {
        val f = line.split("\t", -1)
        f(0) match {
          case "meta" =>
            if (f.length != 3) return Left(s"circuit_taps: bad meta row: $line")
            if (meta.contains(f(1)))
              return Left(s"circuit_taps: duplicate meta '${f(1)}'")
            meta.put(f(1), f(2))
          case "tap" =>
            if (f.length != 7) return Left(s"circuit_taps: bad tap row: $line")
            val idx = f(1).toInt
            if (idx != taps.length)
              return Left(s"circuit_taps: tap index $idx out of order (expected ${taps.length})")
            taps += CircuitTap(f(2).toInt, f(3).toInt, f(4).toInt, f(5).toInt, f(6).toInt)
          case other => return Left(s"circuit_taps: unknown row kind '$other'")
        }
      }
    }

    val required = List("group_names", "group_begin", "group_size", "reg_count",
      "combos_count", "combo_begin", "combo_taps", "tot_combo_backs")
    val missing = required.filterNot(meta.contains)
    if (missing.nonEmpty) return Left(s"circuit_taps: missing meta ${missing.mkString(", ")}")

    // Number parses below throw NumberFormatException, converted to Left by
    // the `parse` boundary.
    def ints(key: String): Array[Int] = meta(key).split(",", -1).map(_.toInt)

    val groupNames = meta("group_names").split(",", -1)
    val groupBegin = ints("group_begin")
    val groupSize = ints("group_size")
    val regCount = meta("reg_count").toInt
    val combosCount = meta("combos_count").toInt
    val comboBegin = ints("combo_begin")
    val comboTaps = ints("combo_taps")
    val totComboBacks = meta("tot_combo_backs").toInt

    val g = groupNames.length
    if (g == 0 || groupNames.exists(_.isEmpty) || groupNames.distinct.length != g)
      return Left("circuit_taps: group_names must be nonempty and pairwise distinct")
    if (groupBegin.length != g + 1 || groupBegin(0) != 0)
      return Left(s"circuit_taps: group_begin must have ${g + 1} entries starting at 0")
    if (groupSize.length != g)
      return Left(s"circuit_taps: group_size must have $g entries")
    var boundary = 0
    while (boundary < g) {
      if (groupBegin(boundary) < 0 || groupBegin(boundary) > taps.length ||
          groupBegin(boundary + 1) < groupBegin(boundary) ||
          groupBegin(boundary + 1) > taps.length)
        return Left(s"circuit_taps: group_begin is out of range or non-monotone at $boundary")
      if (groupSize(boundary) <= 0)
        return Left(s"circuit_taps: group_size($boundary) must be positive")
      boundary += 1
    }
    if (groupBegin(g) != taps.length)
      return Left(s"circuit_taps: group_begin ends at ${groupBegin(g)} but ${taps.length} taps parsed")
    if (regCount < 0)
      return Left(s"circuit_taps: reg_count must be nonnegative, got $regCount")
    if (combosCount <= 0 || comboBegin.isEmpty ||
        combosCount != comboBegin.length - 1 || comboBegin(0) != 0)
      return Left(s"circuit_taps: combo_begin must contain one initial zero plus one end per positive combo count")
    var combo = 0
    while (combo < combosCount) {
      val begin = comboBegin(combo)
      val end = comboBegin(combo + 1)
      if (begin < 0 || begin > comboTaps.length ||
          end <= begin || end > comboTaps.length)
        return Left(s"circuit_taps: combo_begin is out of range or non-increasing at $combo")
      combo += 1
    }
    if (comboBegin(combosCount) != comboTaps.length)
      return Left(s"circuit_taps: combo_begin ends at ${comboBegin(combosCount)} but ${comboTaps.length} combo taps")
    if (totComboBacks < 0 || comboTaps.length != totComboBacks)
      return Left(s"circuit_taps: ${comboTaps.length} combo taps but tot_combo_backs=$totComboBacks")
    var i = 0
    while (i < comboTaps.length) {
      if (comboTaps(i) < 0)
        return Left(s"circuit_taps: combo tap $i is negative")
      i += 1
    }

    // Per-tap validation against group boundaries and column counts.
    i = 0
    while (i < taps.length) {
      val t = taps(i)
      if (t.group < 0 || t.group >= g)
        return Left(s"circuit_taps: tap $i: group ${t.group} not in [0, $g)")
      if (i < groupBegin(t.group) || i >= groupBegin(t.group + 1))
        return Left(s"circuit_taps: tap $i: group ${t.group} inconsistent with group_begin")
      if (t.offset < 0 || t.offset >= groupSize(t.group))
        return Left(s"circuit_taps: tap $i: offset ${t.offset} not in [0, ${groupSize(t.group)})")
      if (t.combo < 0 || t.combo >= combosCount)
        return Left(s"circuit_taps: tap $i: combo ${t.combo} not in [0, $combosCount)")
      if (t.back < 0)
        return Left(s"circuit_taps: tap $i: back ${t.back} is negative")
      if (t.skip < 1)
        return Left(s"circuit_taps: tap $i: skip ${t.skip} < 1")
      i += 1
    }

    // Register walk (upstream RegisterIter): a register starts at row 0 and
    // every `skip` rows thereafter; its taps must agree on
    // group/offset/combo/skip, and its back-list must equal its combo's
    // back-set.
    val regs = scala.collection.mutable.ArrayBuffer.empty[TapRegister]
    i = 0
    while (i < taps.length) {
      val head = taps(i)
      if (head.skip > taps.length - i)
        return Left(s"circuit_taps: register at tap $i overruns the table (skip ${head.skip})")
      val backs = new Array[Int](head.skip)
      var j = 0
      while (j < head.skip) {
        val t = taps(i + j)
        if (t.group != head.group || t.offset != head.offset ||
            t.combo != head.combo || t.skip != head.skip)
          return Left(s"circuit_taps: tap ${i + j} disagrees with its register head at $i")
        backs(j) = t.back
        j += 1
      }
      val comboBacks = java.util.Arrays.copyOfRange(comboTaps, comboBegin(head.combo), comboBegin(head.combo + 1))
      if (!java.util.Arrays.equals(backs, comboBacks))
        return Left(s"circuit_taps: register at tap $i: backs ${backs.mkString(",")} != combo ${head.combo} backs ${comboBacks.mkString(",")}")
      regs += new TapRegister(head.group, head.offset, head.combo, backs)
      i += head.skip
    }
    if (regs.length != regCount)
      return Left(s"circuit_taps: ${regs.length} registers derived but meta reg_count=$regCount")

    Right(new CircuitTapSet(groupNames, groupBegin, groupSize, regCount, combosCount,
      comboBegin, comboTaps, totComboBacks, taps.toArray, regs.toArray))
  }
}
