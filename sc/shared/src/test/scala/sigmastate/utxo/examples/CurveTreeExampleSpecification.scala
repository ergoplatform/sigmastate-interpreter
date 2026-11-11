package sigmastate.utxo.examples

import java.math.BigInteger
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext, ErgoLikeTransaction, Input}
import org.ergoplatform.ErgoBox.{R4, R5, R6, R7, R8, R9, Token}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.VersionContext
import sigma.VersionContext.V6SoftForkVersion
import sigma.ast._
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.syntax._
import sigma.crypto.{CryptoConstants, CryptoFacade, EcPointType}
import sigma.data.{AvlTreeData, CGroupElement, CSigmaDslBuilder, ProveDHTuple}
import sigma.data.CSigmaDslBuilder.Colls
import sigma.exceptions.InterpreterException
import sigma.interpreter.{ContextExtension, ProverResult}
import sigma.serialization.{ErgoTreeSerializer, SerializerException}
import sigma.util.Extensions.EcpOps
import sigma.Extensions.ArrayOps
import sigmastate.CompilerCrossVersionProps
import sigmastate.crypto.{DLogProtocol, DiffieHellmanTupleProverInput}
import sigmastate.exceptions.MethodNotFound
import sigmastate.helpers.{CompilerTestingCommons, ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.Interpreter.emptyEnv
import sigmastate.lang.parsers.ParserException
import sigmastate.utils.Helpers._

import scala.collection.compat.immutable.ArraySeq

/** Zero-knowledge set membership on-chain verification examples based on Curve Trees
  * (see https://github.com/ergoplatform/sigmastate-interpreter/issues/1111,
  * Campanelli et al., "Curve Trees: Practical and Transparent Zero-Knowledge Accumulators",
  * IACR ePrint 2022/756, and https://github.com/a-shannon/ergo-curve-trees).
  *
  * Curve Trees exploit the 2-cycle reciprocal relationship between `secp256k1` (`E1`) and
  * its sister Weierstrass curve `secq256k1` (`E2`, `y^2 = x^3 + 7` over `F_{p2}`):
  *   - `secp256k1` (`E1`): base field prime `p1`, scalar group order `n1 = p2`
  *   - `secq256k1` (`E2`): base field prime `p2 = n1`, scalar group order `n2 = p1`
  *
  * Consequently, any affine x-coordinate produced by a Pedersen commitment on `E1` lies in
  * `[0, p1 - 1] = Z_{n2}` and is natively a valid scalar on `E2` without range proofs or
  * non-native modular reduction, and any affine x-coordinate produced on `E2` lies in
  * `[0, p2 - 1] = Z_{n1}` and is natively a valid scalar on `E1`.
  *
  * This specification provides two self-contained ErgoScript v6 (`V6SoftForkVersion`)
  * contracts exercising on-chain Curve Tree verification:
  *
  *   1. `curveTreeAccumulatorScript`: Depth-3 2-cycle (`E1 -> E2 -> E1`, 8 leaves) Curve
  *      Tree membership verifier coupled with a native zero-knowledge `proveDHTuple` proof
  *      of secret key ownership and deterministic epoch nullifier (`nullifier = epochGen^sk`):
  *        - Uses ErgoScript v6 `Global.fromBigEndianBytes[UnsignedBigInt]` for unsigned
  *          256-bit big-endian decoding (avoiding the signed `byteArrayToBigInt` MSB trap).
  *        - Separates the `E1` scalar modulus (`n1 = p2`) from the `E2` scalar modulus
  *          (`n2 = p1`), preserving valid `E1` x-coordinates in `[p2, p1 - 1]`.
  *        - Enforces canonical Straus-Shamir 2-bit digit encoding (`bits_E2(0) in 1..3`,
  *          all digits in `0..3`, exact 256-bit `UnsignedBigInt` bit-pair reconstruction).
  *        - Evaluates `E2` multi-scalar multiplication `[s1]G1_E2 + [s2]G2_E2` in
  *          division-free homogeneous projective coordinates `(X : Y : Z)` over `F_{p2}`
  *          with prover-assisted affine conversion (`Z * Z^-1 == 1 mod p2`).
  *        - Rejects identity (`point at infinity`) encodings on `E1`.
  *        - Authenticates the spending transaction and emits a unique per-epoch nullifier
  *          via `proveDHTuple(groupGenerator, epochGen, memberPk, nullifier)`.
  *
  *   2. `curveTreeSelectAndRerandomizeScript`: Zero-knowledge 1-of-2 Select-and-Rerandomize
  *      branch proof on a Curve Tree parent node `R = [u0]G1_E1 + [u1]G2_E1`:
  *        - Outputs a freshly blinded commitment `C_u = [u_b]H1_E1 + [gamma]K1_E1` for a
  *          secret branch bit `b in {0, 1}` and proves via a 1-of-2 Cramer-Damgard-
  *          Schoenmakers (CDS) generalized Schnorr OR-proof (bound via Fiat-Shamir to
  *          `SELF.id` and `OUTPUTS(0).propositionBytes`) that `C_u` commits to either `u0`
  *          or `u1` while hiding `u0`, `u1`, and `b`.
  */
class CurveTreeExampleSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps {

  override val printVersions: Boolean = false
  override protected val activatedVersions: Seq[Byte] = Seq(V6SoftForkVersion)
  override val ergoTreeVersions: Seq[Byte] = Seq(V6SoftForkVersion)

  private implicit lazy val IR: TestingIRContext = new TestingIRContext

  /** Base field prime `p1` of `secp256k1` (`E1`), equal to the scalar group order `n2` of `secq256k1` (`E2`). */
  val P1: BigInteger = new BigInteger("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", 16)

  /** Scalar group order `n1` of `secp256k1` (`E1`), equal to the base field prime `p2` of `secq256k1` (`E2`). */
  val P2: BigInteger = CryptoConstants.groupOrder

  private val Three = BigInteger.valueOf(3)
  private val Seven = BigInteger.valueOf(7)

  /** Affine point `(x, y)` on `secq256k1` (`y^2 = x^3 + 7 mod P2`). */
  case class SecqPoint(x: BigInteger, y: BigInteger) {
    require(x.signum() >= 0 && x.compareTo(P2) < 0, "secq256k1 x out of field range")
    require(y.signum() >= 0 && y.compareTo(P2) < 0, "secq256k1 y out of field range")
    require(
      y.multiply(y).mod(P2) == x.modPow(Three, P2).add(Seven).mod(P2),
      "Point does not lie on secq256k1"
    )
  }

  // Nothing-Up-My-Sleeve (NUMS) generators on secp256k1 (E1) and secq256k1 (E2),
  // matching https://github.com/a-shannon/ergo-curve-trees/blob/main/prover/src/generators.ts
  val G1SecpHex: String = "02bb64ecb2b444cd8a669d32fb1ad42fc8bf7133fae09498afc08d2c274b6f8749"
  val G2SecpHex: String = "03d44600e142f2e76580d64b8de085ed2f37e725119e451e1de11f5bf751582c4a"

  lazy val G1Secp: EcPointType = decodeSecpHex(G1SecpHex)
  lazy val G2Secp: EcPointType = decodeSecpHex(G2SecpHex)

  // Additional independent NUMS generators on secp256k1 derived transparently via try-and-increment
  // on Blake2b256("Ergo_CurveTrees_secp256k1_<label>_<nonce>").
  lazy val H1Secp: EcPointType = deriveNumsSecpPoint("Ergo_CurveTrees_secp256k1_H1")
  lazy val K1Secp: EcPointType = deriveNumsSecpPoint("Ergo_CurveTrees_secp256k1_K1")
  lazy val EpochGenSecp: EcPointType = deriveNumsSecpPoint("Ergo_CurveTrees_secp256k1_Epoch_2026")

  val G1Secq: SecqPoint = SecqPoint(
    new BigInteger("365d2cd4a0c1785a2cdba6b7076f9d17a256f8727564016c8fcac776be416774", 16),
    new BigInteger("7399fcaa2eb909a36b6d84497f1b6f2658e0409a9d681719ff8a31060ec61c9d", 16)
  )

  val G2Secq: SecqPoint = SecqPoint(
    new BigInteger("f80d8a3ea7a5b8762206d7fb81b211880c64aab7bd22842bb9c4891e4d4f2b3", 16),
    new BigInteger("794d05bcd921034ef32aeda0104425dd9ce0b788e307ce02a4fe190d9e1edfea", 16)
  )

  lazy val G12Secq: SecqPoint = secqAdd(Some(G1Secq), Some(G2Secq)).get

  private def decodeSecpHex(hex: String): EcPointType = {
    val bytes = Base16.decode(hex).getOrThrow
    CSigmaDslBuilder.decodePoint(bytes.toColl).asInstanceOf[CGroupElement].wrappedValue
  }

  /** Deterministically derives a NUMS point on `secp256k1` (`p1 == 3 mod 4`) from an ASCII domain tag. */
  private def deriveNumsSecpPoint(domainPrefix: String): EcPointType = {
    val exp = P1.add(BigInteger.ONE).shiftRight(2)
    var nonce = 0
    var found: Option[EcPointType] = None
    while (found.isEmpty) {
      val seed = s"${domainPrefix}_$nonce".getBytes("UTF-8")
      val x = new BigInteger(1, Blake2b256.hash(seed))
      if (x.signum() > 0 && x.compareTo(P1) < 0) {
        val rhs = x.modPow(Three, P1).add(Seven).mod(P1)
        val y = rhs.modPow(exp, P1)
        if (y.multiply(y).mod(P1) == rhs) {
          val evenY = if (y.testBit(0)) P1.subtract(y) else y
          val compressed = (if (evenY.testBit(0)) 0x03.toByte else 0x02.toByte) +: toFixed32Bytes(x)
          found = Some(CSigmaDslBuilder.decodePoint(compressed.toColl).asInstanceOf[CGroupElement].wrappedValue)
        }
      }
      nonce += 1
    }
    found.get
  }

  /** Big-endian 32-byte unsigned encoding for a non-negative 256-bit integer. */
  def toFixed32Bytes(value: BigInteger): Array[Byte] = {
    require(value.signum() >= 0, s"Negative integer cannot be encoded as unsigned 32 bytes: $value")
    val raw = value.toByteArray
    if (raw.length == 32) {
      raw
    } else if (raw.length == 33 && raw(0) == 0.toByte) {
      raw.drop(1)
    } else {
      require(raw.length < 32, s"Integer exceeds 32 unsigned bytes (${raw.length}): $value")
      Array.fill(32 - raw.length)(0.toByte) ++ raw
    }
  }

  private def secpXCoord(point: EcPointType): BigInteger = {
    require(!CryptoFacade.isInfinityPoint(point), "Unexpected infinity point on secp256k1")
    val encoded = point.toGroupElement.getEncoded.toArray
    new BigInteger(1, encoded.slice(1, 33))
  }

  // --- Off-chain secq256k1 (E2) reference arithmetic over F_{P2} ---

  private def fieldAddP2(a: BigInteger, b: BigInteger): BigInteger = a.add(b).mod(P2)
  private def fieldSubP2(a: BigInteger, b: BigInteger): BigInteger = a.subtract(b).mod(P2).add(P2).mod(P2)
  private def fieldMulP2(a: BigInteger, b: BigInteger): BigInteger = a.multiply(b).mod(P2)
  private def fieldInvP2(a: BigInteger): BigInteger = a.modInverse(P2)

  def secqDouble(pOpt: Option[SecqPoint]): Option[SecqPoint] = pOpt.flatMap { p =>
    if (p.y.signum() == 0) None
    else {
      val num = fieldMulP2(Three, fieldMulP2(p.x, p.x))
      val den = fieldInvP2(fieldMulP2(BigInteger.valueOf(2), p.y))
      val lambda = fieldMulP2(num, den)
      val x3 = fieldSubP2(fieldSubP2(fieldMulP2(lambda, lambda), p.x), p.x)
      val y3 = fieldSubP2(fieldMulP2(lambda, fieldSubP2(p.x, x3)), p.y)
      Some(SecqPoint(x3, y3))
    }
  }

  def secqAdd(pOpt: Option[SecqPoint], qOpt: Option[SecqPoint]): Option[SecqPoint] = (pOpt, qOpt) match {
    case (None, q) => q
    case (p, None) => p
    case (Some(p), Some(q)) =>
      if (p.x == q.x) {
        if (fieldAddP2(p.y, q.y).signum() == 0) None
        else secqDouble(Some(p))
      } else {
        val lambda = fieldMulP2(fieldSubP2(q.y, p.y), fieldInvP2(fieldSubP2(q.x, p.x)))
        val x3 = fieldSubP2(fieldSubP2(fieldMulP2(lambda, lambda), p.x), q.x)
        val y3 = fieldSubP2(fieldMulP2(lambda, fieldSubP2(p.x, x3)), p.y)
        Some(SecqPoint(x3, y3))
      }
  }

  def secqScalarMul(base: SecqPoint, scalar: BigInteger): Option[SecqPoint] = {
    var k = scalar.mod(P1).add(P1).mod(P1)
    var acc: Option[SecqPoint] = None
    var cur: Option[SecqPoint] = Some(base)
    while (k.signum() > 0) {
      if (k.testBit(0)) acc = secqAdd(acc, cur)
      cur = secqDouble(cur)
      k = k.shiftRight(1)
    }
    acc
  }

  /** Pedersen hash `H_{E1}(s1, s2) = x([s1]g1 + [s2]g2) in F_{P1} = Z_{N2}`. */
  def pedersenHashE1(
      s1: BigInteger,
      s2: BigInteger,
      g1: EcPointType = G1Secp,
      g2: EcPointType = G2Secp): BigInteger = {
    val p1 = CryptoConstants.dlogGroup.exponentiate(g1, s1.mod(P2))
    val p2 = CryptoConstants.dlogGroup.exponentiate(g2, s2.mod(P2))
    val sum = CryptoConstants.dlogGroup.multiplyGroupElements(p1, p2)
    secpXCoord(sum)
  }

  /** Pedersen hash `H_{E2}(s1, s2) = x([s1]G1_E2 + [s2]G2_E2) in F_{P2} = Z_{N1}`. */
  def pedersenHashE2(s1: BigInteger, s2: BigInteger): BigInteger = {
    val p1 = secqScalarMul(G1Secq, s1)
    val p2 = secqScalarMul(G2Secq, s2)
    val sum = secqAdd(p1, p2).getOrElse(sys.error("Hit point at infinity in E2 Pedersen hash"))
    sum.x
  }

  /** Strips leading zero 2-bit digits and packs `(s1, s2)` into canonical Straus-Shamir bytes in `{0, 1, 2, 3}`. */
  def packStrausShamirDigits(s1: BigInteger, s2: BigInteger): Array[Byte] = {
    require(s1.signum() >= 0 && s1.compareTo(P1) < 0, "s1 out of E2 scalar range [0, P1)")
    require(s2.signum() >= 0 && s2.compareTo(P1) < 0, "s2 out of E2 scalar range [0, P1)")
    val buf = scala.collection.mutable.ArrayBuffer.empty[Byte]
    var started = false
    var i = 255
    while (i >= 0) {
      val b1 = if (s1.testBit(i)) 1 else 0
      val b2 = if (s2.testBit(i)) 1 else 0
      val digit = ((b1 << 1) | b2).toByte
      if (digit != 0 || started) {
        started = true
        buf += digit
      }
      i -= 1
    }
    require(buf.nonEmpty, "Both E2 scalars cannot be zero")
    buf.toArray
  }

  /** Mirrors the on-chain homogeneous projective Straus-Shamir fold on `E2` to compute `Z` for `Z^-1 mod P2`. */
  def projectiveStrausShamirFold(digits: Array[Byte]): (BigInteger, BigInteger, BigInteger) = {
    val Two = BigInteger.valueOf(2)
    val Four = BigInteger.valueOf(4)
    val Eight = BigInteger.valueOf(8)

    def projDouble(x1: BigInteger, y1: BigInteger, z1: BigInteger): (BigInteger, BigInteger, BigInteger) = {
      val w = fieldMulP2(Three, fieldMulP2(x1, x1))
      val s = fieldMulP2(y1, z1)
      val b = fieldMulP2(fieldMulP2(x1, y1), s)
      val h = fieldSubP2(fieldMulP2(w, w), fieldMulP2(Eight, b))
      val s2 = fieldMulP2(s, s)
      val dx = fieldMulP2(fieldMulP2(Two, h), s)
      val dy = fieldSubP2(
        fieldMulP2(w, fieldSubP2(fieldMulP2(Four, b), h)),
        fieldMulP2(fieldMulP2(Eight, fieldMulP2(y1, y1)), s2)
      )
      val dz = fieldMulP2(Eight, fieldMulP2(s2, s))
      (dx, dy, dz)
    }

    def projAddAffine(
        x1: BigInteger, y1: BigInteger, z1: BigInteger,
        px: BigInteger, py: BigInteger): (BigInteger, BigInteger, BigInteger) = {
      val u1 = fieldMulP2(py, z1)
      val u2 = y1
      val v1 = fieldMulP2(px, z1)
      val v2 = x1
      val ud = fieldSubP2(u1, u2)
      val vd = fieldSubP2(v1, v2)
      val vsq = fieldMulP2(vd, vd)
      val vcb = fieldMulP2(vsq, vd)
      val vsqV2 = fieldMulP2(vsq, v2)
      val u2w = fieldMulP2(fieldMulP2(ud, ud), z1)
      val twoVsqV2 = fieldAddP2(vsqV2, vsqV2)
      val ar = fieldSubP2(u2w, fieldAddP2(vcb, twoVsqV2))
      val rx = fieldMulP2(vd, ar)
      val ry = fieldSubP2(fieldMulP2(ud, fieldSubP2(vsqV2, ar)), fieldMulP2(vcb, u2))
      val rz = fieldMulP2(vcb, z1)
      (rx, ry, rz)
    }

    val start = digits(0).toInt match {
      case 3 => (G12Secq.x, G12Secq.y, BigInteger.ONE)
      case 2 => (G1Secq.x, G1Secq.y, BigInteger.ONE)
      case 1 => (G2Secq.x, G2Secq.y, BigInteger.ONE)
      case other => sys.error(s"Invalid leading Straus-Shamir digit: $other")
    }

    digits.drop(1).foldLeft(start) { case ((ax, ay, az), b) =>
      val (dx, dy, dz) = projDouble(ax, ay, az)
      b.toInt match {
        case 0 => (dx, dy, dz)
        case 3 => projAddAffine(dx, dy, dz, G12Secq.x, G12Secq.y)
        case 2 => projAddAffine(dx, dy, dz, G1Secq.x, G1Secq.y)
        case 1 => projAddAffine(dx, dy, dz, G2Secq.x, G2Secq.y)
        case other => sys.error(s"Invalid Straus-Shamir digit: $other")
      }
    }
  }

  /** Depth-`d` alternating 2-cycle Curve Tree (`E1` on even levels `0, 2, ...`, `E2` on odd levels `1, 3, ...`). */
  case class BuiltCurveTree(
      depth: Int,
      levels: IndexedSeq[IndexedSeq[BigInteger]],
      g1E1: EcPointType,
      g2E1: EcPointType) {
    def root: BigInteger = levels(depth).head

    def proveLeaf(leafIndex: Int): CurveTreeWitness = {
      require(leafIndex >= 0 && leafIndex < levels.head.length, s"Leaf index out of range: $leafIndex")
      var idx = leafIndex
      val siblings = IndexedSeq.tabulate(depth) { d =>
        val sibIdx = if (idx % 2 == 1) idx - 1 else idx + 1
        idx = idx / 2
        levels(d)(sibIdx)
      }
      idx = leafIndex
      val directions = Array.tabulate(depth) { _ =>
        val dir: Byte = if (idx % 2 == 0) 1.toByte else 0.toByte
        idx = idx / 2
        dir
      }
      val l0Out = levels(1)(leafIndex / 2)
      val l1Sib = siblings(1)
      val (s1L1, s2L1) = if (directions(1) == 1.toByte) (l0Out, l1Sib) else (l1Sib, l0Out)
      val bitsE2 = packStrausShamirDigits(s1L1, s2L1)
      val (projX, _, projZ) = projectiveStrausShamirFold(bitsE2)
      val zInv = fieldInvP2(projZ)
      require(fieldMulP2(projX, zInv) == levels(2)(leafIndex / 4), "Projective fold x mismatch")

      CurveTreeWitness(
        siblingsBytes = siblings.toArray.flatMap(toFixed32Bytes),
        directionBytes = directions,
        strausShamirBytes = bitsE2,
        zInvBytes = toFixed32Bytes(zInv)
      )
    }
  }

  case class CurveTreeWitness(
      siblingsBytes: Array[Byte],
      directionBytes: Array[Byte],
      strausShamirBytes: Array[Byte],
      zInvBytes: Array[Byte])

  def buildCurveTree(
      leaves: IndexedSeq[BigInteger],
      g1E1: EcPointType = G1Secp,
      g2E1: EcPointType = G2Secp): BuiltCurveTree = {
    require(leaves.length == 8, s"Depth-3 Curve Tree requires 8 leaves, got ${leaves.length}")
    val level1 = IndexedSeq.tabulate(4)(i => pedersenHashE1(leaves(2 * i), leaves(2 * i + 1), g1E1, g2E1))
    val level2 = IndexedSeq.tabulate(2)(i => pedersenHashE2(level1(2 * i), level1(2 * i + 1)))
    val level3 = IndexedSeq(pedersenHashE1(level2(0), level2(1), g1E1, g2E1))
    BuiltCurveTree(3, IndexedSeq(leaves, level1, level2, level3), g1E1, g2E1)
  }

  /** Contract 1: Depth-3 (`E1 -> E2 -> E1`) Curve Tree set-membership verifier with
    * zero-knowledge `proveDHTuple` secret-key ownership and deterministic epoch nullifier.
    *
    * SELF registers:
    *   R4: Coll[Byte]   — 32-byte big-endian Curve Tree root x-coordinate
    *   R5: GroupElement — E1 Pedersen generator G1_E1
    *   R6: GroupElement — E1 Pedersen generator G2_E1
    *   R7: Coll[Byte]   — 128-byte E2 generators `g1x || g1y || g2x || g2y`
    *   R8: Coll[Byte]   — 64-byte precomputed `G12_E2 = G1_E2 + G2_E2` (`g12x || g12y`)
    *   R9: GroupElement — Epoch generator `U_epoch` for deterministic nullifiers
    *
    * OUTPUTS(0) registers:
    *   R4: GroupElement — Deterministic nullifier `N = U_epoch^sk`
    *
    * Context extension:
    *   var(0): GroupElement — Member public key `PK = g^sk` (leaf scalar = `x(PK) mod n1`)
    *   var(1): Coll[Byte]   — 96-byte sibling coordinates (`sib0 || sib1 || sib2`)
    *   var(2): Coll[Byte]   — 3-byte path directions (`1` = left child, `0` = right child)
    *   var(3): Coll[Byte]   — Canonical Straus-Shamir 2-bit digits (`1..256` bytes)
    *   var(4): Coll[Byte]   — 32-byte prover-assisted modular inverse `Z^-1 mod p2`
    */
  val curveTreeAccumulatorScript: String =
    """{
      |  val rootExpected = SELF.R4[Coll[Byte]].get
      |  val G1_E1        = SELF.R5[GroupElement].get
      |  val G2_E1        = SELF.R6[GroupElement].get
      |  val e2g          = SELF.R7[Coll[Byte]].get
      |  val g12b         = SELF.R8[Coll[Byte]].get
      |  val epochGen     = SELF.R9[GroupElement].get
      |
      |  val memberPk  = getVar[GroupElement](0).get
      |  val sibBytes  = getVar[Coll[Byte]](1).get
      |  val dirs      = getVar[Coll[Byte]](2).get
      |  val bits_E2   = getVar[Coll[Byte]](3).get
      |  val zInvBytes = getVar[Coll[Byte]](4).get
      |
      |  // p2 = n1 (secp256k1 scalar order = secq256k1 base field prime)
      |  // n2 = p1 (secq256k1 scalar order = secp256k1 base field prime)
      |  val p2 = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
      |  val n2 = unsignedBigInt("115792089237316195423570985008687907853269984665640564039457584007908834671663")
      |
      |  val uZero  = unsignedBigInt("0")
      |  val uOne   = unsignedBigInt("1")
      |  val uTwo   = unsignedBigInt("2")
      |  val uThree = unsignedBigInt("3")
      |  val uFour  = unsignedBigInt("4")
      |  val uEight = unsignedBigInt("8")
      |
      |  val pkEnc = memberPk.getEncoded
      |  val shapesOk =
      |    rootExpected.size == 32 &&
      |    e2g.size == 128 &&
      |    g12b.size == 64 &&
      |    pkEnc(0) != 0.toByte &&
      |    sibBytes.size == 96 &&
      |    dirs.size == 3 &&
      |    bits_E2.size >= 1 &&
      |    bits_E2.size <= 256 &&
      |    zInvBytes.size == 32 &&
      |    OUTPUTS.size >= 1 &&
      |    OUTPUTS(0).R4[GroupElement].isDefined
      |
      |  val treeValid = if (shapesOk) {
      |    val dirsValid = dirs.forall({ (d: Byte) => d == 0.toByte || d == 1.toByte })
      |    val bitsHeaderValid = bits_E2(0) >= 1.toByte && bits_E2(0) <= 3.toByte
      |    val bitsAlphabetValid = bits_E2.forall({ (b: Byte) => b >= 0.toByte && b <= 3.toByte })
      |
      |    if (dirsValid && bitsHeaderValid && bitsAlphabetValid) {
      |      val g1x  = Global.fromBigEndianBytes[UnsignedBigInt](e2g.slice(0, 32))
      |      val g1y  = Global.fromBigEndianBytes[UnsignedBigInt](e2g.slice(32, 64))
      |      val g2x  = Global.fromBigEndianBytes[UnsignedBigInt](e2g.slice(64, 96))
      |      val g2y  = Global.fromBigEndianBytes[UnsignedBigInt](e2g.slice(96, 128))
      |      val g12x = Global.fromBigEndianBytes[UnsignedBigInt](g12b.slice(0, 32))
      |      val g12y = Global.fromBigEndianBytes[UnsignedBigInt](g12b.slice(32, 64))
      |
      |      val sib0 = Global.fromBigEndianBytes[UnsignedBigInt](sibBytes.slice(0, 32))
      |      val sib1 = Global.fromBigEndianBytes[UnsignedBigInt](sibBytes.slice(32, 64))
      |      val sib2 = Global.fromBigEndianBytes[UnsignedBigInt](sibBytes.slice(64, 96))
      |      val zInv = Global.fromBigEndianBytes[UnsignedBigInt](zInvBytes)
      |
      |      val rangesOk = sib0 < p2 && sib1 < n2 && sib2 < p2 && zInv < p2
      |
      |      if (rangesOk) {
      |        // Level 0 -> 1: Native secp256k1 (E1) Pedersen hash
      |        val leafScalar = Global.fromBigEndianBytes[UnsignedBigInt](pkEnc.slice(1, 33)).mod(p2)
      |        val s1_L0 = if (dirs(0) == 1.toByte) leafScalar else sib0
      |        val s2_L0 = if (dirs(0) == 1.toByte) sib0 else leafScalar
      |        val p_L0_enc = G1_E1.expUnsigned(s1_L0).multiply(G2_E1.expUnsigned(s2_L0)).getEncoded
      |
      |        // Level 1 -> 2: Simulated secq256k1 (E2) Pedersen hash over F_{p2}
      |        // Preserve full F_{p1} = Z_{n2} x-coordinate without reducing mod p2
      |        val out_L0 = Global.fromBigEndianBytes[UnsignedBigInt](p_L0_enc.slice(1, 33))
      |        val s1_L1 = if (dirs(1) == 1.toByte) out_L0 else sib1
      |        val s2_L1 = if (dirs(1) == 1.toByte) sib1 else out_L0
      |
      |        // Exact 256-bit unsigned bit-pair reconstruction (no overflow since size <= 256)
      |        val reconstructed = bits_E2.fold((uZero, uZero), { (acc: (UnsignedBigInt, UnsignedBigInt), b: Byte) =>
      |          val b1 = if (b == 2.toByte || b == 3.toByte) uOne else uZero
      |          val b2 = if (b == 1.toByte || b == 3.toByte) uOne else uZero
      |          (acc._1 * uTwo + b1, acc._2 * uTwo + b2)
      |        })
      |        val bitsValid = (reconstructed._1 == s1_L1) && (reconstructed._2 == s2_L1)
      |
      |        val firstB = bits_E2(0)
      |        val startX = if (firstB == 3.toByte) g12x else if (firstB == 2.toByte) g1x else g2x
      |        val startY = if (firstB == 3.toByte) g12y else if (firstB == 2.toByte) g1y else g2y
      |
      |        val sum_L1 = bits_E2.slice(1, bits_E2.size).fold(((startX, startY), uOne), {
      |          (acc: ((UnsignedBigInt, UnsignedBigInt), UnsignedBigInt), b: Byte) =>
      |            val aX = acc._1._1
      |            val aY = acc._1._2
      |            val aZ = acc._2
      |
      |            // Homogeneous projective doubling on y^2 = x^3 + 7 (a = 0) over F_{p2}
      |            val W  = uThree.multiplyMod(aX.multiplyMod(aX, p2), p2)
      |            val S  = aY.multiplyMod(aZ, p2)
      |            val Bb = aX.multiplyMod(aY, p2).multiplyMod(S, p2)
      |            val H  = W.multiplyMod(W, p2).subtractMod(uEight.multiplyMod(Bb, p2), p2)
      |            val S2 = S.multiplyMod(S, p2)
      |            val dX = uTwo.multiplyMod(H, p2).multiplyMod(S, p2)
      |            val dY = W.multiplyMod(uFour.multiplyMod(Bb, p2).subtractMod(H, p2), p2)
      |              .subtractMod(uEight.multiplyMod(aY.multiplyMod(aY, p2), p2).multiplyMod(S2, p2), p2)
      |            val dZ = uEight.multiplyMod(S2.multiplyMod(S, p2), p2)
      |
      |            if (b == 0.toByte) {
      |              ((dX, dY), dZ)
      |            } else {
      |              val px = if (b == 3.toByte) g12x else if (b == 2.toByte) g1x else g2x
      |              val py = if (b == 3.toByte) g12y else if (b == 2.toByte) g1y else g2y
      |
      |              // Mixed projective-affine addition (Z2 = 1) over F_{p2}
      |              val U1 = py.multiplyMod(dZ, p2)
      |              val U2 = dY
      |              val V1 = px.multiplyMod(dZ, p2)
      |              val V2 = dX
      |              val Ud = U1.subtractMod(U2, p2)
      |              val Vd = V1.subtractMod(V2, p2)
      |              val Vsq = Vd.multiplyMod(Vd, p2)
      |              val Vcb = Vsq.multiplyMod(Vd, p2)
      |              val VsqV2 = Vsq.multiplyMod(V2, p2)
      |              val UUsqW = Ud.multiplyMod(Ud, p2).multiplyMod(dZ, p2)
      |              val twoVsqV2 = VsqV2.plusMod(VsqV2, p2)
      |              val Ar = UUsqW.subtractMod(Vcb.plusMod(twoVsqV2, p2), p2)
      |              val rX = Vd.multiplyMod(Ar, p2)
      |              val rY = Ud.multiplyMod(VsqV2.subtractMod(Ar, p2), p2).subtractMod(Vcb.multiplyMod(U2, p2), p2)
      |              val rZ = Vcb.multiplyMod(dZ, p2)
      |              ((rX, rY), rZ)
      |            }
      |        })
      |
      |        // Prover-assisted affine conversion x = X * Z^-1 mod p2.
      |        // Any degenerate projective step (Vd == 0 or S == 0) sets Z = 0 absorptively,
      |        // which unconditionally fails `Z * zInv == 1 mod p2`.
      |        val inverseValid = sum_L1._2.multiplyMod(zInv, p2) == uOne
      |        val e2X = sum_L1._1._1.multiplyMod(zInv, p2)
      |
      |        // Level 2 -> 3: Native secp256k1 (E1) Pedersen root hash
      |        val s1_L2 = if (dirs(2) == 1.toByte) e2X else sib2
      |        val s2_L2 = if (dirs(2) == 1.toByte) sib2 else e2X
      |        val p_L2_enc = G1_E1.expUnsigned(s1_L2).multiply(G2_E1.expUnsigned(s2_L2)).getEncoded
      |
      |        val nonIdentityValid = p_L0_enc(0) != 0.toByte && p_L2_enc(0) != 0.toByte
      |        bitsValid && inverseValid && nonIdentityValid && (p_L2_enc.slice(1, 33) == rootExpected)
      |      } else false
      |    } else false
      |  } else false
      |
      |  val authProp = if (shapesOk) {
      |    val nullifier = OUTPUTS(0).R4[GroupElement].get
      |    val nullifierNonIdentity = nullifier.getEncoded(0) != 0.toByte
      |    sigmaProp(nullifierNonIdentity) && proveDHTuple(groupGenerator, epochGen, memberPk, nullifier)
      |  } else {
      |    sigmaProp(false)
      |  }
      |
      |  sigmaProp(treeValid) && authProp
      |}""".stripMargin

  /** Contract 2: Zero-knowledge 1-of-2 Select-and-Rerandomize branch proof on a Curve Tree
    * parent commitment `R = [u0]G1 + [u1]G2` (`E1`), producing a freshly blinded child
    * commitment `C_u = [u_b]H1 + [gamma]K1` in `OUTPUTS(0).R4[GroupElement]` without
    * revealing `u0`, `u1`, or the selected branch `b in {0, 1}`.
    *
    * SELF registers:
    *   R4: GroupElement — Parent Curve Tree node `R = [u0]G1 + [u1]G2`
    *   R5: GroupElement — `G1`
    *   R6: GroupElement — `G2`
    *   R7: GroupElement — `H1` (blinded child value generator)
    *   R8: GroupElement — `K1` (blinded child randomness generator)
    *
    * OUTPUTS(0) registers:
    *   R4: GroupElement — Rerandomized child commitment `C_u = [u_b]H1 + [gamma]K1`
    *
    * Context extension:
    *   var(0)..var(3): GroupElement — CDS OR-proof commitments `aR0, aC0, aR1, aC1`
    *   var(4): Coll[Byte]           — 256 bytes (`e0 | e1 | z00 | z10 | zg0 | z01 | z11 | zg1`)
    */
  val curveTreeSelectAndRerandomizeScript: String =
    """{
      |  val parentNode = SELF.R4[GroupElement].get
      |  val G1         = SELF.R5[GroupElement].get
      |  val G2         = SELF.R6[GroupElement].get
      |  val H1         = SELF.R7[GroupElement].get
      |  val K1         = SELF.R8[GroupElement].get
      |
      |  val aR0 = getVar[GroupElement](0).get
      |  val aC0 = getVar[GroupElement](1).get
      |  val aR1 = getVar[GroupElement](2).get
      |  val aC1 = getVar[GroupElement](3).get
      |  val scalarsBytes = getVar[Coll[Byte]](4).get
      |
      |  val n1 = unsignedBigInt("115792089237316195423570985008687907852837564279074904382605163141518161494337")
      |
      |  val shapesOk =
      |    scalarsBytes.size == 256 &&
      |    OUTPUTS.size >= 1 &&
      |    OUTPUTS(0).R4[GroupElement].isDefined
      |
      |  val validOrProof = if (shapesOk) {
      |    val blindedChild = OUTPUTS(0).R4[GroupElement].get
      |    val pointsNonIdentity =
      |      parentNode.getEncoded(0) != 0.toByte &&
      |      blindedChild.getEncoded(0) != 0.toByte &&
      |      aR0.getEncoded(0) != 0.toByte &&
      |      aC0.getEncoded(0) != 0.toByte &&
      |      aR1.getEncoded(0) != 0.toByte &&
      |      aC1.getEncoded(0) != 0.toByte
      |
      |    val e0  = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(0, 32))
      |    val e1  = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(32, 64))
      |    val z00 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(64, 96))
      |    val z10 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(96, 128))
      |    val zg0 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(128, 160))
      |    val z01 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(160, 192))
      |    val z11 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(192, 224))
      |    val zg1 = Global.fromBigEndianBytes[UnsignedBigInt](scalarsBytes.slice(224, 256))
      |
      |    val canonicalScalars =
      |      e0 < n1 && e1 < n1 &&
      |      z00 < n1 && z10 < n1 && zg0 < n1 &&
      |      z01 < n1 && z11 < n1 && zg1 < n1
      |
      |    if (pointsNonIdentity && canonicalScalars) {
      |      val challengeBytes = blake2b256(
      |        SELF.id ++
      |        OUTPUTS(0).propositionBytes ++
      |        parentNode.getEncoded ++
      |        blindedChild.getEncoded ++
      |        aR0.getEncoded ++
      |        aC0.getEncoded ++
      |        aR1.getEncoded ++
      |        aC1.getEncoded
      |      )
      |      val e = Global.fromBigEndianBytes[UnsignedBigInt](challengeBytes).mod(n1)
      |
      |      // Branch 0: blindedChild commits to u0 (shared response z00 on G1 and H1)
      |      val r0Ok = G1.expUnsigned(z00).multiply(G2.expUnsigned(z10)) == aR0.multiply(parentNode.expUnsigned(e0))
      |      val c0Ok = H1.expUnsigned(z00).multiply(K1.expUnsigned(zg0)) == aC0.multiply(blindedChild.expUnsigned(e0))
      |
      |      // Branch 1: blindedChild commits to u1 (shared response z11 on G2 and H1)
      |      val r1Ok = G1.expUnsigned(z01).multiply(G2.expUnsigned(z11)) == aR1.multiply(parentNode.expUnsigned(e1))
      |      val c1Ok = H1.expUnsigned(z11).multiply(K1.expUnsigned(zg1)) == aC1.multiply(blindedChild.expUnsigned(e1))
      |
      |      (e0.plusMod(e1, n1) == e) && r0Ok && c0Ok && r1Ok && c1Ok
      |    } else false
      |  } else false
      |
      |  sigmaProp(validOrProof)
      |}""".stripMargin

  private def compileV6Tree(script: String): ErgoTree =
    VersionContext.withVersions(V6SoftForkVersion, V6SoftForkVersion) {
      ErgoTree.fromProposition(
        ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
        compile(emptyEnv, script).asSigmaProp
      )
    }

  private lazy val packedE2Generators: Array[Byte] =
    toFixed32Bytes(G1Secq.x) ++ toFixed32Bytes(G1Secq.y) ++
      toFixed32Bytes(G2Secq.x) ++ toFixed32Bytes(G2Secq.y)

  private lazy val packedG12Secq: Array[Byte] =
    toFixed32Bytes(G12Secq.x) ++ toFixed32Bytes(G12Secq.y)

  private case class MemberSecret(sk: BigInteger, pk: EcPointType, leafScalar: BigInteger, nullifier: EcPointType)

  private def mkMember(seed: Int): MemberSecret = {
    val skBytes = Blake2b256.hash(s"CurveTreeMemberSecret_$seed".getBytes("UTF-8"))
    val sk = new BigInteger(1, skBytes).mod(P2.subtract(BigInteger.ONE)).add(BigInteger.ONE)
    val pk = CryptoConstants.dlogGroup.exponentiate(CryptoConstants.dlogGroup.generator, sk)
    val leafScalar = secpXCoord(pk).mod(P2)
    val nullifier = CryptoConstants.dlogGroup.exponentiate(EpochGenSecp, sk)
    MemberSecret(sk, pk, leafScalar, nullifier)
  }

  private lazy val eightMembers: IndexedSeq[MemberSecret] = IndexedSeq.tabulate(8)(i => mkMember(i + 1))
  private lazy val standardTree: BuiltCurveTree = buildCurveTree(eightMembers.map(_.leafScalar))

  private def accumulatorBox(
      ergoTree: ErgoTree,
      rootBytes: Array[Byte],
      g1E1: EcPointType = G1Secp,
      g2E1: EcPointType = G2Secp,
      e2GenBytes: Array[Byte] = packedE2Generators,
      g12Bytes: Array[Byte] = packedG12Secq,
      epochGen: EcPointType = EpochGenSecp): ErgoBox =
    testBox(
      value = 10000000L,
      ergoTree = ergoTree,
      creationHeight = 100,
      additionalTokens = Seq.empty,
      additionalRegisters = Map(
        R4 -> ByteArrayConstant(rootBytes.toColl),
        R5 -> GroupElementConstant(g1E1.toGroupElement),
        R6 -> GroupElementConstant(g2E1.toGroupElement),
        R7 -> ByteArrayConstant(e2GenBytes.toColl),
        R8 -> ByteArrayConstant(g12Bytes.toColl),
        R9 -> GroupElementConstant(epochGen.toGroupElement)
      )
    )

  private def spendAccumulator(
      ergoTree: ErgoTree,
      inputBox: ErgoBox,
      proverSecretKey: BigInteger,
      memberPk: EcPointType,
      nullifier: EcPointType,
      contextVars: Map[Byte, EvaluatedValue[_ <: SType]],
      recipientTree: ErgoTree = ErgoTree.fromProposition(
        ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
        TrueLeaf.toSigmaProp
      ),
      verifierOutputOverride: Option[ErgoBoxCandidate] = None): (scala.util.Try[ProverResult], ErgoLikeContext) = {
    val outputCandidate = new ErgoBoxCandidate(
      value = inputBox.value,
      ergoTree = recipientTree,
      creationHeight = 101,
      additionalTokens = Colls.emptyColl[Token],
      additionalRegisters = Map(R4 -> GroupElementConstant(nullifier.toGroupElement))
    )
    val unsignedTx = new ErgoLikeTransaction(
      IndexedSeq(Input(inputBox.id, ProverResult(Array.emptyByteArray, ContextExtension(contextVars)))),
      ArraySeq.empty,
      IndexedSeq(outputCandidate)
    )
    val provingCtx = ErgoLikeContextTesting(
      currentHeight = 101,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inputBox),
      spendingTransaction = unsignedTx,
      self = inputBox,
      activatedVersion = V6SoftForkVersion
    )
    val dhTuple = ProveDHTuple(
      CryptoConstants.dlogGroup.generator,
      EpochGenSecp,
      memberPk,
      nullifier
    )
    val dhSecret = DiffieHellmanTupleProverInput(proverSecretKey, dhTuple)
    val prover = (new ContextEnrichingTestProvingInterpreter {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = contextVars
    }).withDHSecrets(Seq(dhSecret))

    val msgToSign = unsignedTx.messageToSign
    val proveRes = prover.prove(emptyEnv, ergoTree, provingCtx, msgToSign)

    val verifyCtx = verifierOutputOverride match {
      case None => provingCtx
      case Some(tamperedOutput) =>
        val tamperedTx = new ErgoLikeTransaction(
          IndexedSeq(Input(inputBox.id, ProverResult(Array.emptyByteArray, ContextExtension(contextVars)))),
          ArraySeq.empty,
          IndexedSeq(tamperedOutput)
        )
        ErgoLikeContextTesting(
          currentHeight = 101,
          lastBlockUtxoRoot = AvlTreeData.dummy,
          minerPubkey = ErgoLikeContextTesting.dummyPubkey,
          boxesToSpend = IndexedSeq(inputBox),
          spendingTransaction = tamperedTx,
          self = inputBox,
          activatedVersion = V6SoftForkVersion
        )
    }
    (proveRes, verifyCtx)
  }

  private def defaultWitnessVars(member: MemberSecret, witness: CurveTreeWitness): Map[Byte, EvaluatedValue[_ <: SType]] =
    Map(
      0.toByte -> GroupElementConstant(member.pk.toGroupElement),
      1.toByte -> ByteArrayConstant(witness.siblingsBytes.toColl),
      2.toByte -> ByteArrayConstant(witness.directionBytes.toColl),
      3.toByte -> ByteArrayConstant(witness.strausShamirBytes.toColl),
      4.toByte -> ByteArrayConstant(witness.zInvBytes.toColl)
    )

  // --- Off-chain 1-of-2 Select-and-Rerandomize CDS OR-prover ---

  private def detScalar(tag: String): BigInteger =
    new BigInteger(1, Blake2b256.hash(tag.getBytes("UTF-8")))
      .mod(P2.subtract(BigInteger.ONE))
      .add(BigInteger.ONE)

  private def buildSelectAndRerandomizeProof(
      selfBox: ErgoBox,
      recipientPropBytes: Array[Byte],
      parentNode: EcPointType,
      u0: BigInteger,
      u1: BigInteger,
      branch: Int,
      gamma: BigInteger): (EcPointType, Map[Byte, EvaluatedValue[_ <: SType]]) = {
    require(branch == 0 || branch == 1, s"Invalid branch: $branch")
    val g = CryptoConstants.dlogGroup
    val selectedScalar = if (branch == 0) u0 else u1
    val blindedChild = g.multiplyGroupElements(
      g.exponentiate(H1Secp, selectedScalar),
      g.exponentiate(K1Secp, gamma)
    )

    val k0 = detScalar(s"cds_k0_$branch")
    val k1 = detScalar(s"cds_k1_$branch")
    val kg = detScalar(s"cds_kg_$branch")
    val simE = detScalar(s"cds_simE_$branch")
    val simZ0 = detScalar(s"cds_simZ0_$branch")
    val simZ1 = detScalar(s"cds_simZ1_$branch")
    val simZg = detScalar(s"cds_simZg_$branch")

    val (aR0, aC0, aR1, aC1) = if (branch == 0) {
      val realAR0 = g.multiplyGroupElements(g.exponentiate(G1Secp, k0), g.exponentiate(G2Secp, k1))
      val realAC0 = g.multiplyGroupElements(g.exponentiate(H1Secp, k0), g.exponentiate(K1Secp, kg))
      val simAR1 = g.multiplyGroupElements(
        g.multiplyGroupElements(g.exponentiate(G1Secp, simZ0), g.exponentiate(G2Secp, simZ1)),
        g.inverseOf(g.exponentiate(parentNode, simE))
      )
      val simAC1 = g.multiplyGroupElements(
        g.multiplyGroupElements(g.exponentiate(H1Secp, simZ1), g.exponentiate(K1Secp, simZg)),
        g.inverseOf(g.exponentiate(blindedChild, simE))
      )
      (realAR0, realAC0, simAR1, simAC1)
    } else {
      val simAR0 = g.multiplyGroupElements(
        g.multiplyGroupElements(g.exponentiate(G1Secp, simZ0), g.exponentiate(G2Secp, simZ1)),
        g.inverseOf(g.exponentiate(parentNode, simE))
      )
      val simAC0 = g.multiplyGroupElements(
        g.multiplyGroupElements(g.exponentiate(H1Secp, simZ0), g.exponentiate(K1Secp, simZg)),
        g.inverseOf(g.exponentiate(blindedChild, simE))
      )
      val realAR1 = g.multiplyGroupElements(g.exponentiate(G1Secp, k0), g.exponentiate(G2Secp, k1))
      val realAC1 = g.multiplyGroupElements(g.exponentiate(H1Secp, k1), g.exponentiate(K1Secp, kg))
      (simAR0, simAC0, realAR1, realAC1)
    }

    val transcript =
      selfBox.id ++
        recipientPropBytes ++
        parentNode.toGroupElement.getEncoded.toArray ++
        blindedChild.toGroupElement.getEncoded.toArray ++
        aR0.toGroupElement.getEncoded.toArray ++
        aC0.toGroupElement.getEncoded.toArray ++
        aR1.toGroupElement.getEncoded.toArray ++
        aC1.toGroupElement.getEncoded.toArray

    val e = new BigInteger(1, Blake2b256.hash(transcript)).mod(P2)

    val scalars = if (branch == 0) {
      val e0 = e.subtract(simE).mod(P2).add(P2).mod(P2)
      val z00 = k0.add(e0.multiply(u0)).mod(P2)
      val z10 = k1.add(e0.multiply(u1)).mod(P2)
      val zg0 = kg.add(e0.multiply(gamma)).mod(P2)
      Array(e0, simE, z00, z10, zg0, simZ0, simZ1, simZg)
    } else {
      val e1 = e.subtract(simE).mod(P2).add(P2).mod(P2)
      val z01 = k0.add(e1.multiply(u0)).mod(P2)
      val z11 = k1.add(e1.multiply(u1)).mod(P2)
      val zg1 = kg.add(e1.multiply(gamma)).mod(P2)
      Array(simE, e1, simZ0, simZ1, simZg, z01, z11, zg1)
    }

    val vars: Map[Byte, EvaluatedValue[_ <: SType]] = Map(
      0.toByte -> GroupElementConstant(aR0.toGroupElement),
      1.toByte -> GroupElementConstant(aC0.toGroupElement),
      2.toByte -> GroupElementConstant(aR1.toGroupElement),
      3.toByte -> GroupElementConstant(aC1.toGroupElement),
      4.toByte -> ByteArrayConstant(scalars.flatMap(toFixed32Bytes).toColl)
    )
    (blindedChild, vars)
  }

  // =========================================================================
  // Properties & Security Regressions
  // =========================================================================

  property("Curve Tree accumulator verifies valid ZK membership + nullifier proofs for all 8 leaves within budget") {
    val tree = mkTestErgoTree(compile(emptyEnv, curveTreeAccumulatorScript).asSigmaProp)
    val inBox = accumulatorBox(tree, toFixed32Bytes(standardTree.root))
    val verifier = new ErgoLikeTestInterpreter

    for (leafIdx <- 0 until 8) {
      val member = eightMembers(leafIdx)
      val witness = standardTree.proveLeaf(leafIdx)
      val vars = defaultWitnessVars(member, witness)

      val (proveRes, verifyCtx) = spendAccumulator(tree, inBox, member.sk, member.pk, member.nullifier, vars)
      val proof = proveRes.getOrThrow
      proof.proof.nonEmpty shouldBe true

      val (verified, cost) = verifier
        .verify(emptyEnv, tree, verifyCtx, proof, verifyCtx.spendingTransaction.messageToSign)
        .getOrThrow
      verified shouldBe true
      // Verification cost should stay comfortably under 100,000 block cost units (limit in tests = 1,000,000)
      (cost < 100000L) shouldBe true
    }
  }

  property("Curve Tree accumulator preserves E1 x-coordinates in [p2, p1 - 1] across the E1 -> E2 boundary") {
    // P2 + 2 = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364143 has a valid point
    // on secp256k1 (E1), strictly above p2 = n1 and below p1 = n2.
    val highXHex = "03fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364143"
    val highPoint = decodeSecpHex(highXHex)
    val highX = secpXCoord(highPoint)
    (highX.compareTo(P2) > 0 && highX.compareTo(P1) < 0) shouldBe true

    // Configure G1_custom = highPoint - [leaf0]G1_base - [leaf1]G2 so that the on-chain L0 Pedersen
    // hash for leaf 0 ([leaf0]G1_custom + [leaf1]G2_custom) evaluates to highPoint itself!
    val member0 = eightMembers(0)
    val member1 = eightMembers(1)
    val g = CryptoConstants.dlogGroup
    // Solve [leaf0]G1_custom + [leaf1]G2Secp = highPoint  =>  G1_custom = [leaf0^-1](highPoint - [leaf1]G2Secp)
    val rhs = g.multiplyGroupElements(highPoint, g.inverseOf(g.exponentiate(G2Secp, member1.leafScalar)))
    val g1Custom = g.exponentiate(rhs, member0.leafScalar.modInverse(P2))
    secpXCoord(g.multiplyGroupElements(
      g.exponentiate(g1Custom, member0.leafScalar),
      g.exponentiate(G2Secp, member1.leafScalar)
    )) shouldBe highX

    val customTree = buildCurveTree(eightMembers.map(_.leafScalar), g1E1 = g1Custom, g2E1 = G2Secp)
    customTree.levels(1)(0) shouldBe highX

    val tree = mkTestErgoTree(compile(emptyEnv, curveTreeAccumulatorScript).asSigmaProp)
    val inBox = accumulatorBox(tree, toFixed32Bytes(customTree.root), g1E1 = g1Custom, g2E1 = G2Secp)
    val witness = customTree.proveLeaf(0)
    val vars = defaultWitnessVars(member0, witness)

    val (proveRes, verifyCtx) = spendAccumulator(tree, inBox, member0.sk, member0.pk, member0.nullifier, vars)
    val proof = proveRes.getOrThrow
    val verifier = new ErgoLikeTestInterpreter
    verifier.verify(emptyEnv, tree, verifyCtx, proof, verifyCtx.spendingTransaction.messageToSign).getOrThrow._1 shouldBe true

    // Regression: reducing `out_L0` modulo `p2` (instead of keeping it in `Z_{n2}`) truncates `highX`
    // to `highX - p2 = 2` and must fail to reconstruct the valid Curve Tree root.
    val truncatedMutantScript = curveTreeAccumulatorScript.replace(
      "val out_L0 = Global.fromBigEndianBytes[UnsignedBigInt](p_L0_enc.slice(1, 33))",
      "val out_L0 = Global.fromBigEndianBytes[UnsignedBigInt](p_L0_enc.slice(1, 33)).mod(p2)"
    )
    truncatedMutantScript should not equal curveTreeAccumulatorScript
    val mutantTree = mkTestErgoTree(compile(emptyEnv, truncatedMutantScript).asSigmaProp)
    val mutantBox = accumulatorBox(mutantTree, toFixed32Bytes(customTree.root), g1E1 = g1Custom, g2E1 = G2Secp)
    val (mutantRes, _) = spendAccumulator(mutantTree, mutantBox, member0.sk, member0.pk, member0.nullifier, vars)
    assertExceptionThrown(
      mutantRes.getOrThrow,
      exceptionLike[InterpreterException]("Script reduced to false")
    )
  }

  property("Curve Tree accumulator rejects non-member keys, mismatched nullifiers, and mempool output front-running") {
    val tree = mkTestErgoTree(compile(emptyEnv, curveTreeAccumulatorScript).asSigmaProp)
    val inBox = accumulatorBox(tree, toFixed32Bytes(standardTree.root))
    val member0 = eightMembers(0)
    val witness0 = standardTree.proveLeaf(0)
    val vars0 = defaultWitnessVars(member0, witness0)
    val verifier = new ErgoLikeTestInterpreter

    // 1. Non-member secret key fails DH-tuple proof against member0.pk
    val outsider = mkMember(999)
    val (wrongSkRes, wrongSkCtx) = spendAccumulator(tree, inBox, outsider.sk, member0.pk, member0.nullifier, vars0)
    val wrongSkProof = wrongSkRes.getOrThrow
    verifier.verify(emptyEnv, tree, wrongSkCtx, wrongSkProof, wrongSkCtx.spendingTransaction.messageToSign).getOrThrow._1 shouldBe false

    // 2. Outsider substituting their own pk + nullifier fails Curve Tree root verification
    val outsiderVars = vars0.updated(0.toByte, GroupElementConstant(outsider.pk.toGroupElement))
    val (outsiderRes, _) = spendAccumulator(tree, inBox, outsider.sk, outsider.pk, outsider.nullifier, outsiderVars)
    assertExceptionThrown(
      outsiderRes.getOrThrow,
      exceptionLike[InterpreterException]("Script reduced to false")
    )

    // 3. Mismatched epoch nullifier (attempting to evade double-spend detection) fails verification
    val forgedNullifier = outsider.nullifier
    val (badNullifierRes, badNullifierCtx) = spendAccumulator(tree, inBox, member0.sk, member0.pk, forgedNullifier, vars0)
    val badNullifierProof = badNullifierRes.getOrThrow
    verifier.verify(
      emptyEnv, tree, badNullifierCtx, badNullifierProof, badNullifierCtx.spendingTransaction.messageToSign
    ).getOrThrow._1 shouldBe false

    // 4. Front-running / output substitution: valid proof signed for honest output is rejected if OUTPUTS(0) changes
    val (honestRes, _) = spendAccumulator(tree, inBox, member0.sk, member0.pk, member0.nullifier, vars0)
    val honestProof = honestRes.getOrThrow
    val attackerTree = ErgoTree.fromProposition(
      ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
      DLogProtocol.DLogProverInput(outsider.sk).publicImage
    )
    val hijackedOutput = new ErgoBoxCandidate(
      value = inBox.value,
      ergoTree = attackerTree,
      creationHeight = 101,
      additionalTokens = Colls.emptyColl[Token],
      additionalRegisters = Map(R4 -> GroupElementConstant(member0.nullifier.toGroupElement))
    )
    val (_, hijackedVerifyCtx) = spendAccumulator(
      tree, inBox, member0.sk, member0.pk, member0.nullifier, vars0, verifierOutputOverride = Some(hijackedOutput)
    )
    verifier.verify(
      emptyEnv, tree, hijackedVerifyCtx, honestProof, hijackedVerifyCtx.spendingTransaction.messageToSign
    ).getOrThrow._1 shouldBe false
  }

  property("Curve Tree accumulator rejects tampered paths, Straus-Shamir digit forgeries, bad Z^-1, and malformed extensions") {
    val tree = mkTestErgoTree(compile(emptyEnv, curveTreeAccumulatorScript).asSigmaProp)
    val inBox = accumulatorBox(tree, toFixed32Bytes(standardTree.root))
    val member = eightMembers(3)
    val witness = standardTree.proveLeaf(3)
    val baseVars = defaultWitnessVars(member, witness)

    def assertRejected(mutatedVars: Map[Byte, EvaluatedValue[_ <: SType]], box: ErgoBox = inBox): Unit = {
      val (res, _) = spendAccumulator(tree, box, member.sk, member.pk, member.nullifier, mutatedVars)
      assertExceptionThrown(
        res.getOrThrow,
        exceptionLike[InterpreterException]("Script reduced to false")
      )
    }

    // Tampered sibling byte at L0, L1, and L2
    for (byteOffset <- Seq(31, 63, 95)) {
      val badSibs = witness.siblingsBytes.clone()
      badSibs(byteOffset) = (badSibs(byteOffset) ^ 1).toByte
      assertRejected(baseVars.updated(1.toByte, ByteArrayConstant(badSibs.toColl)))
    }

    // Flipped direction bit and out-of-domain direction byte (`2`)
    val flippedDirs = witness.directionBytes.clone()
    flippedDirs(0) = (1 - flippedDirs(0)).toByte
    assertRejected(baseVars.updated(2.toByte, ByteArrayConstant(flippedDirs.toColl)))

    val invalidDirByte = witness.directionBytes.clone()
    invalidDirByte(1) = 2.toByte
    assertRejected(baseVars.updated(2.toByte, ByteArrayConstant(invalidDirByte.toColl)))

    // Leading-zero Straus-Shamir digit (`0` prepended — reconstructs identical scalars if unguarded!)
    val leadingZeroDigits = 0.toByte +: witness.strausShamirBytes
    if (leadingZeroDigits.length <= 256) {
      assertRejected(baseVars.updated(3.toByte, ByteArrayConstant(leadingZeroDigits.toColl)))
    }

    // Out-of-alphabet Straus-Shamir digit (`4` instead of `0`)
    val outOfAlphabetDigits = witness.strausShamirBytes.clone()
    val zeroIdx = outOfAlphabetDigits.indexOf(0.toByte, 1)
    if (zeroIdx > 0) {
      outOfAlphabetDigits(zeroIdx) = 4.toByte
      assertRejected(baseVars.updated(3.toByte, ByteArrayConstant(outOfAlphabetDigits.toColl)))
    }

    // Incorrect prover-assisted modular inverse Z^-1
    val badZInv = witness.zInvBytes.clone()
    badZInv(31) = (badZInv(31) ^ 1).toByte
    assertRejected(baseVars.updated(4.toByte, ByteArrayConstant(badZInv.toColl)))

    // Malformed byte lengths (truncated siblings, empty Straus-Shamir array) evaluate cleanly to false
    assertRejected(baseVars.updated(1.toByte, ByteArrayConstant(witness.siblingsBytes.take(64).toColl)))
    assertRejected(baseVars.updated(3.toByte, ByteArrayConstant(Array.emptyByteArray.toColl)))

    // Wrong committed root in R4
    val wrongRoot = toFixed32Bytes(standardTree.root)
    wrongRoot(0) = (wrongRoot(0) ^ 1).toByte
    assertRejected(baseVars, accumulatorBox(tree, wrongRoot))
  }

  property("Select-and-Rerandomize 1-of-2 OR-proof verifies both hidden branches and rejects forged commitments") {
    val tree = mkTestErgoTree(compile(emptyEnv, curveTreeSelectAndRerandomizeScript).asSigmaProp)
    val g = CryptoConstants.dlogGroup
    val verifier = new ErgoLikeTestInterpreter

    val u0 = standardTree.levels(2)(0)
    val u1 = standardTree.levels(2)(1)
    val parentNode = g.multiplyGroupElements(g.exponentiate(G1Secp, u0), g.exponentiate(G2Secp, u1))

    val inBox = testBox(
      value = 10000000L,
      ergoTree = tree,
      creationHeight = 200,
      additionalTokens = Seq.empty,
      additionalRegisters = Map(
        R4 -> GroupElementConstant(parentNode.toGroupElement),
        R5 -> GroupElementConstant(G1Secp.toGroupElement),
        R6 -> GroupElementConstant(G2Secp.toGroupElement),
        R7 -> GroupElementConstant(H1Secp.toGroupElement),
        R8 -> GroupElementConstant(K1Secp.toGroupElement)
      )
    )
    val recipientTree = ErgoTree.fromProposition(
      ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
      TrueLeaf.toSigmaProp
    )

    // Both left (branch = 0) and right (branch = 1) rerandomized selections succeed
    for (branch <- Seq(0, 1)) {
      val gamma = detScalar(s"rerand_gamma_$branch")
      val (blindedChild, vars) = buildSelectAndRerandomizeProof(
        inBox, recipientTree.bytes, parentNode, u0, u1, branch, gamma
      )
      val outCandidate = new ErgoBoxCandidate(
        value = inBox.value,
        ergoTree = recipientTree,
        creationHeight = 201,
        additionalTokens = Colls.emptyColl[Token],
        additionalRegisters = Map(R4 -> GroupElementConstant(blindedChild.toGroupElement))
      )
      val tx = new ErgoLikeTransaction(
        IndexedSeq(Input(inBox.id, ProverResult(Array.emptyByteArray, ContextExtension(vars)))),
        ArraySeq.empty,
        IndexedSeq(outCandidate)
      )
      val ctx = ErgoLikeContextTesting(
        currentHeight = 201,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(inBox),
        spendingTransaction = tx,
        self = inBox,
        activatedVersion = V6SoftForkVersion
      )
      val prover = new ContextEnrichingTestProvingInterpreter {
        override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = vars
      }
      val proof = prover.prove(emptyEnv, tree, ctx, tx.messageToSign).getOrThrow
      val (ok, cost) = verifier.verify(emptyEnv, tree, ctx, proof, tx.messageToSign).getOrThrow
      ok shouldBe true
      (cost < 50000L) shouldBe true
    }

    // Negative 1: Commitment to a third scalar uFake not in {u0, u1} fails verification
    val uFake = u0.add(BigInteger.ONE).mod(P2)
    val gammaFake = detScalar("rerand_gamma_fake")
    val (_, forgedVars) = buildSelectAndRerandomizeProof(
      inBox, recipientTree.bytes, parentNode, uFake, u1, branch = 0, gammaFake
    )
    val forgedChild = g.multiplyGroupElements(g.exponentiate(H1Secp, uFake), g.exponentiate(K1Secp, gammaFake))
    val forgedOut = new ErgoBoxCandidate(
      value = inBox.value,
      ergoTree = recipientTree,
      creationHeight = 201,
      additionalTokens = Colls.emptyColl[Token],
      additionalRegisters = Map(R4 -> GroupElementConstant(forgedChild.toGroupElement))
    )
    val forgedTx = new ErgoLikeTransaction(
      IndexedSeq(Input(inBox.id, ProverResult(Array.emptyByteArray, ContextExtension(forgedVars)))),
      ArraySeq.empty,
      IndexedSeq(forgedOut)
    )
    val forgedCtx = ErgoLikeContextTesting(
      currentHeight = 201,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inBox),
      spendingTransaction = forgedTx,
      self = inBox,
      activatedVersion = V6SoftForkVersion
    )
    val forgedProver = new ContextEnrichingTestProvingInterpreter {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = forgedVars
    }
    assertExceptionThrown(
      forgedProver.prove(emptyEnv, tree, forgedCtx, forgedTx.messageToSign).getOrThrow,
      exceptionLike[InterpreterException]("Script reduced to false")
    )

    // Negative 2: Replaying a valid proof onto a different recipient proposition fails Fiat-Shamir check
    val (validChild, validVars) = buildSelectAndRerandomizeProof(
      inBox, recipientTree.bytes, parentNode, u0, u1, branch = 0, detScalar("rerand_gamma_replay")
    )
    val otherRecipientTree = ErgoTree.fromProposition(
      ErgoTree.headerWithVersion(ZeroHeader, V6SoftForkVersion),
      DLogProtocol.DLogProverInput(BigInteger.valueOf(42L)).publicImage
    )
    val replayOut = new ErgoBoxCandidate(
      value = inBox.value,
      ergoTree = otherRecipientTree,
      creationHeight = 201,
      additionalTokens = Colls.emptyColl[Token],
      additionalRegisters = Map(R4 -> GroupElementConstant(validChild.toGroupElement))
    )
    val replayTx = new ErgoLikeTransaction(
      IndexedSeq(Input(inBox.id, ProverResult(Array.emptyByteArray, ContextExtension(validVars)))),
      ArraySeq.empty,
      IndexedSeq(replayOut)
    )
    val replayCtx = ErgoLikeContextTesting(
      currentHeight = 201,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(inBox),
      spendingTransaction = replayTx,
      self = inBox,
      activatedVersion = V6SoftForkVersion
    )
    val replayProver = new ContextEnrichingTestProvingInterpreter {
      override lazy val contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = validVars
    }
    assertExceptionThrown(
      replayProver.prove(emptyEnv, tree, replayCtx, replayTx.messageToSign).getOrThrow,
      exceptionLike[InterpreterException]("Script reduced to false")
    )
  }

  property2("Curve Tree v6 primitives are gated behind ErgoTree v3 / protocol v6.0 activation") {
    val preV6 = (V6SoftForkVersion - 1).toByte
    VersionContext.withVersions(preV6, preV6) {
      assertExceptionThrown(
        compile(emptyEnv, curveTreeAccumulatorScript),
        exceptionLike[ParserException]("Unsupported type SUnsignedBigInt")
      )
      assertExceptionThrown(
        compile(emptyEnv, "{ sigmaProp(Global.fromBigEndianBytes[BigInt](Coll(1.toByte)) == 1.toBigInt) }"),
        exceptionLike[MethodNotFound]("Cannot find method 'fromBigEndianBytes'")
      )
    }

    val v6Tree = compileV6Tree(curveTreeAccumulatorScript)
    v6Tree.version shouldBe V6SoftForkVersion

    // Deserializing a v3 (v6.0) ErgoTree when the pre-v6 script version is active must be rejected
    VersionContext.withVersions(preV6, preV6) {
      an[SerializerException] should be thrownBy {
        ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(v6Tree.bytes)
      }
    }

    // Executing a v3 (v6.0) ErgoTree against a pre-v6 activated protocol context must be rejected
    val preV6Ctx = ErgoLikeContextTesting.dummy(accumulatorBox(v6Tree, toFixed32Bytes(standardTree.root)), preV6)
    val verifier = new ErgoLikeTestInterpreter
    assertExceptionThrown(
      verifier.verify(emptyEnv, v6Tree, preV6Ctx, Array.emptyByteArray, fakeMessage).getOrThrow,
      exceptionLike[InterpreterException]("ErgoTree version 3 is higher than activated 2")
    )
  }
}
