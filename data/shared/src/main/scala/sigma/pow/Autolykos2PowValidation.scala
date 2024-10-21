package sigma.pow


import scorex.crypto.hash.Blake2b256
import scorex.utils.{Bytes, Ints, Longs}
import sigma.Header
import sigma.crypto.{BcDlogGroup, BigIntegers, CryptoConstants}
import sigma.util.NBitsUtils

/**
  * Functions used to validate Autolykos2 Proof-of-Work.
  */
object Autolykos2PowValidation {

  type Height = Int

  /**
    * k value for k-sum problem Autolykos is based on (find k numbers in table on N size)
    */
  private val k = 32

  /**
    * Initial size of N  value for k-sum problem Autolykos is based on (find k numbers in table on N size).
    * It grows from it since predefined block height in Autolykos 2.
    */
  private val NStart = 26

  /**
    * Group order, used in Autolykos V.1 for non-outsourceability,
    * and also to obtain target in both Autolykos v1 and v2
    */
  private val q: BigInt = CryptoConstants.dlogGroup.order

  /**
    * Number of elements in a table to find k-sum problem solution on top of
    */
  val NBase: Int = Math.pow(2, NStart.toDouble).toInt

  /**
    * Initial height since which table (`N` value) starting to increase by 5% per `IncreasePeriodForN` blocks
    */
  val IncreaseStart: Height = 600 * 1024

  /**
    * Table size (`N`) increased every 50 * 1024 blocks
    */
  val IncreasePeriodForN: Height = 50 * 1024

  /**
    * On this height, the table (`N` value) will stop to grow.
    * Max N on and after this height would be 2,143,944,600 which is still less than 2^^31.
    */
  val NIncreasementHeightMax: Height = 4198400

  /**
    * Blake2b256 hash function invocation
    * @param in - input bit-string
    * @return - 256 bits (32 bytes) array
    */
  def hash(in: Array[Byte]): Array[Byte] = Blake2b256.hash(in)

  /**
    * Convert byte array to unsigned integer
    * @param in - byte array
    * @return - unsigned integer
    */
  def toBigInt(in: Array[Byte]): BigInt = BigInt(BigIntegers.fromUnsignedByteArray(in))

  /**
    * Constant data to be added to hash function to increase its calculation time
    */
  val M: Array[Byte] = (0 until 1024).toArray.flatMap(i => Longs.toByteArray(i.toLong))

  /**
    * Calculates table size (N value) for a given height (moment of time)
    *
    * @see papers/yellow/pow/ErgoPow.tex for full description and test vectors
    * @param headerHeight - height of a header to mine
    * @return - N value
    */
  def calcN(headerHeight: Height): Int = {
    val height = Math.min(NIncreasementHeightMax, headerHeight)
    if (height < IncreaseStart) {
      NBase
    } else {
      val itersNumber = (height - IncreaseStart) / IncreasePeriodForN + 1
      (1 to itersNumber).foldLeft(NBase) { case (step, _) =>
        step / 100 * 105
      }
    }
  }

  def calcN(header: Header): Int = calcN(header.height)

  /**
    * Hash function that takes `m` and `nonceBytes` and returns a list of size `k` with numbers in
    * [0,`N`)
    */
  private def genIndexes(k: Int, seed: Array[Byte], N: Int): Seq[Int] = {
    val hash = Blake2b256(seed)
    val extendedHash = Bytes.concat(hash, hash.take(3))
    (0 until k).map { i =>
      BigInt(1, extendedHash.slice(i, i + 4)).mod(N).toInt
    }
  }.ensuring(_.length == k)

  /**
    * Generate element of Autolykos equation.
    */
  private def genElementV2(indexBytes: Array[Byte], heightBytes: => Array[Byte]): BigInt = {
    // Autolykos v. 2: H(j|h|M) (line 5 from the Algo 2 of the spec)
    toBigInt(hash(Bytes.concat(indexBytes, heightBytes, M)).drop(1))
  }

  def hitForVersion2ForMessage(k: Int, msg: Array[Byte], nonce: Array[Byte], h: Array[Byte], N: Int): BigInt = {

    val prei8 = BigIntegers.fromUnsignedByteArray(hash(Bytes.concat(msg, nonce)).takeRight(8))
    val i = BigIntegers.asUnsignedByteArray(4, prei8.mod(BigInt(N).underlying()))
    val f = Blake2b256(Bytes.concat(i, h, M)).drop(1) // .drop(1) is the same as takeRight(31)
    val seed = Bytes.concat(f, msg, nonce) // Autolykos v1, Alg. 2, line4:

    val indexes = genIndexes(k, seed, N)
    //pk and w not used in v2
    val elems = indexes.map(idx => genElementV2(Ints.toByteArray(idx), h))
    val f2 = elems.sum

    // sum as byte array is always about 32 bytes
    val array: Array[Byte] = BigIntegers.asUnsignedByteArray(32, f2.underlying())
    val ha = hash(array)
    toBigInt(ha)
  }

  /**
    * Header digest ("message" for default GPU miners) a miner is working on
    */
  def msgByHeader(h: Header): Array[Byte] = Blake2b256(h.serializeWithoutPoW.toArray)

  /**
    * Get hit for Autolykos v2 header (to test it then against PoW target)
    *
    * @param header - header to check PoW for
    * @return PoW hit
    */
  def hitForVersion2(header: Header): BigInt = {

    val msg = msgByHeader(header)
    val nonce = header.powNonce

    val h = Ints.toByteArray(header.height)  // used in AL v.2 only

    val N = calcN(header)

    hitForVersion2ForMessage(k, msg, nonce.toArray, h, N)
  }

  /**
    * Get target `b` from encoded difficulty `nBits`
    */
  def getB(nBits: Long): BigInt = {
    q / NBitsUtils.decodeCompactBits(nBits)
  }

  /**
    * Check PoW for Autolykos v2 header
    *
    * @param header - header to check PoW for
    * @return whether PoW is valid or not
    */
  def checkPoWForVersion2(header: Header): Boolean = {
    val b = getB(header.nBits)
    // for version 2, we're calculating hit and compare it with target
    val hit = hitForVersion2(header)
    hit < b
  }

}
