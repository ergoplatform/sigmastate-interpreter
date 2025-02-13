package org.ergoplatform

import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import sigma.Colls
import sigma.crypto.{BigIntegers, CryptoConstants, EcPointType}
import sigma.serialization.{GroupElementSerializer, SigmaByteReader, SigmaByteWriter, SigmaSerializer}

import scala.runtime.ScalaRunTime
import scala.util.hashing.MurmurHash3



/**
  * Solution for an Autolykos PoW puzzle.
  *
  * In Autolykos v.1 all the four fields are used, in Autolykos v.2 only pk and n fields are used.
  *
  * @param pk - miner public key. Should be used to collect block rewards
  * @param w  - one-time public key. Prevents revealing of miners secret
  * @param n  - nonce (8 bytes)
  * @param d  - distance between pseudo-random number, corresponding to nonce `n` and a secret,
  *           corresponding to `pk`. The lower `d` is, the harder it was to find this solution.
  */
class AutolykosSolution(val pk: EcPointType,
                        val w: EcPointType,
                        val n: Array[Byte],
                        val d: BigInt) {

    val encodedPk: Array[Byte] = GroupElementSerializer.toBytes(pk)

    override def hashCode(): Int = {
        var h = pk.hashCode()
        h = h * 31 + w.hashCode()
        h = h * 31 + MurmurHash3.arrayHash(n)
        h = h * 31 + d.hashCode()
        h
    }

    override def equals(obj: Any): Boolean = {
        obj match {
            case other: AutolykosSolution =>
                this.pk == other.pk &&
                  this.n.sameElements(other.n) &&
                  this.w == other.w &&
                  this.d == other.d

            case _ => false
        }
    }
}


object AutolykosSolution {
    // "pk", "w" and "d" values for Autolykos v2 solution, where they not passed from outside
    val pkForV2: EcPointType = CryptoConstants.dlogGroup.identity
    val wForV2: EcPointType = CryptoConstants.dlogGroup.generator
    val dForV2: BigInt = 0

    object sigmaSerializerV1 extends SigmaSerializer[AutolykosSolution, AutolykosSolution] {
        override def serialize(s: AutolykosSolution, w: SigmaByteWriter): Unit = {
            GroupElementSerializer.serialize(s.pk, w)
            GroupElementSerializer.serialize(s.w, w)
            require(s.n.length == 8) // non-consensus check on prover side
            w.putBytes(s.n)
            val dBytes = BigIntegers.asUnsignedByteArray(s.d.bigInteger)
            w.putUByte(dBytes.length)
            w.putBytes(dBytes)
        }

        override def parse(r: SigmaByteReader): AutolykosSolution = {
            val pk = GroupElementSerializer.parse(r)
            val w = GroupElementSerializer.parse(r)
            val nonce = r.getBytes(8)
            val dBytesLength = r.getUByte()
            val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
            new AutolykosSolution(pk, w, nonce, d)
        }
    }

    object sigmaSerializerV2 extends SigmaSerializer[AutolykosSolution, AutolykosSolution] {
        override def serialize(s: AutolykosSolution, w: SigmaByteWriter): Unit = {
            GroupElementSerializer.serialize(s.pk, w)
            require(s.n.length == 8) // non-consensus check on prover side
            w.putBytes(s.n)
        }

        override def parse(r: SigmaByteReader): AutolykosSolution = {
            val pk = GroupElementSerializer.parse(r)
            val nonce = r.getBytes(8)
            new AutolykosSolution(pk, wForV2, nonce, dForV2)
        }
    }
}

/**
  * Header of a block. It authenticates link to a previous block, other block sections
  * (transactions, UTXO set transformation proofs, extension), UTXO set, votes for parameters
  * to be changed and proof-of-work related data.
  *
  * @param version - protocol version
  * @param parentId - id of a parent block header
  * @param ADProofsRoot - digest of UTXO set transformation proofs
  * @param stateRoot - AVL+ tree digest of UTXO set (after the block)
  * @param transactionsRoot - Merkle tree digest of transactions in the block (BlockTransactions section)
  * @param timestamp - block generation time reported by a miner
  * @param nBits - difficulty encoded
  * @param height - height of the block (genesis block height == 1)
  * @param extensionRoot - Merkle tree digest of the extension section of the block
  * @param powSolution - solution for the proof-of-work puzzle
  * @param votes - votes for changing system parameters
  * @param unparsedBytes - bytes from future versions of the protocol our version can't parse
  * @param _bytes - serialized bytes of the header when not `null`
  */
case class ErgoHeader(override val version: ErgoHeader.Version,
                      override val parentId: ModifierId,
                      override val ADProofsRoot: Digest32,
                      override val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                      override val transactionsRoot: Digest32,
                      override val timestamp: ErgoHeader.Timestamp,
                      override val nBits: Long, //actually it is unsigned int
                      override val height: Int,
                      override val extensionRoot: Digest32,
                      powSolution: AutolykosSolution,
                      override val votes: Array[Byte], //3 bytes
                      override val unparsedBytes: Array[Byte],
                      _bytes: Array[Byte]) extends
  HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
    nBits, height, extensionRoot, votes, unparsedBytes) {

    lazy val bytes = if(_bytes != null) {
        _bytes
    } else {
        ErgoHeader.sigmaSerializer.toBytes(this)
    }

    lazy val serializedId: Array[Byte] = Blake2b256.hash(bytes)

    lazy val id = Colls.fromArray(serializedId)

    override def hashCode(): Int = id.hashCode()

    override def equals(other: Any): Boolean = other match {
        case h: ErgoHeader => h.id == this.id
        case _ => false
    }
}


object ErgoHeader {

    type Timestamp = Long

    type Version = Byte

    object sigmaSerializer extends SigmaSerializer[ErgoHeader, ErgoHeader] {
        override def serialize(hdr: ErgoHeader, w: SigmaByteWriter): Unit = {
            HeaderWithoutPowSerializer.serialize(hdr, w)
            if (hdr.version == 1) {
                AutolykosSolution.sigmaSerializerV1.serialize(hdr.powSolution, w)
            } else {
                AutolykosSolution.sigmaSerializerV2.serialize(hdr.powSolution, w)
            }
        }

        override def parse(r: SigmaByteReader): ErgoHeader = {
            val start = r.position
            val headerWithoutPow = HeaderWithoutPowSerializer.parse(r)
            val powSolution = if (headerWithoutPow.version == 1) {
                AutolykosSolution.sigmaSerializerV1.parse(r)
            } else {
                AutolykosSolution.sigmaSerializerV2.parse(r)
            }
            val end = r.position
            val len = end - start
            r.position = start
            val headerBytes = r.getBytes(len) // also moves position back to end
            headerWithoutPow.toHeader(powSolution, headerBytes)
        }
    }
}