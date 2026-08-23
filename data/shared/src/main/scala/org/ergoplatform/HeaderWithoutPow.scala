package org.ergoplatform

import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}
import scorex.util.Extensions._

/**
  * Header without proof-of-work puzzle solution, see Header class description for details.
  */
class HeaderWithoutPow(val version:  Byte, // 1 byte
                       val parentId: ModifierId, // 32 bytes
                       val ADProofsRoot: Digest32, // 32 bytes
                       val stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                       val transactionsRoot: Digest32, // 32 bytes
                       val timestamp: Long,
                       val nBits: Long, //actually it is unsigned int
                       val height: Int,
                       val extensionRoot: Digest32,
                       val votes: Array[Byte], //3 bytes
                       val unparsedBytes: Array[Byte]) {
  def toHeader(powSolution: AutolykosSolution, bytes: Array[Byte]): ErgoHeader =
    ErgoHeader(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, powSolution, votes, unparsedBytes, bytes)

  override def toString: String = {
    s"HeaderWithoutPow($version, $parentId, ${bytesToId(ADProofsRoot)}, ${bytesToId(stateRoot)}, " +
      s"${bytesToId(transactionsRoot)}, $timestamp, $nBits, $height, ${bytesToId(extensionRoot)}, ${bytesToId(votes)}, " +
      s"${bytesToId(unparsedBytes)} )"
  }
}

object HeaderWithoutPow {

  def apply(version: Byte, parentId: ModifierId, ADProofsRoot: Digest32, stateRoot: ADDigest,
            transactionsRoot: Digest32, timestamp: Long, nBits: Long, height: Int,
            extensionRoot: Digest32, votes: Array[Byte], unparsedBytes: Array[Byte]): HeaderWithoutPow = {
    new HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionRoot, votes, unparsedBytes)
  }

}

object HeaderWithoutPowSerializer extends SigmaSerializer[HeaderWithoutPow, HeaderWithoutPow] {

  override def serialize(h: HeaderWithoutPow, w: SigmaByteWriter): Unit = {
    w.put(h.version)
    w.putBytes(idToBytes(h.parentId))
    w.putBytes(h.ADProofsRoot)
    w.putBytes(h.transactionsRoot)
    w.putBytes(h.stateRoot)
    w.putULong(h.timestamp)
    w.putBytes(h.extensionRoot)
    DifficultySerializer.serialize(h.nBits, w)
    w.putUInt(h.height.toLong)
    w.putBytes(h.votes)

    // For block version >= 2, this new byte encodes length of possible new fields.
    // Set to 0 for now, so no new fields.
    if (h.version > HeaderVersion.InitialVersion) {
      w.putUByte(h.unparsedBytes.length)
      w.putBytes(h.unparsedBytes)
    }
  }

  override def parse(r: SigmaByteReader): HeaderWithoutPow = {
    val version = r.getByte()
    val parentId = bytesToId(r.getBytes(32))
    val ADProofsRoot = Digest32 @@ r.getBytes(32)
    val transactionsRoot = Digest32 @@ r.getBytes(32)
    val stateRoot = ADDigest @@ r.getBytes(33)
    val timestamp = r.getULong()
    val extensionHash = Digest32 @@ r.getBytes(32)
    val nBits = DifficultySerializer.parse(r)
    val height = r.getUInt().toIntExact
    val votes = r.getBytes(3)

    // For block version >= 2, a new byte encodes length of possible new fields.
    // If this byte > 0, we read new fields but do nothing, as semantics of the fields is not known.
    val unparsedBytes = if (version > HeaderVersion.InitialVersion) {
      val newFieldsSize = r.getUByte()
      if (newFieldsSize > 0 && version > HeaderVersion.Interpreter60Version) {
        // new bytes could be added only for block version >= 5
        r.getBytes(newFieldsSize)
      } else {
        Array.emptyByteArray
      }
    } else {
      Array.emptyByteArray
    }

    HeaderWithoutPow(version, parentId, ADProofsRoot, stateRoot, transactionsRoot, timestamp,
      nBits, height, extensionHash, votes, unparsedBytes)
  }

}


object DifficultySerializer extends SigmaSerializer[Long, Long] {

  /** Parse 4 bytes from the byte array (starting at the offset) as unsigned 32-bit integer in big endian format. */
  def readUint32BE(bytes: Array[Byte]): Long = ((bytes(0) & 0xffL) << 24) | ((bytes(1) & 0xffL) << 16) | ((bytes(2) & 0xffL) << 8) | (bytes(3) & 0xffL)

  def uint32ToByteArrayBE(value: Long): Array[Byte] = {
    Array(0xFF & (value >> 24), 0xFF & (value >> 16), 0xFF & (value >> 8), 0xFF & value).map(_.toByte)
  }

  override def serialize(obj: Long, w: SigmaByteWriter): Unit = {
    w.putBytes(uint32ToByteArrayBE(obj))
  }

  override def parse(r: SigmaByteReader): Long = {
    readUint32BE(r.getBytes(4))
  }

}

object HeaderVersion {
  type Value = Byte

  /**
    * Block version during mainnet launch
    */
  val InitialVersion: Value = 1.toByte

  /**
    * Block version after the Hardening hard-fork
    * Autolykos v2 PoW, witnesses in transactions Merkle tree
    */
  val HardeningVersion: Value = 2.toByte

  /**
    * Block version after the 5.0 soft-fork
    * 5.0 interpreter with JITC, monotonic height rule (EIP-39)
    */
  val Interpreter50Version: Value = 3.toByte

  /**
    * Block version after the 6.0 soft-fork
    * 6.0 interpreter (EIP-50)
    */
  val Interpreter60Version: Value = 4.toByte

}
