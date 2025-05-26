package sigma.data

import sigma.serialization.{CoreByteReader, CoreByteWriter, CoreSerializer}
import sigma.{Coll, Colls, crypto}


case class AvlTreeFlags(insertAllowed: Boolean, updateAllowed: Boolean, removeAllowed: Boolean) {
  def serializeToByte: Byte = AvlTreeFlags.serializeFlags(this)
}

object AvlTreeFlags {

  lazy val ReadOnly = AvlTreeFlags(insertAllowed = false, updateAllowed = false, removeAllowed = false)

  lazy val AllOperationsAllowed = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)

  lazy val InsertOnly = AvlTreeFlags(insertAllowed = true, updateAllowed = false, removeAllowed = false)

  lazy val RemoveOnly = AvlTreeFlags(insertAllowed = false, updateAllowed = false, removeAllowed = true)

  def apply(serializedFlags: Byte): AvlTreeFlags = {
    val insertAllowed = (serializedFlags & 0x01) != 0
    val updateAllowed = (serializedFlags & 0x02) != 0
    val removeAllowed = (serializedFlags & 0x04) != 0
    AvlTreeFlags(insertAllowed, updateAllowed, removeAllowed)
  }

  def serializeFlags(avlTreeFlags: AvlTreeFlags): Byte = {
    val readOnly = 0
    val i = if(avlTreeFlags.insertAllowed) readOnly | 0x01 else readOnly
    val u = if(avlTreeFlags.updateAllowed) i | 0x02 else i
    val r = if(avlTreeFlags.removeAllowed) u | 0x04 else u
    r.toByte
  }
}

/**
  * Type of data which efficiently authenticates potentially huge dataset having key-value dictionary interface.
  * Only root hash of dynamic AVL+ tree, tree height, key length, optional value length, and access flags are stored
  * in an instance of the datatype.
  *
  * Please note that standard hash function from CryptoConstants is used, and height is stored along with root hash of
  * the tree, thus `digest` size is always CryptoConstants.hashLength + 1 bytes.
  *
  * @param digest authenticated tree digest: root hash along with tree height
  * @param treeFlags - allowed modifications. See AvlTreeFlags description for details
  * @param keyLength  - all the elements under the tree have the same length
  * @param valueLengthOpt - if non-empty, all the values under the tree are of the same length
  */

case class AvlTreeData(digest: Coll[Byte],
                       treeFlags: AvlTreeFlags,
                       keyLength: Int,
                       valueLengthOpt: Option[Int] = None)

object AvlTreeData {
  val DigestSize: Int = crypto.hashLength + 1 //please read class comments above for details
  val TreeDataSize = DigestSize + 3 + 4 + 4

  val dummy = new AvlTreeData(
    Colls.fromArray(Array.fill(DigestSize)(0:Byte)),
    AvlTreeFlags.AllOperationsAllowed,
    keyLength = 32)

  /** Create [[AvlTreeData]] with the given digest and all operations enabled. */
  def avlTreeFromDigest(digest: Coll[Byte]): AvlTreeData = {
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, crypto.hashLength)
  }

  object serializer extends CoreSerializer[AvlTreeData, AvlTreeData] {

    override def serialize(data: AvlTreeData, w: CoreByteWriter): Unit = {
      val tf = AvlTreeFlags.serializeFlags(data.treeFlags)
      w.putBytes(data.digest.toArray)
        .putUByte(tf)
        .putUInt(data.keyLength)
        .putOption(data.valueLengthOpt)(_.putUInt(_))
    }

    override def parse(r: CoreByteReader): AvlTreeData = {
      val digest = r.getBytes(DigestSize)
      val tf = AvlTreeFlags(r.getByte())
      val keyLength = r.getUInt().toInt
      val valueLengthOpt = r.getOption(r.getUInt().toInt)
      // Note, when keyLength and valueLengthOpt < 0 as a result of Int overflow,
      // the deserializer succeeds with invalid AvlTreeData
      // but still some AvlTree operations (remove_eval, update_eval, contains_eval) won't throw
      AvlTreeData(Colls.fromArray(digest), tf, keyLength, valueLengthOpt)
    }
  }
}
