package sigma.eval

import scala.util.Try

/** Represents a verifier of authenticated AVL+ tree created from a proof.
  * The verifier holds the tree data parsed from the proof.
  */
trait AvlTreeVerifier {
  /** Height of the tree. */
  def treeHeight: Int

  /** Looks up a key in the tree.
    * If `key` exists in the tree and the operation succeeds,
    * returns `Success(Some(v))`, where v is the value associated with `key`.
    * If `key` does not exists in the tree and the operation succeeds, returns Success(None).
    * Returns Failure if the operation fails or the proof does not verify.
    * After one failure, all subsequent operations with this verifier will fail and digest
    * is None.
    *
    * @param key key to look up
    * @return  Success(Some(value)), Success(None), or Failure
    */
  def performLookup(key: Array[Byte]): Try[Option[Array[Byte]]]

  /** Check the key-value pair has been inserted in the tree.
    * If `key` exists in the tree and the operation succeeds,
    * returns `Success(Some(value))`, where value is associated with `key`.
    * If `key` does not exists in the tree and the operation succeeds, returns Success(None).
    * Returns Failure if the operation fails or the proof does not verify.
    * After one failure, all subsequent operations with this verifier will fail and digest
    * is None.
    *
    * @param key key to look up
    * @param value value to check it was inserted
    * @return Success(Some(inserted value)), Success(None), or Failure
    */
  def performInsert(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]]

  /** Check the key-value pair has been updated in the tree.
    * If `key` exists in the tree and the operation succeeds,
    * returns `Success(Some(value))`, where value is associated with `key`.
    * If `key` does not exists in the tree and the operation succeeds, returns Success(None).
    * Returns Failure if the operation fails or the proof does not verify.
    * After one failure, all subsequent operations with this verifier will fail and digest
    * is None.
    *
    * @param key   key to look up
    * @param value value to check it was updated
    * @return Success(Some(value)), Success(None), or Failure
    */
  def performUpdate(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]]

  /** Check the key has been removed in the tree.
    * If `key` exists in the tree and the operation succeeds,
    * returns `Success(Some(v))`, where v is old value associated with `key`.
    * If `key` does not exists in the tree and the operation succeeds, returns Success(None).
    * Returns Failure if the operation fails or the proof does not verify.
    * After one failure, all subsequent operations with this verifier will fail and digest
    * is None.
    *
    * @param key   key to look up
    * @return Success(Some(old value)), Success(None), or Failure
    */
  def performRemove(key: Array[Byte]): Try[Option[Array[Byte]]]

  /**
    * Returns Some(d), where d - the current digest of the authenticated data structure.
    * The digest contains the root hash and the root height.
    *
    * Returns None if the proof verification failed at construction
    * or during any of the operations with this verifier.
    *
    * @return - Some[digest] or None
    */
  def digest: Option[Array[Byte]]
}
