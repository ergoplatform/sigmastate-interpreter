package sigmastate.eval

import debox.cfor
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.authds.avltree.batch.{Insert, Lookup, Remove, Update}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.util.encode.Base16
import sigma.ast.SType.AnyOps
import sigma.ast._
import sigma.data.{CBox, Digest32Coll, RType}
import sigma._

import scala.util.{Failure, Success}

object Extensions {

  /** Extension methods for `Array[Byte]` not available for generic `Array[T]`. */
  implicit class ArrayByteOps(val arr: Array[Byte]) extends AnyVal {
    /** Wraps array into TokenId instance. The source array in not cloned. */
    @inline def toTokenId: TokenId = Digest32Coll @@ Colls.fromArray(arr)
    /** Encodes array into hex string */
    @inline def toHex: String = Base16.encode(arr)
  }

  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      CheckingSigmaBuilder.mkConstant(data.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  implicit class ErgoBoxOps(val ebox: ErgoBox) extends AnyVal {
    def toTestBox: Box = {
      /* NOHF PROOF:
      Changed: removed check for ebox == null
      Motivation: box cannot be null
      Safety: used in ErgoLikeContext where boxes cannot be null
      Examined ergo code: all that leads to ErgoLikeContext creation.
      */
      CBox(ebox)
    }
  }

  implicit class AvlTreeOps(val tree: AvlTree) extends AnyVal {

    def contains(key: Coll[Byte], proof: Coll[Byte]): Boolean = {
      val keyBytes = key.toArray
      val bv       = CAvlTreeVerifier(tree, proof)
      bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
        case Success(r) => r match {
          case Some(_) => true
          case _ => false
        }
        case Failure(_) => false
      }
    }

    def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = {
      val keyBytes = key.toArray
      val bv       = CAvlTreeVerifier(tree, proof)
      bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
        case Failure(_) => syntax.error(s"Tree proof is incorrect $tree")
      }
    }

    def getMany(
        keys: Coll[Coll[Byte]],
        proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = {
      val bv = CAvlTreeVerifier(tree, proof)
      keys.map { key =>
        bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
          case Success(r) => r match {
            case Some(v) => Some(Colls.fromArray(v))
            case _ => None
          }
          case Failure(_) => syntax.error(s"Tree proof is incorrect $tree")
        }
      }
    }

    def insert(
        entries: Coll[(Coll[Byte], Coll[Byte])],
        proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isInsertAllowed) {
        None
      } else {
        val bv = CAvlTreeVerifier(tree, proof)
        entries.forall { case (key, value) =>
          val insertRes = bv.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
          if (insertRes.isFailure) {
            syntax.error(s"Incorrect insert for $tree (key: $key, value: $value, digest: ${tree.digest}): ${insertRes.failed.get}}")
          }
          insertRes.isSuccess
        }
        bv.digest match {
          case Some(d) => Some(tree.updateDigest(Colls.fromArray(d)))
          case _ => None
        }
      }
    }

    def update(
        operations: Coll[(Coll[Byte], Coll[Byte])],
        proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isUpdateAllowed) {
        None
      } else {
        val bv = CAvlTreeVerifier(tree, proof)
        operations.forall { case (key, value) =>
          bv.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ value.toArray)).isSuccess
        }
        bv.digest match {
          case Some(d) => Some(tree.updateDigest(Colls.fromArray(d)))
          case _ => None
        }
      }
    }

    def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isRemoveAllowed) {
        None
      } else {
        val bv = CAvlTreeVerifier(tree, proof)
        cfor(0)(_ < operations.length, _ + 1) { i =>
          val key = operations(i).toArray
          bv.performOneOperation(Remove(ADKey @@ key))
        }
        bv.digest match {
          case Some(v) => Some(tree.updateDigest(Colls.fromArray(v)))
          case _ => None
        }
      }
    }
  }
}
