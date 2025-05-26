package sigmastate.utils

import debox.cfor
import io.circe.Decoder
import org.ergoplatform.settings.ErgoAlgos
import sigma.crypto.EcPointType
import sigma.{Coll, Colls, GroupElement}
import sigma.eval.SigmaDsl

import scala.util.{Either, Failure, Right, Success, Try}

object Helpers {

  /** Helper class which encapsulates a mutable value. */
  class MutableCell[T](var value: T)

  def xor(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = ba1.zip(ba2).map(t => (t._1 ^ t._2).toByte)

  /** Same as `xor` but makes in-place update of the first argument (hence suffix `U`)
    * This is boxing-free version.
    * @return reference to the updated first argument to easy chaining of calls. */
  def xorU(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = {
    var i = 0
    while (i < ba1.length) {
      ba1(i) = (ba1(i) ^ ba2(i)).toByte
      i += 1
    }
    ba1
  }

  def xor(bas: Array[Byte]*): Array[Byte] =
    bas.reduce({case (ba, ba1) => xor(ba, ba1)}: ((Array[Byte], Array[Byte]) => Array[Byte]))

  def xor(bas: Coll[Byte]*): Coll[Byte] = {
    require(bas.nonEmpty, "at least one argument is required")
    if (bas.length == 1) bas(0)
    else {
      val res = bas(0).toArray.clone()
      cfor(1)(_ < bas.length, _ + 1) { i =>
        xorU(res, bas(i).toArray)
      }
      Colls.fromArray(res)
    }
  }

  /** Same as `xor` but makes in-place update of the first argument (hence suffix `U`)
    * This is boxing-free version.
    * @return reference to the updated first argument to easy chaining of calls. */
  def xorU(target: Array[Byte], xss: Seq[Array[Byte]]): Array[Byte] = {
    for (xs <- xss) {
      xorU(target, xs)
    }
    target
  }

  implicit class TryOps[+A](val source: Try[A]) extends AnyVal {
    def fold[B](onError: Throwable => B, onSuccess: A => B) = source match {
      case Success(value) => onSuccess(value)
      case Failure(t) => onError(t)
    }
    def toEither: Either[Throwable, A] = source match {
      case Success(value) => Right(value)
      case Failure(t) => Left(t)
    }
    def mapOrThrow[B](f: A => B): B = source.fold(t => throw t, f)
    def getOrThrow: A = source.fold(t => throw t, identity)
  }

  implicit class DecoderResultOps[A](val source: Decoder.Result[A]) extends AnyVal {
    def toTry: Try[A] = source match {
      case Right(value) => Success(value)
      case Left(t) => Failure(t)
    }
  }

  implicit class EitherOps[+A, +B](val source: Either[A, B]) extends AnyVal {
    /** The given function is applied if this is a `Right`.
      *
      *  {{{
      *  Right(12).map(x => "flower") // Result: Right("flower")
      *  Left(12).map(x => "flower")  // Result: Left(12)
      *  }}}
      */
    def mapRight[B1](f: B => B1): Either[A, B1] = source match {
      case Right(b) => Right(f(b))
      case _        => this.asInstanceOf[Either[A, B1]]
    }

    /** Returns a `Some` containing the `Right` value
      * if it exists or a `None` if this is a `Left`.
      *
      * {{{
      * Right(12).toOption // Some(12)
      * Left(12).toOption  // None
      * }}}
      */
    def toOption: Option[B] = source match {
      case Right(value) => Some(value)
      case _ => None
    }
  }

  /** Decodes the given hex string into byte array and then uses
    * [[SigmaDsl.decodePoint()]] to construct [[GroupElement]] instance.
    */
  def decodeGroupElement(hexString: String): GroupElement = {
    val bytes = ErgoAlgos.decodeUnsafe(hexString)
    SigmaDsl.decodePoint(Colls.fromArray(bytes))
  }

  /** Decodes the given hex string into [[GroupElement]] and then extracts the underlying
    * [[EcPointType]] instance
    */
  def decodeECPoint(hexString: String): EcPointType = {
    val ge = decodeGroupElement(hexString)
    SigmaDsl.toECPoint(ge).asInstanceOf[EcPointType]
  }

  /** Decodes the given hex string into a collection of bytes. */
  def decodeBytes(base16String: String): Coll[Byte] = {
    val bytes = ErgoAlgos.decodeUnsafe(base16String)
    Colls.fromArray(bytes)
  }

}

