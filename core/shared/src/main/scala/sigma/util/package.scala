package sigma

import scala.reflect.ClassTag

package object util {
  /** Maximum length of an allocatable array. */
  val MaxArrayLength: Int = 100000

  private def checkLength[A](len: Int)(implicit tA: ClassTag[A]) = {
    if (len > MaxArrayLength)
      throw new RuntimeException(
        s"Cannot allocate array of $tA with $len items: max limit is $MaxArrayLength")
  }

  /** Allocates a new array with `len` items of type `A`.
    * Should be used instead of `new Array[A](n)` or `Array.ofDim[A](n)`
    */
  final def safeNewArray[A](len: Int)(implicit tA: ClassTag[A]): Array[A] = {
    checkLength[A](len)
    new Array[A](len)
  }

  /** Concatenate two arrays checking length limit of the resulting array.
    * Should be used in implementation of Coll operations of v5.0 and above. */
  final def safeConcatArrays_v5[A](arr1: Array[A], arr2: Array[A])
      (implicit tA: ClassTag[A]): Array[A] = {
    checkLength[A](arr1.length + arr2.length)
    CollectionUtil.concatArrays(arr1, arr2)
  }
}
