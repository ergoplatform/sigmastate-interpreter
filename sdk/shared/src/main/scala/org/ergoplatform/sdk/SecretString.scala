package org.ergoplatform.sdk

import debox.cfor


/**
  * Encapsulates secret array of characters (char[]) with proper equality.
  * The secret data can be {@link SecretString# erase ( ) erased} in memory and not leaked to GC.
  * Using this class is more secure and safe than using char[] directly.
  */
object SecretString {
  /**
    * Creates a new instance wrapping the given characters. The given array is not copied.
    */
  def create(data: Array[Char]) = new SecretString(data)

  /**
    * Creates a new instance by copying characters from the given String.
    */
  def create(s: String) = new SecretString(s.toCharArray)

  /**
    * Create a new instance with empty sequence of characters.
    */
  def empty() = new SecretString(new Array[Char](0))
}

/** Encapsulates secret array of characters (char[]) with proper equality.
  * The secret data can be erased in memory and not leaked to GC.
  * Note that calling any methods after `erase()` will throw a runtime exception.
  * Using this class is more secure and safe than using char[] directly.
  *
  * Secret data, should not be copied outside of this instance.
  * Use static methods to construct new instances.
  */
final class SecretString private[sdk](val _data: Array[Char]) {
  /**
    * Erased flag, should not be copied outside of this instance.
    */
  private var _erased = false

  /**
    * Throws exception if SecretString is erased
    */
  private def checkErased(): Unit = {
    if (_erased) throw new RuntimeException("SecretString already erased")
  }

  /**
    * Returns true if the string doesn't have characters.
    */
  def isEmpty(): Boolean = {
    checkErased()
    _data == null || _data.length == 0
  }

  /**
    * Extracts secret characters as an array.
    */
  def getData(): Array[Char] = {
    checkErased()
    _data
  }

  /**
    * Erases secret characters stored in this instance so that they are no longer reside in memory.
    */
  def erase(): Unit = {
    java.util.Arrays.fill(_data, ' ')
    _erased = true
  }

  override def hashCode(): Int = {
    checkErased()
    java.util.Arrays.hashCode(_data)
  }

  /** this is adapted version of java.lang.String */
  override def equals(obj: Any): Boolean = {
    checkErased()
    if (this eq obj.asInstanceOf[AnyRef]) return true
    if (obj == null) return false
    obj match {
      case anotherString: SecretString if _data.length == anotherString._data.length =>
        val n: Int = _data.length
        val v1: Array[Char] = _data
        val v2: Array[Char] = anotherString._data
        cfor(0)(_ < n, _ + 1) { i =>
          if (v1(i) != v2(i)) return false
        }
        true
      case _ => false
    }
  }

  /**
    * Returns unsecured String with secret characters.
    * The secret characters are copied to the new String instance and cannot be erased in memory.
    * So they leak to GC and may remain in memory until overwritten by new data.
    * Usage of this method is discouraged and the method is provided solely to interact with
    * legacy code which keeps secret characters in String.
    */
  def toStringUnsecure(): String = {
    checkErased()
    String.valueOf(_data)
  }

}
