package sigma.reflection

import sigma.RuntimePlatform
import scala.collection.concurrent.TrieMap

/** Platform dependent implementation of reflection methods. */
object Platform {
  /** Stores all classes which were accessed during runtime and whose metadata was not
    * registered.
    * This should be used only for debugging and never in production.
    */
  val unknownClasses = TrieMap.empty[Class[_], JRClass[_]]

  /** Thread-safe storage of class information. */
  private val classes = TrieMap.empty[Class[_], JRClass[_]]

  /** Returns an RClass instance for the given class.
    *
    * @param clazz The class for which to retrieve an RClass instance.
    * @tparam T The type of the class.
    * @return An RClass instance for the given class.
    * @throws java.lang.RuntimeException if RClass metadata for the given class cannot be
    *                                    found.
    */
  def resolveClass[T](clazz: Class[T]): RClass[T] = {
    val cls = memoize(classes)(clazz, new JRClass[T](clazz)).asInstanceOf[JRClass[T]]
    // Uncomment the following lines to collect missing reflection data and generate Scala code for it
    // Should be used only for debugging and never in production.
//    /** Check class registration. Should be used only for debugging. */
//    def checkRegisteredClass[T](clazz: Class[T]): Unit = {
//      ReflectionData.classes.get(clazz) match {
//        case Some(c) =>
//          assert(c.clazz == clazz)
//        case _ =>
//          sys.error(s"Cannot find RClass data for $clazz")
//      }
//    }
//    try {
//      checkRegisteredClass(clazz)
//    } catch {
//      case e: RuntimeException =>
//        memoize(unknownClasses)(clazz, cls)
//    }
    cls
  }

  /** A thread-safe cache class that stores key-value pairs.
    *
    * @tparam K the type of keys used in the cache
    * @tparam V the type of values stored in the cache
    */
  class Cache[K, V] {
    /** A concurrent TrieMap for storing key-value pairs. */
    private val map: TrieMap[K, V] = TrieMap.empty[K, V]

    /** Retrieves the value associated with the given key from the cache or
      * computes and stores the value if the key is not present in the cache.
      *
      * @param key   the key to look up or store in the cache
      * @param value a by-name parameter that computes the value to be stored if the key is not present
      * @return the value associated with the key, either retrieved or computed
      */
    def getOrElseUpdate(key: K, value: => V): V = map.getOrElseUpdate(key, value)
  }

  /** Special character in the name. */
  private def isSpecialChar(c: Char): Boolean = {
    ('0' <= c && c <= '9') || c == '$'
  }

  /** Safe version of `getSimpleName` that works around a bug in Scala compilers 2.11, 2.12.
    * This method is only used for debugging and testing purposes.
    *
    * @see https://github.com/scala/bug/issues/5425
    */
  def safeSimpleName(cl: Class[_]): String = {
    if (cl.getEnclosingClass == null) return cl.getSimpleName
    val simpleName = cl.getName.substring(cl.getEnclosingClass.getName.length)
    val length = simpleName.length
    var index = 0
    while (index < length && isSpecialChar(simpleName.charAt(index))) {index += 1 }
    // Eventually, this is the empty string iff this is an anonymous class
    simpleName.substring(index)
  }

  /** Returns current runtime platform descriptor. */
  def runtimePlatform: RuntimePlatform = RuntimePlatform.JVM
}
