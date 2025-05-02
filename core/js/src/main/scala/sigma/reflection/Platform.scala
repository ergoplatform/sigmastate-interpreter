package sigma.reflection

import sigma.RuntimePlatform
import scala.collection.mutable

/** JS Platform dependent implementation of reflection methods. */
object Platform {
  /** Returns an RClass instance for the given class.
    *
    * @param clazz The class for which to retrieve an RClass instance.
    * @tparam T The type of the class.
    * @return An RClass instance for the given class.
    * @throws java.lang.RuntimeException if RClass metadata for the given class cannot be
    *                                    found.
    */
  def resolveClass[T](clazz: Class[T]): RClass[T] = {
    val res = ReflectionData.classes.get(clazz) match {
      case Some(c) =>
        assert(c.clazz == clazz)
        c
      case _ =>
        sys.error(s"We couldn't find the data for class $clazz. " +
          "Please check that the class name is correct and try again. " +
          "If the issue keeps happening, contact <a href=\"#\">Customer care</a>.")
      // Uncomment the following line to collect missing reflection data and generate Scala code for it
      //        memoize(classes)(clazz, new JRClass[T](clazz))
    }
    res.asInstanceOf[RClass[T]]
  }

  /** A cache that stores key-value pairs using HashMap.
    * This class is thread-safe using the synchronized access to the underlying HashMap
    * instance.
    *
    * @tparam K the type of keys used in the cache
    * @tparam V the type of values stored in the cache
    */
  class Cache[K, V] {
    private val map = mutable.HashMap.empty[K, V]

    /** Retrieves the value associated with the given key from the cache or
      * computes and stores the value if the key is not present in the cache.
      * This method is thread-safe using the synchronized block.
      *
      * @param key the key to look up or store in the cache
      * @param value a by-name parameter that computes the value to be stored if the key is not present
      * @return the value associated with the key, either retrieved or computed
      */
    def getOrElseUpdate(key: K, value: => V): V = synchronized {
      map.getOrElseUpdate(key, value)
    }
  }

  /** This method works for Scala 2.13+. Sigma.js is compiled only with Scala 2.13+, hence
    * using getSimpleName is ok.
    * @see https://github.com/scala/bug/issues/5425 */
  def safeSimpleName(cl: Class[_]): String = {
    cl.getSimpleName
  }

  /** Returns current runtime platform descriptor. */
  def runtimePlatform: RuntimePlatform = RuntimePlatform.JS
}
