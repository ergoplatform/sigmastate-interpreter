package sigma

import scala.collection.mutable

/** Contains the Sigma Reflection API.
  * Sigma reflection is a mechanism for obtaining metadata about classes, methods, fields, etc.
  * at runtime. It is used by kiama to rewrite ErgoTree expressions. It is also used by the
  * ErgoTree interpreter to implement MethodCall nodes.
  *
  * The Sigma Reflection API has two implementations:
  * 1) based on the Java Reflection API
  * 2) based on Sigma Reflection metadata declared in the StaticImpl.scala file
  */
package object reflection {

  /** Memoizes a value in a mutable HashMap.
    *
    * @param map   The mutable HashMap to store the key-value pair.
    * @param key   The key to store in the map.
    * @param value The value to be evaluated and stored in the map if the key is not present.
    * @return The value associated with the given key in the map. If the key is not present in the map,
    *         evaluates the `value` parameter and stores it in the map before returning it.
    */
  def memoize[K, V](map: mutable.Map[K, V])
                   (key: K, value: => V): V = {
    map.get(key) match {
      case Some(v) => v
      case None =>
        val v = value
        map.put(key, v)
        v
    }
  }

  /** Creates a new SRField instance with the given parameters.
    * This is analogous to the Java Reflection API's [[java.lang.reflect.Field]] class.
    *
    * @param clazz      the [[java.lang.Class]] that declares the field
    * @param name       the name of the field
    * @param fieldType  the type of the field value
    * @return a tuple containing the field's name  as its first element,
    *         and an SRField instance as its second element
    * @see [[SRField]]
    */
  def mkField(clazz: Class[_], name: String, fieldType: Class[_]): (String, RField) = {
    name -> new SRField(clazz, name, fieldType)
  }

  /** Creates a new SRMethod instance with the given parameters and handler function.
    * This is analogous to the Java Reflection API's [[java.lang.reflect.Method]] class.
    *
    * @param clazz      the [[java.lang.Class]] that declares the method
    * @param name       the name of the method
    * @param paramTypes the types of the method's parameters
    * @param handler    a function that handles invoking the method, when RMethod.invoke
    *                   is called then this `handler` is called
    * @return a tuple containing the method's name and parameter types as its first element,
    *         and an SRMethod instance as its second element
    * @see [[SRMethod]]
    */
  def mkMethod(clazz: Class[_], name: String, paramTypes: Seq[Class[_]])
      (handler: (Any, Array[AnyRef]) => Any): ((String, Seq[Class[_]]), RMethod) = {
    (name, paramTypes) ->
        new SRMethod(clazz, name, paramTypes) {
          override def invoke(
              obj: Any,
              args: AnyRef*): Any = handler(obj, args.toArray)
        }
  }

  /** Creates a new [[SRConstructor]] instance with the given parameter types and handler function.
    *
    * @param parameterTypes the types of the constructor's parameters
    * @param handler        a function that handles creating a new instance using this constructor
    * @return an SRConstructor instance
    */
  def mkConstructor(parameterTypes: Array[Class[_]])(handler: Array[AnyRef] => Any) = {
    new SRConstructor[Any](parameterTypes) {
      override def newInstance(args: AnyRef*): Any = handler(args.toArray)
    }
  }

}
