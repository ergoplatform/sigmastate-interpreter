package sigma.reflection

/** A representation of a field of a class, providing reflective access to the field's type. */
abstract class RField {
  /** @return the type of this field. */
  def getType: Class[_]
}

/** A representation of a constructor of a class, providing reflective access to the constructor's parameters and ability to create new instances.
  * @tparam T the type of the class
  */
trait RConstructor[T] {

  /** Creates a new instance of the class using the specified arguments.
    * @param args the arguments to pass to the constructor
    * @return a new instance of the class T
    */
  def newInstance(args: AnyRef*): T

  /** Returns the types of the parameters of this constructor.
    * @return an array of Class objects representing the types of the parameters of this constructor
    */
  def getParameterTypes(): Array[Class[_]]
}

/** Represents a method that can be invoked on an object with arguments. */
abstract class RMethod {
  /** Invokes this method on the given object with the given arguments.
    *
    * @param obj  The object on which to invoke this method.
    * @param args The arguments to pass to this method.
    * @return The result of invoking this method on the given object with the given arguments.
    */
  def invoke(obj: Any, args: AnyRef*): Any

  /** Returns the name of this method. */
  def getName: String

  /** Returns the class in which this method is declared. */
  def getDeclaringClass(): Class[_]

  /** Returns an array of `Class` objects representing the parameter types of this method
    * in declaration order.
    */
  def getParameterTypes(): Seq[Class[_]]
}

/** Represents a class that can be reflected upon to obtain information about its fields,
  * methods, constructors and superclass.
  *
  * @tparam T The type represented by this `RClass`.
  */
abstract class RClass[T] {
  /** Returns an `RField` object representing a field with the specified name within this `RClass`.
    *
    * @param name The name of the field to retrieve
    * @return An `RField` object representing a field with the specified name within this `RClass`.
    */
  def getField(name: String): RField

  /** Returns an `RMethod` object representing a method with the specified name and
    * parameter types within this `RClass`.
    *
    * @param name           The name of the method to retrieve
    * @param parameterTypes A list of classes representing each parameter type
    * @return An `RMethod` object representing a method with the specified name and
    *         parameter types within this `RClass`.
    */
  def getMethod(name: String, parameterTypes: Class[_]*): RMethod

  /** Returns the simple name (i.e. without package) of this class. */
  def getSimpleName: String

  /** Returns the fully qualified name of this class. */
  def getName: String

  /** Returns an array containing `RConstructor` objects reflecting all public constructors
    * of this `RClass`
    */
  def getConstructors(): Seq[RConstructor[_]]

  /** Returns a boolean indicating whether or not this `RClass` represents a primitive type. */
  def isPrimitive(): Boolean

  /** Returns an `RClass` object representing the superclass of this `RClass`. */
  def getSuperclass(): RClass[_ >: T]

  /** Returns a boolean indicating whether or not instances of the specified class can be assigned to
    * variables of this `RClass`.
    *
    * @param cls The class to check
    * @return A boolean indicating whether or not instances of the specified class can be assigned to
    *         variables of this `RClass`
    */
  def isAssignableFrom(cls: Class[_]): Boolean

  /** Returns an array containing `RMethod` objects reflecting all declared methods
    * of this `RClass`
    */
  def getDeclaredMethods(): Array[RMethod]
}

object RClass {

  /** Returns an RClass instance for the given class.
    *
    * @param clazz The class for which to retrieve an RClass instance.
    * @tparam T The type of the class.
    * @return An RClass instance for the given class.
    * @throws java.lang.RuntimeException if RClass metadata for the given class cannot be
    *                                    found.
    */
  def apply[T](clazz: Class[T]): RClass[T] = Platform.resolveClass(clazz)

}