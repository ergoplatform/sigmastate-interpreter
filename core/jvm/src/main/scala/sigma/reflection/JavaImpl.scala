package sigma.reflection

import debox.cfor

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
  * A class that represents a Java class of type `T`.
  *
  * @constructor creates a new instance of `JRClass` with the given value.
  * @param value the Java class of type `T`.
  */
class JRClass[T](val value: Class[T]) extends RClass[T] {

  /** A mutable map that stores the fields of this class. */
  val fields = TrieMap.empty[String, RField]

  override def getField(name: String): RField =
    memoize(fields)(name, JRField(value.getField(name)))

  /** A mutable map that stores the methods of this class. */
  val methods = TrieMap.empty[(String, Seq[Class[_]]), RMethod]

  override def getMethod(name: String, parameterTypes: Class[_]*): RMethod = {
    memoize(methods)((name, parameterTypes), JRMethod(value.getMethod(name, parameterTypes:_*)))
  }

  override def getSimpleName: String = value.getSimpleName
  override def getName: String = value.getName

  /** The constructors of this class. */
  var constructors: Seq[RConstructor[_]] = _

  override def getConstructors(): Seq[RConstructor[_]] = {
    if (constructors == null) {
      synchronized {
        if (constructors == null) {
          val cs = value.getConstructors.asInstanceOf[Array[Constructor[Any]]]
          val buf = mutable.ArrayBuilder.make[RConstructor[Any]]
          cfor(0)(_ < cs.length, _ + 1) { i =>
            val c = cs(i)
            buf += JRConstructor[Any](i, c)
          }
          constructors = buf.result().asInstanceOf[Array[RConstructor[_]]]
        }
      }
    }
    constructors
  }

  /** Helper method that returns [[JRConstructor]] instances that were at least once used
    * at runtime.
    */
  def getUsedConstructors(): Seq[JRConstructor[_]] =
    getConstructors().collect { case c: JRConstructor[_] if c.wasUsed => c }

  override def isPrimitive(): Boolean = value.isPrimitive

  override def getSuperclass(): RClass[_ >: T] = RClass(value.getSuperclass)

  override def isAssignableFrom(cls: Class[_]): Boolean = value.isAssignableFrom(cls)

  override def getDeclaredMethods(): Array[RMethod] = value.getDeclaredMethods.map(JRMethod(_))

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRClass[_] =>
      val eq = value == that.value
      // Uncomment the following line when debugging
      // if (!eq)
      //   assert(this.getName != that.getName) // sanity check
      eq
    case _ => false
  })

  override def hashCode(): Int = value.hashCode()

  override def toString: String = s"JRClass($value)"
}


/** Implements [[RField]] using Java reflection.
  *
  * @param value The [[java.lang.reflect.Field]] object to wrap.
  */
class JRField private (val value: Field) extends RField {
  override def getType: Class[_] = value.getType

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRField => value == that.value
    case _ => false
  })
  override def hashCode(): Int = value.hashCode()
  override def toString: String = s"JRField($value)"
}
object JRField {
  private[reflection] def apply(field: Field): RField = new JRField(field)
}

/** Implements [[RConstructor]] using Java reflection.
  *
  * @tparam T The type of the class that declares this constructor.
  * @param index The index of the constructor in the sequence of all constructors of the class.
  * @param value The [[java.lang.reflect.Constructor]] to be wrapped.
  */
class JRConstructor[T] private (val index: Int, val value: Constructor[T]) extends RConstructor[T] {
  @volatile var wasUsed: Boolean = false
  override def newInstance(initargs: AnyRef*): T = {
    wasUsed = true
    value.newInstance(initargs:_*)
  }
  override def getParameterTypes(): Array[Class[_]] = {
    wasUsed = true
    value.getParameterTypes
  }

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRConstructor[_] => value == that.value
    case _ => false
  })
  override def hashCode(): Int = value.hashCode()
  override def toString: String = s"JRConstructor($index, $value)"
}
object JRConstructor {
  private[reflection] def apply[T](index: Int, value: Constructor[T]): RConstructor[T] =
    new JRConstructor[T](index, value)
}

/**
  * Implements [[RMethod]] using Java reflection.
  *
  * @param value          The [[java.lang.reflect.Method]] instance that this JRMethod represents.
  */
class JRMethod private (val value: Method) extends RMethod {
  override def invoke(obj: Any, args: AnyRef*): AnyRef = {
    value.invoke(obj, args:_*)
  }

  override def getName: String = value.getName

  override def getDeclaringClass(): Class[_] = value.getDeclaringClass

  override def getParameterTypes(): Seq[Class[_]] = value.getParameterTypes

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRMethod => value == that.value
    case _ => false
  })

  override def hashCode(): Int = value.hashCode()
  override def toString: String = s"JRMethod($value)"
}
object JRMethod {
  private[reflection] def apply(value: Method): RMethod = new JRMethod(value)
}
