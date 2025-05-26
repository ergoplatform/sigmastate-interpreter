package sigma.reflection

/**
  * Represents a field in an Sigma Reflection metadata.
  * Extends [[RField]] by providing a concrete implementation without relying on Java reflection.
  * The instances of this class are used as parameters of `registerClassEntry` method.
  *
  * @param declaringClass the class that declares the field
  * @param name the name of the field
  * @param tpe the type of the field as runtime [[java.lang.Class]]
  */
class SRField(val declaringClass: Class[_], val name: String, tpe: Class[_]) extends RField {
  override def getType: Class[_] = tpe

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRField => declaringClass == that.declaringClass && name == that.name
    case _ => false
  })
  override def hashCode(): Int = 31 * name.hashCode() + declaringClass.hashCode()
}

/** Represents a constructor in an Sigma Reflection metadata.
  * Extends [[RConstructor]] by providing a concrete implementation without relying on Java reflection.
  *
  * @param parameterTypes the types of the constructor's parameters
  */
abstract class SRConstructor[T](parameterTypes: Array[Class[_]]) extends RConstructor[T] {
  override def getParameterTypes(): Array[Class[_]] = parameterTypes
}

/** Represents a method in an Sigma Reflection metadata.
  * Extends [[RMethod]] by providing a concrete implementation without relying on Java reflection.
  *
  * @param declaringClass the class that declares the method
  * @param name the name of the method
  * @param parameterTypes the types of the method's parameters
  */
abstract class SRMethod(declaringClass: Class[_], name: String, parameterTypes: Seq[Class[_]]) extends RMethod {
  override def getName: String = name
  override def getDeclaringClass(): Class[_] = declaringClass
  override def getParameterTypes(): Seq[Class[_]] = parameterTypes

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRMethod
      if getDeclaringClass == that.getDeclaringClass && getName == that.getName =>
      parameterTypes == that.getParameterTypes()
    case _ => false
  })

  override def toString: String = s"SRMethod(${declaringClass.getName}.$name(${parameterTypes.mkString(", ")}))"
}

/**
  * Represents a class in an Sigma Reflection metadata.
  * Extends [[RClass]] by providing a concrete implementation without relying on Java reflection.
  *
  * @param clazz the [[java.lang.Class]] being represented
  * @param constructors the constructors of the class
  * @param fields the fields of the class (name -> field)
  * @param methods the methods of the class ((name, parameterTypes) -> method)
  */
class SRClass[T](val clazz: Class[T],
                 constructors: Seq[SRConstructor[_]],
                 fields: Map[String, RField],
                 methods: Map[(String, Seq[Class[_]]), RMethod]) extends RClass[T] {

  override def getField(fieldName: String): RField = fields.get(fieldName) match {
    case Some(f) => f
    case _ => throw new NoSuchFieldException(s"${clazz.getName}.$fieldName")
  }

  override def getMethod(name: String,
                         parameterTypes: Class[_]*): RMethod = {
    methods.get((name, parameterTypes)) match {
      case Some(m) => m
      case _ => throw new NoSuchMethodException(s"${clazz.getName}.$name(${parameterTypes.map(_.getName).mkString(",")})")
    }
  }

  override def getSimpleName: String = clazz.getSimpleName

  override def getName: String = clazz.getName

  override def isPrimitive(): Boolean = clazz.isPrimitive

  override def getConstructors(): Seq[RConstructor[_]] = constructors

  override def getSuperclass(): RClass[_ >: T] = clazz.getSuperclass.asInstanceOf[RClass[_ >: T]]

  override def isAssignableFrom(cls: Class[_]): Boolean = clazz.isAssignableFrom(cls)

  override def getDeclaredMethods(): Array[RMethod] = methods.values.toArray

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRClass[_] => clazz == that.clazz
    case _ => false
  })
  override def hashCode(): Int = clazz.hashCode()
}

