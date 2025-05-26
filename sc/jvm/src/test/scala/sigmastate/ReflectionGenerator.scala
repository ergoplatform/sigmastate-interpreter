package sigmastate

import sigma.compiler.ir.{Base, TypeDescs}
import sigma.reflection._
import scala.annotation.unused
import scala.collection.mutable

/** Generates code for registering classes in the ReflectionData.
  * It is not used in the runtime.
  *
  * The generated invocations of  `registerClassEntry`, `mkMethod`, `mkConstructor` may
  * require manual adjustments.
  *
  * It uses [[sigma.reflection.Platform.unknownClasses]] to collect classes which were
  * accessed during runtime
  *
  * @see [[ReflectionData]]
  */
object ReflectionGenerator {

  def normalizeName(name: String): String = {
    val len = name.length
    val res = new mutable.StringBuilder(len)
    var i = 0
    while (i < len) {
      var ch = name(i)
      if (ch == '$' && i < len - 1) {
        val prefix = name.substring(0, i + 1)
        ch = try {
          Class.forName(prefix)
          '.' // prefix is object
        } catch {
          case _: Throwable =>
            '#' // prefix is not object
        }
      }
      res += ch
      i += 1
    }
    res.result()
  }

  val knownPackages = Array(
    "sigma.compiler.ir.primitives.",
    "sigma.",
    "sigma.",
    "sigma.compiler.ir.wrappers.",
    "sigma.ast.",
    "sigmastate.lang.Terms.",
    "sigmastate.interpreter.",
    "sigmastate.utxo.",
    "sigmastate.",
    "sigma.compiler.ir.wrappers.scala.",
    "sigma.compiler.ir.",
    "scala.collection.",
    "scala."
  )

  def stripKnownPackages(name: String): String = {
    knownPackages.find(p => name.startsWith(p)) match {
      case Some(p) => name.stripPrefix(p)
      case None => name
    }
  }

  def className(c: Class[_], inBody: Boolean): String = {
    val name = c match {
      case _ if c == classOf[Base#Ref[_]] && inBody => "ctx.Ref"
      case _ if c == classOf[TypeDescs#Elem[_]] && inBody => "ctx.Elem"
      case _ if c == classOf[scala.Function1[_,_]] && inBody => "Any => Any"
      case _ => stripKnownPackages(normalizeName(c.getName))
    }
    name
  }

  def classExpr(c: Class[_], inBody: Boolean): String = {
    if (c.isArray)
      s"Array[${classExpr(c.getComponentType, inBody)}]"
    else if (c == classOf[Boolean]) "Boolean"
    else if (c == classOf[Byte]) "Byte"
    else if (c == classOf[Short]) "Short"
    else if (c == classOf[Int]) "Int"
    else if (c == classOf[Long]) "Long"
    else if (!c.getTypeParameters.isEmpty)
      s"${className(c, inBody)}[${c.getTypeParameters.map(_ => "_").mkString(",")}]"
    else
      className(c, inBody)
  }

  def genEmptyClass(c: JRClass[_]): String = {
    val name = classExpr(c.value, false)
    s"registerClassEntry(classOf[$name])\n"
  }

  def genConstructor(c: JRClass[_], ctor: JRConstructor[_]): String = {
    val params = ctor.getParameterTypes()
        .map(p => s"classOf[${classExpr(p, false)}]")
        .mkString(", ")
    val clsName = stripKnownPackages(c.getName)
    val args = ctor.getParameterTypes()
        .zipWithIndex
        .map { case (p, i) => s"args($i).asInstanceOf[${classExpr(p, true)}]" }
        .mkString(", ")
    s"""        mkConstructor(Array($params)) { args =>
      |          new $clsName($args)
      |        }""".stripMargin
  }

  def genField(@unused c: JRClass[_], f: JRField): String = {
    f.toString
  }

  def genMethod(c: JRClass[_], m: JRMethod): String = {
    val name = m.getName
    val params = m.getParameterTypes()
        .map(p => s"classOf[${classExpr(p, false)}]")
        .mkString(", ")
    val args = m.getParameterTypes()
        .zipWithIndex
        .map { case (p, i) => s"args($i).asInstanceOf[${classExpr(p, true)}]" }
        .mkString(", ")
    s"""        mkMethod(clazz, "$name", Array[Class[_]]($params)) { (obj, args) =>
      |          obj.asInstanceOf[${classExpr(c.value, true)}].$name($args)
      |        }""".stripMargin
  }

  def genClassRegistrationEntry(c: JRClass[_]): String = {
    val name = classExpr(c.value, false)
    val cs = c.getUsedConstructors()
    val constructors = if (cs.isEmpty) ""
    else {
      s""",
        |    constructors = Array(
        |${cs.map(genConstructor(c, _)).mkString(",\n")}
        |    )""".stripMargin
    }
    val fields = if (c.fields.isEmpty) ""
    else {
      s""",
        |    fields = Map(
        |      ${c.fields.map { case (_, f: JRField) => genField(c, f) }.mkString(",\n")}
        |    )""".stripMargin
    }
    val methods = if (c.methods.isEmpty) ""
    else {
      s""",
        |    methods = Map(
        |      ${c.methods.map { case (_, m: JRMethod) => genMethod(c, m) }.mkString(",\n")}
        |    )""".stripMargin
    }
    if (c.fields.isEmpty && c.methods.isEmpty) {
      s"""
        |registerClassEntry(classOf[$name]${constructors}${fields}${methods}
        |)
        |""".stripMargin
    } else {
      s"""
        |{ val clazz = classOf[$name]
        |  registerClassEntry(clazz${constructors}${fields}${methods}
        |  )
        |}
        |""".stripMargin
    }
  }

  def isEmpty(rcls: JRClass[_]): Boolean =
    (rcls.constructors == null || rcls.constructors.isEmpty) && rcls.fields.isEmpty && rcls.methods.isEmpty

  def genClass(cls: Class[_], rcls: JRClass[_], b: mutable.StringBuilder) = {
    if (isEmpty(rcls)) {
      b.append(genEmptyClass(rcls))
    }
    else {
      b.append(genClassRegistrationEntry(rcls))
    }
  }

  def generateReport(): String = {
    val b = new mutable.StringBuilder(100)
    for ( (cls, rcls) <- collectEmptyClasses.sortBy(_._1.getName) ) {
      genClass(cls, rcls, b)
    }
    for ( (cls, rcls) <- collectNonEmptyClasses.sortBy(_._1.getName) ) {
      genClass(cls, rcls, b)
    }
    b.result()
  }

  private def collectEmptyClasses = {
    sigma.reflection.Platform.unknownClasses.toSeq.filter(e =>
      isEmpty(e._2) && // don't contain constructors, fields or methods
          !ReflectionData.classes.contains(e._1)) // not already registered
  }

  private def collectNonEmptyClasses = {
    sigma.reflection.Platform.unknownClasses.toSeq.filter(e =>
      !isEmpty(e._2) &&
          !ReflectionData.classes.contains(e._1))
  }
}
