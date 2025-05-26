package sigma.util

/** Helper methods for pretty printing of collections and optional values. */
object PrintExtensions {
  implicit class AnyExtension[A](x: A) {
    /** Prints `x` if `p(x)` is true, otherwise prints `default`. */
    def when(
        p: A => Boolean,
        show: A => String,
        default: String = "") = if (p(x)) show(x) else default
  }

  implicit class IterableExtensions[A](val it: Iterable[A]) extends AnyVal {
    /** Prints `show(it)` if `it` is not empty, otherwise prints `default`. */
    def opt(show: Iterable[A] => String = _.mkString, default: String = ""): String =
      if (it.isEmpty) default else show(it)

    /** Prints `show(x)` for each `x` in `it` and `sep` between elements. */
    def rep(
        show: A => String = _.toString,
        sep: String = ", "): String = it.map(show).mkString(sep)

    def asTypeParams(show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString("[", ", ", "]") else ""

    /** Prints `start` and `end` around content, where
      * content = `show(x)` for each `x` in `it` and `sep` between elements. */
    def optList(
        start: String,
        end: String,
        sep: String = ", ",
        show: A => String = _.toString) =
      if (it.nonEmpty) it.map(show).mkString(start, sep, end) else ""
  }

  implicit class OptionExtensions[A](val opt: Option[A]) extends AnyVal {
    /** Prints `show(a)` if `opt = Some(a)`, otherwise prints `default`. */
    def opt(show: A => String = _.toString, default: String = ""): String = opt match {
      case None => default
      case Some(a) => show(a)
    }

    /** Returns the given value if the `opt` is defined, otherwise an empty string. */
    def ifDefined(value: String): String = if (opt.isDefined) value else ""
  }

  implicit class BooleanExtensions(val opt: Boolean) extends AnyVal {
    /** Prints `show` if `opt` is true, otherwise prints `default`. */
    def opt(show: => String, default: => String = ""): String = if (opt) show else default
  }

  /** Joins the string representations of the given elements.
    *
    * This function takes a variable number of arguments and returns a string that is the
    * concatenation of their string representations. If an argument is an Iterable, its
    * elements are recursively joined using this function. Empty strings are filtered out
    * before joining.
    *
    * @param xs The elements to join
    * @return The joined string
    */
  def join(xs: Any*) = xs.map {
    case x: Iterable[_] => x.rep()
    case x => x.toString
  }.filter(_.nonEmpty).rep()
}
