package sigma

/**
  * Kiama is a Scala library for language processing. It is a project of the
  * Programming Languages Research Group at Macquarie University. For full
  * project details see the web site `http://github.com/inkytonik/kiama`.
  *
  * Kiama's main components address tree decoration via attribute grammars
  * (package `attribution`), tree transformation via strategic term rewriting
  * (package `rewriting`), dynamic semantics (package `machine`) and
  * pretty-printing (package `output`).
  *
  * The `util` package contains support modules for parsing,  input/output,
  * read-eval-print loops (REPLs) and pattern matching.
  *
  * The `examples` package (available as part of the Kiama tests) contains
  * many examples of using Kiama to solve small to medium language processing
  * problems.
  */
package object kiama {
  /**
    * Convenient type constructor for partial functions.
    */
  type ==>[T, U] = PartialFunction[T, U]
  /**
    * Another convenient type constructor for partial functions.
    */
  type ===>[T] = PartialFunction[T, T]
}
