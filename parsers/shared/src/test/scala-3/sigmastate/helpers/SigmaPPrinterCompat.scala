package sigmastate.helpers

import pprint.{PPrinter, Tree}

/** Retain pprint 0.6.3's escaping and positional products with pprint 0.8.1. */
abstract class SigmaPPrinterCompat
    extends PPrinter(defaultEscapeUnicode = true, defaultShowFieldNames = false) {
  def treeify(value: Any): Tree =
    super.treeify(value, defaultEscapeUnicode, defaultShowFieldNames)
}
