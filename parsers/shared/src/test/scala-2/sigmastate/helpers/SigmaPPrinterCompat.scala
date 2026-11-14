package sigmastate.helpers

import pprint.PPrinter

/** pprint 0.6.3 controls product field names through Walker. */
abstract class SigmaPPrinterCompat extends PPrinter {
  override def showFieldNames = false
}
