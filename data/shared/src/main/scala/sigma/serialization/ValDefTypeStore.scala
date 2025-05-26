package sigma.serialization

import sigma.ast.SType

import scala.collection.mutable

class ValDefTypeStore() {

  private val store = mutable.Map[Int, SType]()

  def apply(id: Int): SType = store(id)
  def update(id: Int, tpe: SType): Unit = store(id) = tpe
}
