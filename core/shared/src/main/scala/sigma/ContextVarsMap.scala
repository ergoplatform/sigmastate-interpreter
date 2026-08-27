package sigma

trait ContextVarsMap {

  def maxKey: Byte

  def getNullable(key: Byte): AnyValue

  def anyIterator: Iterator[(Byte, AnyValue)]

}
