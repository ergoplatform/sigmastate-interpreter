package sigma

/** Version-specific implementations used by shared sources. */
package object compat {
  /** Keep Scala 2 tags identical to their original supertagged types. */
  type TaggedType[T] = supertagged.TaggedType[T]
}
