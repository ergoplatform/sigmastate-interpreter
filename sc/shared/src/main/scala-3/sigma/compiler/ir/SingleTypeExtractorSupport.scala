package sigma.compiler.ir

/** Each hidden type occurs once, so these extractors need no correlated carrier. */
trait SingleTypeExtractorSupport { self: IRContext =>
  type ThunkDefArgsOf[T] = (Ref[T], Schedule)
  type ThunkDefArgs = ThunkDefArgsOf[_]
  type BoxGetRegArgsOf[T] = (Ref[Box], Ref[Int], Elem[T])
  type BoxGetRegArgs = BoxGetRegArgsOf[_]
  type ContextGetVarArgsOf[T] = (Ref[Context], Ref[Byte], Elem[T])
  type ContextGetVarArgs = ContextGetVarArgsOf[_]
  type SubstConstantsArgsOf[T] = (Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Int]], Ref[Coll[T]])
  type SubstConstantsArgs = SubstConstantsArgsOf[_]
  type DeserializeToArgsOf[T] = (Ref[SigmaDslBuilder], Ref[Coll[Byte]], Elem[T])
  type DeserializeToArgs = DeserializeToArgsOf[_]
}
