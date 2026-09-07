package sigma.compiler.ir

import scala.language.existentials

/** Retains the original tuple existentials for extractors with one hidden type occurrence. */
trait SingleTypeExtractorSupport { self: IRContext =>
  type ThunkDefArgs = (Ref[T], Schedule) forSome { type T }
  type BoxGetRegArgs = (Ref[Box], Ref[Int], Elem[T]) forSome { type T }
  type ContextGetVarArgs = (Ref[Context], Ref[Byte], Elem[T]) forSome { type T }
  type SubstConstantsArgs = (Ref[SigmaDslBuilder], Ref[Coll[Byte]], Ref[Coll[Int]], Ref[Coll[T]]) forSome { type T }
  type DeserializeToArgs = (Ref[SigmaDslBuilder], Ref[Coll[Byte]], Elem[T]) forSome { type T }
}
