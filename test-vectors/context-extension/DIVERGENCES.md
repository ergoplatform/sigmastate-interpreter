# ContextExtension byte-order divergence: Scala 2.12 vs 2.13

This document compares the byte output of `ContextExtension.serializer` across Scala 2.12
(`immutable.HashMap` backed by `HashTrieMap`) and Scala 2.13 (`immutable.HashMap` backed by
CHAMP). Inputs and methodology are described in
[../../data/jvm/src/test/scala/sigma/interpreter/ContextExtensionTestVectors.scala](../../data/jvm/src/test/scala/sigma/interpreter/ContextExtensionTestVectors.scala).

Source vectors:
- [2.12.json](2.12.json) (`scala.collection.immutable.HashMap$HashTrieMap`)
- [2.13.json](2.13.json) (`scala.collection.immutable.HashMap`, CHAMP)

## Summary

| Fixture              | Iteration order | Bytes identical? |
|----------------------|-----------------|------------------|
| size-0               | `[]`            | ✅               |
| size-1               | `[0]`           | ✅               |
| size-2               | `[0, 1]`        | ✅               |
| size-3               | `[0, 1, 2]`     | ✅               |
| size-4               | `[0, 1, 2, 3]`  | ✅               |
| size-5-contiguous    | `[0, 1, 2, 3, 4]` | ✅             |
| size-6-contiguous    | `[0, 5, 1, 2, 3, 4]` | ✅          |
| size-7-contiguous    | `[0, 5, 1, 6, 2, 3, 4]` | ✅       |
| size-8-contiguous    | `[0, 5, 1, 6, 2, 7, 3, 4]` | ✅    |
| size-16-contiguous   | `[0, 5, 10, 14, 1, 6, 9, 13, 2, 12, 7, 3, 11, 8, 4, 15]` | ✅ |
| **size-8-sparse**    | **diverges** (see below) | ❌      |

**Headline:** 10 of 11 fixtures produce byte-identical output across 2.12 and 2.13. Only the
sparse-key fixture diverges.

## The one divergence: size-8-sparse

Input keys: `[0, 7, 13, 31, 64, 100, 120, 127]` — non-contiguous bytes that span the full
positive range and cause bucket collisions at the top level of the trie (e.g. 0 and 64 share
the low 5 hash bits; 31 and 127 likewise).

|        | Iteration order                                  | Serialized hex |
|--------|--------------------------------------------------|----------------|
| 2.12   | `[0, 120, 13, 64, 7, 127, 31, 100]`              | `080004007804f0010d041a4004800107040e7f04fe011f043e6404c801` |
| 2.13   | `[0, 120, 13, 64, 7, 100, 127, 31]`              | `080004007804f0010d041a4004800107040e6404c8017f04fe011f043e` |

The first five entries (`0, 120, 13, 64, 7`) appear in the same order in both versions. The
last three entries differ:

- 2.12 tail: `127, 31, 100`
- 2.13 tail: `100, 127, 31`

Diff against the byte string (column-aligned for clarity):

```
2.12: 0800 04 00 7804f001 0d041a 4004 8001 07040e 7f04fe01 1f043e 6404c801
2.13: 0800 04 00 7804f001 0d041a 4004 8001 07040e 6404c801 7f04fe01 1f043e
                                                ^^^^^^^^^^ ^^^^^^^^^^
                                                tail differs in element order, not content
```

(Whitespace inserted for readability; actual files have no spaces.)

## What this means for consensus

`ErgoLikeTransaction.id` is `Blake2b256.hash(messageToSign)`, and `messageToSign` is computed
by **re-serializing** the in-memory representation
([ErgoLikeTransaction.scala:64,100,192–198](../../data/shared/src/main/scala/org/ergoplatform/ErgoLikeTransaction.scala#L64)).
Therefore a node that re-serializes a TX containing a sparse `ContextExtension` (like
size-8-sparse above) will compute a different TX ID under 2.13 than under 2.12.

Implications:

- **Contiguous keys are safe across 2.12 and 2.13.** Any TX whose context vars use keys
  `0, 1, ..., n-1` (the conventional pattern in ErgoScript) is unaffected.
- **Sparse keys are unsafe across 2.12 and 2.13.** A TX with non-contiguous context vars
  could trigger a consensus split between 2.12 and 2.13 nodes — divergent TX IDs, divergent
  Merkle roots, divergent block validation.
- **The window is narrow but real.** The risk is concrete enough that the project's implicit
  Scala-2.12 pin is load-bearing for any mainnet TX that ever uses a sparse extension layout.

## Why the contiguous cases agree

Both `HashTrieMap` (2.12) and the CHAMP-based `HashMap` (2.13) hash `Byte` keys identically
(`Byte.hashCode == byte.toInt`) and place them into trie buckets using the same low-bit slice
strategy at level 0. For contiguous keys in the low byte range, each key lands in a distinct
top-level bucket and no collision-resolution occurs. With no collisions, the two trie
implementations walk the same bucket order and emit entries in the same sequence.

Divergence appears only when bucket collisions force the trie to recurse into sub-nodes,
where the two implementations organise overflow chains differently. The sparse fixture
exercises exactly that path (keys 0 and 64 collide on the low-5-bit slot; keys 31 and 127
also collide). Hence the difference is concentrated at the tail of the iteration.

## Reproducibility

```bash
sbt "++2.12.21 dataJVM/Test/runMain sigma.interpreter.ContextExtensionTestVectors"
sbt "++2.13.18 dataJVM/Test/runMain sigma.interpreter.ContextExtensionTestVectors"
diff 2.12.json 2.13.json
```

Same machine, different `sbt ++` cross-build, immediately reproducible.
