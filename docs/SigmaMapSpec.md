# SigmaMap — Ordering Contract Specification

Version 1.0 · Reference implementation: `data/shared/src/main/scala/sigma/interpreter/SigmaMap.scala`
Conformance data: [`sc/jvm/src/test/resources/sigmastate/SigmaMapVectors.json`](../sc/jvm/src/test/resources/sigmastate/SigmaMapVectors.json)

This document specifies `SigmaMap`, the map data structure used to hold context variables
(`ContextExtension.values`), in an implementation-neutral way so that it can be re-implemented
in other languages (e.g., Rust). Its **traversal ordering is pinned to the default
`scala.collection.immutable.Map` of the Scala 2.12 SDK** so that serialization output and any
order-dependent behavior remain identical across implementations and across Scala versions.

## 1. Definitions

* **Key**: a byte value in the range `0..127`. Negative keys are invalid input.
* **Binding**: a `(key, value)` pair. Values are opaque to this specification (in the Scala
  implementation they are `EvaluatedValue[_ <: SType]`; a Rust implementation may be generic
  over the value type).
* **Map**: a finite set of bindings with distinct keys.
* **Traversal** (iteration): enumerating all bindings of a map exactly once in a defined order.
  Traversal order is part of the observable behavior specified here.

## 2. Construction contract

A map is constructed from a sequence of bindings `b1, b2, ..., bn`.

1. **Duplicate resolution** — if a key occurs more than once, exactly one binding is kept:
   * position = position of its **first** occurrence;
   * value = value of its **last** occurrence.
   This mirrors building a `scala.collection.immutable.Map` via repeated `updated` calls.
2. All construction paths (factory from pairs, parallel arrays, deserialization) MUST apply
   this normalization, so that the resulting traversal depends only on the given sequence of
   bindings, never on the internal representation chosen.

## 3. Traversal contract

Let `n` be the number of distinct keys after duplicate resolution.

### 3.1 Small maps (`n <= 4`) — insertion-order regime

Bindings are traversed in their normalized insertion order (first-occurrence positions),
i.e., in exactly the order of the deduplicated input sequence.

This reproduces the behavior of the dedicated small-map representations
(`Map1..Map4` classes) of Scala 2.12's default immutable `Map`.

### 3.2 Large maps (`n > 4`) — hash-trie regime

Bindings are traversed in the order induced by the fixed table `INDICES` below: enumerate
`INDICES` and yield each key that is present in the map.

Equivalently, `traversal(map) = [k for k in INDICES if k in map]`.

Consequently, for `n > 4` the traversal order is a pure function of the **key set** and is
independent of the insertion order. This reproduces the hash-trie iteration of Scala 2.12's
`immutable.HashMap` (the representation used by the default `Map` once it exceeds 4 entries).

```text
INDICES (128 entries; the i-th entry is traversed i-th whenever present):
69, 101, 0, 88, 115, 5, 120, 10, 56, 42, 24, 37, 25, 52, 14, 110, 125, 20,
46, 93, 57, 78, 29, 106, 121, 84, 61, 89, 116, 1, 74, 6, 60, 117, 85, 102,
28, 38, 70, 21, 33, 92, 65, 97, 9, 53, 109, 124, 77, 96, 13, 41, 73, 105, 2,
32, 34, 45, 64, 17, 22, 44, 59, 118, 27, 71, 12, 54, 49, 86, 113, 81, 76, 7,
39, 98, 103, 91, 66, 108, 3, 80, 35, 112, 123, 48, 63, 18, 95, 50, 67, 16,
127, 31, 11, 72, 43, 99, 87, 104, 40, 26, 55, 114, 23, 8, 75, 119, 58, 82,
36, 30, 51, 19, 107, 4, 126, 79, 94, 47, 15, 68, 62, 90, 111, 122, 83, 100
```

`INDICES` is the traversal order of a map containing ALL keys `0..127`. It was recorded from
the real Scala library and can be independently derived as the level-order enumeration of the
Scala 2.12 `HashTrieMap` built over the improved hashes
`improve(k) = { h = k + ~(k << 9); h ^= h >>> 14; h += h << 4; h ^= h >>> 10 }` (32-bit
arithmetic), where at trie level `L` children are ordered by `(hash >>> (5*L)) & 0x1f`.

### 3.3 Restriction property (MUST hold)

For any key set `S` with `|S| > 4`:

```
traversal(S) == filter(INDICES, S)      # both sides enumerate the same keys in equal order
```

i.e., the traversal of any subset is the restriction of the full-set traversal. Implementations
and datasets can use this property for cross-checking.

## 4. Operations

For a map `m`:

| Operation | Semantics |
|---|---|
| `contains(k)` | `true` iff `k` is bound; `false` for out-of-domain keys (no panic on `k < 0` or `k > maxKey`) |
| `get(k)` | the bound value wrapped in "some", or "none" if absent |
| `size` | number of distinct bindings |
| `isEmpty` | `size == 0` |
| `maxKey` | maximum bound key; `-1` for the empty map |
| `iterator()` | traversal per Section 3 |

## 5. Equality

Two maps are equal iff they contain the same set of bindings. **Equality MUST NOT depend on
insertion or traversal order** — two maps constructed with different orders but identical
bindings are equal even when they traverse differently (`n <= 4` case).

Implementations SHOULD also make `hashCode` order-independent. For bit-level compatibility
with the Scala/JVM implementation (informational, not required for conformance): mix each
entry hash `h = murmur3_mix(murmur3_mix(productSeed, k)(v))` finalized over arity 2 into the
unordered accumulator `(a += h, b ^= h, c *= h if h != 0)` and finalize with
`murmur3_mapSeed`, exactly as `scala.collection.Map` does.

## 6. Reference algorithm

```rust
pub struct SigmaMap<V> {
    entries: Vec<(u8, V)>,   // deduplicated bindings in normalized insertion order
}

impl<V: Clone> SigmaMap<V> {
    pub fn new(bindings: &[(u8, V)]) -> Self {
        let mut entries: Vec<(u8, V)> = Vec::new();
        for (k, v) in bindings {
            match entries.iter().position(|(ek, _)| ek == k) {
                Some(i) => entries[i].1 = v.clone(), // last value wins, position kept
                None => entries.push((*k, v.clone())),
            }
        }
        SigmaMap { entries }
    }

    pub fn contains(&self, k: u8) -> bool {
        self.entries.iter().any(|(ek, _)| *ek == k)
    }

    pub fn get(&self, k: u8) -> Option<&V> {
        self.entries.iter().find(|(ek, _)| *ek == k).map(|(_, v)| v)
    }

    /// Traversal per Section 3.
    pub fn iter(&self) -> impl Iterator<Item = &(u8, V)> {
        if self.entries.len() > 4 {
            const INDICES: [u8; 128] = [ /* table from Section 3.2 */ ];
            let entries = self.entries.as_slice();
            INDICES.iter().filter_map(move |&ik| {
                entries.iter().find(|(ek, _)| *ek == ik)
            })
        } else {
            self.entries.iter()
        }
    }
}
```

(The sketch favors clarity over performance; production code may index entries by key.)

## 7. Conformance testing

`SigmaMapVectors.json` (and the equivalent line-based dataset resource
`SigmaMapVectors.txt` consumed by the Scala tests) contains golden vectors recorded from a
real Scala 2.12.20 run:

```jsonc
{
  "meta": { "scalaVersion": "...", "seed": 20250821, "smallMapMaxSize": 4, ... },
  "fullOrder128": [ /* 128 ints: traversal of ALL keys 0..127 */ ],
  "vectors": [
    { "size": 3, "insertionKeys": [31, 73, 35], "expectedOrder": [73, 35, 31] },
    ...
  ]
}
```

A conforming implementation MUST satisfy, for every vector:

1. Build a map from `insertionKeys` (values may be arbitrary, e.g., the key itself);
   iterating the map yields keys exactly in `expectedOrder`.
2. **Self-check of the dataset** (no implementation needed):
   * `fullOrder128` is a permutation of `0..127`;
   * every vector's `expectedOrder` is a permutation of `insertionKeys`;
   * for `size <= 4`: `expectedOrder == insertionKeys`;
   * for `size > 4`: `expectedOrder == filter(fullOrder128, set(insertionKeys))`.

Additionally, equality checks: maps built from a vector's keys in different permutations must
be equal (Section 5) while traversing per Sections 3.1/3.2.

The current dataset contains **16384 unique vectors** (881 small, 15503 large); no vector is
repeated. Consumers SHOULD fail hardly if counts change unexpectedly after a regeneration.

## 8. Wire-format notes (ContextExtension)

On-chain serialization of context variables is ordered pairs: `[UByte count][key: Byte, value:
Value]*`. Constraints: `count <= 127` (a `UByte` payload limit enforced with an error above
it), keys non-negative. Serialization writes bindings **in traversal order**, hence the
ordering contract above determines the exact bytes produced for a given set of variables;
deserialization preserves the wire order as the insertion order.

## 9. Provenance & regeneration

Vectors were recorded by observing the runtime library itself (`scala.collection.immutable.Map`)
under Scala 2.12.20, validating every large vector against multiple construction paths
(vararg factory, `foldLeft(+ )`, `toMap`, growth from a 4-entry map, shrinking by removals,
in-place replacement) before recording. Regenerate with:

```
sbt '++2.12.20 scJVM/Test/runMain sigmastate.SigmaMapVectorGenerator \
    <path>/SigmaMapVectors.txt <path>/SigmaMapVectors.json'
```

The committed dataset resource (`sc/jvm/src/test/resources/sigmastate/SigmaMapVectors.txt`,
loaded by the JVM conformance tests) and the JSON export are checked for three-way lockstep
equivalence by a dedicated property in `SigmaMapLiveDifferentialSpecification`.
