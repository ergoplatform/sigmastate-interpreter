# Comparing serialized ErgoTrees without the header byte

`stripErgoTreeHeader(treeBytes: Coll[Byte]): Coll[Byte]` is an ErgoScript
compiler helper equivalent to `treeBytes.slice(1, treeBytes.size)`.

```scala
val receiptTreeHash = blake2b256(stripErgoTreeHeader(receiptOutput.propositionBytes))
```

To compare two byte collections while ignoring their first byte:

```scala
stripErgoTreeHeader(firstTreeBytes) == stripErgoTreeHeader(secondTreeBytes)
```

The helper lowers to existing collection slice/size operations. It introduces
no serialized opcode, interpreter change, or new costing rule.

It removes exactly one byte, not the complete serialization metadata. Empty
and one-byte inputs yield an empty collection, just like the explicit slice.
The helper does not validate ErgoTree bytes. It does not canonicalize size
prefixes, constant segregation, or otherwise equivalent scripts. In particular,
this comparison alone must not be taken as proof of semantic equivalence of
arbitrary ErgoTrees.
