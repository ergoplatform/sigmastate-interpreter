const { Type$ } = require("sigmastate-js/main");

describe("Smoke tests for Types", () => {

  it("Should create primitive types", () => {
    expect(Type$.Byte.name).toEqual("Byte");
    expect(Type$.Short.name).toEqual("Short");
    expect(Type$.Int.name).toEqual("Int");
    expect(Type$.Long.name).toEqual("Long");
    expect(Type$.BigInt.name).toEqual("BigInt");
    expect(Type$.GroupElement.name).toEqual("GroupElement");
    expect(Type$.SigmaProp.name).toEqual("SigmaProp");
    expect(Type$.Box.name).toEqual("Box");
    expect(Type$.AvlTree.name).toEqual("AvlTree");
    expect(Type$.Context.name).toEqual("Context");
    expect(Type$.Header.name).toEqual("Header");
    expect(Type$.PreHeader.name).toEqual("PreHeader");
    expect(Type$.SigmaDslBuilder.name).toEqual("SigmaDslBuilder");
  });

  it("Should create complex types", () => {
    expect(Type$.pairType(Type$.Int, Type$.Long).name).toEqual("(Int, Long)");
    expect(Type$.collType(Type$.Int).name).toEqual("Coll[Int]");
    expect(Type$.collType(Type$.pairType(Type$.Int, Type$.Long)).name)
        .toEqual("Coll[(Int, Long)]");
  });
});
