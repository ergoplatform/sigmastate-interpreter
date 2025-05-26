const { Value$, SigmaCompiler$ } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  let compiler = SigmaCompiler$.forMainnet();

  it("Should create SigmaCompiler", () => {
    expect(compiler).not.toBeUndefined();
  });

  let segregatedTreeHex = "100104c801d191a37300";
  let embedddedTreeHex = "00d191a304c801";

  it("SigmaCompiler should compile", () => {
    let treeWithSegregation = compiler.compile({}, true, 0, "sigmaProp(HEIGHT > 100)");
    expect(treeWithSegregation).not.toBeUndefined();
    expect(treeWithSegregation.toHex()).toEqual(segregatedTreeHex)

    let treeWithoutSegregation = compiler.compile({}, false, 0, "sigmaProp(HEIGHT > 100)");
    expect(treeWithoutSegregation.toHex()).toEqual(embedddedTreeHex)
  });

  it("SigmaCompiler should compile with named constants", () => {
    let treeWithSegregation = compiler.compile(
        {"deadline": Value$.ofInt(100)},
        true, 0, "sigmaProp(HEIGHT > deadline)");
    expect(treeWithSegregation).not.toBeUndefined();
    expect(treeWithSegregation.toHex()).toEqual(segregatedTreeHex)
  });
});

