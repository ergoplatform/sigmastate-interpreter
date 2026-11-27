const { Value$, SigmaCompiler$, ErgoTree$ } = require("sigmastate-js/main");

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

  it("SigmaCompiler should scope VersionContext from treeHeader for v4 verifyStark", () => {
    const script = "sigmaProp(verifyStark(Coll(Coll(1.toByte)), Coll(2.toByte), Coll(3.toByte), Coll(4.toByte)))";

    const v4Segregated = compiler.compile({}, true, 4, script);
    expect(v4Segregated.version()).toEqual(4);
    expect(v4Segregated.hasSize()).toEqual(true);
    expect(v4Segregated.isConstantSegregation()).toEqual(true);
    expect(ErgoTree$.fromHex(v4Segregated.toHex()).toHex()).toEqual(v4Segregated.toHex());

    const v4Embedded = compiler.compile({}, false, 4, script);
    expect(v4Embedded.version()).toEqual(4);
    expect(v4Embedded.hasSize()).toEqual(true);
    expect(v4Embedded.isConstantSegregation()).toEqual(false);
    expect(ErgoTree$.fromHex(v4Embedded.toHex()).toHex()).toEqual(v4Embedded.toHex());

    let v3Error = null;
    try {
      compiler.compile({}, true, 3, script);
    } catch (e) {
      v3Error = String(e);
    }
    expect(v3Error).toMatch(/ErgoTree version 4/);
  });
});

