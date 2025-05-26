const { ErgoTree, ErgoTree$ } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  it("Should export ErgoTree object", () => {
    expect(ErgoTree).not.toBeUndefined();
  });
});

describe("Smoke tests for ErgoTree", () => {
  let hex = "100604000e000400040005000500d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202";

  it("Should create fromHex", () => {
    let tree = ErgoTree$.fromHex(hex);
    expect(tree.toString()).not.toBeUndefined();
    expect(tree.toHex()).toEqual(hex)
  });

  it("Has properties", () => {
    let tree = ErgoTree$.fromHex(hex);

    expect(tree.header()).toEqual(0x10)
    expect(tree.version()).toEqual(0)
    expect(tree.isConstantSegregation()).toEqual(true)
    expect(tree.hasSize()).toEqual(false)
  });

  it("Has constants", () => {
    let tree = ErgoTree$.fromHex(hex);
    let constants = tree.constants().map(c => c.toHex())
    expect(constants).toEqual(["0400", "0e00", "0400", "0400", "0500", "0500"])
  });

  it("Has template", () => {
    let tree = ErgoTree$.fromHex(hex);
    let templateHex = tree.templateHex();
    expect(templateHex).toEqual("d803d601e30004d602e4c6a70408d603e4c6a7050595e67201d804d604b2a5e4720100d605b2db63087204730000d606db6308a7d60799c1a7c17204d1968302019683050193c27204c2a7938c720501730193e4c672040408720293e4c672040505720393e4c67204060ec5a796830201929c998c7205029591b1720673028cb272067303000273047203720792720773057202")
  });
});

