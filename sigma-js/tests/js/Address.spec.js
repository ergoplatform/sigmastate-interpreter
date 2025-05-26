const { Address, Address$ } = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
  it("Should export Address object", () => {
    expect(Address).not.toBeUndefined();
  });
});

describe("Address", () => {
  let addrStr = "9iJd9drp1KR3R7HLi7YmQbB5sJ5HFKZoPb5MxGepamggJs5vDHm";
  let p2sStr = "JryiCXrZf5VDetH1PM7rKDX3q4sLF34AdErWJFqG87Hf5ikTDf636b35Nr7goWMdRUKA3ZPxdeqFNbQzBjhnDR9SUMYwDX1tdV8ZuGgXwQPaRVcB9"

  it("roundtrip for P2PK", () => {
    let addr = Address$.fromString(addrStr);
    expect(addr.isP2PK()).toEqual(true)
    expect(addr.isP2S()).toEqual(false)

    expect(addr.toString()).not.toBeUndefined();
    expect(addr.toString()).toEqual(addrStr)
    expect(addr.asP2PK()).not.toBeUndefined();
  });

  it("roundtrip for P2S", () => {
    let addr = Address$.fromString(p2sStr);
    expect(addr.isP2S()).toEqual(true)
    expect(addr.isP2PK()).toEqual(false)

    expect(addr.toString()).not.toBeUndefined();
    expect(addr.toString()).toEqual(p2sStr)
    expect(addr.asP2S()).not.toBeUndefined();
  });

  it("toSigmaPropOpt", () => {
    let addr = Address$.fromString(addrStr);

    expect(addr.isMainnet()).toEqual(true)
    expect(addr.toSigmaPropOpt()).not.toBeUndefined()
  });

  it("other properties", () => {
    let addr = Address$.fromString(addrStr);
    expect(addr.toErgoTree()).not.toBeUndefined()
    expect(addr.asP2PK()).not.toBeUndefined()
    expect(addr.asP2PK().isP2PK()).toEqual(true)
    expect(addr.asP2PK().toErgoTree()).not.toBeUndefined()
    expect(addr.asP2PK().toSigmaProp()).not.toBeUndefined()
    expect(addr.asP2PK().toPropositionBytes()).not.toBeUndefined()
    expect(addr.asP2PK().addressTypePrefix()).not.toBeUndefined()
    expect(addr.asP2PK().getPublicKeyGE()).not.toBeUndefined()
  });

});

