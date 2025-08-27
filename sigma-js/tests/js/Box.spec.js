const {Box$, Value$} = require("sigmastate-js/main");

describe("Box", () => {
    it("should serialize from/to hex", () => {
        let boxHex = "63b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e01"
        let deserialized = Value$.fromHex(boxHex);
        let expected = {
            additionalRegisters: {
                R4: '0101',
                R5: '0e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d65',
                R6: '05a4a7b5a2e7a4a4dd3a',
                R7: '05feffffffffffffffff01'
            },
            assets: [],
            boxId: '3a0089be265460e29ca47d26e5b55a6f3e3ffaf5b4aed941410a2437913848ad',
            creationHeight: 1000000,
            ergoTree: '00d1968302010100ff83020193040204020100',
            index: 1,
            transactionId: '003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e',
            value: '12345'
        }
        let fleetBox = deserialized.data.box
        expect(fleetBox).toEqual(expected)

        let newBoxValue = Value$.ofBox(fleetBox)
        expect(newBoxValue.toHex()).toEqual(boxHex)
    });
});