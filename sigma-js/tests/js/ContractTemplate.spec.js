const {ContractTemplate, ContractTemplate$, Value$} = require("sigmastate-js/main");

describe("Smoke tests for API exporting", () => {
    it("Should export ContractTempate object", () => {
        expect(ContractTemplate).not.toBeUndefined();
    });
});

describe("ContractTemplate", () => {
    let templateJsonStr = "{\n" +
        "  \"treeVersion\" : null,\n" +
        "  \"name\" : \"TestContractTemplate\",\n" +
        "  \"description\" : \"TestContractTemplateDescription\",\n" +
        "  \"constTypes\" : [\n" +
        "    \"02\",\n" +
        "    \"02\",\n" +
        "    \"02\"\n" +
        "  ],\n" +
        "  \"constValues\" : [\n" +
        "    10,\n" +
        "    20,\n" +
        "    30\n" +
        "  ],\n" +
        "  \"parameters\" : [\n" +
        "    {\n" +
        "      \"name\" : \"p1\",\n" +
        "      \"description\" : \"p1_description\",\n" +
        "      \"constantIndex\" : 0\n" +
        "    },\n" +
        "    {\n" +
        "      \"name\" : \"p2\",\n" +
        "      \"description\" : \"p2_description\",\n" +
        "      \"constantIndex\" : 1\n" +
        "    },\n" +
        "    {\n" +
        "      \"name\" : \"p3\",\n" +
        "      \"description\" : \"p3_description\",\n" +
        "      \"constantIndex\" : 2\n" +
        "    }\n" +
        "  ],\n" +
        "  \"expressionTree\" : \"d1939a730073017302\"\n" +
        "}";

    let template = ContractTemplate$.fromJsonString(templateJsonStr);

    it("Json encoding roundtrip", () => {
            expect(template).not.toBeUndefined();
            expect(template.toJsonString()).toEqual(templateJsonStr);
        }
    );

    it("applyTemplate", () => {
            let templateValues = {
                "p1": Value$.ofByte(10),
                "p2": Value$.ofByte(40),
                "p3": Value$.ofByte(50)
            };
            let tree = template.applyTemplate(2, templateValues);
            expect(tree.toHex()).toEqual("1a1003020a02280232d1939a730073017302");
        }
    );

});

