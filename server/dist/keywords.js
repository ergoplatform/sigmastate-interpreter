"use strict";
/**
 * ErgoScript keywords and their documentation
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.getKeywordDocumentation = exports.KEYWORD_NAMES = exports.ERGOSCRIPT_KEYWORDS = void 0;
exports.ERGOSCRIPT_KEYWORDS = [
    {
        name: "val",
        documentation: "val - Defines an immutable value binding",
    },
    {
        name: "def",
        documentation: "def - Defines a function",
    },
    {
        name: "if",
        documentation: "if - Conditional expression (if-then-else)",
    },
    {
        name: "else",
        documentation: "else - Alternative branch of conditional",
    },
    {
        name: "for",
        documentation: "for - Loop construct",
    },
    {
        name: "match",
        documentation: "match - Pattern matching expression",
    },
    {
        name: "case",
        documentation: "case - Pattern in a match expression",
    },
    {
        name: "sigmaProp",
        documentation: "sigmaProp - Creates a sigma proposition from a boolean",
    },
    {
        name: "proveDlog",
        documentation: "proveDlog - Proves knowledge of discrete logarithm",
    },
    {
        name: "proveDhTuple",
        documentation: "proveDhTuple - Proves Diffie-Hellman tuple",
    },
    {
        name: "AND",
        documentation: "AND - Logical AND operator",
    },
    {
        name: "OR",
        documentation: "OR - Logical OR operator",
    },
    {
        name: "THRESHOLD",
        documentation: "THRESHOLD - M-of-N threshold signature",
    },
    {
        name: "INPUTS",
        documentation: "INPUTS - Array of inputs in the transaction",
    },
    {
        name: "OUTPUTS",
        documentation: "OUTPUTS - Array of outputs in the transaction",
    },
    {
        name: "HEIGHT",
        documentation: "HEIGHT - Current blockchain height",
    },
    {
        name: "SELF",
        documentation: "SELF - Current box being validated",
    },
    {
        name: "true",
        documentation: "true - Boolean true value",
    },
    {
        name: "false",
        documentation: "false - Boolean false value",
    },
    {
        name: "Option",
        documentation: "Option - Optional value type (Some/None)",
    },
    {
        name: "Some",
        documentation: "Some - Wraps a value in Option",
    },
    {
        name: "None",
        documentation: "None - Represents absence of value",
    },
];
exports.KEYWORD_NAMES = exports.ERGOSCRIPT_KEYWORDS.map((k) => k.name);
function getKeywordDocumentation(keyword) {
    const info = exports.ERGOSCRIPT_KEYWORDS.find((k) => k.name === keyword);
    return info?.documentation;
}
exports.getKeywordDocumentation = getKeywordDocumentation;
//# sourceMappingURL=keywords.js.map