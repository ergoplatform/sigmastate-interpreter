/**
 * ErgoScript keywords and their documentation
 */

export interface KeywordInfo {
  name: string;
  documentation: string;
}

export const ERGOSCRIPT_KEYWORDS: KeywordInfo[] = [
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

export const KEYWORD_NAMES = ERGOSCRIPT_KEYWORDS.map((k) => k.name);

export function getKeywordDocumentation(keyword: string): string | undefined {
  const info = ERGOSCRIPT_KEYWORDS.find((k) => k.name === keyword);
  return info?.documentation;
}
