# ErgoScript LSP (MVP)

> **⚠️ MVP/Prototype Notice**  
> This is a **proof-of-concept** Language Server Protocol implementation for ErgoScript, built as a hackathon MVP. It provides basic IDE support and is NOT a production compiler.

## Overview

ErgoScript LSP provides language support for ErgoScript, the scripting language used by the Ergo blockchain. It enables IDE features like syntax diagnostics, hover documentation, and structured analysis for AI agents.

### What is ErgoScript?

ErgoScript is a declarative scripting language compiled into ErgoTree for the Ergo blockchain. It follows an extended UTXO (eUTXO) model with Sigma-protocol based authentication, enabling powerful smart contracts with zero-knowledge proofs.

**Important:** This is NOT Solidity or an EVM language. It's specific to the Ergo blockchain.

## Features

✅ **Implemented:**
- Syntax diagnostics (unmatched braces, parentheses, brackets)
- Basic malformed expression detection
- Hover documentation for common ErgoScript keywords
- Token-based parsing for AI agent consumption
- VS Code extension integration

❌ **Out of Scope:**
- Full ErgoScript compiler
- Type inference and checking
- Formal verification
- Code optimization
- EVM/Solidity concepts (gas, reentrancy, accounts)

## Installation & Usage

### Prerequisites

- Node.js >= 18.0.0
- VS Code >= 1.75.0
- npm or yarn

### Setup

1. **Clone and install dependencies:**

```bash
cd ergoscript-lsp
npm install
```

2. **Compile the project:**

```bash
npm run compile
```

This compiles both the server and client.

3. **Run in VS Code:**

- Open the `ergoscript-lsp` folder in VS Code
- Press `F5` to launch the Extension Development Host
- In the new window, open `examples/sample.es`

### Testing

**Diagnostics:**
- Open `examples/sample.es`
- Uncomment the error examples at the bottom
- Red squiggly lines should appear under syntax errors
- Hover over errors to see diagnostic messages

**Hover Documentation:**
- Hover over keywords like `SigmaProp`, `HEIGHT`, `OUTPUTS`, `INPUTS`
- Documentation should appear in a popup

## Architecture

```
ergoscript-lsp/
├── server/              # Language server (Node.js/TypeScript)
│   ├── src/
│   │   ├── index.ts     # LSP entry point
│   │   ├── parser.ts    # Tokenization and parsing
│   │   ├── diagnostics.ts  # Syntax validation
│   │   └── hover.ts     # Hover provider
├── client/              # VS Code extension
│   ├── src/
│   │   └── extension.ts # Extension activation
│   ├── syntaxes/        # TextMate grammar
│   └── package.json     # Extension manifest
└── examples/
    └── sample.es        # Example ErgoScript file
```

### How It Works

1. **Client (VS Code Extension):**
   - Registers `.es` and `.ergo` file extensions
   - Starts the language server as a Node.js process
   - Forwards LSP requests/responses

2. **Server (Language Server):**
   - Receives document open/change events
   - Tokenizes ErgoScript code
   - Validates syntax (brace matching, etc.)
   - Provides hover documentation
   - Outputs structured token data for AI agents

3. **Parser:**
   - Simple token-based parsing (not a full grammar)
   - Recognizes keywords, operators, literals, identifiers
   - Extracts structural information (braces, etc.)

## Supported Keywords

The LSP recognizes and provides documentation for:

**Context Variables:**
- `HEIGHT`, `OUTPUTS`, `INPUTS`, `SELF`, `CONTEXT`

**Types:**
- `SigmaProp`, `Box`, `Coll`, `Option`, `Int`, `Long`, `Boolean`

**Functions:**
- `sigmaProp`, `proveDlog`, `anyOf`, `allOf`, `atLeast`

**Properties:**
- `tokens`, `value`, `propositionBytes`, `id`

**Collection Methods:**
- `size`, `exists`, `forall`, `map`, `filter`, `fold`

## AI Agent Support

The language server outputs structured token data to the console, which can be consumed by AI agents for code analysis:

```json
{
  "uri": "file:///path/to/file.es",
  "version": 1,
  "tokens": [
    { "type": "keyword", "value": "val", "position": { "line": 0, "column": 2 } },
    { "type": "identifier", "value": "x", "position": { "line": 0, "column": 6 } }
  ]
}
```

## Limitations

This is an MVP with intentional scope limitations:

- **No type system:** No type inference or checking
- **Simple parsing:** Regex/token-based, not a full AST
- **Limited diagnostics:** Only basic syntax errors
- **No code completion:** Only hover support implemented
- **No go-to-definition:** Not implemented in this version

## Future Enhancements

Potential improvements for a production version:

- Full ErgoScript grammar parser
- Type inference and validation
- Code completion (IntelliSense)
- Go-to-definition and find references
- Code formatting
- Snippet support
- Integration with ErgoScript compiler
- More comprehensive diagnostics

## Contributing

This is a hackathon MVP. For production ErgoScript tooling, refer to:
- [Ergo Platform Documentation](https://docs.ergoplatform.com/)
- [ErgoScript GitHub](https://github.com/ScorexFoundation/sigmastate-interpreter)

## License

MIT License - This is an experimental project for educational purposes.

## Resources

- [ErgoScript Tutorial](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/)
- [VS Code Extension API](https://code.visualstudio.com/api)
