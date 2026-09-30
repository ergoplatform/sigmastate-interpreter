# ErgoScript Language Server Protocol (LSP)

**Modern IDE support for ErgoScript smart contract development**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Overview

This project provides a complete Language Server Protocol (LSP) implementation for ErgoScript, the smart contract language for the Ergo blockchain. It brings modern IDE features to ErgoScript development.

**Features:**
- ✅ **Autocomplete** - IntelliSense for keywords, functions, types, and variables
- ✅ **Diagnostics** - Real-time error checking and warnings
- ✅ **Hover Information** - Type information and documentation on hover
- ✅ **Syntax Highlighting** - Full syntax highlighting for ErgoScript
- ✅ **VS Code Extension** - Ready-to-use extension for Visual Studio Code

---

## Quick Start

### Installation

#### Option 1: Install from VSIX (Recommended)

1. Download `ergoscript-lsp-0.1.0.vsix`
2. Open VS Code
3. Go to Extensions (Ctrl+Shift+X)
4. Click "..." menu → "Install from VSIX..."
5. Select the downloaded file

#### Option 2: Build from Source

```bash
# Clone the repository
cd sigmastate-interpreter/ergoscript-lsp

# Install server dependencies
cd server
npm install
npm run compile

# Install client dependencies
cd ../client
npm install
npm run compile

# Package extension
npm run package
```

### Usage

1. **Create an ErgoScript file** with `.es` extension
2. **Start typing** - autocomplete will suggest keywords, functions, and variables
3. **Hover over symbols** - see type information and documentation
4. **Get instant feedback** - errors and warnings appear as you type

---

## Features Demo

### Autocomplete

Type `HEI` and get suggestions:
- `HEIGHT` - Current blockchain height
- Other context variables

Type `SELF.` and get box properties:
- `value` - Monetary value in NanoErg
- `propositionBytes` - Guarding script
- `tokens` - Token collection
- `R4`, `R5`, ... - Registers

Type `blake` and get:
- `blake2b256` - Blake2b hash function

### Hover Information

Hover over `proveDlog` to see:
```
proveDlog(value: GroupElement) => SigmaProp
Creates a sigma proposition for discrete logarithm proof.
Used for signature verification.
```

### Diagnostics

```ergoscript
val x: Int = "hello"  // Error: Unknown type 'String' in this context
val y = unknownVar    // Warning: 'unknownVar' might be undefined
```

### Syntax Highlighting

- **Keywords**: `val`, `def`, `if`, `else`
- **Built-ins**: `blake2b256`, `proveDlog`, `sigmaProp`
- **Context Variables**: `HEIGHT`, `SELF`, `INPUTS`, `OUTPUTS`
- **Types**: `Int`, `Long`, `Box`, `SigmaProp`
- **Comments**: `//` and `/* */`

---

## Supported Features

| Feature | Status | Description |
|---------|--------|-------------|
| Autocomplete | ✅ | Keywords, functions, variables, types |
| Diagnostics | ✅ | Error and warning detection |
| Hover | ✅ | Type info and documentation |
| Syntax Highlighting | ✅ | Full ErgoScript syntax |
| Go-to-Definition | 🚧 | Coming soon |
| Signature Help | 🚧 | Coming soon |
| Code Formatting | 🚧 | Coming soon |

---

## Example Contracts

### Simple Contract
```ergoscript
{
  val owner = SELF.R4[GroupElement].get
  val deadline = SELF.R5[Int].get
  
  val deadlinePassed = HEIGHT > deadline
  sigmaProp(deadlinePassed && proveDlog(owner))
}
```

### Auction Contract
```ergoscript
{
  val seller = SELF.R4[GroupElement].get
  val minBid = SELF.R5[Long].get
  val endHeight = SELF.R6[Int].get
  
  val isBid = HEIGHT < endHeight && OUTPUTS(0).value >= minBid
  val isClose = HEIGHT >= endHeight && proveDlog(seller)
  
  sigmaProp(isBid || isClose)
}
```

See `examples/` directory for more contracts.

---

## Built-in Support

### Keywords
`val`, `def`, `if`, `else`, `true`, `false`, `return`

### Context Variables
- `HEIGHT` - Current blockchain height
- `SELF` - Current box being evaluated
- `INPUTS` - Input boxes in transaction
- `OUTPUTS` - Output boxes in transaction
- `CONTEXT` - Full context object

### Built-in Functions
- `blake2b256(data)` - Blake2b hash
- `proveDlog(pk)` - Signature verification
- `sigmaProp(condition)` - Boolean to SigmaProp
- `atLeast(k, props)` - Threshold signatures
- `serialize(value)` - Serialize to bytes
- And many more...

### Types
`Int`, `Long`, `BigInt`, `Boolean`, `Byte`, `Box`, `SigmaProp`, `GroupElement`, `Coll[T]`, `Option[T]`

---

## Architecture

```
ergoscript-lsp/
├── server/              # Language server (TypeScript)
│   ├── src/
│   │   ├── server.ts    # Main LSP server
│   │   └── builtins.ts  # ErgoScript definitions
│   └── package.json
├── client/              # VS Code extension
│   ├── src/
│   │   └── extension.ts # Extension activation
│   ├── syntaxes/
│   │   └── ergoscript.tmLanguage.json  # Syntax grammar
│   └── package.json
└── examples/            # Example contracts
```

**Technology:**
- **Server**: TypeScript + Node.js + `vscode-languageserver`
- **Client**: TypeScript + `vscode-languageclient`
- **Protocol**: LSP (Language Server Protocol)

---

## Development

### Prerequisites
- Node.js 18+
- npm 9+
- VS Code 1.75+

### Build

```bash
# Server
cd server
npm install
npm run compile

# Client
cd ../client
npm install
npm run compile
```

### Debug

1. Open project in VS Code
2. Press F5 to launch Extension Development Host
3. Open an `.es` file
4. Test autocomplete, hover, diagnostics

### Test

```bash
cd server
npm test
```

---

## Contributing

Contributions welcome! Areas for improvement:

- [ ] Parser improvements
- [ ] Type inference enhancements
- [ ] Go-to-definition
- [ ] Signature help
- [ ] Code formatting
- [ ] More example contracts
- [ ] Better error messages

---

## Resources

- [ErgoScript Documentation](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [ErgoTree Specification](https://ergoplatform.org/docs/ErgoTree.pdf)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
- [VS Code Extension API](https://code.visualstudio.com/api)

---

## License

MIT License - see LICENSE file

---

## Team

**Team Dev Engers** - LNMIIT Open Source Hackathon 2025
- Pushkar Modi (@Pushkar111)
- Parth Raninga
- Pranjal Yadav

---

## Acknowledgments

- Ergo Platform team for ErgoScript
- Microsoft for LSP specification
- VS Code team for excellent tooling

---

**Bring modern IDE features to ErgoScript development!** 🚀
