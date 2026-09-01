# ErgoScript Language Server (LSP)

A minimal Language Server Protocol implementation for ErgoScript (.es files) providing IDE support within VS Code.

## Features

- **Syntax Highlighting**: Automatic highlighting for ErgoScript keywords and constructs
- **Keyword Completion**: Auto-complete suggestions for ErgoScript keywords:
  - Control flow: `if`, `else`, `for`, `match`, `case`, `def`, `val`
  - Cryptographic operations: `sigmaProp`, `proveDlog`, `proveDhTuple`
  - Transaction context: `INPUTS`, `OUTPUTS`, `SELF`, `HEIGHT`
  - Logic operators: `AND`, `OR`, `THRESHOLD`
  - Type utilities: `Option`, `Some`, `None`
  - Literals: `true`, `false`

- **Hover Documentation**: Hover over keywords to see their documentation

- **Basic Diagnostics**:
  - Reports errors for unbalanced braces `{}`, brackets `[]`, and parentheses `()`
  - Reports warnings for empty files

## Architecture

```
├── server/
│   ├── src/
│   │   ├── server.ts       - Main LSP server implementation
│   │   └── keywords.ts     - ErgoScript keywords and documentation
│   ├── package.json
│   └── tsconfig.json
├── client/
│   ├── src/
│   │   └── extension.ts    - VS Code extension activation
│   ├── package.json
│   ├── language-configuration.json
│   ├── syntaxes/
│   │   └── ergoscript.tmLanguage.json
│   └── tsconfig.json
```

## Building and Testing

### Prerequisites

- Node.js 14+
- npm or yarn
- VS Code 1.60+

### Setup

1. Install dependencies for both server and client:

```bash
cd server
npm install

cd ../client
npm install
```

2. Compile TypeScript:

```bash
# In server/
npm run compile

# In client/
npm run compile
```

### Running in Development

1. Open the project in VS Code:
```bash
code .
```

2. Press `F5` to launch the extension in the Extension Development Host

3. In the new VS Code window, create a `.es` file with ErgoScript code:

```ergoscript
val height = HEIGHT
val inputsLen = INPUTS.size

if (height > 1000) {
  sigmaProp(proveDlog(someProposition))
} else {
  sigmaProp(true)
}
```

### Expected Behavior

- When you open a `.es` file:
  - The LSP server activates automatically
  - Keywords are highlighted with syntax colors
  - Typing triggers keyword auto-complete (Ctrl+Space)
  - Hovering over keywords shows documentation
  - Unbalanced braces are reported as errors
  - Empty files generate a warning

### Building the Extension Package

```bash
cd client
npm run package
```

This generates a `.vsix` file that can be installed in VS Code.

## LSP Capabilities

The server implements:

1. **textDocumentSync**: Full text document synchronization
2. **completionProvider**: Keyword completion
3. **hoverProvider**: Hover documentation for keywords

## Limitations

- Regex-based parsing (suitable for syntax highlighting and basic validation)
- No blockchain logic validation
- No type checking
- No cross-file analysis

## Future Enhancements

- Full ErgoScript syntax support
- Function signature help
- Go to definition
- Rename refactoring
- Code formatting
- Semantic highlighting
- Type checking based on ErgoScript type system

## License

MIT

## Contributing

Contributions are welcome! Please submit issues and pull requests.
