# ErgoScript Minimal LSP (Proof-of-Concept)

This folder contains a minimal, low-cost Language Server Protocol (LSP) implementation for ErgoScript. It is intended as a hackathon proof-of-concept to provide basic IDE features to support development and AI agents: diagnostics, hover, and document symbols.

## What it provides

- Basic diagnostics: unmatched braces/parentheses and a simple `TODO` info marker.
- Document symbols: extracts `val`, `def`, `let`, `func`, `type` symbols via regex.
- Hover: shows the token under cursor with a small help message.

This is intentionally minimal to be low-effort and useful for tooling integration.

## Quick start

1. Install Node.js (>=14) and npm
2. Install dependencies and run server

```bash
cd lsp
npm install
npm start
```

## How to use in VS Code (manual demo)

You can test the server with any LSP client that supports connecting to a stdio language server. For VS Code, the easiest path is to use the "LSP Client" extension or the "vscode-languageclient" example extension and point it to this server's `node server.js` process.

### Example (using `node` only, advanced)

The server speaks LSP over stdio. If you have an LSP client that can attach to a stdio server you can configure it to start `node /path/to/lsp/server.js` as the server command.

## Example file

Create `example.es` with some ErgoScript content and open it in the editor connected to this server. The server will emit diagnostics for unmatched braces and provide symbol extraction for lines beginning with `val`, `def`, `let`, `func`, or `type`.

## Limitations and next steps

- This implementation uses simple regex parsing and basic heuristics; it is not a replacement for a full parser.
- Next steps to improve quality and LSP features:
  - Wire the server to the actual parser in this repo (`parsers/`), calling it to produce real diagnostics and symbol data.
  - Add completions and signature help by reusing interpreter AST.
  - Provide workspace/x references and find-definition using interpreter symbols.
  - Optionally publish as an npm package and provide a VS Code extension to simplify usage.

## Files

- `server.js` - minimal LSP server (stdio)
- `package.json` - Node package manifest
- `README.md` - this file

## License

Same as the repository (see LICENSE)
