# ErgoScript Minimal LSP (Proof-of-Concept)

This folder contains a minimal, low-cost Language Server Protocol (LSP) implementation for ErgoScript. It is intended as a hackathon proof-of-concept to provide basic IDE features to support development and AI agents: diagnostics, hover, and document symbols.

## What it provides

- **Real parser diagnostics**: When the Scala parser is available, calls `SigmaParser` via a CLI wrapper to get accurate parse errors with line/column information.
- **Fallback regex diagnostics**: If the Scala parser is unavailable, uses simple pattern matching for unmatched braces/parentheses and `TODO` markers.
- **Document symbols**: Extracts `val`, `def`, `let`, `func`, `type` symbols via regex.
- **Hover**: Shows the token under cursor with a small help message.

This is intentionally minimal to be low-effort and useful for tooling integration, with a clear upgrade path to full parser integration.

## Quick start

1. **Build the parser** (optional but recommended for real diagnostics):
   ```bash
   # from repository root
   sbt "parsers/publishLocal"
   ```

2. **Install Node.js dependencies and run server**:
   ```bash
   cd lsp
   npm install
   npm start
   ```

The server automatically detects if the Scala parser JAR is available and uses it; otherwise it falls back to regex-based validation.

## How to use in VS Code (manual demo)

You can test the server with any LSP client that supports connecting to a stdio language server. For VS Code, the easiest path is to use the "LSP Client" extension or the "vscode-languageclient" example extension and point it to this server's `node server.js` process.

### Example (using `node` only, advanced)

The server speaks LSP over stdio. If you have an LSP client that can attach to a stdio server you can configure it to start `node /path/to/lsp/server.js` as the server command.

## Example file

Create `example.es` with some ErgoScript content and open it in the editor connected to this server. The server will emit diagnostics for unmatched braces and provide symbol extraction for lines beginning with `val`, `def`, `let`, `func`, or `type`.

## Architecture

```
┌─────────────────┐
│ LSP Client      │  (VS Code, Neovim, etc.)
│ (stdio)         │
└────────┬────────┘
         │ LSP protocol
         v
┌─────────────────┐
│ server.js       │  Node.js LSP server
│ (stdio)         │  • Hover, symbols, diagnostics
└────────┬────────┘
         │
         ├─────────────────┐
         │                 │
         v                 v
┌─────────────────┐  ┌─────────────────┐
│ ParserCLI.scala │  │ Regex fallback  │
│ (via scala CLI) │  │ (basic checks)  │
│ Uses SigmaParser│  └─────────────────┘
└─────────────────┘
         │
         v
┌─────────────────┐
│ SigmaParser     │  Actual ErgoScript parser
│ (Scala/fastparse)
└─────────────────┘
```

When the Scala parser JAR is built (`sbt parsers/publishLocal`), the LSP server invokes `ParserCLI.scala` via the `scala` command to get real parse errors. Otherwise, it falls back to simple regex validation.

## Limitations and next steps

- **Parser invocation overhead**: Currently spawns a new `scala` process per validation. For production, consider:
  - Running a persistent Scala server (e.g., via HTTP or Unix socket) to avoid startup costs.
  - Compiling `ParserCLI.scala` to a native binary with Scala Native or GraalVM.
- **Limited symbol resolution**: Document symbols use regex, not the actual AST. Future work should query the parser's symbol table.
- Next steps to improve quality and LSP features:
  - Add completions and signature help by reusing interpreter AST.
  - Provide workspace/x references and find-definition using interpreter symbols.
  - Optionally publish as an npm package and provide a VS Code extension to simplify usage.

## Files

- `server.js` - LSP server (stdio) with auto-detection of Scala parser
- `ParserCLI.scala` - Scala wrapper for SigmaParser that outputs JSON diagnostics
- `package.json` - Node package manifest
- `README.md` - this file
- `example.es` - sample ErgoScript file for testing

## License

Same as the repository (see LICENSE)
