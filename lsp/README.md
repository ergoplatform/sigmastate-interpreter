# ErgoScript Language Server (LSP)

A Language Server Protocol implementation for ErgoScript, the smart contract language of the Ergo blockchain.

## Features

- ✅ **Syntax Error Diagnostics** - Real-time error detection as you type
- ✅ **Type Error Diagnostics** - Catch type mismatches before deployment
- ✅ **Autocomplete** - Smart completions for globals, methods, and keywords
- ✅ **Hover Documentation** - Detailed docs when you hover over symbols
- ✅ **Go to Definition** - Jump to variable/function definitions
- ✅ **Signature Help** - Parameter hints when typing function calls

## Quick Start

### Build the LSP Server

```bash
# From the project root
sbt "lsp/assembly"

# The JAR will be at:
# lsp/target/scala-2.13/ergoscript-lsp.jar
```

### Command Line Usage

```bash
# Check a file for errors
java -jar ergoscript-lsp.jar --check examples/simple.es

# Show completions for a prefix
java -jar ergoscript-lsp.jar --completions "SELF."

# Show hover documentation
java -jar ergoscript-lsp.jar --hover "sigmaProp"

# Start LSP server (for editor integration)
java -jar ergoscript-lsp.jar --stdio
```

### Run Tests

```bash
sbt "lsp/test"
```

## Editor Integration

### VS Code

1. Build the JAR (see above)
2. Install the ErgoScript VS Code extension (coming soon)
3. Configure the extension to point to the JAR

Manual configuration in `settings.json`:
```json
{
  "ergoscript.lsp.path": "/path/to/ergoscript-lsp.jar"
}
```

### Neovim (with nvim-lspconfig)

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

configs.ergoscript = {
  default_config = {
    cmd = { 'java', '-jar', '/path/to/ergoscript-lsp.jar', '--stdio' },
    filetypes = { 'ergoscript' },
    root_dir = lspconfig.util.find_git_ancestor,
    settings = {},
  },
}

lspconfig.ergoscript.setup{}
```

### Other Editors

Any editor supporting LSP can use this server. Configure it to run:
```bash
java -jar /path/to/ergoscript-lsp.jar --stdio
```

## File Extensions

The LSP server recognizes these file extensions:
- `.es` - ErgoScript
- `.ergo` - ErgoScript
- `.ergoscript` - ErgoScript

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Editor (VS Code, Neovim, etc.)          │
├─────────────────────────────────────────────────────────────┤
│                    JSON-RPC over stdio                      │
├─────────────────────────────────────────────────────────────┤
│                ErgoScriptLanguageServer                     │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ DiagnosticsProvider │ Uses SigmaCompiler for parsing │  │
│  │ CompletionProvider  │ Box methods, globals, keywords │  │
│  │ HoverProvider       │ Markdown documentation         │  │
│  │ DefinitionProvider  │ Find val/def declarations      │  │
│  │ SignatureProvider   │ Function parameter hints       │  │
│  └──────────────────────────────────────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                    sigmastate-interpreter                   │
│            (SigmaParser, SigmaTyper, SigmaBinder)          │
└─────────────────────────────────────────────────────────────┘
```

## Example ErgoScript Files

See the `examples/` directory for sample contracts:

- `simple.es` - Basic time-locked contract
- `multisig.es` - 2-of-3 multisignature
- `timelock.es` - Time-locked vault
- `token-transfer.es` - Token handling
- `htlc.es` - Hash Time-Locked Contract
- `self-replicating.es` - State machine pattern
- `errors.es` - Intentional errors for testing diagnostics

## Supported Completions

### Global Variables
- `SELF`, `INPUTS`, `OUTPUTS`, `HEIGHT`, `CONTEXT`

### Global Functions
- `sigmaProp`, `atLeast`, `blake2b256`, `sha256`
- `proveDlog`, `proveDHTuple`, `decodePoint`
- `allOf`, `anyOf`, `xor`, `min`, `max`
- `PK`, `fromBase16`, `fromBase64`, `getVar`

### Box Methods
- `value`, `tokens`, `propositionBytes`, `id`, `bytes`
- `R0` through `R9` (registers)
- `creationInfo`, `bytesWithoutRef`

### Collection Methods
- `size`, `map`, `filter`, `fold`
- `exists`, `forall`, `slice`, `append`
- `indexOf`, `zip`, `flatMap`, `getOrElse`

### Option Methods
- `get`, `getOrElse`, `isDefined`, `isEmpty`
- `map`, `filter`

### Keywords & Types
- `val`, `def`, `if`, `else`, `true`, `false`
- `Boolean`, `Byte`, `Int`, `Long`, `BigInt`
- `Box`, `Coll`, `Option`, `SigmaProp`, `GroupElement`

## Contributing

This is part of the sigmastate-interpreter project. See the main repository's [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

## License

Same license as the parent sigmastate-interpreter project.
