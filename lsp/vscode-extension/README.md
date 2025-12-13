# ErgoScript VS Code Extension

A Visual Studio Code extension providing full language support for **ErgoScript** - the smart contract language for the Ergo blockchain.

## 🎬 Features Demo

This extension provides:

- ✅ **Syntax Highlighting** - Beautiful colorization for ErgoScript code
- ✅ **Real-time Error Detection** - See syntax and type errors as you type
- ✅ **Auto-completion** - IntelliSense for globals, methods, and functions
- ✅ **Hover Documentation** - Detailed docs when hovering over symbols
- ✅ **Go to Definition** - Jump to val/def declarations
- ✅ **Signature Help** - Parameter hints for functions
- ✅ **Code Snippets** - Quick templates for common patterns

---

## 🚀 Quick Setup (For Video Demo)

### Prerequisites

1. **Java 11+** installed
   ```bash
   # Check Java version
   java -version
   
   # macOS with Homebrew
   brew install openjdk
   ```

2. **Node.js 18+** installed
   ```bash
   node -v
   npm -v
   ```

3. **VS Code** (latest version)

### Step 1: Build the LSP Server

First, build the ErgoScript Language Server JAR:

```bash
# Navigate to the repo root
cd sigmastate-interpreter

# Build the LSP assembly JAR
sbt "lsp/assembly"
```

This creates: `lsp/target/scala-2.13/ergoscript-lsp.jar`

### Step 2: Install Extension Dependencies

```bash
# Navigate to the extension directory
cd lsp/vscode-extension

# Install npm dependencies
npm install
```

### Step 3: Compile the Extension

```bash
# Compile TypeScript
npm run compile
```

### Step 4: Run the Extension

1. Open VS Code in the extension directory:
   ```bash
   code .
   ```

2. Press **F5** to launch the Extension Development Host

3. In the new VS Code window, open an `.es` file:
   ```bash
   # Try the examples
   code ../examples/simple.es
   ```

---

## 📹 Recording Your Demo Video

### Demo Script

Here's a suggested flow for your video:

#### 1. Show Syntax Highlighting (30 sec)
- Open `lsp/examples/simple.es`
- Point out the colored keywords, types, and strings

#### 2. Show Error Detection (1 min)
- Open `lsp/examples/errors.es`
- Show the red squiggly lines under errors
- Hover over an error to see the diagnostic message
- Fix an error and show it disappears

#### 3. Show Auto-completion (1 min)
- Create a new file `demo.es`
- Type `SELF.` and show the completion list (value, tokens, propositionBytes, R0-R9)
- Type `sig` and select `sigmaProp` from completions
- Type `bl` and select `blake2b256`

#### 4. Show Hover Documentation (30 sec)
- Hover over `SELF` to see its documentation
- Hover over `HEIGHT` to see its documentation
- Hover over `sigmaProp` to see the function signature

#### 5. Show Code Snippets (30 sec)
- Type `htlc` and press Tab to expand the HTLC template
- Type `multisig` and show the multisig template
- Type `timelock` and show the timelock template

#### 6. Show Go to Definition (30 sec)
- Open `lsp/examples/htlc.es`
- Right-click on a variable and select "Go to Definition"
- Or Cmd/Ctrl+Click on a variable

---

## 🛠️ Troubleshooting

### "LSP server JAR not found"

Make sure you built the assembly:
```bash
cd sigmastate-interpreter
sbt "lsp/assembly"
```

Verify the JAR exists:
```bash
ls -la lsp/target/scala-2.13/ergoscript-lsp.jar
```

### "Java not found"

Set the Java path in VS Code settings:
```json
{
  "ergoscript.java.home": "/path/to/java/home"
}
```

Or on macOS:
```json
{
  "ergoscript.java.home": "/opt/homebrew/opt/openjdk"
}
```

### Extension doesn't activate

1. Check the Output panel (View → Output)
2. Select "ErgoScript Language Server" from the dropdown
3. Look for error messages

### No completions appearing

1. Make sure the file has `.es` extension
2. Check that the LSP server started successfully
3. Try running "ErgoScript: Restart Language Server" command

---

## 📁 Project Structure

```
lsp/vscode-extension/
├── package.json              # Extension manifest
├── tsconfig.json             # TypeScript config
├── language-configuration.json
├── src/
│   └── extension.ts          # Extension entry point
├── syntaxes/
│   └── ergoscript.tmLanguage.json   # Syntax highlighting
├── snippets/
│   └── ergoscript.json       # Code snippets
└── out/                      # Compiled JavaScript (after npm run compile)
```

---

## 🔧 Configuration Options

| Setting | Default | Description |
|---------|---------|-------------|
| `ergoscript.lsp.serverPath` | `""` | Path to LSP server JAR |
| `ergoscript.java.home` | `""` | Path to Java home |
| `ergoscript.trace.server` | `"off"` | Trace LSP communication |

---

## 📝 Example ErgoScript Files

Check out the examples in `lsp/examples/`:

- `simple.es` - Basic contract
- `multisig.es` - Multi-signature (2-of-3)
- `timelock.es` - Time-locked funds
- `htlc.es` - Hash Time-Locked Contract
- `token-transfer.es` - Token transfer
- `self-replicating.es` - Self-replicating contract
- `errors.es` - Example with intentional errors

---

## 🎯 Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| Trigger Suggestions | `Ctrl+Space` |
| Go to Definition | `F12` or `Cmd/Ctrl+Click` |
| Show Hover | `Cmd/Ctrl+K Cmd/Ctrl+I` |
| Trigger Parameter Hints | `Cmd/Ctrl+Shift+Space` |

---

## 📦 Packaging for Distribution

To create a `.vsix` file for distribution:

```bash
# Install vsce if not already installed
npm install -g @vscode/vsce

# Package the extension
npm run package
```

This creates `ergoscript-lsp-0.1.0.vsix` which can be installed in any VS Code.

---

## 🙏 Credits

- **ErgoScript** - Created by the Ergo Platform team
- **sigmastate-interpreter** - The underlying Scala implementation
- **LSP4J** - Eclipse LSP library for Java/Scala

---

## 📄 License

This extension is part of the sigmastate-interpreter project and is licensed under CC0 1.0 Universal.
