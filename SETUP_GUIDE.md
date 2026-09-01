# ErgoScript LSP - Setup & Deployment Guide

This guide walks you through setting up the ErgoScript Language Server Protocol (LSP) implementation.

## Overview

The implementation consists of two main components:

1. **Server** (`/server`) - LSP server that provides language features
2. **Client** (`/client`) - VS Code extension that connects to the server

## Installation

### Step 1: Install Dependencies

```bash
# Install server dependencies
cd server
npm install

# Install client dependencies
cd ../client
npm install
cd ..
```

### Step 2: Compile TypeScript

```bash
# Compile server
cd server
npm run compile
cd ..

# Compile client
cd client
npm run compile
cd ..
```

**Expected Result**: No compilation errors, `server/dist/server.js` and `client/dist/extension.js` exist.

## Development Mode (Debug in VS Code)

### Launching the Extension Host

1. Open the project folder in VS Code:
   ```bash
   code .
   ```

2. Press **F5** to start debugging (or use Run > Start Debugging)

3. A new "Extension Development Host" window will open with your extension loaded

4. In the Extension Development Host window:
   - Create a new file with `.es` extension (e.g., `test.es`)
   - The LSP will automatically activate

### Development Features

- **Hot Reload**: Modify server code, recompile (`npm run compile` in server folder), and reload window (Ctrl+R)
- **Debugger**: Set breakpoints in `server/src/server.ts` and inspect variables
- **Output Panel**: View diagnostics and errors in Output > "ErgoScript Language Server"

## Testing Checklist

Use the [example.es](./example.es) file or create your own test file:

### Test 1: Language Registration ✓
- [ ] Create a `.es` file
- [ ] Verify bottom-right shows "ergoscript" as language mode
- [ ] Verify VS Code recognizes it as ErgoScript file

### Test 2: Syntax Highlighting ✓
- [ ] Open `example.es`
- [ ] Verify keywords are colored:
  - `val`, `def`, `if`, `else` - control flow color
  - `sigmaProp`, `proveDlog` - proposition color
  - `INPUTS`, `OUTPUTS`, `HEIGHT` - constant color
  - `true`, `false` - literal color

### Test 3: Auto-Complete ✓
- [ ] Type: `val x = `
- [ ] Press **Ctrl+Space**
- [ ] Verify keyword menu appears with options like `HEIGHT`, `INPUTS`, etc.
- [ ] Select `HEIGHT` and press **Enter**
- [ ] Verify it inserts `HEIGHT`

### Test 4: Hover Documentation ✓
- [ ] Hover mouse over `HEIGHT`
- [ ] Verify tooltip shows: "HEIGHT - Current blockchain height"
- [ ] Test other keywords: `sigmaProp`, `proveDlog`, `OUTPUTS`, etc.

### Test 5: Diagnostic - Unbalanced Braces ✓
- [ ] Create a file with unbalanced braces:
  ```ergoscript
  val x = {
    val y = 5
  // missing closing brace
  ```
- [ ] Verify red error squiggle appears
- [ ] Hover over error to see message

### Test 6: Diagnostic - Empty File ✓
- [ ] Create empty file: `empty.es`
- [ ] Verify orange warning at line 1: "File is empty"

### Test 7: Multi-line Support ✓
- [ ] Create file with multiple lines:
  ```ergoscript
  val height = HEIGHT
  val size = INPUTS.size
  
  if (height > 1000) {
    sigmaProp(true)
  }
  ```
- [ ] Verify completion works on any line
- [ ] Verify hover works on keywords anywhere

## Production Deployment

### Creating a Publishable Extension

1. Install VSCE (VS Code Extension Manager):
   ```bash
   npm install -g @vscode/vsce
   ```

2. Package the extension:
   ```bash
   cd client
   npm run package
   ```

   This creates `ergoscript-language-support-1.0.0.vsix`

3. Install the extension locally:
   - In VS Code: Cmd/Ctrl+Shift+P → "Extensions: Install from VSIX"
   - Select the generated `.vsix` file

### Publishing to VS Code Marketplace

1. Create an Azure DevOps organization account
2. Create a Personal Access Token (PAT)
3. Login with VSCE:
   ```bash
   vsce login <publisher-name>
   ```
4. Publish:
   ```bash
   cd client
   vsce publish
   ```

## Troubleshooting

### Extension Not Activating

**Problem**: `.es` files don't trigger the LSP

**Solution**:
1. Check language registration:
   - Verify `.es` extension is in `client/package.json` > `contributes.languages`
   - Verify `activationEvents` includes `"onLanguage:ergoscript"`

2. Check Output panel:
   - View > Output > "ErgoScript Language Server"
   - Look for error messages

3. Reload extension:
   - Press **Ctrl+Shift+P** → "Developer: Reload Window"

### No Completions Showing

**Problem**: Ctrl+Space doesn't show keywords

**Solution**:
1. Verify server is running:
   - Check Extension Development Host's Output panel
   - Look for "ErgoScript LSP started" message

2. Verify connection:
   - In server logs, check for client connection messages
   - Monitor network activity if using remote connection

3. Recompile and reload:
   ```bash
   cd server && npm run compile
   cd ../client && npm run compile
   ```
   Then press **Ctrl+Shift+P** → "Developer: Reload Window"

### Syntax Highlighting Not Working

**Problem**: Keywords aren't colored

**Solution**:
1. Verify grammar file exists:
   - Check `client/syntaxes/ergoscript.tmLanguage.json`

2. Reload syntax:
   - Reload window: **Ctrl+Shift+P** → "Developer: Reload Window"
   - Or restart VS Code

3. Check language config:
   - Verify `client/language-configuration.json` is valid JSON

### Server Crashes

**Problem**: Server process terminates unexpectedly

**Solution**:
1. Check error logs:
   - Output > "ErgoScript Language Server"
   - Extension Development Host's debug console

2. Verify dependencies:
   ```bash
   cd server
   npm install --save vscode-languageserver vscode-languageserver-textdocument
   npm run compile
   ```

3. Check for runtime errors:
   - Add console.log statements in `server/src/server.ts`
   - Recompile and reload

### Performance Issues

**Problem**: Slow completions or hover

**Solution**:
- Current implementation uses simple regex parsing
- For large files, consider optimizing:
  - Add debouncing for diagnostics
  - Cache keyword index
  - Implement incremental text updates

## File Structure Reference

```
sigmastate-interpreter/
├── server/                          # LSP Server Implementation
│   ├── src/
│   │   ├── server.ts               # Main server implementation
│   │   │   - textDocumentSync
│   │   │   - completion handler
│   │   │   - hover handler
│   │   │   - validation/diagnostics
│   │   └── keywords.ts             # Keyword definitions & docs
│   ├── dist/                       # Compiled JavaScript (generated)
│   ├── package.json                # Server dependencies
│   └── tsconfig.json               # TypeScript config
│
├── client/                          # VS Code Extension
│   ├── src/
│   │   └── extension.ts            # Extension activation & setup
│   ├── dist/                       # Compiled JavaScript (generated)
│   ├── syntaxes/
│   │   └── ergoscript.tmLanguage.json  # TextMate grammar
│   ├── language-configuration.json # Language config (brackets, comments)
│   ├── package.json                # Extension metadata & manifest
│   ├── .vscodeignore               # Files to exclude from package
│   └── tsconfig.json               # TypeScript config
│
├── example.es                       # Example ErgoScript file
├── QUICK_START.md                   # Quick start guide
└── LSP_README.md                    # Full documentation
```

## Advanced: Customizing the Implementation

### Adding New Keywords

Edit `server/src/keywords.ts`:

```typescript
export const ERGOSCRIPT_KEYWORDS: KeywordInfo[] = [
  // ... existing keywords
  {
    name: "myKeyword",
    documentation: "myKeyword - My custom keyword description",
  },
];
```

Recompile and reload.

### Adding New Diagnostics

Edit `server/src/server.ts` > `validateDocument()` function:

```typescript
function validateDocument(doc: TextDocument): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  const text = doc.getText();
  
  // Add new validation rules here
  if (someCondition) {
    diagnostics.push({
      severity: DiagnosticSeverity.Error,
      range: { start: { line: 0, character: 0 }, end: { line: 0, character: 1 } },
      message: "Error message",
      source: "ergoscript",
    });
  }
  
  return diagnostics;
}
```

### Adding New Language Features

For signature help, definition lookup, etc., add handlers:

```typescript
connection.onSignatureHelp((params) => {
  // Implement signature help
  return null;
});

connection.onDefinition((params) => {
  // Implement go to definition
  return null;
});
```

## Next Steps

1. **Enhance Syntax**: Expand the TextMate grammar in `client/syntaxes/ergoscript.tmLanguage.json`
2. **Add Type Information**: Implement semantic analysis in the server
3. **Cross-file Analysis**: Support project-wide definitions and references
4. **Blockchain Integration**: Add smart contract validation features
5. **Format Provider**: Implement code formatting
6. **Code Actions**: Implement quick fixes and refactorings

## Support & Resources

- [VS Code Extension API](https://code.visualstudio.com/api)
- [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
- [vscode-languageserver Documentation](https://github.com/Microsoft/vscode-languageserver-node)
- [TextMate Grammar Reference](https://macromates.com/manual/en/language_grammars)

## License

MIT - See LICENSE file for details
