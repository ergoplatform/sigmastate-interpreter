# ErgoScript LSP - Quick Start Guide

## Installation & Setup

### Step 1: Build the Server and Client

```bash
# Navigate to the server directory
cd server
npm install
npm run compile

# Navigate to the client directory
cd ../client
npm install
npm run compile
```

### Step 2: Launch in VS Code

Option A - **Development Mode** (with debugger):
1. Open the project root in VS Code
2. Press `F5` to launch "Extension Development Host"
3. A new VS Code window opens with the extension enabled

Option B - **Package and Install**:
```bash
cd client
npm run package
# Install the generated .vsix file in VS Code
```

## Testing the LSP

### Test 1: File Recognition
1. Create a new file: `test.es`
2. Verify that VS Code recognizes it as "ErgoScript" (bottom right status bar)

### Test 2: Syntax Highlighting
1. Type the following in `test.es`:
```ergoscript
val x = HEIGHT
if (x > 100) {
  sigmaProp(true)
}
```
2. Verify keywords are colored (if, val, sigmaProp, true)

### Test 3: Keyword Completion
1. Type: `val x = `
2. Press Ctrl+Space
3. Verify keyword suggestions appear
4. Select "HEIGHT" and press Enter

### Test 4: Hover Documentation
1. Hover your mouse over the word `HEIGHT`
2. A tooltip should appear with: "HEIGHT - Current blockchain height"

### Test 5: Error Diagnostics - Unbalanced Braces
1. Type:
```ergoscript
val x = {
  HEIGHT
// Missing closing brace
```
2. Verify red error squiggle under the closing position
3. Error message: "Unbalanced braces: expected '}'"

### Test 6: Warning - Empty File
1. Create a new file: `empty.es`
2. Keep it empty
3. Verify orange warning squiggle at line 1
4. Warning message: "File is empty"

## File Structure

```
sigmastate-interpreter/
├── server/              # LSP Server
│   ├── src/
│   │   ├── server.ts    # Main LSP implementation
│   │   └── keywords.ts  # Keyword definitions
│   ├── dist/            # Compiled JavaScript (generated)
│   └── package.json
├── client/              # VS Code Extension
│   ├── src/
│   │   └── extension.ts # Extension activation code
│   ├── syntaxes/        # TextMate grammar
│   ├── dist/            # Compiled JavaScript (generated)
│   └── package.json
├── example.es           # Example ErgoScript file
└── LSP_README.md        # Full documentation
```

## Troubleshooting

### Extension doesn't activate
- Ensure file has `.es` extension
- Check output panel (View > Output > "ErgoScript Language Server")
- Check Extension Host debugger console (F5 mode)

### No completions showing
- Make sure you're in a `.es` file
- Try pressing Ctrl+Space
- Check that client can connect to server

### Syntax highlighting not working
- Restart VS Code
- Clear cache: Delete `/client/dist` and recompile
- Verify `syntaxes/ergoscript.tmLanguage.json` exists

### Server crashes
- Check browser console (F12) in Extension Development Host
- Look for error messages in output panel
- Verify both `server/dist/server.js` and `client/dist/extension.js` exist

## Key Features Summary

| Feature | Shortcut | How to Test |
|---------|----------|-------------|
| Completion | Ctrl+Space | Type in .es file and trigger |
| Hover | Hover mouse | Move cursor over keywords |
| Diagnostics | Auto | Save file with errors |
| Syntax Highlighting | N/A | Should work automatically |

## Next Steps

1. Read [LSP_README.md](./LSP_README.md) for full documentation
2. Modify [server/src/keywords.ts](./server/src/keywords.ts) to add more keywords
3. Enhance diagnostics in [server/src/server.ts](./server/src/server.ts)
4. Add more language features as needed

## Support

For issues or questions:
1. Check the output panel in VS Code
2. Review error messages in the debugger console
3. Examine `server/src/server.ts` for diagnostic rules
