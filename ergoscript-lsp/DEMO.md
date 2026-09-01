# ErgoScript LSP - Live Demo Guide

## Quick Demo Instructions

To see the LSP working live, follow these steps:

### Option 1: VS Code Extension (Recommended)

1. **Open VS Code in the ergoscript-lsp directory:**
   ```bash
   cd c:\projects\LNMIIT-Open-Source-Hackathon-2025\sigmastate-interpreter\ergoscript-lsp
   code .
   ```

2. **Press F5** to launch Extension Development Host
   - This opens a new VS Code window with the extension loaded

3. **Open an example contract:**
   - Open `examples/simple_contract.es`
   - Open `examples/auction.es`
   - Open `examples/token_sale.es`

4. **Test the features:**

   **Autocomplete:**
   - Type `val` and press Ctrl+Space → See keyword suggestions
   - Type `HEI` and press Ctrl+Space → See HEIGHT suggestion
   - Type `SELF.` → See box properties (value, propositionBytes, tokens, R4, R5, etc.)
   - Type `blake` → See blake2b256 function

   **Hover Information:**
   - Hover over `proveDlog` → See signature and documentation
   - Hover over `HEIGHT` → See type and description
   - Hover over `blake2b256` → See function signature

   **Syntax Highlighting:**
   - Keywords are blue (val, def, if, else)
   - Built-in functions are yellow (blake2b256, proveDlog)
   - Context variables are purple (HEIGHT, SELF)
   - Comments are gray

   **Diagnostics:**
   - Type `val x: InvalidType = 10` → See error squiggle
   - Type `val y = unknownVar` → See warning squiggle

   **Code Snippets:**
   - Type `contract-simple` and press Tab → Insert simple contract template
   - Type `contract-auction` and press Tab → Insert auction contract
   - Type `val` and press Tab → Insert val declaration

### Option 2: Manual Testing (Without VS Code Extension)

1. **Compile the code:**
   ```bash
   cd server
   npm run compile
   cd ../client
   npm run compile
   ```

2. **Check compilation output:**
   - Server: `server/out/` directory should have compiled .js files
   - Client: `client/out/` directory should have compiled .js files

3. **Verify files exist:**
   - `server/out/server.js`
   - `server/out/parser/lexer.js`
   - `server/out/parser/parser.js`
   - `server/out/features/completion.js`
   - etc.

### Option 3: Package the Extension

1. **Install vsce (if not already installed):**
   ```bash
   npm install -g @vscode/vsce
   ```

2. **Package the extension:**
   ```bash
   cd client
   npm run package
   ```

3. **Install the .vsix file:**
   - Open VS Code
   - Go to Extensions (Ctrl+Shift+X)
   - Click "..." → "Install from VSIX..."
   - Select `ergoscript-lsp-client-0.1.0.vsix`

4. **Test with .es files:**
   - Create a new file with `.es` extension
   - Start typing ErgoScript code
   - See autocomplete, syntax highlighting, etc.

---

## Live Demo Video

A screen recording showing all features in action has been created and can be found in the documentation.

---

## Expected Behavior

### ✅ Autocomplete
- Typing triggers suggestions
- Keywords, functions, types, variables appear
- Documentation shown in completion items

### ✅ Syntax Highlighting
- Different colors for different syntax elements
- Keywords, functions, types, comments all highlighted

### ✅ Hover Information
- Hovering shows type information
- Function signatures displayed
- Documentation appears in tooltip

### ✅ Diagnostics
- Errors shown with red squiggles
- Warnings shown with yellow squiggles
- Error messages appear on hover

### ✅ Code Snippets
- Tab completion for templates
- Contract templates available
- Common patterns included

---

## Troubleshooting

**Extension not loading:**
- Make sure you compiled both server and client
- Check VS Code Output → ErgoScript Language Server
- Reload window (Ctrl+Shift+P → Reload Window)

**No autocomplete:**
- Press Ctrl+Space manually
- Check file extension is `.es`
- Wait a moment for server to start

**No syntax highlighting:**
- Check file is recognized as ErgoScript
- Look at bottom right of VS Code for language
- Click and select "ErgoScript"

---

**The LSP is fully functional and ready to use!**
