# ✅ Getting Started Checklist

Use this checklist to quickly set up and test the ErgoScript LSP implementation.

## Prerequisites (2 minutes)

- [ ] Node.js 14+ installed (`node --version`)
- [ ] npm installed (`npm --version`)
- [ ] VS Code 1.60+ installed

## Installation (5 minutes)

### Step 1: Install Server Dependencies
```bash
cd server
npm install
```
**Expected**: "added X packages" message, no errors

### Step 2: Install Client Dependencies
```bash
cd ../client
npm install
cd ..
```
**Expected**: "added X packages" message, no errors

### Step 3: Compile Both Components
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
**Expected**: No errors, files created:
- `server/dist/server.js`
- `client/dist/extension.js`

## Testing (10 minutes)

### Test 1: Launch Extension Development Host

1. Open VS Code in the project root:
   ```bash
   code .
   ```

2. Press **F5** (or Run > Start Debugging)

3. Wait for new "Extension Development Host" window to open

4. In the new window, verify:
   - [ ] No error messages in the console

**Expected**: A new VS Code window opens with the extension loaded

---

### Test 2: Create a Test File

1. In Extension Development Host window (the second VS Code):

2. Create a new file: **File > New File**

3. Save as: **test.es**

4. Verify in status bar: Shows "ergoscript" as language

**Expected**: 
- File is recognized as ErgoScript
- Syntax highlighting is applied (if any keywords are present)

---

### Test 3: Syntax Highlighting

1. Type the following code in test.es:
```ergoscript
val height = HEIGHT
if (true) {
  sigmaProp(proveDlog(x))
}
```

2. Verify:
   - [ ] `val` is colored differently from normal text
   - [ ] `HEIGHT` is colored  
   - [ ] `if`, `true` are colored
   - [ ] `sigmaProp`, `proveDlog` are colored

**Expected**: Keywords appear with distinct colors

---

### Test 4: Auto-Complete

1. In test.es, create a new line:
```
val x = 
```

2. Position cursor after `=` and space

3. Press **Ctrl+Space**

4. Verify:
   - [ ] Completion menu appears
   - [ ] Shows keyword suggestions (HEIGHT, INPUTS, OUTPUTS, etc.)
   - [ ] Can scroll through options

5. Select `HEIGHT` and press **Enter**

**Expected**: 
- `HEIGHT` is inserted at cursor
- Completion menu closes

---

### Test 5: Hover Documentation

1. In the code you typed, find the word `HEIGHT`

2. Hover mouse over it

3. Verify:
   - [ ] A tooltip appears
   - [ ] Shows text like: "HEIGHT - Current blockchain height"

4. Repeat for other keywords:
   - [ ] Hover over `INPUTS` → shows "Array of inputs in the transaction"
   - [ ] Hover over `sigmaProp` → shows "Creates a sigma proposition from a boolean"

**Expected**: Tooltip appears with documentation for each keyword

---

### Test 6: Diagnostic - Unbalanced Braces

1. Create a test file with unbalanced braces:
```ergoscript
val x = {
  val y = 5
// Missing closing brace
```

2. Verify:
   - [ ] Red squiggle appears under an error location
   - [ ] Error says "Unbalanced braces: expected '}'"

3. Try with brackets:
```ergoscript
val arr = [
  1, 2, 3
// Missing closing bracket
```

4. Verify:
   - [ ] Red squiggle appears
   - [ ] Error says "Unbalanced braces: expected ']'"

**Expected**: Errors are detected and reported with helpful messages

---

### Test 7: Diagnostic - Empty File Warning

1. Create new file: **empty.es**

2. Leave it empty

3. Verify:
   - [ ] Orange/yellow squiggle appears at line 1
   - [ ] Warning says "File is empty"

**Expected**: Warning appears for empty files

---

### Test 8: Multiple Keywords

1. Create a file with multiple keyword uses:
```ergoscript
def checkHeight(h: Long): Boolean = {
  h > HEIGHT
}

val inputCount = INPUTS.size

if (inputCount > 0) {
  sigmaProp(proveDlog(key))
} else {
  sigmaProp(false)
}

val output: Option[Box] = Some(OUTPUTS(0))
```

2. Test on each line:
   - [ ] Completions work (Ctrl+Space on each line)
   - [ ] Hover works on all keywords
   - [ ] Syntax coloring is consistent

**Expected**: All features work across multiple lines and different keywords

---

## Verification Checklist

After completing all tests, verify:

- [ ] Extension activates when opening .es files
- [ ] Syntax highlighting shows keyword colors
- [ ] Ctrl+Space shows completion menu with keywords
- [ ] Hovering shows documentation tooltips
- [ ] Unbalanced braces are detected as errors
- [ ] Empty files generate warnings
- [ ] No console errors in Extension Development Host
- [ ] No errors in Output panel

## Troubleshooting

If any test fails, check:

1. **Extension not activating**
   - Verify file has `.es` extension
   - Check Output panel (View > Output > "ErgoScript Language Server")
   - Restart: Ctrl+Shift+P → Developer: Reload Window

2. **Syntax highlighting not working**
   - Reload window: Ctrl+Shift+P → Developer: Reload Window
   - Check that keywords are actually in the file
   - Verify `syntaxes/ergoscript.tmLanguage.json` exists

3. **Completions not showing**
   - Verify you're in a `.es` file
   - Check Output panel for errors
   - Try Ctrl+Space explicitly

4. **Hover not working**
   - Verify keyword is spelled correctly
   - Check Output panel for errors
   - Hover over exact keyword (not partial word)

5. **Diagnostics not appearing**
   - Make sure file is saved
   - Check Output panel for validation errors
   - Reload window if changes not reflecting

## What's Next?

### ✨ After successful testing:

1. **Understand the implementation**
   - Read [IMPLEMENTATION_SUMMARY.md](./IMPLEMENTATION_SUMMARY.md) (10 min)

2. **Learn the architecture**
   - Read [ARCHITECTURE.md](./ARCHITECTURE.md) (20 min)

3. **Add more features**
   - Follow [SETUP_GUIDE.md](./SETUP_GUIDE.md) § Advanced section
   - Add new keywords to `server/src/keywords.ts`
   - Add new diagnostics to `server/src/server.ts`

4. **Deploy to production**
   - Follow [SETUP_GUIDE.md](./SETUP_GUIDE.md) § Production Deployment
   - Package and publish extension

## Sample Files

- **example.es** - Complete example of ErgoScript code
- **test.es** - Your test file (create during testing)

## Quick Command Reference

| Action | Command |
|--------|---------|
| Install dependencies | `npm install` (in server/, then client/) |
| Compile TypeScript | `npm run compile` (in server/, then client/) |
| Clean build artifacts | `npm run clean` (in server/ or client/) |
| Launch debugger | **F5** in VS Code |
| Reload extension | **Ctrl+Shift+P** → Developer: Reload Window |
| Open output panel | **Ctrl+Shift+P** → Output |
| View problems | **Ctrl+Shift+M** |

## Checklist Summary

- [ ] Prerequisites verified (Node, npm, VS Code)
- [ ] Server dependencies installed
- [ ] Client dependencies installed
- [ ] Code compiled (no errors)
- [ ] Extension launches (F5)
- [ ] Test file created and recognized
- [ ] Syntax highlighting works
- [ ] Auto-complete works
- [ ] Hover documentation works
- [ ] Unbalanced braces detected
- [ ] Empty file warning appears
- [ ] Multi-line support verified
- [ ] No console errors
- [ ] Ready for next steps!

---

**Estimated Total Time**: 15-20 minutes from zero to fully tested

If you complete all checks above, your ErgoScript LSP is working correctly! ✅

**Next**: Read [INDEX.md](./INDEX.md) to understand what to do next, or jump to [IMPLEMENTATION_SUMMARY.md](./IMPLEMENTATION_SUMMARY.md) to learn more about what was built.
