# ErgoScript LSP Demo Guide

This guide walks you through testing and demonstrating the ErgoScript Language Server Protocol implementation.

## Quick Start

### 1. Initial Setup

```bash
# Navigate to project directory
cd ergoscript-lsp

# Install dependencies
npm install

# Compile server and client
npm run compile
```

Expected output:
```
> ergoscript-lsp@0.1.0 compile
> npm run compile:server && npm run compile:client
```

### 2. Launch Extension Development Host

1. Open the `ergoscript-lsp` folder in VS Code
2. Press `F5` (or Run > Start Debugging)
3. A new VS Code window titled "[Extension Development Host]" will open
4. This window has the ErgoScript extension loaded

### 3. Open Sample File

In the Extension Development Host window:

1. File > Open File
2. Navigate to `ergoscript-lsp/examples/sample.es`
3. The file should open with syntax highlighting

## Testing Features

### Feature 1: Syntax Highlighting

**What to test:**
- Keywords like `val`, `if`, `sigmaProp` should be highlighted
- Context variables like `HEIGHT`, `OUTPUTS` should be colored
- Comments should appear in green/gray
- Strings and numbers should have distinct colors

**Expected result:**  
ErgoScript code appears with appropriate syntax coloring based on the TextMate grammar.

---

### Feature 2: Hover Documentation

**What to test:**

1. Hover your mouse over the keyword `SigmaProp` in the sample file
2. Wait for the hover popup to appear
3. Try hovering over other keywords:
   - `HEIGHT`
   - `OUTPUTS`
   - `INPUTS`
   - `SELF`
   - `tokens`
   - `sigmaProp`
   - `atLeast`

**Expected result:**  
A popup appears with markdown-formatted documentation explaining:
- What the keyword does
- Its role in ErgoScript
- Example usage

**Example hover for HEIGHT:**
```
HEIGHT - Blockchain Height

A context variable representing the current block height
of the Ergo blockchain.

Used for time-locked contracts and temporal conditions.

Example: HEIGHT > 500000 (spendable after block 500000)
```

---

### Feature 3: Syntax Diagnostics (Error Detection)

**What to test:**

1. In `sample.es`, scroll to the bottom where there are commented error examples
2. Uncomment the first error example (unmatched opening brace):

```ergoscript
// Example 1: Unmatched opening brace
{
  val x = HEIGHT > 100
```

3. Save the file

**Expected result:**  
- A red squiggly underline appears under the opening `{`
- The Problems panel (View > Problems) shows an error: "Unclosed '{'"
- Hovering over the red squiggle shows the error message

**Test other errors:**

Uncomment and test each error type:

- **Unmatched closing parenthesis:**
  ```ergoscript
  val result = (OUTPUTS.size + INPUTS.size))
  ```
  Expected: Error on extra `)`

- **Unmatched bracket:**
  ```ergoscript
  val tokens = SELF.tokens[0
  ```
  Expected: Error about unclosed `[`

- **Mismatched braces:**
  ```ergoscript
  {
    val condition = HEIGHT > 500000
  ]
  ```
  Expected: Error about expecting `}` but found `]`

---

### Feature 4: Live Validation

**What to test:**

1. Open `sample.es`
2. Add a new line with an error:
   ```ergoscript
   val broken = (1 + 2
   ```
3. Don't save - just type

**Expected result:**  
Diagnostics appear in real-time as you type (after a brief pause). Red squiggle appears without needing to save.

---

### Feature 5: Language Configuration

**What to test:**

1. Type an opening brace: `{`
2. Type an opening parenthesis: `(`
3. Type a quote: `"`

**Expected result:**  
The closing character is automatically inserted:
- `{` → `{}`
- `(` → `()`
- `"` → `""`

---

## AI Agent Support Demo

### Viewing Structured Token Output

The language server outputs structured token data for AI agent consumption.

**How to view:**

1. Open the Extension Development Host
2. Open `sample.es`
3. In the **main** VS Code window (not Extension Development Host):
   - View > Output
   - Select "ErgoScript Language Server" from dropdown
4. Make a change to `sample.es` in the Extension Development Host

**Expected output in console:**
```json
{
  "uri": "file:///path/to/sample.es",
  "version": 2,
  "tokens": [
    { "type": "comment", "value": "/**", "position": { "line": 0, "column": 0 } },
    { "type": "keyword", "value": "val", "position": { "line": 10, "column": 2 } },
    { "type": "identifier", "value": "owner1", "position": { "line": 10, "column": 6 } }
    // ... more tokens
  ]
}
```

This structured data can be consumed by AI agents for analysis.

---

## Troubleshooting

### Extension Not Activating

**Problem:** ErgoScript files don't show any LSP features

**Solution:**
1. Check the file extension is `.es` or `.ergo`
2. Reload the Extension Development Host: `Ctrl+R` (Cmd+R on Mac)
3. Check the Output panel for errors (View > Output > ErgoScript Language Server)

### Server Not Starting

**Problem:** "Language server failed to start" error

**Solution:**
1. Ensure server was compiled: `npm run compile:server`
2. Check that `server/dist/index.js` exists
3. Look for errors in the Extension Host output panel

### Hover Not Working

**Problem:** No popup appears when hovering

**Solution:**
1. Ensure you're hovering directly over the keyword (not whitespace)
2. Wait 1-2 seconds for the hover to appear
3. Check that the word is a recognized keyword (see README)

### Diagnostics Not Appearing

**Problem:** Errors are visible but no red squiggles

**Solution:**
1. Save the file (Ctrl+S / Cmd+S)
2. Check View > Problems for diagnostic messages
3. Verify the syntax error is one the LSP detects (braces, parentheses)

---

## Demo Script (for Presentations)

Use this script when demonstrating the LSP:

### 1. Introduction (30 seconds)
"This is an MVP Language Server Protocol implementation for ErgoScript, the smart contract language for the Ergo blockchain. It provides IDE support for developers and structured analysis for AI agents."

### 2. Show Hover (1 minute)
"Let me show you the hover documentation. When I hover over core ErgoScript keywords like HEIGHT..." [hover] "...I get detailed documentation explaining what it does in the context of Ergo's blockchain."

[Hover over OUTPUTS, SigmaProp, tokens]

### 3. Show Diagnostics (1 minute)
"The LSP also provides real-time syntax validation. Watch what happens when I introduce an error..." [Type unclosed brace] "...the LSP immediately detects the unmatched brace and shows an error."

[Show Problems panel, hover over error]

### 4. Show AI Agent Output (1 minute)
"For AI agent support, the server outputs structured token data. Here in the output console, you can see the tokenized representation..." [Show Output panel] "...which AI agents can consume for code analysis."

### 5. Emphasize Scope (30 seconds)
"This is an MVP focused on IDE tooling, not a full compiler. It provides syntax checking, documentation, and structure - the foundation for more advanced features like type inference and code completion."

---

## Next Steps

After testing the demo:

1. **Examine the code:**
   - Review `server/src/parser.ts` for tokenization logic
   - Check `server/src/diagnostics.ts` for validation rules
   - See `server/src/hover.ts` for keyword documentation

2. **Extend the functionality:**
   - Add more keywords to the hover provider
   - Implement additional diagnostic checks
   - Add code completion (onCompletion handler)

3. **Test with real ErgoScript:**
   - Create your own `.es` files
   - Test complex ErgoScript contracts
   - Report issues or suggest improvements

---

## Additional Resources

- [LSP Specification](https://microsoft.github.io/language-server-protocol/)
- [VS Code Extension Development](https://code.visualstudio.com/api)
- [ErgoScript Documentation](https://docs.ergoplatform.com/)
- [Sigma Protocols](https://docs.ergoplatform.com/dev/scs/sigma/)

---

**Questions or Issues?**  
This is a hackathon MVP. For production ErgoScript tooling, refer to the official Ergo Platform resources.
