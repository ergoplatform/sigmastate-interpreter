# ErgoScript LSP Usage Guide

This guide shows how to use the ErgoScript Language Server Protocol (LSP) for smart contract development.

---

## Installation

### VS Code Extension

1. **Download** the `.vsix` file
2. **Open VS Code**
3. **Go to Extensions** (Ctrl+Shift+X)
4. **Click "..." menu** → "Install from VSIX..."
5. **Select** the downloaded file
6. **Reload** VS Code

### Building from Source

```bash
# Clone repository
cd sigmastate-interpreter/ergoscript-lsp

# Install server dependencies
cd server
npm install
npm run compile

# Install client dependencies
cd ../client
npm install
npm run compile

# Package extension
npm run package
```

---

## Features

### 1. Autocomplete

The LSP provides intelligent code completion as you type.

#### Keywords
Type `val` and press Ctrl+Space to see:
- `val` - Value declaration
- `def` - Function declaration
- `if` - If expression
- `else` - Else clause

#### Built-in Functions
Type `blake` and see:
- `blake2b256` - Blake2b hash function
- Complete signature and documentation

#### Context Variables
Type `HEI` and see:
- `HEIGHT` - Current blockchain height
- `SELF` - Current box
- `INPUTS` - Input boxes
- `OUTPUTS` - Output boxes

#### Box Properties
Type `SELF.` and see:
- `value` - Box value in NanoErg
- `propositionBytes` - Guarding script
- `tokens` - Token collection
- `R4`, `R5`, ... - Registers

#### Type Methods
Type a number and `.` to see:
- `toByte`, `toShort`, `toInt`, `toLong`
- `toBigInt`, `toBytes`, `toBits`

---

### 2. Diagnostics

Real-time error checking as you type.

#### Undefined Variables
```ergoscript
val y = unknownVar
        ^^^^^^^^^^
// Warning: 'unknownVar' might be undefined
```

#### Type Errors
```ergoscript
val x: InvalidType = 10
       ^^^^^^^^^^^
// Error: Unknown type 'InvalidType'
```

---

### 3. Hover Information

Hover over any symbol to see documentation.

#### Functions
```ergoscript
val hash = blake2b256(data)
           ^^^^^^^^^^
// Hover shows:
// blake2b256(data: Coll[Byte]) => Coll[Byte]
// Computes Blake2b256 hash of the input bytes. Returns 32-byte hash.
```

#### Variables
```ergoscript
val height = HEIGHT
             ^^^^^^
// Hover shows:
// HEIGHT: Int
// Current blockchain height (block number). Shortcut for CONTEXT.HEIGHT.
```

---

### 4. Syntax Highlighting

Full syntax highlighting for ErgoScript.

- **Keywords**: `val`, `def`, `if`, `else` (blue)
- **Built-ins**: `blake2b256`, `proveDlog` (yellow)
- **Context Variables**: `HEIGHT`, `SELF` (purple)
- **Types**: `Int`, `Box`, `SigmaProp` (green)
- **Comments**: `//` and `/* */` (gray)
- **Strings**: `"text"` (orange)
- **Numbers**: `123` (light blue)

---

### 5. Code Snippets

Type a prefix and press Tab to insert a snippet.

#### Contract Templates
- `contract-simple` → Simple time-locked contract
- `contract-auction` → Auction contract

#### Common Patterns
- `val` → Value declaration
- `valt` → Value with type annotation
- `def` → Function declaration
- `if` → If-else expression

#### ErgoScript Specific
- `proveDlog` → Signature verification
- `blake` → Blake2b hash
- `register` → Box register access
- `output` → Output validation
- `map`, `filter`, `fold` → Collection operations
- `atleast` → Threshold signature
- `token` → Token validation

---

## Example Workflow

### 1. Create a New Contract

1. **Create file**: `my_contract.es`
2. **Type snippet**: `contract-simple` + Tab
3. **Customize**: Edit the template

### 2. Write Contract Logic

```ergoscript
{
  // 1. Type 'val' + Tab for declaration
  val owner = SELF.R4[GroupElement].get
  
  // 2. Type 'HEI' + Ctrl+Space for HEIGHT
  val deadline = HEIGHT + 100
  
  // 3. Type 'prove' + Ctrl+Space for proveDlog
  val sig = proveDlog(owner)
  
  // 4. Hover over 'proveDlog' to see docs
  sigmaProp(sig)
}
```

### 3. Check for Errors

- **Red squiggles** = Errors
- **Yellow squiggles** = Warnings
- **Hover** over squiggles to see message

### 4. Use Autocomplete

- **Ctrl+Space** = Trigger autocomplete
- **Type** to filter suggestions
- **Enter** to accept

---

## Tips and Tricks

### Faster Development

1. **Use snippets** for common patterns
2. **Let autocomplete** suggest properties
3. **Hover** to learn about functions
4. **Check diagnostics** before testing

### Learning ErgoScript

1. **Hover over built-ins** to see signatures
2. **Use example contracts** as reference
3. **Read error messages** carefully
4. **Experiment** with autocomplete

### Debugging

1. **Check diagnostics** first
2. **Hover** to see types
3. **Use simple examples** to test
4. **Compare** with working contracts

---

## Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| Trigger autocomplete | Ctrl+Space |
| Show hover | Ctrl+K Ctrl+I |
| Format document | Shift+Alt+F |
| Go to definition | F12 |
| Find references | Shift+F12 |

---

## Troubleshooting

### Extension Not Working

1. **Check** extension is installed
2. **Reload** VS Code
3. **Check** file extension is `.es`
4. **View** Output → ErgoScript Language Server

### No Autocomplete

1. **Press** Ctrl+Space manually
2. **Check** cursor position
3. **Wait** a moment for server to start
4. **Check** server is running in Output

### Diagnostics Not Showing

1. **Save** the file
2. **Wait** for server to analyze
3. **Check** syntax is valid
4. **Reload** window if needed

---

## Advanced Usage

### Custom Settings

Add to VS Code `settings.json`:

```json
{
  "ergoscript.trace.server": "verbose",
  "files.associations": {
    "*.ergoscript": "ergoscript"
  }
}
```

### Multiple File Extensions

The LSP supports:
- `.es` (recommended)
- `.ergoscript`

---

## Support

For issues or questions:
- **GitHub**: Open an issue
- **Documentation**: See README.md
- **Examples**: Check examples/ directory

---

**Happy ErgoScript coding!** 🚀
