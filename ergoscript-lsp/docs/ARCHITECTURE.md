# ErgoScript LSP Architecture

Technical architecture documentation for the ErgoScript Language Server Protocol implementation.

---

## Overview

The ErgoScript LSP is built using the Language Server Protocol specification, providing IDE features for ErgoScript smart contract development.

**Key Components:**
- Language Server (TypeScript/Node.js)
- VS Code Extension (TypeScript)
- Parser (Lexer + AST)
- Type System
- LSP Features (Completion, Diagnostics, Hover)

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                        VS Code                              │
│  ┌───────────────────────────────────────────────────────┐  │
│  │              ErgoScript Extension                     │  │
│  │  - Language Client                                    │  │
│  │  - Syntax Grammar (TextMate)                          │  │
│  │  - Code Snippets                                      │  │
│  └───────────────────────────────────────────────────────┘  │
└──────────────────────────┬──────────────────────────────────┘
                           │ JSON-RPC over IPC
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   Language Server (Node.js)                 │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                    Server Core                        │  │
│  │  - LSP Protocol Handler                               │  │
│  │  - Document Manager                                   │  │
│  │  - Configuration                                      │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                      Parser                           │  │
│  │  - Lexer (Tokenizer)                                  │  │
│  │  - AST Builder                                        │  │
│  │  - Syntax Validator                                   │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                    Analyzer                           │  │
│  │  - Type Checker                                       │  │
│  │  - Symbol Table                                       │  │
│  │  - Semantic Validator                                 │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                  LSP Features                         │  │
│  │  - Completion Provider                                │  │
│  │  - Diagnostics Provider                               │  │
│  │  - Hover Provider                                     │  │
│  │  - Definition Provider                                │  │
│  │  - Signature Help Provider                            │  │
│  └───────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │                   Built-ins                           │  │
│  │  - ErgoScript Types                                   │  │
│  │  - Built-in Functions                                 │  │
│  │  - Context Variables                                  │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

---

## Component Details

### 1. VS Code Extension (Client)

**Location:** `client/`

**Responsibilities:**
- Activate extension when `.es` files are opened
- Start and manage language server process
- Provide syntax highlighting via TextMate grammar
- Offer code snippets for common patterns
- Handle user configuration

**Key Files:**
- `extension.ts` - Extension activation and lifecycle
- `syntaxes/ergoscript.tmLanguage.json` - Syntax grammar
- `snippets/ergoscript.json` - Code snippets
- `package.json` - Extension manifest

**Communication:**
- Uses `vscode-languageclient` library
- Communicates with server via JSON-RPC over IPC
- Sends document changes, requests completions, etc.

---

### 2. Language Server (Server)

**Location:** `server/`

**Responsibilities:**
- Parse ErgoScript code into AST
- Perform type checking and semantic analysis
- Provide LSP features (completion, diagnostics, hover)
- Maintain document state
- Respond to client requests

**Key Files:**
- `server.ts` - Main server entry point
- `parser/` - Lexer and parser
- `analyzer/` - Type checker and validator
- `features/` - LSP feature implementations
- `builtins.ts` - ErgoScript definitions

---

### 3. Parser

**Location:** `server/src/parser/`

**Components:**

#### Lexer (`lexer.ts`)
- Tokenizes ErgoScript source code
- Recognizes keywords, operators, literals, identifiers
- Handles comments (line and block)
- Tracks line/column positions

**Token Types:**
- Keywords: `val`, `def`, `if`, `else`, `true`, `false`
- Operators: `+`, `-`, `*`, `/`, `==`, `!=`, `&&`, `||`, etc.
- Delimiters: `(`, `)`, `{`, `}`, `[`, `]`
- Literals: numbers, strings, booleans

#### AST Builder (`ast.ts`)
- Defines AST node types
- Represents program structure
- Includes position information for error reporting

**Node Types:**
- `Program` - Root node
- `ValDeclaration` - Value declarations
- `DefDeclaration` - Function declarations
- `BinaryExpression` - Binary operations
- `CallExpression` - Function calls
- `MemberExpression` - Property access
- `IfExpression` - Conditional expressions
- `LambdaExpression` - Anonymous functions

---

### 4. Type System

**Location:** `server/src/analyzer/`

**ErgoScript Types:**
- **Primitives:** `Int`, `Long`, `BigInt`, `Boolean`, `Byte`, `Short`
- **Special:** `Box`, `SigmaProp`, `GroupElement`, `AvlTree`
- **Generic:** `Coll[T]`, `Option[T]`, `(T1, T2)`

**Type Inference:**
- Infers types from literals
- Propagates types through expressions
- Handles generic type parameters
- Validates type compatibility

**Type Checking:**
- Validates function signatures
- Checks method calls
- Ensures type safety
- Reports type errors

---

### 5. LSP Features

**Location:** `server/src/features/`

#### Completion Provider
- Suggests keywords, functions, variables, types
- Context-aware completions (e.g., box properties after `.`)
- Includes documentation in completion items
- Filters based on user input

#### Diagnostics Provider
- Detects undefined variables
- Finds type mismatches
- Identifies syntax errors
- Reports warnings and errors in real-time

#### Hover Provider
- Shows type information
- Displays function signatures
- Provides documentation
- Formats as Markdown

#### Definition Provider
- Finds variable declarations
- Locates function definitions
- Supports go-to-definition

#### Signature Help Provider
- Shows parameter information
- Displays function signatures
- Highlights current parameter

---

### 6. Built-in Definitions

**Location:** `server/src/builtins.ts`

**Contents:**
- **Keywords:** `val`, `def`, `if`, `else`, etc.
- **Functions:** `blake2b256`, `proveDlog`, `sigmaProp`, etc.
- **Context Variables:** `HEIGHT`, `SELF`, `INPUTS`, `OUTPUTS`
- **Types:** All ErgoScript types with descriptions
- **Box Properties:** `value`, `propositionBytes`, `tokens`, etc.
- **Collection Methods:** `map`, `filter`, `fold`, etc.

**Format:**
```typescript
{
  name: string;
  signature: string;
  documentation: string;
}
```

---

## Data Flow

### 1. Document Open

```
User opens .es file
  ↓
VS Code activates extension
  ↓
Extension starts language server
  ↓
Server initializes
  ↓
Extension sends textDocument/didOpen
  ↓
Server parses document
  ↓
Server sends diagnostics
```

### 2. Autocomplete Request

```
User types and triggers completion
  ↓
Extension sends textDocument/completion
  ↓
Server analyzes context
  ↓
Server generates completion items
  ↓
Server sends completion list
  ↓
Extension shows suggestions
```

### 3. Hover Request

```
User hovers over symbol
  ↓
Extension sends textDocument/hover
  ↓
Server finds symbol definition
  ↓
Server formats hover content
  ↓
Server sends hover response
  ↓
Extension displays tooltip
```

---

## Performance Considerations

### Incremental Parsing
- Only re-parse changed portions
- Cache AST for unchanged documents
- Background processing for large files

### Caching
- Cache type information
- Store symbol tables
- Reuse completion items

### Debouncing
- Debounce diagnostics updates
- Throttle completion requests
- Batch document changes

---

## Error Handling

### Graceful Degradation
- Continue on parse errors
- Provide partial completions
- Show best-effort diagnostics

### Error Recovery
- Recover from syntax errors
- Continue parsing after errors
- Report multiple errors

### Logging
- Log server activity
- Debug protocol messages
- Track performance metrics

---

## Extension Points

### Adding New Features

1. **New LSP Feature:**
   - Implement handler in `server.ts`
   - Add feature module in `features/`
   - Register capability in initialization

2. **New Built-in:**
   - Add to `builtins.ts`
   - Include signature and documentation
   - Autocomplete will pick it up automatically

3. **New Syntax:**
   - Update lexer token types
   - Add AST node type
   - Update parser logic
   - Add to TextMate grammar

---

## Testing Strategy

### Unit Tests
- Test lexer tokenization
- Test parser AST generation
- Test type inference
- Test completion generation

### Integration Tests
- Test LSP protocol messages
- Test document synchronization
- Test feature interactions

### Manual Testing
- Test in VS Code
- Verify autocomplete
- Check diagnostics
- Test hover information

---

## Dependencies

### Server
- `vscode-languageserver` - LSP protocol implementation
- `vscode-languageserver-textdocument` - Document management
- TypeScript - Language and type system
- Node.js - Runtime environment

### Client
- `vscode-languageclient` - LSP client library
- VS Code Extension API - Editor integration
- TypeScript - Language and type system

---

## Build Process

### Development
```bash
# Server
cd server
npm install
npm run watch  # Continuous compilation

# Client
cd client
npm install
npm run watch  # Continuous compilation
```

### Production
```bash
# Compile both
npm run compile

# Package extension
cd client
npm run package  # Creates .vsix file
```

---

## Future Enhancements

### Phase 1: Enhanced Parser
- Full ErgoScript syntax support
- Better error recovery
- Incremental parsing

### Phase 2: Advanced Type System
- Complete type inference
- Generic type resolution
- Type narrowing

### Phase 3: More Features
- Code formatting
- Refactoring (rename, extract)
- Code actions (quick fixes)
- Semantic highlighting

### Phase 4: Integration
- Vim/Neovim support
- IntelliJ IDEA support
- Web-based editor
- CI/CD integration

---

## Resources

- [LSP Specification](https://microsoft.github.io/language-server-protocol/)
- [VS Code Extension API](https://code.visualstudio.com/api)
- [ErgoScript Documentation](https://docs.ergoplatform.com/dev/scs/ergoscript/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)

---

**Architecture designed for extensibility, performance, and maintainability.**
