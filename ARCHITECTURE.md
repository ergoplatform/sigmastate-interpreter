# ErgoScript LSP - Architecture & Design

## System Architecture

### High-Level Overview

```
┌──────────────────────────────────────────────────────────────┐
│                         VS Code                              │
├──────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────┐ │
│  │            ErgoScript Extension (Client)                │ │
│  │  ────────────────────────────────────────────────────   │ │
│  │  • Detects .es files                                    │ │
│  │  • Registers language handlers                          │ │
│  │  • Manages syntax highlighting                          │ │
│  │  • Forwards editor commands to server                   │ │
│  └──────────────────────┬──────────────────────────────────┘ │
│                         │                                     │
│                 IPC / Socket Connection                       │
│                         │                                     │
│  ┌──────────────────────▼──────────────────────────────────┐ │
│  │         ErgoScript Language Server (Backend)            │ │
│  │  ────────────────────────────────────────────────────   │ │
│  │  • Processes .es files                                  │ │
│  │  • Validates syntax (diagnostics)                       │ │
│  │  • Provides completions                                 │ │
│  │  • Responds to hover requests                           │ │
│  └─────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```

## Detailed Component Breakdown

### Client-Side Components (client/)

```
┌─ client/
│
├─ src/
│  └─ extension.ts (50 lines)
│     ├─ activate(context)
│     │  ├─ Constructs server module path
│     │  ├─ Configures server options (IPC transport)
│     │  ├─ Creates LanguageClient instance
│     │  └─ Starts language client
│     │
│     └─ deactivate()
│        └─ Stops language client
│
├─ syntaxes/
│  └─ ergoscript.tmLanguage.json
│     ├─ Comment patterns (// and /* */)
│     ├─ String patterns (" and ')
│     ├─ Number patterns
│     ├─ Keyword patterns
│     ├─ Constant patterns
│     └─ Bracket patterns
│
├─ language-configuration.json
│  ├─ Line comments: //
│  ├─ Block comments: /* */
│  ├─ Auto-closing pairs: {} [] ()
│  ├─ Folding markers
│
├─ package.json
│  ├─ activationEvents: ["onLanguage:ergoscript"]
│  ├─ contributes:
│  │  ├─ languages: [{ id: "ergoscript", extensions: [".es"] }]
│  │  └─ grammars: [TextMate grammar reference]
│  └─ Dependencies: vscode-languageclient
│
└─ tsconfig.json
   └─ Compile options (ES2020, CommonJS)
```

### Server-Side Components (server/)

```
┌─ server/
│
├─ src/
│  │
│  ├─ server.ts (270+ lines)
│  │  ├─ Initialization
│  │  │  └─ createConnection(ProposedFeatures.all)
│  │  │
│  │  ├─ Text Document Management
│  │  │  ├─ documents: TextDocuments(TextDocument)
│  │  │  └─ documents.listen(connection)
│  │  │
│  │  ├─ Validation Engine
│  │  │  ├─ validateDocument(doc)
│  │  │  │  ├─ Check if empty
│  │  │  │  └─ Check brace balance
│  │  │  │     ├─ Count: { } [ ] ( )
│  │  │  │     ├─ Detect: unbalanced closing
│  │  │  │     └─ Return: error location
│  │  │  │
│  │  │  └─ checkBraceBalance(text)
│  │  │     └─ Returns: { isUnbalanced, line, char, expected }
│  │  │
│  │  ├─ Completion Handler
│  │  │  ├─ connection.onCompletion()
│  │  │  └─ Returns: CompletionItem[] from KEYWORDS
│  │  │
│  │  ├─ Hover Handler
│  │  │  ├─ connection.onHover()
│  │  │  ├─ Extract word at position
│  │  │  ├─ Lookup documentation
│  │  │  └─ Return: Hover { contents }
│  │  │
│  │  └─ Server Lifecycle
│  │     ├─ connection.onInitialize()
│  │     │  └─ Return: ServerCapabilities
│  │     ├─ documents.onDidChangeContent()
│  │     │  └─ Validate and send diagnostics
│  │     └─ connection.listen()
│  │        └─ Start listening for requests
│  │
│  └─ keywords.ts (100+ lines)
│     ├─ ERGOSCRIPT_KEYWORDS[]
│     │  ├─ Control flow: val, def, if, else, ...
│     │  ├─ Cryptography: sigmaProp, proveDlog, ...
│     │  ├─ Context: INPUTS, OUTPUTS, HEIGHT, ...
│     │  ├─ Logic: AND, OR, THRESHOLD, ...
│     │  └─ Types: Option, Some, None, ...
│     │
│     ├─ getKeywordDocumentation(keyword)
│     │  └─ Returns: string | undefined
│     │
│     └─ KEYWORD_NAMES[]
│        └─ Extracted array of keyword names
│
├─ dist/ (Generated)
│  ├─ server.js
│  ├─ server.d.ts
│  ├─ keywords.js
│  └─ keywords.d.ts
│
├─ package.json
│  ├─ Dependencies: vscode-languageserver, -textdocument
│  └─ Scripts: compile, watch, clean
│
└─ tsconfig.json
   └─ Compile options (ES2020, CommonJS)
```

## Data Flow Diagrams

### 1. Extension Activation Flow

```
User opens .es file
     │
     ├─→ VS Code recognizes "ergoscript" language
     │
     ├─→ Triggers activation event: "onLanguage:ergoscript"
     │
     ├─→ Calls extension.ts: activate(context)
     │
     ├─→ Creates LanguageClient
     │
     ├─→ Sets serverOptions (IPC transport)
     │
     ├─→ Spawns node process: server/dist/server.js
     │
     ├─→ Establishes IPC connection
     │
     ├─→ connection.onInitialize() called
     │
     └─→ Server ready for LSP requests
```

### 2. Completion Request Flow

```
User types Ctrl+Space in .es file
     │
     ├─→ Client: textDocument.onCompletion(position)
     │
     ├─→ Sends request to server
     │
     ├─→ Server: connection.onCompletion()
     │
     ├─→ Returns all ERGOSCRIPT_KEYWORDS as CompletionItem[]
     │
     ├─→ Client receives completions
     │
     └─→ VS Code displays completion menu
```

### 3. Hover Request Flow

```
User hovers over "HEIGHT" keyword
     │
     ├─→ Client: textDocument.onHover(position)
     │
     ├─→ Sends hover request to server
     │
     ├─→ Server: connection.onHover()
     │
     ├─→ Extracts word at position: "HEIGHT"
     │
     ├─→ Calls: getKeywordDocumentation("HEIGHT")
     │
     ├─→ Returns: Hover { contents: "HEIGHT - Current blockchain height" }
     │
     ├─→ Client receives hover info
     │
     └─→ VS Code displays tooltip
```

### 4. Diagnostics Flow

```
User edits file in editor
     │
     ├─→ Client: documents.onDidChangeContent()
     │
     ├─→ Sends didChange notification to server
     │
     ├─→ Server: documents.onDidChangeContent()
     │
     ├─→ Calls: validateDocument(doc)
     │    │
     │    ├─→ Check if empty
     │    │    └─ Return warning diagnostic
     │    │
     │    └─→ Call: checkBraceBalance(text)
     │         └─ Return error diagnostic if unbalanced
     │
     ├─→ Collects all Diagnostic[]
     │
     ├─→ Sends: connection.sendDiagnostics()
     │
     ├─→ Client receives diagnostics
     │
     └─→ VS Code displays errors/warnings in editor
```

## Communication Protocol

### LSP Message Examples

#### Completion Request
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/completion",
  "params": {
    "textDocument": { "uri": "file:///path/to/test.es" },
    "position": { "line": 0, "character": 7 }
  }
}
```

#### Completion Response
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": [
    {
      "label": "val",
      "kind": 14,
      "detail": "val - Defines an immutable value binding",
      "insertText": "val"
    },
    {
      "label": "def",
      "kind": 14,
      "detail": "def - Defines a function",
      "insertText": "def"
    }
  ]
}
```

#### Hover Request
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "textDocument/hover",
  "params": {
    "textDocument": { "uri": "file:///path/to/test.es" },
    "position": { "line": 0, "character": 14 }
  }
}
```

#### Hover Response
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "contents": {
      "kind": "plaintext",
      "value": "HEIGHT - Current blockchain height"
    }
  }
}
```

## Class Relationships

### TextDocument Lifecycle

```
LanguageClient
    │
    ├─→ TextDocuments (manages all open documents)
    │    │
    │    ├─→ TextDocument (individual file)
    │    │    │
    │    │    ├─ uri: string
    │    │    ├─ languageId: "ergoscript"
    │    │    ├─ version: number
    │    │    └─ getText(): string
    │    │
    │    └─ Events:
    │       ├─ onDidOpen
    │       ├─ onDidChangeContent
    │       └─ onDidClose
    │
    └─→ Server handlers register on these events
```

### Completion Flow

```
CompletionParams
    │
    ├─ textDocument: VersionedTextDocumentIdentifier
    │  └─ uri: string (identifies the .es file)
    │
    └─ position: Position
       ├─ line: number
       └─ character: number
    
    ↓ Process in onCompletion handler ↓

CompletionItem[]
    │
    ├─ label: string ("val", "HEIGHT", etc.)
    ├─ kind: CompletionItemKind (Keyword = 14)
    ├─ detail: string (documentation)
    └─ insertText: string (text to insert)
```

## State Management

### Server State

```
Server State:
├─ documents: Map<uri, TextDocument>
│  └─ Tracks all open files and their content
│
├─ keywords: KeywordInfo[]
│  └─ Static list (doesn't change during session)
│
└─ No additional state
   └─ Stateless design (all info in text documents)
```

### Diagnostic State

```
Diagnostics Flow:
├─ Input: TextDocument content (text)
├─ Processing: Stateless validation rules
│  ├─ Rule 1: Empty file check
│  └─ Rule 2: Brace balance check
└─ Output: Diagnostic[]
   └─ Sent to client (not stored on server)
```

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Validate document | O(n) | Linear scan for braces |
| Extract word at position | O(m) | m = line length |
| Get completions | O(k) | k = keyword count (~30) |
| Lookup documentation | O(k) | Linear search in keywords |
| Brace balance check | O(n) | Single pass through text |

### Space Complexity

| Component | Complexity | Notes |
|-----------|-----------|-------|
| Keywords store | O(k) | k = keyword count |
| Open documents | O(d) | d = number of open files |
| Brace balance | O(1) | Uses counters |

## Extension Points

### Easy to Add Features

1. **New Keywords**
   - Edit: `keywords.ts` > `ERGOSCRIPT_KEYWORDS`
   - Complexity: O(1)

2. **New Diagnostics**
   - Edit: `server.ts` > `validateDocument()`
   - Add validation rule function
   - Complexity: Depends on rule

3. **New Language Features**
   - Edit: `server.ts` > Add connection handler
   - Examples: signatureHelp, definition, references
   - Complexity: Depends on feature

4. **Better Syntax Highlighting**
   - Edit: `ergoscript.tmLanguage.json`
   - Add regex patterns for new constructs
   - Complexity: Regex expertise needed

## Security Considerations

### Input Validation
- All text document content is accepted as-is
- No code execution or evaluation
- Regex-based parsing is safe

### Resource Limits
- No per-document limits enforced
- Very large files may cause lag (optimization opportunity)
- Memory usage scales with open document count

### Message Size
- JSON messages parsed by VS Code extension framework
- Safe handling by Node.js IPC
- No hardcoded limits

## Testing Strategy

### Unit Testing (Not yet implemented)
```
- Test validateDocument() with various inputs
- Test checkBraceBalance() with edge cases
- Test getKeywordDocumentation() lookup
- Test word extraction at position
```

### Integration Testing (Manual)
```
✓ Language registration
✓ Syntax highlighting  
✓ Completion suggestions
✓ Hover tooltips
✓ Error diagnostics
✓ Multi-line support
```

### Stress Testing
```
- Large files (1MB+)
- Rapid edits (burst typing)
- Many open documents
- Memory profiling
```

---

This architecture document provides a complete understanding of the ErgoScript LSP implementation structure and can be used as a reference for modifications and enhancements.
