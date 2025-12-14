const {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  DiagnosticSeverity,
  SymbolKind,
  DocumentSymbol
} = require('vscode-languageserver');

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments();

connection.onInitialize(() => {
  return {
    capabilities: {
      textDocumentSync: documents.syncKind,
      hoverProvider: true,
      documentSymbolProvider: true,
      diagnosticProvider: true
    }
  };
});

// Simple diagnostics: unmatched braces and basic forbidden tokens
function validateText(text) {
  const diagnostics = [];
  const stack = [];
  const open = { '{': '}', '(': ')', '[': ']' };
  const close = { '}': '{', ')': '(', ']': '[' };

  for (let i = 0; i < text.length; i++) {
    const ch = text[i];
    if (open[ch]) stack.push({ ch, pos: i });
    if (close[ch]) {
      const last = stack.pop();
      if (!last || last.ch !== close[ch]) {
        const diag = {
          severity: DiagnosticSeverity.Error,
          range: {
            start: { line: 0, character: 0 },
            end: { line: 0, character: 1 }
          },
          message: `Unmatched closing '${ch}' at pos ${i}`,
          source: 'ergoscript-lsp'
        };
        diagnostics.push(diag);
      }
    }
  }
  if (stack.length > 0) {
    const diag = {
      severity: DiagnosticSeverity.Error,
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 1 }
      },
      message: `Unmatched opening '${stack[stack.length - 1].ch}'`,
      source: 'ergoscript-lsp'
    };
    diagnostics.push(diag);
  }

  // Example: warn on TODO tokens
  const todoRegex = /TODO/gi;
  let m;
  while ((m = todoRegex.exec(text)) !== null) {
    const pos = m.index;
    diagnostics.push({
      severity: DiagnosticSeverity.Information,
      range: {
        start: { line: 0, character: pos },
        end: { line: 0, character: pos + m[0].length }
      },
      message: "TODO found",
      source: 'ergoscript-lsp'
    });
  }

  return diagnostics;
}

// Provide simple document symbols (val/def/func/style)
function extractSymbols(text) {
  const lines = text.split(/\r?\n/);
  const symbols = [];
  const pattern = /^(?:\s*)(?:val|def|let|func|type)\s+([a-zA-Z_][a-zA-Z0-9_]*)/;
  for (let i = 0; i < lines.length; i++) {
    const m = lines[i].match(pattern);
    if (m) {
      symbols.push(DocumentSymbol.create(
        m[1],
        'ErgoScript symbol',
        SymbolKind.Function,
        { start: { line: i, character: 0 }, end: { line: i, character: lines[i].length } },
        { start: { line: i, character: 0 }, end: { line: i, character: lines[i].length } }
      ));
    }
  }
  return symbols;
}

// Simple hover: show the token under cursor and a tiny help
connection.onHover((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return null;
  const pos = params.position;
  const line = doc.getText({ start: { line: pos.line, character: 0 }, end: { line: pos.line + 1, character: 0 } });
  const words = line.split(/[^a-zA-Z0-9_]/).filter(Boolean);
  const word = words.find(w => line.indexOf(w) <= pos.character && line.indexOf(w) + w.length >= pos.character);
  if (!word) return null;
  return { contents: { kind: 'markdown', value: `**${word}**\n\nErgoScript token (hover info is minimal).` } };
});

// Handle document changes
documents.onDidChangeContent(change => {
  const diagnostics = validateText(change.document.getText());
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

documents.onDidOpen(e => {
  const diagnostics = validateText(e.document.getText());
  connection.sendDiagnostics({ uri: e.document.uri, diagnostics });
});

connection.onDocumentSymbol((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return [];
  return extractSymbols(doc.getText());
});

documents.listen(connection);
connection.listen();
