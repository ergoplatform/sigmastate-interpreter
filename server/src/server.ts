import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  InitializeResult,
  ServerCapabilities,
  TextDocumentPositionParams,
  CompletionItem,
  CompletionItemKind,
  Hover,
  MarkupContent,
  MarkupKind,
  ProposedFeatures,
} from "vscode-languageserver/node";

import { TextDocument } from "vscode-languageserver-textdocument";
import { ERGOSCRIPT_KEYWORDS, getKeywordDocumentation } from "./keywords";

// Create a connection for the server, using Node's IPC as a transport.
// Include all proposed features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents = new TextDocuments(TextDocument);

/**
 * Validates ErgoScript document and returns diagnostics
 */
function validateDocument(doc: TextDocument): Diagnostic[] {
  const diagnostics: Diagnostic[] = [];
  const text = doc.getText();

  // Check if file is empty
  if (text.trim().length === 0) {
    diagnostics.push({
      severity: DiagnosticSeverity.Warning,
      range: {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 1 },
      },
      message: "File is empty",
      source: "ergoscript",
    });
  }

  // Check for unbalanced braces
  const braceBalance = checkBraceBalance(text);
  if (braceBalance.isUnbalanced) {
    const diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Error,
      range: {
        start: { line: braceBalance.line, character: braceBalance.character },
        end: {
          line: braceBalance.line,
          character: braceBalance.character + 1,
        },
      },
      message: `Unbalanced braces: expected '${braceBalance.expected}'`,
      source: "ergoscript",
    };
    diagnostics.push(diagnostic);
  }

  return diagnostics;
}

/**
 * Checks if braces are balanced in the text
 */
function checkBraceBalance(
  text: string
): {
  isUnbalanced: boolean;
  line: number;
  character: number;
  expected?: string;
} {
  let braceCount = 0;
  let bracketCount = 0;
  let parenCount = 0;
  let line = 0;
  let character = 0;
  let lastChar = "\0";

  for (let i = 0; i < text.length; i++) {
    const char = text[i];

    if (char === "\n") {
      line++;
      character = 0;
    } else {
      character++;
    }

    // Skip strings and comments
    if (lastChar === "/" && char === "/") {
      // Skip line comment
      while (i < text.length && text[i] !== "\n") {
        i++;
      }
      continue;
    }

    if (char === "{") braceCount++;
    if (char === "}") {
      braceCount--;
      if (braceCount < 0) {
        return {
          isUnbalanced: true,
          line,
          character,
          expected: "{",
        };
      }
    }

    if (char === "[") bracketCount++;
    if (char === "]") {
      bracketCount--;
      if (bracketCount < 0) {
        return {
          isUnbalanced: true,
          line,
          character,
          expected: "[",
        };
      }
    }

    if (char === "(") parenCount++;
    if (char === ")") {
      parenCount--;
      if (parenCount < 0) {
        return {
          isUnbalanced: true,
          line,
          character,
          expected: "(",
        };
      }
    }

    lastChar = char;
  }

  const isUnbalanced = braceCount !== 0 || bracketCount !== 0 || parenCount !== 0;
  return {
    isUnbalanced,
    line: line,
    character: Math.max(0, character - 1),
    expected:
      braceCount > 0 ? "}" : bracketCount > 0 ? "]" : parenCount > 0 ? ")" : undefined,
  };
}

/**
 * Gets word at position in document
 */
function getWordAtPosition(
  document: TextDocument,
  line: number,
  character: number
): string {
  const text = document.getText({
    start: { line, character: 0 },
    end: { line: line + 1, character: 0 },
  });

  let start = character;
  let end = character;

  // Find start of word
  while (start > 0 && /\w/.test(text[start - 1])) {
    start--;
  }

  // Find end of word
  while (end < text.length && /\w/.test(text[end])) {
    end++;
  }

  return text.substring(start, end);
}

/**
 * Extracts all words from document for context
 */
function getWordsBeforeCursor(
  document: TextDocument,
  position: { line: number; character: number }
): string {
  const range = {
    start: { line: 0, character: 0 },
    end: {
      line: position.line,
      character: position.character,
    },
  };
  return document.getText(range);
}

// On initialization
connection.onInitialize((): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: 1, // Full text document sync
      completionProvider: {
        resolveProvider: false,
      },
      hoverProvider: true,
    } as ServerCapabilities,
  };
});

// On document change, validate
documents.onDidChangeContent((change) => {
  const diagnostics = validateDocument(change.document);
  connection.sendDiagnostics({
    uri: change.document.uri,
    diagnostics,
  });
});

// Handle completion requests
connection.onCompletion(
  (textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
    const document = documents.get(textDocumentPosition.textDocument.uri);
    if (!document) {
      return [];
    }

    const items: CompletionItem[] = ERGOSCRIPT_KEYWORDS.map((keyword) => ({
      label: keyword.name,
      kind: CompletionItemKind.Keyword,
      detail: keyword.documentation,
      insertText: keyword.name,
    }));

    return items;
  }
);

// Handle hover requests
connection.onHover(
  (textDocumentPosition: TextDocumentPositionParams): Hover | null => {
    const document = documents.get(textDocumentPosition.textDocument.uri);
    if (!document) {
      return null;
    }

    const word = getWordAtPosition(
      document,
      textDocumentPosition.position.line,
      textDocumentPosition.position.character
    );

    const doc = getKeywordDocumentation(word);
    if (doc) {
      return {
        contents: {
          kind: MarkupKind.PlainText,
          value: doc,
        } as MarkupContent,
      };
    }

    return null;
  }
);

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
