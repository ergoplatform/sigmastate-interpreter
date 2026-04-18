#!/bin/bash
# Test script for ParserCLI.scala
# Requires: sbt parsers/publishLocal to have been run

set -e

echo "Testing ErgoScript parser CLI wrapper..."

# Find the parser JAR
PARSER_JAR=$(find ../parsers/shared/target -name "parsers_*.jar" | head -1)

if [ -z "$PARSER_JAR" ]; then
  echo "Error: Parser JAR not found. Run 'sbt parsers/publishLocal' first."
  exit 1
fi

echo "Using parser JAR: $PARSER_JAR"

# Test 1: Valid ErgoScript
echo ""
echo "Test 1: Valid ErgoScript"
echo '{ val x = 10; sigmaProp(x > 5) }' | scala -cp "$PARSER_JAR" ParserCLI.scala
echo ""

# Test 2: Invalid ErgoScript (syntax error)
echo "Test 2: Invalid ErgoScript (missing closing brace)"
echo '{ val x = 10' | scala -cp "$PARSER_JAR" ParserCLI.scala || echo "Expected error caught"
echo ""

# Test 3: Parse example file
echo "Test 3: Parse example.es file"
scala -cp "$PARSER_JAR" ParserCLI.scala example.es
echo ""

echo "Tests complete!"
