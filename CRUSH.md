# CRUSH.md - Sigma State Interpreter

## Build Commands
- `sbt test` - Run all Scala tests
- `sbt coreJS/test` - Run core JS tests
- `sbt interpreterJS/test` - Run interpreter JS tests
- `sbt "testOnly *.MySpec"` - Run specific test class
- `sbt "testOnly *.MySpec -- -z "test name""` - Run specific test by name
- `npm test` (in sigma-js/) - Run JavaScript tests

## Code Style Guidelines
- **Scala Version**: Supports 2.11, 2.12, 2.13 (default 2.13)
- **Imports**: Group by package, alphabetical order
- **Naming**: camelCase for methods/vars, PascalCase for types
- **Testing**: Use ScalaTest with property-based testing (ScalaCheck)
- **Error Handling**: Use Either/Validation patterns, avoid exceptions
- **Collections**: Use custom `Coll` trait instead of standard Scala collections
- **Types**: Use explicit type annotations for public APIs
- **Documentation**: Scaladoc comments with @tparam, @return, @throws

## Important Restrictions
- **DO NOT MODIFY** non-testing code unless explicitly requested
- Changes should be limited to test files unless otherwise specified
- Production code changes require explicit approval and thorough testing

## Project Structure
- `core/` - Core data types and utilities
- `data/` - Data serialization and crypto
- `interpreter/` - Sigma language interpreter
- `parsers/` - Language parsers
- `sdk/` - Ergo platform SDK
- `sc/` - Sigma compiler
- `sigma-js/` - JavaScript bindings

## Key Dependencies
- ScalaTest, ScalaCheck for testing
- Scrypto for cryptography
- FastParse for parsing
- Circe for JSON
- BouncyCastle for crypto primitives

## Testing Patterns
- Extend `BaseTests`, `BaseNestedTests`, or `BaseShouldTests`
- Use property-based testing with `forAll` generators
- Test utilities in `TestUtils` trait
- JavaScript tests use Jest framework