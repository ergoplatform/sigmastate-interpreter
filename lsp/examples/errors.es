// ============================================
// This file has INTENTIONAL ERRORS for testing
// LSP diagnostics. DO NOT use in production!
// ============================================

{
  // ERROR 1: Syntax error - incomplete expression
  val x = SELF.value +
  
  // ERROR 2: Type error - can't add Long and String
  val y = SELF.value + "hello"
  
  // ERROR 3: Undefined variable
  val z = undefinedVariable + 10
  
  // ERROR 4: Wrong type for sigmaProp
  sigmaProp(SELF.value)  // SELF.value is Long, not Boolean
  
  // ERROR 5: Wrong number of arguments
  atLeast(2)
  
  // ERROR 6: Missing closing brace
