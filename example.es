// Example ErgoScript contract
// This demonstrates the ErgoScript language features

// Get the current block height
val currentHeight = HEIGHT

// Access transaction inputs and outputs
val inputCount = INPUTS.size
val outputCount = OUTPUTS.size

// Check if height is above threshold
if (currentHeight > 1000) {
  // Use sigmaProp to create a proposition
  sigmaProp(proveDlog(someProposition))
} else {
  // Simple true proposition
  sigmaProp(true)
}

// Example function definition
def checkInputs(inputs: Array[Box]): Boolean = {
  val firstInput = inputs(0)
  firstInput.value > 1000000
}

// Pattern matching example
val optionValue: Option[Long] = Some(100L)

optionValue match {
  case Some(value) => println(value)
  case None => println("No value")
}

// AND/OR logic
sigmaProp(checkInputs(INPUTS) AND proveDlog(someKey))
