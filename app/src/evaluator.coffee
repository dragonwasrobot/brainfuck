# # Evaluator

# The evaluator takes an abstract syntax tree (AST) and execute the
# corresponding brainfuck program.

# ## Requirements

prompt = require('sync-prompt').prompt

tokens = require('./lexer').tokens
INC_POINTER = tokens.INC_POINTER
DEC_POINTER = tokens.DEC_POINTER
INC_BYTE = tokens.INC_BYTE
DEC_BYTE = tokens.DEC_BYTE
OUTPUT_BYTE = tokens.OUTPUT_BYTE
INPUT_BYTE = tokens.INPUT_BYTE
START_BLOCK = tokens.START_BLOCK
END_BLOCK = tokens.END_BLOCK

labels = require('./parser').labels
PROGRAM = labels.PROGRAM
BLOCK = labels.BLOCK

optimizedLabels = require('./optimizer').labels
INC_POINTER_N = optimizedLabels.INC_POINTER_N
DEC_POINTER_N = optimizedLabels.DEC_POINTER_N
INC_BYTE_N = optimizedLabels.INC_BYTE_N
DEC_BYTE_N = optimizedLabels.DEC_BYTE_N

# ## Machine Model

# The machine model we are going to use for this interpreter is very simple:

# - Our memory consists of 100 cells (the original version uses 30000).
TAPE_SIZE = 100
cells = (0 for i in [0...TAPE_SIZE])

# - There's a data pointer which points to a specific cell and is initialized at
#   the leftmost cell, an error will be reported if the pointer runs off the
#   tape at either end.
pointer = 0

# - A data cell is 8 bits, and an error will be reported if the program tries
#   to perform under- or overflow, i.e. decrement 0 or increment 255.
CELL_SIZE = 255

# - Two streams of bytes for input and output using the ASCII character
#   encoding.
output = (byte) -> outputBuffer += String.fromCharCode byte
input = () -> prompt 'Input byte value> '

# ## Helper functions and definitions

# Results will be printed to an output buffer
outputBuffer = ''

# Reset function for our machine model
reset = () ->
  outputBuffer = ''
  pointer = 0
  cells = (0 for i in [0...TAPE_SIZE])

# List of valid commands
COMMANDS = [
  # Normal commands
  INC_POINTER
  DEC_POINTER
  INC_BYTE
  DEC_BYTE
  OUTPUT_BYTE
  INPUT_BYTE
  # Optimized commands
  INC_POINTER_N
  DEC_POINTER_N
  INC_BYTE_N
  DEC_BYTE_N
]

# predicate functions
isCommand = (node) -> node.value in COMMANDS
isBlock = (node) -> node.value is BLOCK

# ## Evaluate

handleIncrementPointer = (command) ->
  pointer++
  if pointer > TAPE_SIZE
    throw new Error("Pointer ran off tape (right) while evaluating:
      #{command.row}:#{command.column}.")

handleDecrementPointer = (command) ->
  pointer--
  if pointer < 0
    throw new Error("Pointer ran off tape (left) while evaluating:
      #{command.row}:#{command.column}.")

handleIncrementByte = (command) ->
  cells[pointer]++
  if cells[pointer] > CELL_SIZE
    throw new Error("Integer overflow while evaluating:
      #{command.row}:#{command.column}.")

handleDecrementByte = (command) ->
  cells[pointer]--
  if cells[pointer] < 0
    throw new Error("Integer underflow while evaluating:
      #{command.row}:#{command.column}.")

handleOutputByte = () -> output cells[pointer]

handleInputByte = () -> cells[pointer] = input()

handleIncrementPointerN = (command) ->
  pointer += command.count
  if pointer > TAPE_SIZE
    throw new Error("Pointer ran off tape (right) while evaluating:
      #{command.row}:#{command.column}.")

handleDecrementPointerN = (command) ->
  pointer -= command.count
  if pointer < 0
    throw new Error("Pointer ran off tape (left) while evaluating:
      #{command.row}:#{command.column}.")

handleIncrementByteN = (command) ->
  cells[pointer] += command.count
  if cells[pointer] > CELL_SIZE
    throw new Error("Integer overflow while evaluating:
      #{getCommandRow(command)}:#{getCommandColumn(command)}.")

handleDecrementByteN = (command) ->
  cells[pointer] -= command.count
  if cells[pointer] < 0
    throw new Error("Integer underflow while evaluating:
      #{getCommandRow(command)}:#{getCommandColumn(command)}.")

evaluateCommand = (command) ->

  commandDict = {
    # Normal commands
    INC_POINTER: () -> handleIncrementPointer command
    DEC_POINTER: () -> handleDecrementPointer command
    INC_BYTE: () -> handleIncrementByte command
    DEC_BYTE: () -> handleDecrementByte command
    OUTPUT_BYTE: () -> handleOutputByte()
    INPUT_BYTE: () -> handleInputByte()
    # Optimized commands
    INC_POINTER_N: () -> handleIncrementPointerN command
    DEC_POINTER_N: () -> handleDecrementPointerN command
    INC_BYTE_N: () -> handleIncrementByteN command
    DEC_BYTE_N: () -> handleDecrementByteN command
  }

  if command.value in COMMANDS then commandDict[command.value]()
  else throw new Error("Unknown command: #{command.value}")

evaluateBlock = (block) ->
  for child in block.children
    if isCommand(child) then evaluateCommand(child)
    else if isBlock(child) then evaluateBlock(child) while cells[pointer] > 0

evaluate = (tree) ->
  evaluateBlock tree
  outputBuffer

# ## Exports
exports.evaluate = evaluate
