# This is a brainfuck interpreter.

# The brainfuck language consists of 8 commands, all of which are represented by
# one symbol each:

# - `>` increment the data pointer.
# - `<` decrement the data pointer.
# - `+` increment the byte at the data pointer.
# - `-` decrement the byte at the data pointer.
# - `.` output the byte at the data pointer.
# - `,` input a byte and store it in the byte at the data pointer.
# - `[` jump forward past the matching `]` if the byte at the data pointer is zero.
# - `]` jump back to the matching `[` unless the byte at the data pointer is zero.

# ## Requirements

fs = require 'fs'
prompt = require('sync-prompt').prompt
# Needed for handling I/O.

# # Lexer

# The lexer converts a sequence of characters, the program representation,
# into a sequence of tokens.

# ## Valid Symbols

# First, we construct a list of the valid symbols in the brainfuck language
SYMBOLS = [ '>', '<', '+', '-', '.', ',', '[', ']' ]
# while all other symbols will be treated as comments.

# ## Tokens

# Given the tokens for each of the commands
INC_POINTER = "INC_POINTER"
DEC_POINTER = "DEC_POINTER"
INC_BYTE = "INC_BYTE"
DEC_BYTE = "DEC_BYTE"
OUTPUT_BYTE = "OUTPUT_BYTE"
INPUT_BYTE = "INPUT_BYTE"
START_BLOCK = "START_BLOCK"
END_BLOCK = "END_BLOCK"

# we create a corresponding list of these
TOKENS = [INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE,
  OUTPUT_BYTE, INPUT_BYTE, START_BLOCK, END_BLOCK]

# ## Tokenize

# Token constructor
createToken = (char, row, column, charTokenMap) -> {
    'value' : charTokenMap[char]
    'row' : row,
    'column' : column,
  }

# Token selectors
getTokenValue = (token) -> token.value
getTokenRow = (token) -> token.row
getTokenColumn = (token) -> token.column

tokenize = (program) ->

  # First we construct a map between the character symbols and their
  # corresponding token symbols.
  charTokenMap = {}
  (charTokenMap[char] = TOKENS[index]) for char, index in SYMBOLS

  # Then, we introduce two variables, for keeping track of the row and column of
  # each symbol we encounter.
  row = 1
  column = 0

  tokenizeChar = (char, charTokenMap) ->
    token = createToken(char, row, column, charTokenMap)
    column++
    if char is '\n'
      row = row + 1
      column = 0
    return token

  # Now, given a program, represented by a string, `tokenizeProgram` returns a
  # list of tokens.
  tokenizeProgram = (program) ->
    # turn the program string into a character array
    program.split("")
      # map each symbol to a token object
      .map((char) -> tokenizeChar(char, charTokenMap))
      # remove comments
      .filter((token) -> getTokenValue(token) isnt undefined)

  return tokenizeProgram program

# Now, we can hand over the list of tokens to the parser.

# # Parser

# The parser takes a sequence of tokens and constructs a parse tree, an abstract
# syntax tree.

# ## Grammar

# The grammar of brainfuck is very simple (we ignore comments):

# ```
# <program> ::= { <block> | <command> }
# <block>   ::= '[' { <block> | <command> } ']'
# <command> ::= '>' | '<' | '+' | '-' | '.' | ','
# ```

# ## Labels

# We notice that we need two new labels for tagging the root node and blocks.
PROGRAM = "PROGRAM"
BLOCK = "BLOCK"

# The Parser construct an abstract syntax tree based on the list of tokens.

# ## Parse

# Command constructor
createCommand = (token, parent) -> {
  'value' : getTokenValue(token),
  'parent' : parent,
  'row' : getTokenRow(token),
  'column' : getTokenColumn(token)
}

# Command selector
getCommandValue = (command) -> command.value
getCommandParent = (command) -> command.parent
getCommandRow = (command) -> command.row
getCommandColumn = (command) -> command.column

# Block constructor
createBlock = (token, parent) -> {
  'value' : BLOCK,
  'parent' : parent,
  'children' : [],
  'row' : getTokenRow(token),
  'column' : getTokenColumn(token)
}

# Block selector
getBlockValue = (block) -> block.value
getBlockParent = (block) -> block.parent
getBlockChildren = (block) -> block.children
getBlockRow = (block) -> block.row
getBlockColumn = (block) -> block.column

# Block mutators
addChildToBlock = (block, child) -> getBlockChildren(block).push child

parse = (tokens) ->

  # Again, we create two local variables, the `root` which is the root of the
  # abstract syntax tree which we will build in the parsing process, and
  # `currentParent` which is a pointer that keeps track of the current parent.
  root = {
    'value' : PROGRAM,
    'parent' : null,
    'children' : [],
    'row' : 0,
    'column' : 0
  }
  currentParent = root

  # `parseTokens` takes a list of tokens and returns an abstract syntax tree of
  # the program.
  parseTokens = (tokens) ->

    if tokens.length == 0
      # EOF checks
      if getBlockValue(currentParent) isnt PROGRAM
        throw "Block starting at position #{getBlockRow(currentParent)}
:#{getBlockColumn(currentParent)} was not closed before EOF."
      return # finished parsing

    token = head(tokens)
    switch getTokenValue(token)

      when INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE, OUTPUT_BYTE, INPUT_BYTE
        command = createCommand(token, currentParent)
        addChildToBlock(currentParent, command)
        parseTokens tail(tokens)

      when START_BLOCK
        block = createBlock(token, currentParent)
        addChildToBlock(currentParent, block)
        currentParent = block
        parseTokens tail(tokens)

      when END_BLOCK
        if getBlockValue(currentParent) is PROGRAM
          throw "Found unmatched ']' at position #{token.row}:#{token.column}."
        currentParent = getBlockParent(currentParent)
        parseTokens tail(tokens)

  # Parse token list
  parseTokens tokens

  # Return resulting root
  return root

# # Evaluator

# ## Machine Model

# The machine model we are going to use for this interpreter is very simple:
# - Our memory consists of 100 cells (the original version uses 30000).
# - There's a data pointer which points to a specific cell and is initialized at
#   the leftmost cell, an error will be reported if the pointer runs off the
#   tape at either end.
# - A data cell is 8 bytes, and an error will be reported if the program tries
#   to perform under- or overflow, i.e. decrement 0 or increment 255.
# - Two streams of bytes for input and output using the ASCII character
#   encoding.
tapeSize = 100
cells = (0 for i in [0...tapeSize])
cellsize = 255
pointer = 0
output = (byte) -> console.log String.fromCharCode(byte)
input = () -> prompt "Input byte value> "

# ## Evaluate

# (ast : tree String) -> unit
evaluate = (tree) ->

  commands = [ INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE,
    OUTPUT_BYTE, INPUT_BYTE ]

  isCommand = (node) -> node.value in commands
  isBlock = (node) -> node.value is BLOCK

  evaluateCommand = (command) ->
    log "{ value: #{getCommandValue(command)},\n
  row: #{getCommandRow(command)},\n
  column: #{getCommandColumn(command)} }"
    switch getCommandValue(command)

      when INC_POINTER
        if pointer is tapeSize
          throw "Pointer ran off tape, to the right, while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."
        pointer++

      when DEC_POINTER
        if pointer is 0
            throw "Pointer ran off tape, to the left, while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."
        pointer--

      when INC_BYTE
        if cells[pointer] is cellsize
          throw "Integer overflow while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."
        cells[pointer]++

      when DEC_BYTE
        if cells[pointer] is 0
          throw "Integer underflow while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."
        cells[pointer]--

      when OUTPUT_BYTE
        output cells[pointer]

      when INPUT_BYTE
        cells[pointer] = input()

  evaluateBlock = (block) ->
    for child in getBlockChildren(block)
      if isCommand(child) then evaluateCommand(child)
      if isBlock(child) then evaluateBlock(child) while cells[pointer] > 0

  return evaluateBlock tree

# # Helper functions
head = (list) -> list[0]
tail = (list) -> list[1..]

debug = false
log = (string) -> if debug then console.log string

# # Main

# We presume no one in their right mind would want to type brainfuck directly
# into the interpreter, but rather specify the path of a file to execute.

# filename = prompt("Enter path of input program: ")
filename = "test-programs/helloworld.bf"
program = fs.readFileSync(filename, 'utf8')

log "-*- Input Program -*-"
log '\"' + program + '\"'

log "-*- Tokenizing -*-"
tokens = tokenize(program)
log tokens

log "-*- Parsing -*-"
tree = parse(tokens)
log tree

log "-*- Evaluating -*-"
evaluate tree

log "-*- Program Execution Terminated -*-"
