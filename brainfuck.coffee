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
output = (byte) -> outputBuffer += String.fromCharCode(byte)
input = () -> prompt "Input byte value> "

outputBuffer = ""

# ## Evaluate

# (ast : tree String) -> unit
evaluate = (tree) ->

  # Reset tape, pointer and outputBuffer
  outputBuffer = ""
  pointer = 0
  cells = (0 for i in [0...tapeSize])

  commands = [ INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE,
    OUTPUT_BYTE, INPUT_BYTE,
    # Optimized commands
    INC_POINTER_N, DEC_POINTER_N, INC_BYTE_N, DEC_BYTE_N ]

  isCommand = (node) -> node.value in commands
  isBlock = (node) -> node.value is BLOCK

  evaluateCommand = (command) ->
    switch getCommandValue(command)

      # Normal Commands
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

      # Optimized Commands
      when INC_POINTER_N
        pointerIncValue = command.count
        pointer += pointerIncValue
        if pointer > tapeSize
          throw "Pointer ran off tape, to the right, while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."

      when DEC_POINTER_N
        pointerDecValue = command.count
        pointer -= pointerDecValue
        if pointer < 0
            throw "Pointer ran off tape, to the left, while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."

      when INC_BYTE_N
        cellIncValue = command.count
        cells[pointer] += cellIncValue
        if cells[pointer] > cellsize
          throw "Integer overflow while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."

      when DEC_BYTE_N
        cellDecValue = command.count
        cells[pointer] -= cellDecValue
        if cells[pointer] < 0
          throw "Integer underflow while evaluating:
#{getCommandRow(command)}:#{getCommandColumn(command)}."

  evaluateBlock = (block) ->
    for child in getBlockChildren(block)
      if isCommand(child) then evaluateCommand(child)
      if isBlock(child) then evaluateBlock(child) while cells[pointer] > 0

  evaluateBlock tree

  return outputBuffer

# (ast : tree String) -> String (CoffeeScript)
compileToCoffeeScript = (tree) ->
  js = "# Compiled Brainfuck code:\n
cells = (0 for i in [0...#{tapeSize}])\n
pointer = 0\n
fun = () ->\n"

  # Indentation bookkeeper
  insertTabs = (tabs) -> (js += '\t') for tab in [tabs...0]
  addString = (tabs, str) ->
    insertTabs(tabs)
    (js += "#{str}\n")

  # Predicate functions
  commands = [ INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE,
    OUTPUT_BYTE, INPUT_BYTE,
    # Optimized commands
    INC_POINTER_N, DEC_POINTER_N, INC_BYTE_N, DEC_BYTE_N ]

  isCommand = (node) -> node.value in commands
  isBlock = (node) -> node.value is BLOCK

  # compile functions
  compileCommand = (tabs, command) ->

    switch getCommandValue(command)

      when INC_POINTER then addString(tabs, "pointer++")
      when DEC_POINTER then addString(tabs, "pointer--")

      when INC_BYTE then addString(tabs, "cells[pointer]++")
      when DEC_BYTE then addString(tabs, "cells[pointer]--")

      when OUTPUT_BYTE
        addString(tabs, "console.log(String.fromCharCode(cells[pointer]))")
      when INPUT_BYTE then addString(tabs, "") # todo

      # Optimized commands
      when INC_POINTER_N then addString(tabs, "pointer += #{command.count}")
      when DEC_POINTER_N then addString(tabs, "pointer -= #{command.count}")
      when INC_BYTE_N then addString(tabs, "cells[pointer] += #{command.count}")
      when DEC_BYTE_N then addString(tabs, "cells[pointer] -= #{command.count}")

  compileBlock = (tabs, block) ->
    tabs++
    for child in getBlockChildren(block)
      if isCommand(child) then compileCommand(tabs, child)
      if isBlock(child)
        addString(tabs, "while cells[pointer] > 0")
        compileBlock(tabs, child)

  compileBlock(0, tree)

  addString(0, "fun()")

  return js

# Labels for optimized commands
INC_POINTER_N = 'INC_POINTER_N'
DEC_POINTER_N = 'DEC_POINTER_N'
INC_BYTE_N = 'INC_BYTE_N'
DEC_BYTE_N = 'DEC_BYTE_N'

# (ast : tree String) -> (ast : tree String)
optimize = (tree) ->

  # Helper functions
  isBlock = (node) -> node.value is BLOCK

  # Fuse higher order function
  fuseValue = (commands, oldValue, newValue) ->
    command = head(commands)
    row = command.row
    column = command.column
    count = 0

    while command?.value is oldValue
      count++
      commands = tail(commands)
      command = head(commands)

    node = {
      'value' : newValue,
      'count' : count,
      'row' : row,
      'column' : column
    }

    return [node].concat(commands)

  fuseIncrementPointers = (commands) ->
    fuseValue(commands, INC_POINTER, INC_POINTER_N)
  fuseDecrementPointers = (commands) ->
    fuseValue(commands, DEC_POINTER, DEC_POINTER_N)
  fuseIncrementBytes = (commands) ->
    fuseValue(commands, INC_BYTE, INC_BYTE_N)
  fuseDecrementBytes = (commands) ->
    fuseValue(commands, DEC_BYTE, DEC_BYTE_N)

  # Actual fuse function
  fuse = (tree) ->

    optimizeCommands = (commands) ->
      if commands.length is 0 then return []
      command = head(commands)

      if isBlock(command)
        command.children = optimizeCommands(command.children)
        return [command].concat(optimizeCommands(tail(commands)))

      switch command.value
        when INC_POINTER then return optimizeCommands(fuseIncrementPointers(commands))
        when DEC_POINTER then return optimizeCommands(fuseDecrementPointers(commands))
        when INC_BYTE then return optimizeCommands(fuseIncrementBytes(commands))
        when DEC_BYTE then return optimizeCommands(fuseDecrementBytes(commands))

      return [command].concat(optimizeCommands(tail(commands)))

    tree.children = optimizeCommands(tree.children)
    return tree

  # Fuse Increments/Decrements
  optimizedTree = fuse tree

  return optimizedTree

# Pretty printer
prettyPrint = (tree) ->
  pretty = ""

  # Indentation bookkeeper
  insertTabs = (tabs) -> (pretty += '\t') for tab in [tabs...0]
  addString = (tabs, str) ->
    insertTabs(tabs)
    (pretty += "#{str}\n")

  # Predicate functions
  commands = [ INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE,
    OUTPUT_BYTE, INPUT_BYTE,
    # Optimized commands
    INC_POINTER_N, DEC_POINTER_N, INC_BYTE_N, DEC_BYTE_N ]

  isCommand = (node) -> node.value in commands
  isBlock = (node) -> node.value is BLOCK

  # compile functions
  compileCommand = (tabs, command) ->

    switch getCommandValue(command)

      when INC_POINTER then addString(tabs, "pointer++")
      when DEC_POINTER then addString(tabs, "pointer--")

      when INC_BYTE then addString(tabs, "cells[pointer]++")
      when DEC_BYTE then addString(tabs, "cells[pointer]--")

      when OUTPUT_BYTE
        addString(tabs, "console.log(String.fromCharCode(cells[pointer]))")
      when INPUT_BYTE then addString(tabs, "") # todo

  compileBlock = (tabs, block) ->
    tabs++
    for child in getBlockChildren(block)
      if isCommand(child) then compileCommand(tabs, child)
      if isBlock(child)
        addString(tabs, "while cells[pointer] > 0")
        compileBlock(tabs, child)

  compileBlock(0, tree)

  addString(0, "fun()")

  fs.writeFile("output.coffee", pretty, (err) ->
    if err then log err
    else log "output file saved")

  return pretty


# # Helper functions
head = (list) -> list[0]
tail = (list) -> list[1..]

debug = true
log = (string) -> if debug then console.log string

# # Main

processSource = (source, shouldOptimize, resultFunction) ->
  log "-*- Source -*-"
  log source

  tokens = tokenize(source)
  log "-*- Tokens -*-"
  log tokens

  ast = parse(tokens)
  log "-*- Abstract Syntax Tree -*-"
  log ast

  if shouldOptimize
    ast = optimize(ast)
    console.log "-*- Optimized Abstract Syntax Tree -*-"

  result = resultFunction(ast)
  log "-*- Result -*-"
  log result
  return result

interpret = (source, shouldOptimize) ->
  processSource(source, shouldOptimize, evaluate)

compile = (source, shouldOptimize) ->
  processSource(source, shouldOptimize, compileToCoffeeScript)

# ## Exports
exports.interpret = interpret
exports.compile = compile
