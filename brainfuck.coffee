# brainfuck.coffee

# This is a Brainfuck interpreter. Brainfuck has two types of constructs:
# commands and comments.
# There are 8 commands and all consist of 1 symbol: (> < + - . , [ ]).
# Everything else is a comment.

# # Lexer

# The Lexer converts the sequence of characters (aka the program representation)
# into a sequence of tokens.

# ## Valid Symbols
GT = ">" # Increments the pointer (++p; in C)
LT = "<" # Decrements the pointer (--p; in C)
PLUS = "+" # Increments the byte at the pointer (++*p; in C)
MINUS = "-" # Decrements the byte at the pointer (--*p; in C)
DOT = "." # Output the byte at the pointer (putchar(*p); in C)
COMMA = "," # Input a byte and store it in the byte at the pointer
# (*p = getchar() in C);
LBRACKET = "[" # Jump forward past the matching ] if the byte at the pointer
# is zero. (while (*p) { in C)
RBRACKET = "]" # Jump backward to the matching [ unless the byte at the
# pointer is zero. (} in C)

SYMBOLS = [GT, LT, PLUS, MINUS, DOT, COMMA, LBRACKET, RBRACKET]

# ## Tokens
INC_POINTER = "INC_POINTER"
DEC_POINTER = "DEC_POINTER"
INC_BYTE = "INC_BYTE"
DEC_BYTE = "DEC_BYTE"
OUTPUT_BYTE = "OUTPUT_BYTE"
INPUT_BYTE = "INPUT_BYTE"
START_BLOCK = "START_BLOCK"
END_BLOCK = "END_BLOCK"

TOKENS = [INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE, OUTPUT_BYTE, INPUT_BYTE,
  START_BLOCK, END_BLOCK]

symbolTokenMap = {}
(symbolTokenMap[symbol] = TOKENS[index]) for symbol, index in SYMBOLS

# ## Tokenize

# (program : String) -> (tokens : list String)
tokenize = (program) ->
  program.split("") # Turns program string into a character array
    .filter((char) -> char in SYMBOLS) # Remove comments
    .map((symbol) -> symbolTokenMap[symbol]) # Map symbols to tokens

# # Parser

# Labels
ROOT = "ROOT"
BLOCK = "BLOCK"

# The Parser construct an abstract syntax tree based on the list of tokens.

# ## Parse
ast = { 'value' : ROOT, 'parent' : null , 'children' : [] }
currentParent = ast

# (tokens : list String) -> (ast : tree String)
parse = (tokens) ->
  token = head(tokens)
  switch token

    when INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE, OUTPUT_BYTE, INPUT_BYTE
      currentParent.children.push token
      parse tail(tokens)

    when START_BLOCK
      blockObj = { 'value' : BLOCK, 'parent' : currentParent , 'children' : [] }
      currentParent.children.push blockObj
      currentParent = blockObj
      parse tail(tokens)

    when END_BLOCK
      currentParent = currentParent.parent
      parse tail(tokens)

  return ast

# # Evaluator

# ## Machine Model
environment = (0 for i in [0...25]) # Should be 30000
pointer = 0
# TODO: handle input

# ## Evaluate

# (ast : tree String) -> unit
evaluate = (ast) ->
  evaluateBlock ast

evaluateBlock = (block) ->
  for child in block.children
    if isCommand(child) then evaluateCommand(child)
    if isBlock(child) then evaluateBlock(child) while environment[pointer] > 0

evaluateCommand = (command) ->
  switch command
    when INC_POINTER
      pointer++
    when DEC_POINTER
      pointer--
    when INC_BYTE
      environment[pointer]++
    when DEC_BYTE
      environment[pointer]--
    when OUTPUT_BYTE
      console.log(String.fromCharCode(environment[pointer]))
    when INPUT_BYTE
      console.log "(Should prompt user)"
      # prompt("Input:")

isCommand = (child) -> typeof child is 'string'
isBlock = (child) -> typeof child is 'object'

# # Helper functions
head = (list) -> list[0]
tail = (list) -> list[1..]

# # Main
prompt = require 'prompt'
fs = require 'fs'
prompt.start()
prompt.get(['filename'], (error, result) ->
  if error
    console.log("Error: " + error)
    return
  console.log "-*- Input Program -*-"
  console.log("Filename: " + result.filename)
  fs.readFile(result.filename, 'utf8', (error, program) ->
    if error
      console.log("Error: " + error)
      return
    console.log '\"' + program + '\"'
    console.log "-*- Tokenizing -*-"
    tokens = tokenize(program)
    console.log tokens
    console.log "-*- Parsing -*-"
    ast = parse(tokens)
    console.log ast
    console.log "-*- Evaluating -*-"
    evaluate ast
    console.log "-*- Program Execution Terminated -*-"))
