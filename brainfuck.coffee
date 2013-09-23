# brainfuck.coffee

# ## Require
prompt = require('sync-prompt').prompt
fs = require 'fs'

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
environment = (0 for i in [0...100]) # Originally 30000 instead of 100
pointer = 0
output = (byte) -> console.log String.fromCharCode(byte)
input = () -> prompt "Input byte value: "

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
      output environment[pointer]
    when INPUT_BYTE
      environment[pointer] = input()

isCommand = (child) -> typeof child is 'string'
isBlock = (child) -> typeof child is 'object'

# # Helper functions
head = (list) -> list[0]
tail = (list) -> list[1..]

debug = false
log = (string) -> if debug then console.log string

# # Main

# We presume no one in their right mind would want to type brainfuck directly
# into the interpreter, but rather specify the path of a file to execute.
filename = prompt("Enter path of input program: ")
program = fs.readFileSync(filename, 'utf8')

log "-*- Input Program -*-"
log '\"' + program + '\"'

log "-*- Tokenizing -*-"
tokens = tokenize(program)
log tokens

log "-*- Parsing -*-"
ast = parse(tokens)
log ast

log "-*- Evaluating -*-"
evaluate ast

log "-*- Program Execution Terminated -*-"
