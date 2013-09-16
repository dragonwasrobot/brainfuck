# brainfuck.coffee

# This is a Brainfuck interpreter. Brainfuck has two types of constructs:
# commands and comments.
# There are 8 commands and all consist of 1 symbol: (> < + - . , [ ]).
# Everything else is a comment.

# # Lexer

# The Lexer converts the sequence of characters (aka the program representation)
# into a sequence of tokens.

# ## Tokens
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

COMMANDS = [GT, LT, PLUS, MINUS, DOT, COMMA, LBRACKET, RBRACKET]

# ## Tokenize
tokenize = (programString, tokenList) ->
  console.log "-*- Tokenizing -*-"
  for char in programString
    if char in COMMANDS
      tokenList.push char
    # else it should be treated as a comment.
  return tokenList

# # Parser

# The Parser construct an abstract syntax tree based on the list of tokens.

# ## Labels
INC_POINTER = "INC_POINTER"
DEC_POINTER = "DEC_POINTER"
INC_BYTE = "INC_BYTE"
DEC_BYTE = "DEC_BYTE"
OUTPUT_BYTE = "OUTPUT_BYTE"
INPUT_BYTE = "INPUT_BYTE"
JUMP_FORWARD = "JUMP_FORWARD"
JUMP_BACKWARD = "JUMP_BACKWARD"

# ## Parse
parse = (tokenList, abstractSyntaxTree) ->
  console.log "-*- Parsing -*-"
  for token in tokenList
    switch token
      when GT
        abstractSyntaxTree.push INC_POINTER
      when LT
        abstractSyntaxTree.push DEC_POINTER
      when PLUS
        abstractSyntaxTree.push INC_BYTE
      when MINUS
        abstractSyntaxTree.push DEC_BYTE
      when DOT
        abstractSyntaxTree.push OUTPUT_BYTE
      when COMMA
        abstractSyntaxTree.push INPUT_BYTE
      when LBRACKET
        abstractSyntaxTree.push JUMP_FORWARD
      when RBRACKET
        abstractSyntaxTree.push JUMP_BACKWARD

  return abstractSyntaxTree

# # Machine Model
instructionPointer = 0
byteCells = (0 for i in [10..1]) # Should be 30000
dataPointer = 0
# input
# output = console.log

# # Main
program = "this+>>is>>..,<<a,++[-test.--].program"
console.log "Input program:"
console.log program
tokenList = tokenize(program, [])
console.log "Resulting token list:"
console.log tokenList
abstractSyntaxTree = parse(tokenList, [])
console.log "Resulting abstract syntax tree:"
console.log abstractSyntaxTree
