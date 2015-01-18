# # Lexer

# The lexer converts a sequence of characters, the program representation, into
# a sequence of tokens.

# ## Valid Symbols

# The brainfuck language consists of 8 commands, all of which are represented by
# one symbol each:
SYMBOLS = [
  '>' # increment the data pointer
  '<' # decrement the data pointer
  '+' # increment the byte at the data pointer
  '-' # decrement the byte at the data pointer
  '.' # output the byte at the data pointer
  ',' # input a byte and store it in the byte at the data pointer
  '[' # jump forward past the matching `]` if the byte at data pointer is zero
  ']'  # jump back to the matching `[` unless the byte at data pointer is zero
]
# while all other symbols will be treated as comments.

# ## Tokens

# Given the tokens for each of the commands,
INC_POINTER = 'INC_POINTER'
DEC_POINTER = 'DEC_POINTER'
INC_BYTE = 'INC_BYTE'
DEC_BYTE = 'DEC_BYTE'
OUTPUT_BYTE = 'OUTPUT_BYTE'
INPUT_BYTE = 'INPUT_BYTE'
START_BLOCK = 'START_BLOCK'
END_BLOCK = 'END_BLOCK'

# we construct a map between the character symbols and their corresponding token
# symbols.
TOKEN_MAP = {
  '>': INC_POINTER
  '<': DEC_POINTER
  '+': INC_BYTE
  '-': DEC_BYTE
  '.': OUTPUT_BYTE
  ',': INPUT_BYTE
  '[': START_BLOCK
  ']': END_BLOCK
}

# ## Tokenize

tokenize = (program) ->

  # Then, we introduce two variables, for keeping track of the row and column of
  # each symbol we encounter.
  row = 1
  column = 0

  tokenizeChar = (char) ->

    token = {
      'value' : TOKEN_MAP[char]
      'row' : row
      'column' : column
    }

    column++
    if char is '\n'
      row = row + 1
      column = 0

    token

  # Now, given a program, represented by a string, `tokenizeProgram` returns a
  # list of tokens.
  tokenizeProgram = (program) ->
    # turn the program string into a character array
    program.split('')
      # map each symbol to a token object
      .map( (char) -> tokenizeChar(char, charTokenMap) )
      # remove comments
      .filter( (token) -> token.value? )

  tokenizeProgram program

# ## Exports
exports.tokenize = tokenize
exports.tokens = {}
exports.tokens.INC_POINTER = INC_POINTER
exports.tokens.DEC_POINTER = DEC_POINTER
exports.tokens.INC_BYTE = INC_BYTE
exports.tokens.DEC_BYTE = DEC_BYTE
exports.tokens.OUTPUT_BYTE = OUTPUT_BYTE
exports.tokens.INPUT_BYTE = INPUT_BYTE
exports.tokens.START_BLOCK = START_BLOCK
exports.tokens.END_BLOCK = END_BLOCK
