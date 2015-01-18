# # Parser

# The parser takes a sequence of tokens and constructs a parse tree, an abstract
# syntax tree.

# ## Requirements

util = require './util'
head = util.head
tail = util.tail
debug = util.debug
log = util.log

tokens = require('./lexer').tokens
INC_POINTER = tokens.INC_POINTER
DEC_POINTER = tokens.DEC_POINTER
INC_BYTE = tokens.INC_BYTE
DEC_BYTE = tokens.DEC_BYTE
OUTPUT_BYTE = tokens.OUTPUT_BYTE
INPUT_BYTE = tokens.INPUT_BYTE
START_BLOCK = tokens.START_BLOCK
END_BLOCK = tokens.END_BLOCK

# ## Grammar

# The grammar of brainfuck is very simple (we ignore comments):

#    <program> ::= { <block> | <command> }
#    <block>   ::= '[' { <block> | <command> } ']'
#    <command> ::= '>' | '<' | '+' | '-' | '.' | ','

# ## Labels

# We notice that we need two new labels for tagging the program root and blocks.
PROGRAM = 'PROGRAM'
BLOCK = 'BLOCK'

# The Parser construct an abstract syntax tree based on the list of tokens.

# ## Parse

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

    if tokens.length is 0
      # EOF checks
      if currentParent.value isnt PROGRAM
        throw new Error("Block starting at position
          #{currentParent.row}:#{currentParent.column} was not
          closed before EOF.")
      return # finished parsing

    token = head(tokens)
    switch token.value

      when INC_POINTER, DEC_POINTER, INC_BYTE, DEC_BYTE, OUTPUT_BYTE, INPUT_BYTE
        command = {
          'value': token.value
          'parent': currentParent
          'row': token.row
          'column': token.column
        }
        currentParent.children.push command
        parseTokens tail(tokens)

      when START_BLOCK
        block = {
          'value': BLOCK
          'parent': currentParent
          'children': []
          'row': token.row
          'column': token.column
        }
        currentParent.children.push command
        currentParent = block
        parseTokens tail(tokens)

      when END_BLOCK
        if currentParent.value is PROGRAM
          throw new Error("Found unmatched ']' at position
            #{token.row}:#{token.column}.")
        currentParent = currentParent.parent
        parseTokens tail(tokens)

      else throw new Error("Unknown token: #{token.value}")

  # Parse token list
  parseTokens tokens

  # Return resulting root
  root

# ## Exports
exports.parse = parse
exports.labels = {}
exports.labels.PROGRAM = PROGRAM
exports.labels.BLOCK = BLOCK
