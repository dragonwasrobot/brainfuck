# # Pretty print

# The pretty printer prints [...]

# ## Requirements

fs = require 'fs'

# ## Pretty printing

# Indentation bookkeeper
insertTabs = (tabs) -> (pretty += '\t') for tab in [tabs...0]
addString = (tabs, str) ->
  insertTabs(tabs)
  (pretty += "#{str}\n")

# Predicate functions
isCommand = (node) -> node.value in [
  INC_POINTER,
  DEC_POINTER,
  INC_BYTE,
  DEC_BYTE,
  OUTPUT_BYTE,
  INPUT_BYTE,
  # Optimized commands
  INC_POINTER_N,
  DEC_POINTER_N,
  INC_BYTE_N,
  DEC_BYTE_N
]
isBlock = (node) -> node.value is BLOCK

# pretty printing functions
prettyPrintCommand = (tabs, command) ->

  switch getCommandValue(command)

    when INC_POINTER then addString(tabs, 'pointer++')
    when DEC_POINTER then addString(tabs, 'pointer--')

    when INC_BYTE then addString(tabs, 'cells[pointer]++')
    when DEC_BYTE then addString(tabs, 'cells[pointer]--')

    when OUTPUT_BYTE
      addString(tabs, 'console.log(String.fromCharCode(cells[pointer]))')
    when INPUT_BYTE then addString(tabs, '') # todo

prettyPrintBlock = (tabs, block) ->
  tabs++
  for child in getBlockChildren(block)
    if isCommand(child) then prettyPrintCommand(tabs, child)
    if isBlock(child)
      addString(tabs, 'while cells[pointer] > 0')
      prettyPrintBlock(tabs, child)

# Pretty printer
prettyPrint = (tree) ->
  prettyResult = ''

  prettyPrintBlock(0, tree)

  addString(0, 'fun()')

  fs.writeFile('output.coffee', pretty, (err) ->
    if err then log err
    else log 'output file saved')

  prettyResult
