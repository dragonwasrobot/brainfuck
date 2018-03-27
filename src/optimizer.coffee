# # Optimizer

# ## Requirements

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

# ## Helper functions
isBlock = (node) -> node.value is BLOCK

# ## Labels

INC_POINTER_N = 'INC_POINTER_N'
DEC_POINTER_N = 'DEC_POINTER_N'
INC_BYTE_N = 'INC_BYTE_N'
DEC_BYTE_N = 'DEC_BYTE_N'

# ## Optimize

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

  [node].concat(commands)

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
      when INC_POINTER then optimizeCommands(fuseIncrementPointers(commands))
      when DEC_POINTER then optimizeCommands(fuseDecrementPointers(commands))
      when INC_BYTE then optimizeCommands(fuseIncrementBytes(commands))
      when DEC_BYTE then optimizeCommands(fuseDecrementBytes(commands))
      else [command].concat(optimizeCommands(tail(commands)))

  tree.children = optimizeCommands(tree.children)
  return tree

optimize = (tree) ->

  # Fuse increments/decrements and left/right movement (pointer and cell values)
  optimizedTree = fuse tree

  # TODO: more optimizations

  # Return optimized tree
  optimizedTree

# ## Exports
exports.optimize = optimize
exports.labels = {}
exports.labels.INC_POINTER_N = INC_POINTER_N
exports.labels.DEC_POINTER_N = DEC_POINTER_N
exports.labels.INC_BYTE_N = INC_BYTE_N
exports.labels.DEC_BYTE_N = DEC_BYTE_N
