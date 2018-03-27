# # Decompiler

# The decompiler decompiles the brainfuck source code into CoffeeScript.

# ## Requirements

# TODO

# ## Decompile

decompile = (tree) ->
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

  # decompile functions
  decompileCommand = (tabs, command) ->

    switch getCommandValue(command)

      when INC_POINTER then addString(tabs, 'pointer++')
      when DEC_POINTER then addString(tabs, 'pointer--')

      when INC_BYTE then addString(tabs, 'cells[pointer]++')
      when DEC_BYTE then addString(tabs, 'cells[pointer]--')

      when OUTPUT_BYTE
        addString(tabs, 'console.log(String.fromCharCode(cells[pointer]))')
      when INPUT_BYTE then addString(tabs, '') # todo

      # Optimized commands
      when INC_POINTER_N then addString(tabs, "pointer += #{command.count}")
      when DEC_POINTER_N then addString(tabs, "pointer -= #{command.count}")
      when INC_BYTE_N then addString(tabs, "cells[pointer] += #{command.count}")
      when DEC_BYTE_N then addString(tabs, "cells[pointer] -= #{command.count}")

  decompileBlock = (tabs, block) ->
    tabs++
    for child in getBlockChildren(block)
      if isCommand(child) then decompileCommand(tabs, child)
      if isBlock(child)
        addString(tabs, 'while cells[pointer] > 0')
        decompileBlock(tabs, child)

  decompileBlock(0, tree)

  addString(0, 'fun()')

  return js

# ## Exports
exports.decompile = decompile
