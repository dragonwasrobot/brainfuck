# This is a brainfuck interpreter.

# This script contains the main function for evaluating, compiling or pretty
# printing brainfuck source code.

# ## Requirements

lexer = require './lexer'
parser = require './parser'
optimizer = require './optimizer'
evaluator = require './evaluator'
decompiler = require './decompiler'
prettyPrinter = require './prettyPrinter'

# ## Main

processSource = (source, shouldOptimize, resultFunction) ->
  log '-*- Source -*-'
  log source

  tokens = lexer.tokenize(source)
  log '-*- Tokens -*-'
  log tokens

  ast = parser.parse(tokens)
  log '-*- Abstract Syntax Tree -*-'
  log ast

  if shouldOptimize
    ast = optimizer.optimize(ast)
    console.log '-*- Optimized Abstract Syntax Tree -*-'

  result = resultFunction(ast)
  log '-*- Result -*-'
  log result
  return result

interpret = (source, shouldOptimize) ->
  processSource(source, shouldOptimize, evaluate)

decompile = (source, shouldOptimize) ->
  processSource(source, shouldOptimize, decompileToCoffeeScript)

# ## Exports
exports.interpret = interpret
exports.decompile = decompile
