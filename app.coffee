express = require 'express'
brainfuck = require './brainfuck'

# ## Configure server
app = express()
app.use express.bodyParser()
app.use express.methodOverride()
app.use app.router

app.use express.static(__dirname + '/html/')

# ## Create interface for brainfuck interpreter
app.get('/interpret/:source', (request, response) ->
  source = request.params.source
  response.send(200, brainfuck.interpret(source) + '\n'))

app.get('/compile/:source', (request, response) ->
  source = request.params.source
  response.send(200, brainfuck.compile(source) + '\n'))

# ## Start server
app.listen 8000
console.log "Server listening on port 8000"
