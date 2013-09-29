all: install docs

install:
	npm install

docs:
	./node_modules/.bin/docco -l linear brainfuck.coffee

.PHONY: all docs install
