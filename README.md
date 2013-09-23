Brainfuck
=========
This is, at the moment, a rather naive brainfuck interpreter written in CoffeeScript.

## Commands

- '>' increments the data pointer
- '<' decrements the data pointer
- '+' increments the byte at the data pointer
- '-' decrements the byte at the data pointer
- '.' outputs the byte at the data pointer
- ',' prompts the user for a one byte input
- '[' jumps to the next ']' if the byte at the data pointer is zero
- ']' jumps to the previous '[' if the byte at the data pointer is nonzero
