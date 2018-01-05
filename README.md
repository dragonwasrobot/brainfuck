Brainfuck
=========

A brainfuck interpreter written in CoffeeScript.

The goal of the project is to write an interpreter which is just sophisticated
enough to be interesting to study while still being easy to understand.

## Commands

- `>` increments the data pointer
- `<` decrements the data pointer
- `+` increments the byte at the data pointer
- `-` decrements the byte at the data pointer
- `.` outputs the byte at the data pointer
- `,` prompts the user for a one byte input
- `[` jumps to the next `]` if the byte at the data pointer is zero
- `]` jumps to the previous `[` if the byte at the data pointer is nonzero
