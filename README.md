# Brainfuck

This is an implementation of a basic brainfuck interpreter written in Elm.

## Commands

The brainfuck language consists of the following eight commands:

- `>` increments the data pointer
- `<` decrements the data pointer
- `+` increments the byte at the data pointer
- `-` decrements the byte at the data pointer
- `.` outputs the byte at the data pointer
- `,` prompts the user for a one byte input
- `[` jumps to the next `]` if the byte at the data pointer is zero
- `]` jumps to the previous `[` if the byte at the data pointer is nonzero

## Installation

**An online demo can be found here:** https://www.dragonwasrobot.com/brainfuck/

Ensure you have Elm 0.19 installed. I personally recommend using asdf,
https://github.com/asdf-vm/asdf, to handle version management of compilers.

With Elm installed, perform the following steps:

- Run the command `elm make src/Main.elm --output=elm.js`,
- start `elm reactor`,
- go to http://localhost:8000, and
- open index.html
