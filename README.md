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

Ensure you have Elm 0.19.1 installed. I personally recommend using asdf,
https://github.com/asdf-vm/asdf, to handle version management of compilers.

With Elm installed, perform the following steps:

- Run the command `./build.sh` to compile the source, then
- open `docs/index.html` in your favorite browser.

### Development loop

    $ npm run css-watch
    $ npm run compile-watch
    $ npm run test-watch
    # npm run local-server

- css-watch: tailwindcli tool for spotting changes to CSS classes.
- compile-watch: elm-watch recompiling on code change.
- test-watch: elm-test re-running test suite on code change.
- local-server: python3 HTTP server to support serving BF source files.

## Inspiration

- The look and feel of the UI is adapted from the 70's technical documentation
  inspired mock-ups at https://usgraphics.com/products/berkeley-mono
- Most of the example brainfuck programs are from https://brainfuck.org/ and
  authored by Daniel B Cristofani.
