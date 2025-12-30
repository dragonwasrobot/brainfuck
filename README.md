# Brainfuck

This is an implementation of a brainfuck interpreter written in Elm.

**An online demo can be found here:** https://www.dragonwasrobot.com/brainfuck/

## Setup

Ensure you have node.js 24 and Elm 0.19.1 installed. The project uses
[mise-en-place](https://mise.jdx.dev/) for managing compiler/runtime versions
and task management.

### Local development loop

With Elm installed, perform the following steps:

    $ mise watch dev
    $ mise run serve

in two different terminal windows/tabs. These commands together do the following:

- Recompile tailwind CSS upon file changes.
- Recompile Elm to JS upon file changes.
- Rerun tests upon file changes.
- Setup a tiny local HTTP server to serve brainfuck source files.

If you don't want to use `mise` you can consult the different `scripts` found in
`package.json` and run a way you prefer.

### Production build

- Run the command `./build.sh` to compile the source, then
- open `docs/index.html` in your favorite browser.

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

## Inspiration

- The look and feel of the UI is adapted from the 70's technical documentation
  inspired mock-ups at https://usgraphics.com/products/berkeley-mono
- Most of the example brainfuck programs are from https://brainfuck.org/ and
  originally authored by Daniel B Cristofani.
