Brainfuck
=========

A brainfuck interpreter written in CoffeeScript. The goal of the project is to
write an interpreter which is just sophisticated enough to be interesting to
study while still being easy to explain. Thus, I plan to extensively document
the project in such a way that the source code can be read using the linear
[docco](http://jashkenas.github.io/docco/) format. Furthermore, I hope to have a
web-based visualization of the step-wise execution of a brainfuck program up and
running in the not-too-distant future.

## General
- **Author:** Peter Urbak, peter@dragonwasrobot.com
- **Created:** 2013-09-16
- **Last Modified:** 2013-09-29
- **URL:** https://github.com/dragonwasrobot/brainfuck
- **License:** MIT License

## Install

Run `make install` followed by `coffee brainfuck`.

## Commands

- `>` increments the data pointer
- `<` decrements the data pointer
- `+` increments the byte at the data pointer
- `-` decrements the byte at the data pointer
- `.` outputs the byte at the data pointer
- `,` prompts the user for a one byte input
- `[` jumps to the next `]` if the byte at the data pointer is zero
- `]` jumps to the previous `[` if the byte at the data pointer is nonzero
