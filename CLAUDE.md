# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Approach

- Think before acting. Read existing files before writing code.
- Be concise in output but thorough in reasoning.
- Prefer editing over rewriting whole files.
- Do not re-read files you have already read unless the file may have changed.
- Test your code before declaring done.
- No sycophantic openers or closing fluff.
- Keep solutions simple and direct.
- User instructions always override this file.

## Project Overview

A web-based Brainfuck interpreter written in **Elm 0.19.1**, with TailwindCSS 4 for styling. The compiled output lives in `docs/` and is deployed to GitHub Pages.

## Commands

### Development
```bash
mise run dev      # build (lint + CSS + compile) + test
mise run serve    # start Python HTTP server at localhost:8000
mise watch dev    # watch for changes and rerun dev
```

### Individual tasks
```bash
npm run css       # compile TailwindCSS → docs/main.css
npm run compile   # compile Elm → docs/elm.js (debug mode)
npm run format    # check Elm formatting (elm-format)
npm run test      # run elm-test suite
```

### Production build
```bash
./build.sh        # full minified build (compiles with --optimize, minifies with uglify-js)
```

### Running a single test
Elm Test doesn't support filtering by name directly, but you can isolate tests by temporarily wrapping them in `Test.only`:
```elm
Test.only <| test "my test" <| \_ -> ...
```
Then run `npm run test`.

## Architecture

### Data flow
```
User code (string)
  → Parser.elm       — string → Expression AST
  → Evaluator.elm    — walks AST via zipper, executes commands
  → VirtualMachine.elm — holds tape (30,000 cells), data pointer, I/O streams
  → Main.elm         — renders output, manages UI state
```

### Key modules (`src/brainfuck/`)

- **Parser.elm** — converts raw BF source to an `Expression` tree (`ExpRoot | ExpBlock | ExpCommand`). Non-command characters are silently ignored (they act as comments).
- **VirtualMachine.elm** — tape is `Array (Array Int)` (1000 rows × 30 cols). Tracks data pointer, input stream, output stream. All mutations return a new VM (immutable).
- **Evaluator.elm** — uses a **zipper pattern** (breadcrumb trail) for efficient tree traversal. Exposes `evaluateStep` for step-by-step execution. State machine: `Running | Paused | Finished | Crashed`.
- **ASCII.elm** — lookup table (0–127) mapping byte values to decimal, hex, character, and description — used by the Reference page.

### Main.elm

Single-page app with four pages (Elm `Browser.element`):
- **Interpreter** — code editor, paper tape display, step/run controls
- **Reference Manual** — BF command reference
- **Source Archives** — pre-loaded example BF programs (fetched via HTTP from `docs/bf-programs/`)
- **Character Sets** — ASCII table

The `Model` carries: current page, code string, input mode (text/bytes), evaluator state, and a `isProd` flag (injected via Elm flags from `index.html`).

### Base path flag

`index.html` passes `basePath: ""` as a flag to the Elm app. `build.sh` rewrites it to `"/brainfuck"` for the production deploy. Elm uses it only to construct the BF programs fetch URL (`basePath ++ "/bf-programs/"`), with no environment-conditional logic in Elm itself.

## Testing

Tests live in `tests/` and cover Parser, Evaluator, and VirtualMachine. Run with `npm run test` (uses `elm-test`). `elm-verify-examples` also validates doc-comment examples.

## CI/CD

GitHub Actions (`.github/workflows/elm.yml`) runs tests on every push/PR, and deploys to GitHub Pages on passing merges to `master` via `./build.sh`.
