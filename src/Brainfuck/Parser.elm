module Brainfuck.Parser exposing
    ( AbstractSyntaxTree(..)
    , Expression(..)
    , parse
    )

import Parser as P exposing ((|.), (|=), Parser, Step(..))


{-| The `parse` function takes a Brainfuck source program as a `String` and
constructs an `AbstractSyntaxTree` of the program.

Syntax:

    <program> ::= { <block> | <command> }
    <block>   ::= '[' { <block> | <command> } ']'
    <command> ::= '>' | '<' | '+' | '-' | '.' | ','

    <program> ::= { <instr> }
    <instr>   ::= <loop> | <command>
    <loop>    ::= "[" { <instr> } "]"
    <command> ::= ">" | "<" | "+" | "-" | "." | ","

Semantics:

  - '>' -- increment the data pointer
  - '<' -- decrement the data pointer
  - '+' -- increment the byte at the data pointer
  - '-' -- decrement the byte at the data pointer
  - '.' -- output the byte at the data pointer
  - ',' -- input a byte and store it in the byte at the data pointer
  - '[' -- jump forward past the matching `]` if the byte at data pointer is zero
  - ']' -- jump back to the matching `[` unless the byte at data pointer is zero

-}
parse : String -> Result (List P.DeadEnd) AbstractSyntaxTree
parse source =
    let
        cleanedSource =
            -- Bit of a hack, should also parse comments
            source
                |> String.filter (\c -> List.member c [ '[', ']', '+', '-', '>', '<', ',', '.' ])
    in
    P.run pProgram cleanedSource


type AbstractSyntaxTree
    = AbstractSyntaxTree (List Expression)


type Expression
    = Block (List Expression)
    | IncrementPointer
    | DecrementPointer
    | IncrementByte
    | DecrementByte
    | OutputByte
    | InputByte


pProgram : Parser AbstractSyntaxTree
pProgram =
    P.map AbstractSyntaxTree (many pExpression)


pExpression : Parser Expression
pExpression =
    P.oneOf [ pBlock, pCommand ]


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    P.loop [] (manyHelp p)


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    P.oneOf
        [ P.succeed (\v -> Loop (v :: vs))
            |= p
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> Done (List.reverse vs))
        ]


pBlock : Parser Expression
pBlock =
    P.succeed Block
        |= brackets (many (P.lazy (\_ -> pExpression)))


{-| Parse an expression between square brackets.

    brackets p == between (symbol "[") (symbol "]") p

-}
brackets : Parser a -> Parser a
brackets =
    between (P.symbol "[") (P.symbol "]")


{-| Parse an expression between two other parsers
-}
between : Parser opening -> Parser closing -> Parser a -> Parser a
between opening closing p =
    P.succeed identity
        |. opening
        |. P.spaces
        |= p
        |. P.spaces
        |. closing


pCommand : Parser Expression
pCommand =
    P.oneOf
        [ P.map (\_ -> IncrementPointer) (P.symbol ">")
        , P.map (\_ -> DecrementPointer) (P.symbol "<")
        , P.map (\_ -> IncrementByte) (P.symbol "+")
        , P.map (\_ -> DecrementByte) (P.symbol "-")
        , P.map (\_ -> OutputByte) (P.symbol ".")
        , P.map (\_ -> InputByte) (P.symbol ",")
        ]
