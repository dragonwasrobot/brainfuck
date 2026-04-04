module Brainfuck.Parser exposing
    ( Command(..)
    , Expression(..)
    , parse
    )

import Parser as P exposing ((|.), (|=), DeadEnd, Parser, Problem(..), Step(..))


{-| The `parse` function takes a Brainfuck source program as a `String` and
constructs an `ExpRoot` of the program.

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
parse : String -> Result String Expression
parse source =
    let
        cleanedSource =
            -- TODO: Bit of a hack, should also parse comments
            source
                |> String.filter (\c -> List.member c [ '[', ']', '+', '-', '>', '<', ',', '.' ])
    in
    cleanedSource
        |> P.run pProgram
        |> Result.mapError deadEndsToString


{-| Pretty prints a `List DeadEnd` error result from parsing Brainfuck source code.
-}
deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:"
                        ++ String.fromInt deadEnd.row
                        ++ " and "
                        ++ "col:"
                        ++ String.fromInt deadEnd.col
                        ++ "\n"
            in
            case deadEnd.problem of
                Expecting string ->
                    "Expecting '" ++ string ++ "' at " ++ position

                ExpectingInt ->
                    "Expecting int at " ++ position

                ExpectingHex ->
                    "Expecting hex at " ++ position

                ExpectingOctal ->
                    "Expecting octal at " ++ position

                ExpectingBinary ->
                    "Expecting binary at " ++ position

                ExpectingFloat ->
                    "Expecting float at " ++ position

                ExpectingNumber ->
                    "Expecting number at " ++ position

                ExpectingVariable ->
                    "Expecting variable at " ++ position

                ExpectingSymbol symbol ->
                    "Expecting symbol '" ++ symbol ++ "' at " ++ position

                ExpectingKeyword keyword ->
                    "Expecting keyword '" ++ keyword ++ "' at " ++ position

                ExpectingEnd ->
                    "Expecting end at " ++ position

                UnexpectedChar ->
                    "Unexpected char at " ++ position

                Problem string ->
                    "Problem string '" ++ string ++ "' at " ++ position

                BadRepeat ->
                    "Bad repeat at " ++ position
    in
    List.foldl (++) "" (List.map deadEndToString deadEnds)


type Expression
    = ExpRoot (List Expression)
    | ExpBlock (List Expression)
    | ExpCommand Command


type Command
    = IncrementPointer
    | DecrementPointer
    | IncrementByte
    | DecrementByte
    | OutputByte
    | InputByte


pProgram : Parser Expression
pProgram =
    P.succeed ExpRoot
        |= many pExpression
        |. P.end


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
    P.succeed ExpBlock
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
    P.map ExpCommand <|
        P.oneOf
            [ P.map (\_ -> IncrementPointer) (P.symbol ">")
            , P.map (\_ -> DecrementPointer) (P.symbol "<")
            , P.map (\_ -> IncrementByte) (P.symbol "+")
            , P.map (\_ -> DecrementByte) (P.symbol "-")
            , P.map (\_ -> OutputByte) (P.symbol ".")
            , P.map (\_ -> InputByte) (P.symbol ",")
            ]
