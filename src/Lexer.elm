module Lexer exposing (Symbol(..), Position, Token, tokenize)

{-| Converts a sequence of characters, the program representation, into a
sequence of tokens.


# Symbols

The Brainfuck language consists of the following 8 symbols, each representing a
command:

[ '>' -- increment the data pointer
, '<' -- decrement the data pointer
, '+' -- increment the byte at the data pointer
, '-' -- decrement the byte at the data pointer
, '.' -- output the byte at the data pointer
, ',' -- input a byte and store it in the byte at the data pointer
, '[' -- jump forward past the matching `]` if the byte at data pointer is zero
, ']' -- jump back to the matching `[` unless the byte at data pointer is zero
]

-}


type Symbol
    = IncrementPointer
    | DecrementPointer
    | IncrementByte
    | DecrementByte
    | OutputByte
    | InputByte
    | StartBlock
    | EndBlock


{-| Tokens

Given the tokens for each of the commands,

-}
type alias Position =
    { row : Int
    , column : Int
    }


type alias Token =
    { value : Symbol
    , position : Position
    }



{- we construct a mapping between characters and their corresponding
   token symbols.
-}


charToSymbol : Char -> Maybe Symbol
charToSymbol symbol =
    case symbol of
        '>' ->
            Just IncrementPointer

        '<' ->
            Just DecrementPointer

        '+' ->
            Just IncrementByte

        '-' ->
            Just DecrementByte

        '.' ->
            Just OutputByte

        ',' ->
            Just InputByte

        '[' ->
            Just StartBlock

        ']' ->
            Just EndBlock

        _ ->
            Nothing


{-| tokenize

We introduce two variables, for keeping track of the row and column of each
symbol we encounter. Then, given a program, represented by a string,
`tokenizeProgram` returns a list of tokens.

-}
tokenize : String -> List Token
tokenize program =
    let
        initPos =
            { row = 1, column = 0 }

        tokenizeChar : Char -> Position -> ( Maybe Token, Position )
        tokenizeChar char position =
            let
                newPosition =
                    if char == '\n' then
                        { row = position.row + 1, column = 0 }
                    else
                        { row = position.row, column = position.column + 1 }
            in
                case charToSymbol char of
                    Just symbol ->
                        let
                            token =
                                { value = symbol
                                , position = position
                                }
                        in
                            ( Just token, newPosition )

                    Nothing ->
                        ( Nothing, newPosition )

        tokenizeProgram : String -> List Token
        tokenizeProgram program =
            program
                |> String.toList
                |> List.foldl
                    (\char ( tokens, pos ) ->
                        let
                            ( maybeToken, newPos ) =
                                tokenizeChar char pos
                        in
                            case maybeToken of
                                Just token ->
                                    ( tokens ++ [ token ], newPos )

                                Nothing ->
                                    ( tokens, newPos )
                    )
                    ( [], initPos )
                |> Tuple.first
    in
        tokenizeProgram program
