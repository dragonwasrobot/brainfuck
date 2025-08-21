module LexerTests exposing (..)

import Brainfuck.Lexer as Lexer exposing (Position, Symbol(..), Token)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Lexer"
        [ test "Can tokenize a simple program" <|
            \_ ->
                let
                    inputTokens =
                        "[+++]"

                    expectedTokens =
                        [ Token StartBlock (Position 1 0)
                        , Token IncrementByte (Position 1 1)
                        , Token IncrementByte (Position 1 2)
                        , Token IncrementByte (Position 1 3)
                        , Token EndBlock (Position 1 4)
                        ]
                in
                Expect.equal expectedTokens (Lexer.tokenize inputTokens)
        ]
