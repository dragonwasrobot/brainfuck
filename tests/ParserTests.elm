module ParserTests exposing (..)

import Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))
import Brainfuck.Parser as Parser
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    let
        shouldGiveUnmatchedLeftBracketError =
            test "should give 'unmatched [' error" <|
                \_ ->
                    let
                        inputProgram =
                            "+++++[>+++++++>++<<-]>.>.["

                        expectedResult =
                            Err "Expecting symbol ']' at row:1 and col:27\n"
                    in
                    Expect.equal expectedResult (Parser.parse inputProgram)

        shouldGiveUnmatchedRightBracketError =
            test "should give 'unmatched ]' error" <|
                \_ ->
                    let
                        inputProgram =
                            "+++++[>+++++++>++<<-]>.>.]["

                        expectedResult =
                            Err "Expecting end at row:1 and col:26\n"
                    in
                    Expect.equal expectedResult (Parser.parse inputProgram)

        shouldParseSimpleProgram =
            test "should parse simple program" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "[+[<++>]--]"

                        -- When
                        outputAST =
                            Parser.parse inputProgram

                        -- Then
                        expectedAST =
                            Ok <|
                                ExpRoot
                                    [ ExpBlock
                                        [ ExpCommand IncrementByte
                                        , ExpBlock
                                            [ ExpCommand DecrementPointer
                                            , ExpCommand IncrementByte
                                            , ExpCommand IncrementByte
                                            , ExpCommand IncrementPointer
                                            ]
                                        , ExpCommand DecrementByte
                                        , ExpCommand DecrementByte
                                        ]
                                    ]
                    in
                    Expect.equal expectedAST outputAST
    in
    describe "Parser:"
        [ shouldParseSimpleProgram
        , shouldGiveUnmatchedLeftBracketError
        , shouldGiveUnmatchedRightBracketError
        ]
