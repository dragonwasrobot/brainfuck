module ParserTests exposing (..)

import Brainfuck.Parser as Parser exposing (AbstractSyntaxTree(..), Command(..), Expression(..))
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Parser"
        [ test "Can parse a simple program" <|
            \_ ->
                let
                    inputProgram =
                        "[+[<++>]--]"

                    expectedAST =
                        Ok <|
                            AbstractSyntaxTree
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
                Expect.equal expectedAST (Parser.parse inputProgram)
        ]
