module ParserTests exposing (..)

import Brainfuck.Parser as Parser exposing (AbstractSyntaxTree(..), Expression(..))
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
                                [ Block
                                    [ IncrementByte
                                    , Block
                                        [ DecrementPointer
                                        , IncrementByte
                                        , IncrementByte
                                        , IncrementPointer
                                        ]
                                    , DecrementByte
                                    , DecrementByte
                                    ]
                                ]
                in
                Expect.equal expectedAST (Parser.parse inputProgram)
        ]
