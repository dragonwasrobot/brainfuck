module EvaluatorTests exposing (..)

import Brainfuck.Evaluator as Evaluator exposing (ExpCrumb(..), ExpZipper)
import Brainfuck.Parser exposing (Command(..), Expression(..))
import Expect exposing (Expectation)
import Test exposing (..)


referenceAst =
    -- The program: [+[<++>]--]
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


suite : Test
suite =
    describe "Evaluator"
        [ describe "Zipper"
            [ test "Can move down to first child expression in block" <|
                \_ ->
                    let
                        inputExp =
                            ExpRoot
                                [ ExpCommand DecrementPointer
                                , ExpCommand IncrementByte
                                , ExpCommand IncrementByte
                                ]

                        inputZipper =
                            { expression = inputExp
                            , breadcrumbs = []
                            }

                        outputZipper =
                            Evaluator.childExp inputZipper

                        expectedZipper =
                            Just
                                { expression = ExpCommand DecrementPointer
                                , breadcrumbs =
                                    [ ExpCrumb True
                                        []
                                        [ ExpCommand IncrementByte
                                        , ExpCommand IncrementByte
                                        ]
                                    ]
                                }
                    in
                    Expect.equal expectedZipper outputZipper
            ]
        , test "Can move up to parent expression in block" <|
            \_ ->
                let
                    inputZipper =
                        { expression = ExpCommand IncrementByte
                        , breadcrumbs =
                            [ ExpCrumb True
                                [ ExpCommand IncrementByte
                                , ExpCommand DecrementPointer
                                ]
                                []
                            ]
                        }

                    outputZipper =
                        Evaluator.parentExp inputZipper

                    expectedZipper =
                        Just
                            { expression =
                                ExpRoot
                                    [ ExpCommand DecrementPointer
                                    , ExpCommand IncrementByte
                                    , ExpCommand IncrementByte
                                    ]
                            , breadcrumbs = []
                            }
                in
                Expect.equal expectedZipper outputZipper
        , test "Can move to next sibling expression in block" <|
            \_ ->
                let
                    inputZipper =
                        { expression = ExpCommand DecrementPointer
                        , breadcrumbs =
                            [ ExpCrumb False
                                []
                                [ ExpCommand IncrementByte
                                , ExpCommand IncrementByte
                                ]
                            ]
                        }

                    outputZipper =
                        Evaluator.nextSiblingExp inputZipper

                    expectedZipper =
                        Just
                            { expression = ExpCommand IncrementByte
                            , breadcrumbs =
                                [ ExpCrumb False
                                    [ ExpCommand DecrementPointer ]
                                    [ ExpCommand IncrementByte ]
                                ]
                            }
                in
                Expect.equal expectedZipper outputZipper
        , test "Moving right and left gives returns original zipper" <|
            \_ ->
                let
                    inputZipper =
                        { expression = ExpCommand DecrementPointer
                        , breadcrumbs =
                            [ ExpCrumb False
                                []
                                [ ExpCommand IncrementByte
                                , ExpCommand IncrementByte
                                ]
                            , ExpCrumb True
                                [ ExpCommand IncrementByte ]
                                []
                            ]
                        }

                    outputZipper =
                        inputZipper
                            |> Evaluator.nextSiblingExp
                            |> Maybe.andThen Evaluator.nextSiblingExp
                            |> Maybe.andThen Evaluator.parentExp
                            |> Maybe.andThen Evaluator.childExp

                    expectedZipper =
                        Just
                            { expression = ExpCommand DecrementPointer
                            , breadcrumbs =
                                [ ExpCrumb False
                                    []
                                    [ ExpCommand IncrementByte
                                    , ExpCommand IncrementByte
                                    ]
                                , ExpCrumb True
                                    [ ExpCommand IncrementByte ]
                                    []
                                ]
                            }
                in
                Expect.equal expectedZipper outputZipper
        , test "Can move to previous sibling expression in block" <|
            \_ ->
                let
                    inputZipper =
                        { expression = ExpCommand IncrementByte
                        , breadcrumbs =
                            [ ExpCrumb False
                                [ ExpCommand DecrementPointer ]
                                [ ExpCommand IncrementByte
                                ]
                            ]
                        }

                    outputZipper =
                        Evaluator.previousSiblingExp inputZipper

                    expectedZipper =
                        Just
                            { expression = ExpCommand DecrementPointer
                            , breadcrumbs =
                                [ ExpCrumb False
                                    []
                                    [ ExpCommand IncrementByte
                                    , ExpCommand IncrementByte
                                    ]
                                ]
                            }
                in
                Expect.equal expectedZipper outputZipper
        , test "Can't move to a parent expression of root" <|
            \_ ->
                let
                    inputZipper =
                        { expression = ExpRoot [ ExpCommand DecrementPointer ]
                        , breadcrumbs = []
                        }

                    outputZipper =
                        Evaluator.parentExp inputZipper

                    expectedZipper =
                        Nothing
                in
                Expect.equal expectedZipper outputZipper
        ]
