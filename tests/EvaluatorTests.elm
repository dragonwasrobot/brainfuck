module EvaluatorTests exposing (..)

import Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))
import Brainfuck.Evaluator as Evaluator exposing (ExpCrumb(..), ExpZipper)
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
    let
        shouldMoveDownToFirstChild =
            test "should move down to first child expression in block" <|
                \_ ->
                    let
                        -- Given
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

                        -- When
                        outputZipper =
                            Evaluator.childExp inputZipper

                        -- Then
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

        shouldMoveUpToParent =
            test "should move up to parent expression in block" <|
                \_ ->
                    let
                        -- Given
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

                        -- When
                        outputZipper =
                            Evaluator.parentExp inputZipper

                        -- Then
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

        shouldMoveToNextSibling =
            test "should move to next sibling expression in block" <|
                \_ ->
                    let
                        -- Given
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

                        -- When
                        outputZipper =
                            Evaluator.nextSiblingExp inputZipper

                        -- Then
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

        shouldMoveBackToOriginal =
            test "should move back to original when going right then left" <|
                \_ ->
                    let
                        -- Given
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

                        -- When
                        outputZipper =
                            inputZipper
                                |> Evaluator.nextSiblingExp
                                |> Maybe.andThen Evaluator.nextSiblingExp
                                |> Maybe.andThen Evaluator.parentExp
                                |> Maybe.andThen Evaluator.childExp

                        -- Then
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

        shouldMoveToPreviousSibling =
            test "should move to previous sibling expression in block" <|
                \_ ->
                    let
                        -- Given
                        inputZipper =
                            { expression = ExpCommand IncrementByte
                            , breadcrumbs =
                                [ ExpCrumb False
                                    [ ExpCommand DecrementPointer ]
                                    [ ExpCommand IncrementByte
                                    ]
                                ]
                            }

                        -- When
                        outputZipper =
                            Evaluator.previousSiblingExp inputZipper

                        -- Then
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

        shouldNotMoveBeyondRoot =
            test "should not move further up than root" <|
                \_ ->
                    let
                        -- Given
                        inputZipper =
                            { expression = ExpRoot [ ExpCommand DecrementPointer ]
                            , breadcrumbs = []
                            }

                        -- When
                        outputZipper =
                            Evaluator.parentExp inputZipper

                        -- Then
                        expectedZipper =
                            Nothing
                    in
                    Expect.equal expectedZipper outputZipper
    in
    describe "Evaluator: "
        [ describe "Zipper"
            [ shouldMoveDownToFirstChild
            , shouldMoveUpToParent
            , shouldMoveToNextSibling
            , shouldMoveBackToOriginal
            , shouldMoveToPreviousSibling
            , shouldNotMoveBeyondRoot
            ]
        ]
