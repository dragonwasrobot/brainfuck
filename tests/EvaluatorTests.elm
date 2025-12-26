module EvaluatorTests exposing (..)

import Brainfuck.Evaluator as Evaluator exposing (Breadcrumbs, Crumb(..))
import Brainfuck.Parser exposing (AbstractSyntaxTree(..), Command(..), Expression(..))
import Expect exposing (Expectation)
import Test exposing (..)


referenceAst =
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


suite : Test
suite =
    describe "Evaluator"
        [ describe "followBreadcrumbs"
            [ test "Can follow to expression" <|
                \_ ->
                    let
                        inputBreadcrumbs =
                            [ Crumb 0, Crumb 1, Crumb 3 ]

                        expectedExpr =
                            Just <| ExpCommand IncrementPointer

                        outputExpr =
                            Evaluator.followBreadcrumbs referenceAst inputBreadcrumbs
                    in
                    Expect.equal expectedExpr outputExpr
            ]
        , describe "layBreadcrumb"
            [ test "Can find next expression: middle of block" <|
                \_ ->
                    let
                        inputExpr =
                            ExpCommand IncrementByte

                        inputBreadcrumbs =
                            [ Crumb 0, Crumb 1, Crumb 2 ]

                        expectedBreadcrumbs =
                            [ Crumb 0, Crumb 1, Crumb 3 ]

                        outputBreadcrumbs =
                            Evaluator.layBreadcrumb referenceAst inputExpr inputBreadcrumbs
                    in
                    Expect.equal expectedBreadcrumbs outputBreadcrumbs
            , test "Can find next expression: end of block" <|
                \_ ->
                    let
                        inputExpr =
                            ExpCommand IncrementPointer

                        inputBreadcrumbs =
                            [ Crumb 0, Crumb 1, Crumb 3 ]

                        expectedBreadcrumbs =
                            [ Crumb 0, Crumb 1 ]

                        outputBreadcrumbs =
                            Evaluator.layBreadcrumb referenceAst inputExpr inputBreadcrumbs
                    in
                    Expect.equal expectedBreadcrumbs outputBreadcrumbs
            ]
        ]
