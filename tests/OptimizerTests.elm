module OptimizerTests exposing (..)

import Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))
import Brainfuck.Optimizer as Optimizer
import Brainfuck.Parser as Parser
import Expect
import Test exposing (..)


suite : Test
suite =
    let
        optimizeSource src =
            src
                |> Parser.parse
                |> Result.map Optimizer.optimize

        shouldMergeConsecutiveIncrements =
            test "should merge consecutive IncrementByte into AddToByte" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "+++"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok (ExpRoot [ ExpCommand (AddToByte 3) ])
                    in
                    Expect.equal expectedAST outputAST

        shouldMergeConsecutiveDecrements =
            test "should merge consecutive DecrementByte into RemoveFromByte" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "--"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok (ExpRoot [ ExpCommand (RemoveFromByte 2) ])
                    in
                    Expect.equal expectedAST outputAST

        shouldMergeConsecutiveIncrementPointer =
            test "should merge consecutive IncrementPointer into AddToPointer" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            ">>>"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok (ExpRoot [ ExpCommand (AddToPointer 3) ])
                    in
                    Expect.equal expectedAST outputAST

        shouldMergeConsecutiveDecrementPointer =
            test "should merge consecutive DecrementPointer into RemoveFromPointer" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "<<"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok (ExpRoot [ ExpCommand (RemoveFromPointer 2) ])
                    in
                    Expect.equal expectedAST outputAST

        shouldNotMergeAcrossDirections =
            test "should not merge across different directions" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "+-"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok
                                (ExpRoot
                                    [ ExpCommand IncrementByte
                                    , ExpCommand DecrementByte
                                    ]
                                )
                    in
                    Expect.equal expectedAST outputAST

        shouldNotMergePointerAcrossDirections =
            test "should not merge pointer across different directions" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            ">>><<"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok
                                (ExpRoot
                                    [ ExpCommand (AddToPointer 3)
                                    , ExpCommand (RemoveFromPointer 2)
                                    ]
                                )
                    in
                    Expect.equal expectedAST outputAST

        shouldKeepSingleCommandUnchanged =
            test "should keep single command as ExpCommand, not AddToByte 1" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "+"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok (ExpRoot [ ExpCommand IncrementByte ])
                    in
                    Expect.equal expectedAST outputAST

        shouldMergeInsideBlocks =
            test "should merge commands inside blocks" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            "[+++[--]++]"

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok
                                (ExpRoot
                                    [ ExpBlock
                                        [ ExpCommand (AddToByte 3)
                                        , ExpBlock [ ExpCommand (RemoveFromByte 2) ]
                                        , ExpCommand (AddToByte 2)
                                        ]
                                    ]
                                )
                    in
                    Expect.equal expectedAST outputAST

        shouldNotMergeOutputByte =
            test "should not merge consecutive OutputByte" <|
                \_ ->
                    let
                        -- Given
                        inputProgram =
                            ".."

                        -- When
                        outputAST =
                            optimizeSource inputProgram

                        -- Then
                        expectedAST =
                            Ok
                                (ExpRoot
                                    [ ExpCommand OutputByte
                                    , ExpCommand OutputByte
                                    ]
                                )
                    in
                    Expect.equal expectedAST outputAST
    in
    describe "Optimizer:"
        [ shouldMergeConsecutiveIncrements
        , shouldMergeConsecutiveDecrements
        , shouldMergeConsecutiveIncrementPointer
        , shouldMergeConsecutiveDecrementPointer
        , shouldNotMergeAcrossDirections
        , shouldNotMergePointerAcrossDirections
        , shouldKeepSingleCommandUnchanged
        , shouldMergeInsideBlocks
        , shouldNotMergeOutputByte
        ]
