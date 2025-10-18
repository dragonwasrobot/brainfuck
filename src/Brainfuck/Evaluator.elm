module Brainfuck.Evaluator exposing (evaluate)

import Brainfuck.Ascii as Ascii
import Brainfuck.Parser as Parser exposing (AbstractSyntaxTree(..), Expression(..))
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import List.Extra as List exposing (Step(..))


evaluate : AbstractSyntaxTree -> VirtualMachine -> Result String VirtualMachine
evaluate (AbstractSyntaxTree exprs) vm =
    List.stoppableFoldl
        (\expr vmAcc ->
            case evaluateExpression expr vmAcc of
                Err reason ->
                    Stop (Err reason)

                Ok newVm ->
                    Continue (Ok newVm)
        )
        (Ok vm)
        exprs


evaluateExpression : Expression -> Result String VirtualMachine -> Result String VirtualMachine
evaluateExpression expr vmResult =
    case vmResult of
        Err reason ->
            Err reason

        Ok vm ->
            case expr of
                Block subExprs ->
                    evaluateBlock subExprs vm

                IncrementPointer ->
                    handleIncrementPointer vm

                DecrementPointer ->
                    handleDecrementPointer vm

                IncrementByte ->
                    handleIncrementByte vm

                DecrementByte ->
                    handleDecrementByte vm

                OutputByte ->
                    handleOutputByte vm

                InputByte ->
                    handleInputByte vm


evaluateBlock : List Expression -> VirtualMachine -> Result String VirtualMachine
evaluateBlock exprs vm =
    let
        pointer =
            vm.pointer

        cellValue =
            vm |> VirtualMachine.getCell pointer |> Maybe.withDefault 0
    in
    if cellValue > 0 then
        let
            vmResult =
                List.stoppableFoldl
                    (\expr vmAcc ->
                        case evaluateExpression expr vmAcc of
                            Err reason ->
                                Stop (Err reason)

                            Ok newVm ->
                                Continue (Ok newVm)
                    )
                    (Ok vm)
                    exprs
        in
        case vmResult of
            Err reason ->
                Err reason

            Ok newVm ->
                evaluateBlock exprs newVm

    else
        Ok vm


handleIncrementPointer : VirtualMachine -> Result String VirtualMachine
handleIncrementPointer vm =
    let
        tapeSize =
            30000

        newPointer =
            vm.pointer + 1

        newVm =
            { vm | pointer = newPointer }
    in
    if newPointer >= tapeSize then
        Err "Pointer ran off tape (right)"

    else
        Ok newVm


handleDecrementPointer : VirtualMachine -> Result String VirtualMachine
handleDecrementPointer vm =
    let
        newPointer =
            vm.pointer - 1

        newVm =
            { vm | pointer = newPointer }
    in
    if newPointer < 0 then
        Err "Pointer ran off tape (left)"

    else
        Ok newVm


handleIncrementByte : VirtualMachine -> Result String VirtualMachine
handleIncrementByte vm =
    let
        cellSize =
            255

        pointer =
            vm.pointer

        oldCellValue =
            vm
                |> VirtualMachine.getCell pointer
                |> Maybe.withDefault 0

        newCellValue =
            oldCellValue + 1

        newVm =
            VirtualMachine.setCell pointer newCellValue vm
    in
    if newCellValue > cellSize then
        Err "Integer overflow"

    else
        Ok newVm


handleDecrementByte : VirtualMachine -> Result String VirtualMachine
handleDecrementByte vm =
    let
        pointer =
            vm.pointer

        oldCellValue =
            vm
                |> VirtualMachine.getCell pointer
                |> Maybe.withDefault 0

        newCellValue =
            oldCellValue - 1

        newVm =
            vm |> VirtualMachine.setCell pointer newCellValue
    in
    if newCellValue < 0 then
        Err "Integer underflow"

    else
        Ok newVm


handleOutputByte : VirtualMachine -> Result String VirtualMachine
handleOutputByte vm =
    let
        pointer =
            vm.pointer

        cells =
            vm.cells

        cellValue =
            vm
                |> VirtualMachine.getCell pointer
                |> Maybe.withDefault 0
    in
    outputByte cellValue vm


outputByte : Int -> VirtualMachine -> Result String VirtualMachine
outputByte cellValue vm =
    case Ascii.toChar cellValue of
        Just char ->
            let
                newOutput =
                    vm.output ++ String.fromChar char
            in
            Ok { vm | output = newOutput }

        Nothing ->
            Err <|
                "Tried to output invalid ASCII char: '"
                    ++ String.fromInt cellValue
                    ++ "'"


handleInputByte : VirtualMachine -> Result String VirtualMachine
handleInputByte vm =
    case List.head vm.input of
        Nothing ->
            -- TODO: No input left -> halt program and prompt for more input!
            Err "Input empty!"

        Just byte ->
            let
                newInput =
                    List.drop 1 vm.input

                newVm =
                    vm |> VirtualMachine.setCell vm.pointer byte
            in
            Ok { newVm | input = newInput }
