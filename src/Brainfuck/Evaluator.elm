module Brainfuck.Evaluator exposing (evaluate)

import Array
import Brainfuck.Ascii as Ascii
import Brainfuck.Lexer as Lexer exposing (Symbol(..))
import Brainfuck.Parser as Parser exposing (AbstractSyntaxTree(..), Block, Command)
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)


evaluate : AbstractSyntaxTree -> Result String VirtualMachine -> Result String VirtualMachine
evaluate node vmResult =
    vmResult
        |> Result.andThen
            (\vm ->
                case node of
                    Node block ->
                        evaluateBlock block vm

                    Leaf command ->
                        evaluateCommand command vm
            )


evaluateBlock : Block -> VirtualMachine -> Result String VirtualMachine
evaluateBlock block vm =
    List.foldl
        (\child accVm ->
            case child of
                Leaf childCommand ->
                    Result.andThen (evaluateCommand childCommand) accVm

                Node childBlock ->
                    Result.andThen (evaluateChildBlock childBlock) accVm
        )
        (Ok vm)
        block.children


evaluateChildBlock : Block -> VirtualMachine -> Result String VirtualMachine
evaluateChildBlock childBlock vm =
    let
        pointer =
            vm.pointer

        cellValue =
            vm |> VirtualMachine.getCell pointer |> Maybe.withDefault 0
    in
    if cellValue > 0 then
        let
            newAcc =
                evaluateBlock childBlock vm
        in
        newAcc
            |> Result.andThen (evaluateChildBlock childBlock)

    else
        Ok vm


evaluateCommand : Command -> VirtualMachine -> Result String VirtualMachine
evaluateCommand command vm =
    case command.value of
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

        StartBlock ->
            Err "Unexpected command: start block!"

        EndBlock ->
            Err "Unexpected command: end block!"


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
            -- TODO: No input left!
            Err "Input empty!"

        Just byte ->
            let
                newInput =
                    List.drop 1 vm.input

                newVm =
                    vm |> VirtualMachine.setCell vm.pointer byte
            in
            Ok { newVm | input = newInput }
