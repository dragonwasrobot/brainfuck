module Evaluator exposing (evaluate)

import Array
import Ascii
import Lexer exposing (Symbol(..))
import Parser exposing (AbstractSyntaxTree(..), Block, Command)
import VirtualMachine exposing (VirtualMachine)


evaluate : VirtualMachine -> AbstractSyntaxTree -> Result String VirtualMachine
evaluate vm node =
    case node of
        Node block ->
            evaluateBlock block vm

        Leaf command ->
            evaluateCommand command vm


evaluateBlock : Block -> VirtualMachine -> Result String VirtualMachine
evaluateBlock block vm =
    block.children
        |> List.foldl
            (\child accVm ->
                case child of
                    Leaf childCommand ->
                        Result.andThen (evaluateCommand childCommand) accVm

                    Node childBlock ->
                        Result.andThen (evaluateChildBlock childBlock) accVm
            )
            (Ok vm)


evaluateChildBlock : Block -> VirtualMachine -> Result String VirtualMachine
evaluateChildBlock childBlock vm =
    let
        cells =
            vm.cells

        pointer =
            vm.pointer

        cellValue =
            cells |> Array.get pointer |> Maybe.withDefault 0
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
            100

        newPointer =
            vm.pointer + 1

        newVm =
            { vm | pointer = newPointer }
    in
    if newPointer > tapeSize then
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

        cells =
            vm.cells

        pointer =
            vm.pointer

        oldCellValue =
            cells |> Array.get pointer |> Maybe.withDefault 0

        newCellValue =
            oldCellValue + 1

        newCells =
            Array.set
                pointer
                newCellValue
                cells

        newVm =
            { vm | cells = newCells }
    in
    if newCellValue > cellSize then
        Err "Integer overflow"

    else
        Ok newVm


handleDecrementByte : VirtualMachine -> Result String VirtualMachine
handleDecrementByte vm =
    let
        cells =
            vm.cells

        pointer =
            vm.pointer

        oldCellValue =
            cells |> Array.get pointer |> Maybe.withDefault 0

        newCellValue =
            oldCellValue - 1

        newCells =
            Array.set
                pointer
                newCellValue
                cells

        newVm =
            { vm | cells = newCells }
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
            cells
                |> Array.get pointer
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
    -- We do not currently do anything with input bytes.
    Ok vm
