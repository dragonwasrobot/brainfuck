module Evaluator exposing (evaluate)

import Array exposing (Array)
import Ascii
import Lexer exposing (Symbol(..))
import Parser exposing (AbstractSyntaxTree(..), Block, Command)
import VirtualMachine exposing (VirtualMachine)


evaluate : VirtualMachine -> AbstractSyntaxTree -> VirtualMachine
evaluate vm node =
    case node of
        Node block ->
            evaluateBlock block vm

        Leaf command ->
            evaluateCommand command vm


evaluateBlock : Block -> VirtualMachine -> VirtualMachine
evaluateBlock block vm =
    block.children
        |> List.foldl
            (\child accVm ->
                case child of
                    Leaf childCommand ->
                        evaluateCommand childCommand accVm

                    Node childBlock ->
                        evaluateChildBlock childBlock accVm
            )
            vm


evaluateChildBlock : Block -> VirtualMachine -> VirtualMachine
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
        evaluateChildBlock childBlock newAcc

    else
        vm


evaluateCommand : Command -> VirtualMachine -> VirtualMachine
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
            Debug.todo "Unexpected command!"

        EndBlock ->
            Debug.todo "Unexpected command!"


handleIncrementPointer : VirtualMachine -> VirtualMachine
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
        Debug.todo "Pointer ran off tape (right)"

    else
        newVm


handleDecrementPointer : VirtualMachine -> VirtualMachine
handleDecrementPointer vm =
    let
        newPointer =
            vm.pointer - 1

        newVm =
            { vm | pointer = newPointer }
    in
    if newPointer < 0 then
        Debug.todo "Pointer ran off tape (left)"

    else
        newVm


handleIncrementByte : VirtualMachine -> VirtualMachine
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
        Debug.todo "Integer overflow"

    else
        newVm


handleDecrementByte : VirtualMachine -> VirtualMachine
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
        Debug.todo "Integer underflow"

    else
        newVm


handleOutputByte : VirtualMachine -> VirtualMachine
handleOutputByte vm =
    let
        pointer =
            vm.pointer

        cells =
            vm.cells

        output =
            vm.output

        cellValue =
            cells
                |> Array.get pointer
                |> Maybe.withDefault 0
    in
    outputByte cellValue vm


outputByte : Int -> VirtualMachine -> VirtualMachine
outputByte cellValue vm =
    case Ascii.toChar cellValue of
        Just char ->
            let
                newOutput =
                    vm.output ++ String.fromChar char
            in
            { vm | output = newOutput }

        Nothing ->
            vm


handleInputByte : VirtualMachine -> VirtualMachine
handleInputByte vm =
    -- TODO
    vm
