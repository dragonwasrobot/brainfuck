module Brainfuck.VirtualMachine exposing (Cells, Index, VirtualMachine, getCell, init, setCell)

{-| Machine Model

The machine model we are going to use for this interpreter is very simple:

  - Our memory consists of 30,000 cells.

  - There's a data pointer which points to a specific cell and is initialized at
    the leftmost cell, an error will be reported if the pointer runs off the
    tape at either end.
    pointer = 0

  - A data cell is 8 bits (1 byte), and an error will be reported if the program
    tries to perform under- or overflow, i.e. decrement 0 or increment 255.

  - Two streams of bytes for input and output using the ASCII character
    encoding.

-}

import Array exposing (Array)
import Brainfuck.ASCII exposing (Byte)


type alias Index =
    Int


type alias VirtualMachine =
    { pointer : Index
    , cells : Cells
    , output : List Byte
    , input : List Byte
    }


init : List Byte -> VirtualMachine
init input =
    { pointer = 0
    , cells = initCells
    , output = []
    , input = input
    }


type alias Cells =
    Array (Array Byte)


rows : Int
rows =
    1000


columns : Int
columns =
    30


initCells : Cells
initCells =
    -- Tape size: 30_000 = rows * columns = 1000 * 30
    Array.initialize rows (\_ -> Array.initialize columns (\_ -> 0))


getCell : Index -> VirtualMachine -> Maybe Byte
getCell pointer vm =
    let
        oldCells =
            vm.cells

        rowIdx =
            pointer // columns

        columnIdx =
            modBy columns pointer

        maybeRow =
            Array.get rowIdx oldCells
    in
    case maybeRow of
        Nothing ->
            Nothing

        Just row ->
            Array.get columnIdx row


setCell : Index -> Byte -> VirtualMachine -> VirtualMachine
setCell pointer value vm =
    let
        oldCells =
            vm.cells

        rowIdx =
            pointer // columns

        columnIdx =
            modBy columns pointer

        maybeRow =
            Array.get rowIdx vm.cells

        updateRow oldRow =
            Array.set columnIdx value oldRow

        updateCells newRow =
            Array.set rowIdx newRow oldCells
    in
    case maybeRow of
        Nothing ->
            vm

        Just oldRow ->
            let
                newCells =
                    oldRow
                        |> updateRow
                        |> updateCells
            in
            { vm | cells = newCells }
