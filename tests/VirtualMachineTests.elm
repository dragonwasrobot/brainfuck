module VirtualMachineTests exposing (..)

import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "VirtualMachine"
        [ test "Given an initialized virtual machine when fetching a cells then 0 is returned" <|
            \_ ->
                let
                    pointer =
                        62

                    initVm =
                        VirtualMachine.init []

                    initialLastByte =
                        VirtualMachine.getCell pointer initVm
                in
                Expect.equal initialLastByte (Just 0)
        , test "Given a valid cell index when setting a cell value then the set value is returned" <|
            \_ ->
                let
                    value =
                        7

                    pointer =
                        62

                    newVm =
                        VirtualMachine.init []
                            |> VirtualMachine.setCell pointer value

                    updatedByte =
                        VirtualMachine.getCell pointer newVm
                in
                Expect.equal updatedByte (Just 7)
        ]
