module VirtualMachine exposing (VirtualMachine, initVirtualMachine)

import Array exposing (Array)


{-| Machine Model

The machine model we are going to use for this interpreter is very simple:

  - Our memory consists of 100 cells (the original version uses 30000).

  - There's a data pointer which points to a specific cell and is initialized at
    the leftmost cell, an error will be reported if the pointer runs off the
    tape at either end.
    pointer = 0

  - A data cell is 8 bits, and an error will be reported if the program tries
    to perform under- or overflow, i.e. decrement 0 or increment 255.

  - Two streams of bytes for input and output using the ASCII character
    encoding.

-}
type alias VirtualMachine =
    { pointer : Int
    , cells : Array Int
    , output : String
    }


initVirtualMachine : VirtualMachine
initVirtualMachine =
    { pointer = 0
    , cells = Array.initialize 100 (\_ -> 0)
    , output = ""
    }
