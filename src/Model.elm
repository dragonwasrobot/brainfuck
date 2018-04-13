module Model exposing (Model, initModel, VirtualMachine)

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


type alias Code =
    String


type alias Model =
    { vm : VirtualMachine
    , programCode : Code
    }


initModel : Model
initModel =
    { vm = initVirtualMachine
    , programCode = initialProgram
    }


initialProgram : String
initialProgram =
    "Print 'Hello world!'\n+++++ +++++             initialize counter (cell #0) to 10\n[                       use loop to set the next four cells to 70/100/30/10\n> +++++ ++              add  7 to cell #1\n> +++++ +++++           add 10 to cell #2\n> +++                   add  3 to cell #3\n> +                     add  1 to cell #4\n<<<< -                  decrement counter (cell #0)\n]\n> ++ .                  print 'H'\n> + .                   print 'e'\n+++++ ++ .              print 'l'\n.                       print 'l'\n+++ .                   print 'o'\n> ++ .                  print ' '\n<< +++++ +++++ +++++ .  print 'W'\n> .                     print 'o'\n+++ .                   print 'r'\n----- - .               print 'l'\n----- --- .             print 'd'\n> + .                   print '!'\n> .                     print '\n'"
