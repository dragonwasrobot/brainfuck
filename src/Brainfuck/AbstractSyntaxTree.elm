module Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))

-- ** TYPES


type Expression
    = ExpRoot (List Expression)
    | ExpBlock (List Expression)
    | ExpCommand Command


type Command
    = IncrementPointer
    | DecrementPointer
    | IncrementByte
    | DecrementByte
    | OutputByte
    | InputByte
      -- Optimizer-produced (never emitted by parser)
    | AddToPointer Int
    | RemoveFromPointer Int
    | AddToByte Int
    | RemoveFromByte Int
