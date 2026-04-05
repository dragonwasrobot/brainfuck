module Brainfuck.Optimizer exposing (optimize)

import Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))


{-| Optimize an `Expression` tree by merging consecutive identical
increment/decrement commands into a single bulk command.

For example, three consecutive `IncrementByte` commands become one `AddToByte
3`.

The parser output is unchanged; this is an optimal pass between parsing and
evaluation.

-}
optimize : Expression -> Expression
optimize expr =
    case expr of
        ExpRoot children ->
            children
                |> List.map optimize
                |> List.foldr mergeStep []
                |> ExpRoot

        ExpBlock children ->
            children
                |> List.map optimize
                |> List.foldr mergeStep []
                |> ExpBlock

        ExpCommand _ ->
            expr


mergeStep : Expression -> List Expression -> List Expression
mergeStep expr acc =
    case ( expr, acc ) of
        ( ExpCommand IncrementPointer, (ExpCommand IncrementPointer) :: rest ) ->
            ExpCommand (AddToPointer 2) :: rest

        ( ExpCommand IncrementPointer, (ExpCommand (AddToPointer n)) :: rest ) ->
            ExpCommand (AddToPointer (n + 1)) :: rest

        ( ExpCommand DecrementPointer, (ExpCommand DecrementPointer) :: rest ) ->
            ExpCommand (RemoveFromPointer 2) :: rest

        ( ExpCommand DecrementPointer, (ExpCommand (RemoveFromPointer n)) :: rest ) ->
            ExpCommand (RemoveFromPointer (n + 1)) :: rest

        ( ExpCommand IncrementByte, (ExpCommand IncrementByte) :: rest ) ->
            ExpCommand (AddToByte 2) :: rest

        ( ExpCommand IncrementByte, (ExpCommand (AddToByte n)) :: rest ) ->
            ExpCommand (AddToByte (n + 1)) :: rest

        ( ExpCommand DecrementByte, (ExpCommand DecrementByte) :: rest ) ->
            ExpCommand (RemoveFromByte 2) :: rest

        ( ExpCommand DecrementByte, (ExpCommand (RemoveFromByte n)) :: rest ) ->
            ExpCommand (RemoveFromByte (n + 1)) :: rest

        _ ->
            expr :: acc
