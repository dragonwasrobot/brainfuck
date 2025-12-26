module Brainfuck.Evaluator exposing
    ( Breadcrumbs
    , Crumb(..)
    , EvaluationContext
    , State(..)
    , evaluate
    , evaluateStep
    , followBreadcrumbs
    , initContext
    , layBreadcrumb
    )

import Brainfuck.Ascii as Ascii
import Brainfuck.Parser exposing (AbstractSyntaxTree(..), Command(..), Expression(..))
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import List
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe



-- ** Types


type alias EvaluationContext =
    { ast : AbstractSyntaxTree
    , state : State
    , vm : VirtualMachine
    , breadcrumbs : Breadcrumbs
    }


initContext : AbstractSyntaxTree -> VirtualMachine -> EvaluationContext
initContext ast vm =
    { ast = ast
    , state = Running
    , vm = vm
    , breadcrumbs = []
    }


type State
    = Crashed String
    | Finished
    | Running


{-| We define a single (bread) crumb as a list of integer indices.

If we look at the code snippet:

    +++
    [
      > +++++ ++
      > +
      [>>>]
    ]

we can represent the first parts of it as the `AbstractSyntaxTree`:

    AbstractSyntaxTree [ ExpCommand IncrementByte
                       , ExpCommand IncrementByte
                       , ExpCommand IncrementByte
                       , ExpBlock [ IncrementPointer
                                  , IncrementByte
                                  , IncrementByte
                                  ...
                                  ]
                       ...
                       ]

If we start evaluating this we get the following breadcrumbs:

      []   -- Initial step of program
      [Crumb 0]  -- IncrementByte
      [Crumb 1]  -- IncrementByte
      [Crumb 2]  -- IncrementByte
      [Crumb 3]  -- Initial step of ExpBlock
      [Crumb 3, Crumb 0]  -- IncrementPointer inside ExpBlock
      [Crumb 3, Crumb 1]  -- IncrementByte inside ExpBlock
      [Crumb 3, Crumb 2]  -- IncrementByte inside ExpBlock
      [Crumb 3]  -- recheck ExpBlock
      []  -- Final step of program
      ...

Using this relatively simple structure of integer indices where the absence of
an integer means we are in an initial state of the program or block should be
enough to cover the limited control flow of Brainfuck.

-}
type alias Breadcrumbs =
    List Crumb


type Crumb
    = Crumb Int



-- ** Functions


isRunning : EvaluationContext -> Bool
isRunning context =
    context.state == Running


evaluate : EvaluationContext -> EvaluationContext
evaluate context =
    if isRunning context then
        context
            |> evaluateStep
            |> evaluate

    else
        context


evaluateStep : EvaluationContext -> EvaluationContext
evaluateStep context =
    let
        optExpr =
            followBreadcrumbs context.ast (Debug.log "breadcrumbs" context.breadcrumbs)
    in
    if not <| isRunning context then
        context

    else
        case optExpr of
            Nothing ->
                { context | state = Crashed "Failed to follow breadcrumbs" }

            Just expr ->
                evaluateExpression expr context


evaluateExpression : Expression -> EvaluationContext -> EvaluationContext
evaluateExpression expr context =
    case expr of
        ExpBlock subExprs ->
            evaluateBlock subExprs context

        ExpCommand command ->
            evaluateCommand command context


evaluateBlock : List Expression -> EvaluationContext -> EvaluationContext
evaluateBlock exprs context =
    let
        vm =
            context.vm

        pointer =
            vm.pointer

        cellValue =
            vm |> VirtualMachine.getCell pointer |> Maybe.withDefault 0
    in
    if List.isEmpty context.breadcrumbs || cellValue > 0 then
        { context | breadcrumbs = layBreadcrumb context.ast (ExpBlock exprs) context.breadcrumbs }

    else
        let
            -- TODO Need to do something cleaner
            bogusExp =
                ExpCommand IncrementByte
        in
        { context | breadcrumbs = layBreadcrumb context.ast bogusExp context.breadcrumbs }


evaluateCommand : Command -> EvaluationContext -> EvaluationContext
evaluateCommand command context =
    let
        evalCommand vm =
            case command of
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
    in
    case evalCommand context.vm of
        Err reason ->
            { context | state = Crashed reason }

        Ok newVm ->
            let
                newBreadcrumbs =
                    layBreadcrumb
                        context.ast
                        (ExpCommand command)
                        context.breadcrumbs
            in
            if List.isEmpty newBreadcrumbs then
                { context | vm = newVm, state = Finished }

            else
                { context | vm = newVm, breadcrumbs = newBreadcrumbs }


{-| Follows `Breadcrumbs` down through the `AbstractSyntaxTree`.

Cases:

  - Base: Breadcrumbs == [] -> eval block expression.
  - Ind1: Breadcrumbs == [idx] -> eval expression at index `idx` in block.
  - Ind2: Breadcrumbs == [idx1 | idx2] -> eval expr at index `idx2` inside block at index `idx1`.

-}
followBreadcrumbs : AbstractSyntaxTree -> Breadcrumbs -> Maybe Expression
followBreadcrumbs (AbstractSyntaxTree exprs) breadcrumbs =
    let
        followCrumb : Crumb -> Maybe Expression -> Maybe Expression
        followCrumb (Crumb index) =
            Maybe.andThen
                (\exp ->
                    case exp of
                        ExpBlock subExprs ->
                            List.getAt index subExprs

                        ExpCommand command ->
                            Just (ExpCommand command)
                )
    in
    List.foldl
        followCrumb
        (Just (ExpBlock exprs))
        breadcrumbs


parentExpression : AbstractSyntaxTree -> Breadcrumbs -> Maybe Expression
parentExpression ast breadcrumbs =
    breadcrumbs
        |> List.unconsLast
        |> Maybe.andThen
            (\( _, parentcrumbs ) ->
                followBreadcrumbs ast parentcrumbs
            )


{-| Lays new `Breadcrumbs` down through the `AbstractSyntaxTree`.

Cases:

  - expr == ExpBlock -> breadcrumbs |> push 0
  - expr == ExpCommand (middle of tree) -> breadcrumbs |> get lastIdx |> plus 1
  - expr == ExpCommand (last leaf) -> breadcrumbs |> pop lastIdx

-}
layBreadcrumb : AbstractSyntaxTree -> Expression -> Breadcrumbs -> Breadcrumbs
layBreadcrumb ast expr breadcrumbs =
    let
        optParentExpr =
            parentExpression ast breadcrumbs

        optLastCrumbValue =
            breadcrumbs |> List.last |> Maybe.unwrap 0 (\(Crumb idx) -> idx)

        hasNextSibling =
            optParentExpr
                |> Maybe.unwrap False
                    (\parentExpr ->
                        case parentExpr of
                            ExpCommand _ ->
                                False

                            ExpBlock exprs ->
                                List.length exprs - 1 > optLastCrumbValue
                    )
    in
    case expr of
        ExpBlock _ ->
            List.append breadcrumbs [ Crumb 0 ]

        ExpCommand _ ->
            if hasNextSibling then
                List.updateAt (List.length breadcrumbs - 1)
                    (\(Crumb idx) -> Crumb (idx + 1))
                    breadcrumbs

            else
                breadcrumbs
                    |> List.unconsLast
                    |> Maybe.unwrap [] Tuple.second


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
            let
                newOutput =
                    vm.output ++ "0x" ++ String.fromInt cellValue
            in
            Ok { vm | output = newOutput }


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
