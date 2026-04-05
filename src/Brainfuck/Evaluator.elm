module Brainfuck.Evaluator exposing
    ( EvaluationContext
    , ExpCrumb(..)
    , ExpZipper
    , State(..)
    , childExp
    , evaluate
    , evaluateStep
    , initContext
    , nextSiblingExp
    , parentExp
    , previousSiblingExp
    )

import Brainfuck.ASCII as ASCII exposing (ASCII, Byte)
import Brainfuck.AbstractSyntaxTree exposing (Command(..), Expression(..))
import Brainfuck.VirtualMachine as VirtualMachine exposing (VirtualMachine)
import List
import List.Extra as List exposing (Step(..))
import Maybe.Extra as Maybe



-- ** TYPES


type alias EvaluationContext =
    { state : State
    , vm : VirtualMachine
    , zipper : ExpZipper
    }


initContext : Expression -> VirtualMachine -> EvaluationContext
initContext expr vm =
    { state = Running
    , vm = vm
    , zipper =
        { expression = expr
        , breadcrumbs = []
        }
    }


type State
    = Running
    | Paused
    | Finished
    | Crashed String


type ExpCrumb
    = ExpCrumb Bool (List Expression) (List Expression)


type alias ExpZipper =
    { expression : Expression, breadcrumbs : List ExpCrumb }


parentExp : ExpZipper -> Maybe ExpZipper
parentExp { expression, breadcrumbs } =
    case breadcrumbs of
        [] ->
            Nothing

        (ExpCrumb isRoot lefts rights) :: crumbs ->
            let
                newExp =
                    if isRoot then
                        ExpRoot (List.reverse (expression :: lefts) ++ rights)

                    else
                        ExpBlock (List.reverse (expression :: lefts) ++ rights)
            in
            Just <| ExpZipper newExp crumbs


childExp : ExpZipper -> Maybe ExpZipper
childExp { expression, breadcrumbs } =
    let
        firstChildExp isRoot childExps =
            case childExps of
                [] ->
                    Nothing

                child :: children ->
                    let
                        newBreadcrumbs =
                            ExpCrumb isRoot [] children :: breadcrumbs
                    in
                    Just { expression = child, breadcrumbs = newBreadcrumbs }
    in
    case expression of
        ExpRoot childExps ->
            firstChildExp True childExps

        ExpBlock childExps ->
            firstChildExp False childExps

        ExpCommand command ->
            Nothing


nextSiblingExp : ExpZipper -> Maybe ExpZipper
nextSiblingExp { expression, breadcrumbs } =
    let
        nextSibling (ExpCrumb isRoot lefts rights) crumbs =
            case rights of
                [] ->
                    Nothing

                newExpression :: newRights ->
                    let
                        newCrumb =
                            ExpCrumb isRoot (expression :: lefts) newRights
                    in
                    Just { expression = newExpression, breadcrumbs = newCrumb :: crumbs }
    in
    case ( expression, breadcrumbs ) of
        ( ExpRoot _, _ ) ->
            -- Root -> no siblings
            Nothing

        ( _, [] ) ->
            -- No crumbs -> no siblings
            Nothing

        ( _, crumb :: crumbs ) ->
            nextSibling crumb crumbs


previousSiblingExp : ExpZipper -> Maybe ExpZipper
previousSiblingExp { expression, breadcrumbs } =
    let
        previousSibling (ExpCrumb isRoot lefts rights) crumbs =
            case lefts of
                [] ->
                    Nothing

                newExpression :: newLefts ->
                    let
                        newCrumb =
                            ExpCrumb isRoot newLefts (expression :: rights)
                    in
                    Just { expression = newExpression, breadcrumbs = newCrumb :: crumbs }
    in
    case ( expression, breadcrumbs ) of
        ( ExpRoot _, _ ) ->
            -- Root -> no siblings
            Nothing

        ( _, [] ) ->
            -- No crumbs -> no siblings
            Nothing

        ( _, crumb :: crumbs ) ->
            previousSibling crumb crumbs


nextExp : ExpZipper -> Maybe ExpZipper
nextExp zipper =
    let
        isExpRoot exp =
            case exp of
                ExpRoot _ ->
                    True

                _ ->
                    False

        optNewZipper =
            case nextSiblingExp zipper of
                Just newZipper ->
                    Just newZipper

                Nothing ->
                    parentExp zipper
    in
    optNewZipper
        |> Maybe.andThen
            (\newZipper ->
                if isExpRoot newZipper.expression then
                    Nothing

                else
                    Just newZipper
            )



-- ** FUNCTIONS


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
        zipper =
            context.zipper
    in
    if not <| isRunning context then
        context

    else
        evaluateExpression context


evaluateExpression : EvaluationContext -> EvaluationContext
evaluateExpression context =
    case context.zipper.expression of
        ExpRoot subExprs ->
            evaluateRoot subExprs context

        ExpBlock subExprs ->
            evaluateBlock subExprs context

        ExpCommand command ->
            evaluateCommand command context


evaluateRoot : List Expression -> EvaluationContext -> EvaluationContext
evaluateRoot exprs context =
    let
        oldZipper =
            context.zipper

        optNewZipper =
            childExp oldZipper
    in
    case optNewZipper of
        Nothing ->
            { context | state = Crashed "No program to run!" }

        Just newZipper ->
            { context | zipper = newZipper }


evaluateBlock : List Expression -> EvaluationContext -> EvaluationContext
evaluateBlock exprs context =
    let
        oldZipper =
            context.zipper

        cellValue =
            context.vm
                |> VirtualMachine.getCell context.vm.pointer
                |> Maybe.withDefault 0
    in
    if cellValue > 0 then
        case childExp oldZipper of
            Nothing ->
                { context | state = Crashed "No program to run!" }

            Just newZipper ->
                { context | zipper = newZipper }

    else
        case nextExp oldZipper of
            Nothing ->
                { context | state = Finished }

            Just newZipper ->
                { context | zipper = newZipper }


evaluateCommand : Command -> EvaluationContext -> EvaluationContext
evaluateCommand command context =
    let
        evalCommand vm =
            case command of
                IncrementPointer ->
                    handleAddToPointer 1 vm

                DecrementPointer ->
                    handleRemoveFromPointer 1 vm

                IncrementByte ->
                    handleAddToByte 1 vm

                DecrementByte ->
                    handleRemoveFromByte 1 vm

                OutputByte ->
                    handleOutputByte vm

                InputByte ->
                    handleInputByte vm

                -- Optimizer-produced
                AddToPointer count ->
                    handleAddToPointer count vm

                RemoveFromPointer count ->
                    handleRemoveFromPointer count vm

                AddToByte count ->
                    handleAddToByte count vm

                RemoveFromByte count ->
                    handleRemoveFromByte count vm
    in
    case evalCommand context.vm of
        Err reason ->
            { context | state = Crashed reason }

        Ok newVm ->
            case nextExp context.zipper of
                Nothing ->
                    { context | vm = newVm, state = Finished }

                Just newZipper ->
                    { context | vm = newVm, zipper = newZipper }


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


outputByte : Byte -> VirtualMachine -> Result String VirtualMachine
outputByte cellValue vm =
    let
        newOutput =
            vm.output ++ [ cellValue ]
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


handleAddToPointer : Int -> VirtualMachine -> Result String VirtualMachine
handleAddToPointer count vm =
    let
        tapeSize =
            30000

        newPointer =
            vm.pointer + count
    in
    if newPointer >= tapeSize then
        Err "Pointer ran off tape (right)"

    else
        Ok { vm | pointer = newPointer }


handleRemoveFromPointer : Int -> VirtualMachine -> Result String VirtualMachine
handleRemoveFromPointer count vm =
    let
        newPointer =
            vm.pointer - count
    in
    if newPointer < 0 then
        Err "Pointer ran off tape (left)"

    else
        Ok { vm | pointer = newPointer }


handleAddToByte : Int -> VirtualMachine -> Result String VirtualMachine
handleAddToByte count vm =
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
            oldCellValue + count
    in
    if newCellValue > cellSize then
        Err "Integer overflow"

    else
        Ok (VirtualMachine.setCell pointer newCellValue vm)


handleRemoveFromByte : Int -> VirtualMachine -> Result String VirtualMachine
handleRemoveFromByte count vm =
    let
        pointer =
            vm.pointer

        oldCellValue =
            vm
                |> VirtualMachine.getCell pointer
                |> Maybe.withDefault 0

        newCellValue =
            oldCellValue - count
    in
    if newCellValue < 0 then
        Err "Integer underflow"

    else
        Ok (VirtualMachine.setCell pointer newCellValue vm)
