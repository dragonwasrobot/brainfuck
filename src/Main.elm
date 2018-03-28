module Main exposing (..)

import Array exposing (Array)
import Html exposing (Html, div)
import Keyboard exposing (KeyCode)
import Ports


-- Lexer.elm


{-| Converts a sequence of characters, the program representation, into a
sequence of tokens.


# Symbols

The Brainfuck language consists of the following 8 symbols, each representing a
command:

[ '>' -- increment the data pointer
, '<' -- decrement the data pointer
, '+' -- increment the byte at the data pointer
, '-' -- decrement the byte at the data pointer
, '.' -- output the byte at the data pointer
, ',' -- input a byte and store it in the byte at the data pointer
, '[' -- jump forward past the matching `]` if the byte at data pointer is zero
, ']' -- jump back to the matching `[` unless the byte at data pointer is zero
]

-}
type Symbol
    = IncrementPointer
    | DecrementPointer
    | IncrementByte
    | DecrementByte
    | OutputByte
    | InputByte
    | StartBlock
    | EndBlock


{-|


# Tokens

Given the tokens for each of the commands,

-}
type alias Position =
    { row : Int, column : Int }


type alias Token =
    { value : Symbol
    , position : Position
    }



{- we construct a mapping between characters and their corresponding
   token symbols.
-}


charToSymbol : Char -> Maybe Symbol
charToSymbol symbol =
    case symbol of
        '>' ->
            Just IncrementPointer

        '<' ->
            Just DecrementPointer

        '+' ->
            Just IncrementByte

        '-' ->
            Just DecrementByte

        '.' ->
            Just OutputByte

        ',' ->
            Just InputByte

        '[' ->
            Just StartBlock

        ']' ->
            Just EndBlock

        _ ->
            Nothing


{-| tokenize

We introduce two variables, for keeping track of the row and column of each
symbol we encounter. Then, given a program, represented by a string,
`tokenizeProgram` returns a list of tokens.

-}
tokenize : String -> List Token
tokenize program =
    let
        initPos =
            { row = 1, column = 0 }

        tokenizeChar : Char -> Position -> ( Maybe Token, Position )
        tokenizeChar char position =
            let
                newPosition =
                    if char == '\n' then
                        { row = position.row + 1, column = 0 }
                    else
                        { row = position.row, column = position.column + 1 }
            in
                case charToSymbol char of
                    Just symbol ->
                        let
                            token =
                                { value = symbol
                                , position = position
                                }
                        in
                            ( Just token, newPosition )

                    Nothing ->
                        ( Nothing, newPosition )

        tokenizeProgram : String -> List Token
        tokenizeProgram program =
            program
                |> String.toList
                |> List.foldl
                    (\char ( tokens, pos ) ->
                        let
                            ( maybeToken, newPos ) =
                                tokenizeChar char pos
                        in
                            case maybeToken of
                                Just token ->
                                    ( tokens ++ [ token ], newPos )

                                Nothing ->
                                    ( tokens, newPos )
                    )
                    ( [], initPos )
                |> Tuple.first
    in
        tokenizeProgram program



-- Parser.elm


{-| The parser takes a sequence of tokens and constructs a parse tree, an
abstract syntax tree.


## Grammar

The grammar of brainfuck is very simple (we ignore comments):

    <program> ::= { <block> | <command> }
    <block>   ::= '[' { <block> | <command> } ']'
    <command> ::= '>' | '<' | '+' | '-' | '.' | ','

-}
type alias Command =
    { value : Symbol
    , position : Position
    }


type alias Block =
    { children : List AbstractSyntaxTree
    , position : Position
    }


type AbstractSyntaxTree
    = Leaf Command
    | Node Block


{-| The Parser construct an abstract syntax tree based on the list of tokens.
-}
parse : List Token -> AbstractSyntaxTree
parse tokens =
    let
        root =
            Node <|
                { position = { row = 0, column = 0 }
                , children = []
                }

        ( node, remainingTokens ) =
            parseTokens tokens root
    in
        if List.isEmpty remainingTokens then
            node
        else
            Debug.crash "Finished parsing too early!"


{-| `parseTokens` takes a list of tokens and returns an abstract syntax
tree of the program.
-}
parseTokens : List Token -> AbstractSyntaxTree -> ( AbstractSyntaxTree, List Token )
parseTokens tokensList node =
    case tokensList of
        [] ->
            ( node, [] )

        token :: tokens ->
            case token.value of
                StartBlock ->
                    parseStartBlock token tokens node

                EndBlock ->
                    parseEndBlock tokens node

                _ ->
                    parseCommand token tokens node


parseStartBlock :
    Token
    -> List Token
    -> AbstractSyntaxTree
    -> ( AbstractSyntaxTree, List Token )
parseStartBlock token tokens node =
    case node of
        Leaf _ ->
            Debug.crash "Unexpected command!"

        Node block ->
            let
                childBlock =
                    Node <|
                        { children = []
                        , position = token.position
                        }

                ( parsedChildBlock, remainingTokens ) =
                    parseTokens tokens childBlock

                newChildren =
                    block.children ++ [ parsedChildBlock ]

                newNode =
                    Node { block | children = newChildren }
            in
                parseTokens remainingTokens newNode


parseEndBlock :
    List Token
    -> AbstractSyntaxTree
    -> ( AbstractSyntaxTree, List Token )
parseEndBlock tokens node =
    ( node, tokens )


parseCommand :
    Token
    -> List Token
    -> AbstractSyntaxTree
    -> ( AbstractSyntaxTree, List Token )
parseCommand token tokens node =
    case node of
        Leaf _ ->
            Debug.crash "Enclosing block was command!"

        Node block ->
            let
                command =
                    Leaf <|
                        { value = token.value
                        , position = token.position
                        }

                newChildren =
                    block.children ++ [ command ]

                newNode =
                    Node { block | children = newChildren }
            in
                parseTokens tokens newNode



-- Evaluate.elm


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
    }


initVirtualMachine : VirtualMachine
initVirtualMachine =
    { pointer = 0
    , cells = Array.initialize 100 (\_ -> 0)
    }


evaluate : VirtualMachine -> AbstractSyntaxTree -> ( VirtualMachine, Cmd Msg )
evaluate vm node =
    case node of
        Node block ->
            evaluateBlock vm block

        Leaf command ->
            evaluateCommand vm command


evaluateBlock : VirtualMachine -> Block -> ( VirtualMachine, Cmd Msg )
evaluateBlock vm block =
    let
        evaluateChildCommand ( vm, cmds ) childCommand =
            let
                ( newVm, newCmd ) =
                    evaluateCommand vm childCommand
            in
                ( newVm, Cmd.batch [ newCmd, cmds ] )

        evaluateChildBlock ( vm, cmds ) childBlock =
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
                        ( newVm, newCmd ) =
                            evaluateBlock vm childBlock

                        newAcc =
                            ( newVm, Cmd.batch [ newCmd, cmds ] )
                    in
                        evaluateChildBlock newAcc childBlock
                else
                    ( vm, cmds )
    in
        List.foldl
            (\child ( accVm, accCmds ) ->
                case child of
                    Leaf childCommand ->
                        evaluateChildCommand ( accVm, accCmds ) childCommand

                    Node childBlock ->
                        evaluateChildBlock ( accVm, accCmds ) childBlock
            )
            ( vm, Cmd.none )
            block.children


evaluateCommand : VirtualMachine -> Command -> ( VirtualMachine, Cmd Msg )
evaluateCommand vm command =
    case command.value of
        IncrementPointer ->
            let
                newVm =
                    handleIncrementPointer vm
            in
                ( newVm, Cmd.none )

        DecrementPointer ->
            let
                newVm =
                    handleDecrementPointer vm
            in
                ( newVm, Cmd.none )

        IncrementByte ->
            let
                newVm =
                    handleIncrementByte vm
            in
                ( newVm, Cmd.none )

        DecrementByte ->
            let
                newVm =
                    handleDecrementByte vm
            in
                ( newVm, Cmd.none )

        OutputByte ->
            handleOutputByte vm

        InputByte ->
            handleInputByte vm

        StartBlock ->
            Debug.crash "Unexpected command!"

        EndBlock ->
            Debug.crash "Unexpected command!"


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
            Debug.crash "Pointer ran off tape (right)"
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
            Debug.crash "Pointer ran off tape (left)"
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
            (cells |> Array.get pointer |> Maybe.withDefault 0)

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
            Debug.crash "Integer overflow"
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
            (cells |> Array.get pointer |> Maybe.withDefault 0)

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
            Debug.crash "Integer underflow"
        else
            newVm


handleOutputByte : VirtualMachine -> ( VirtualMachine, Cmd Msg )
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
        ( vm, Ports.outputByte cellValue )


handleInputByte : VirtualMachine -> ( VirtualMachine, Cmd Msg )
handleInputByte vm =
    -- input = () -> prompt 'Input byte value> '
    -- () -> cells[pointer] = input()
    ( vm, Cmd.none )



-- Main.elm


type alias Model =
    { vm : VirtualMachine }


initModel : Model
initModel =
    { vm = initVirtualMachine }


type Msg
    = KeyPress KeyCode
    | Evaluate String


evaluateProgram : String -> Model -> ( Model, Cmd Msg )
evaluateProgram program model =
    let
        vm =
            model.vm

        ( newVm, cmds ) =
            program
                |> tokenize
                |> parse
                |> evaluate vm

        newModel =
            { model | vm = newVm }
    in
        ( newModel, cmds )


addKeyPress : KeyCode -> Model -> ( Model, Cmd Msg )
addKeyPress keyCode model =
    -- TODO
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Evaluate code ->
            evaluateProgram code model

        KeyPress keyCode ->
            addKeyPress keyCode model


view : Model -> Html Msg
view model =
    div [] []


init : ( Model, Cmd Msg )
init =
    let
        model =
            initModel
    in
        ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Keyboard.presses KeyPress
        , Ports.evaluate Evaluate
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> subscriptions)
        }
