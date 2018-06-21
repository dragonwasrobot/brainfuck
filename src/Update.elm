module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Lexer exposing (tokenize)
import Parser exposing (parse)
import Evaluator exposing (evaluate)
import VirtualMachine exposing (initVirtualMachine)


evaluateProgram : Model -> ( Model, Cmd Msg )
evaluateProgram model =
    let
        newVm =
            model.programCode
                |> tokenize
                |> parse
                |> evaluate initVirtualMachine

        newModel =
            { model | vm = newVm }
    in
        ( newModel, Cmd.none )


setCode : String -> Model -> ( Model, Cmd Msg )
setCode code model =
    ( { model | programCode = code }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Evaluate ->
            evaluateProgram model

        SetCode code ->
            setCode code model
