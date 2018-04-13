module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Lexer exposing (tokenize)
import Parser exposing (parse)
import Evaluator exposing (evaluate)


evaluateProgram : Model -> ( Model, Cmd Msg )
evaluateProgram model =
    let
        program =
            model.programCode

        vm =
            model.vm

        newVm =
            program
                |> tokenize
                |> parse
                |> evaluate vm

        newModel =
            { model | vm = newVm }
    in
        ( newModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Evaluate ->
            evaluateProgram model
