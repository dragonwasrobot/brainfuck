module Main exposing (main)

import Browser
import Evaluator exposing (evaluate)
import Html exposing (Html, button, div, h1, text, textarea)
import Html.Attributes exposing (cols, disabled, id, rows, type_)
import Html.Events exposing (onClick, onInput)
import Lexer exposing (tokenize)
import Parser exposing (parse)
import VirtualMachine exposing (VirtualMachine, initVirtualMachine)



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Code =
    String


type alias Model =
    { vm : VirtualMachine
    , programCode : Code
    , error : Maybe String
    }


initModel : Model
initModel =
    { vm = initVirtualMachine
    , programCode = initialProgram
    , error = Nothing
    }


initialProgram : Code
initialProgram =
    "Print 'Hello world!'\n+++++ +++++             initialize counter (cell #0) to 10\n[                       use loop to set the next four cells to 70/100/30/10\n> +++++ ++              add  7 to cell #1\n> +++++ +++++           add 10 to cell #2\n> +++                   add  3 to cell #3\n> +                     add  1 to cell #4\n<<<< -                  decrement counter (cell #0)\n]\n> ++ .                  print 'H'\n> + .                   print 'e'\n+++++ ++ .              print 'l'\n.                       print 'l'\n+++ .                   print 'o'\n> ++ .                  print ' '\n<< +++++ +++++ +++++ .  print 'W'\n> .                     print 'o'\n+++ .                   print 'r'\n----- - .               print 'l'\n----- --- .             print 'd'\n> + .                   print '!'"


init : Model
init =
    initModel



-- Msg


type Msg
    = Evaluate
    | SetCode String



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Evaluate ->
            evaluateProgram model

        SetCode code ->
            setCode code model


evaluateProgram : Model -> Model
evaluateProgram model =
    let
        newVmResult =
            model.programCode
                |> tokenize
                |> parse
                |> Result.andThen (evaluate initVirtualMachine)
    in
    case newVmResult of
        Ok newVm ->
            { model | vm = newVm }

        Err error ->
            { model | error = Just error }


setCode : String -> Model -> Model
setCode code model =
    { model | programCode = code }



-- View


view : Model -> Html Msg
view model =
    div [ id "content" ]
        [ viewHeader
        , viewInterpreter model
        , viewButtons model
        ]


viewHeader : Html msg
viewHeader =
    h1 [] [ text "Brainfuck interpreter" ]


viewInterpreter : Model -> Html Msg
viewInterpreter model =
    div [ id "text-areas" ]
        [ textarea
            [ id "codeInput"
            , rows 25
            , cols 60
            , onInput SetCode
            ]
            [ text model.programCode ]
        , textarea
            [ id "codeOutput"
            , rows 25
            , cols 40
            ]
            [ case model.error of
                Just error ->
                    text error

                Nothing ->
                    text model.vm.output
            ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div [ id "button-area" ]
        [ button
            [ type_ "submit"
            , onClick Evaluate
            ]
            [ text "Evaluate" ]
        ]
