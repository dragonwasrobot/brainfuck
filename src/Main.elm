module Main exposing (main)

import Browser
import Evaluator
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Lexer
import Parser
import VirtualMachine exposing (VirtualMachine)



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
    , result : Result String String
    }


init : Model
init =
    { vm = VirtualMachine.init
    , programCode = initialProgram
    , result = Ok ""
    }


initialProgram : Code
initialProgram =
    "Print 'Hello world!'\n+++++ +++++             initialize counter (cell #0) to 10\n[                       use loop to set the next four cells to 70/100/30/10\n> +++++ ++              add  7 to cell #1\n> +++++ +++++           add 10 to cell #2\n> +++                   add  3 to cell #3\n> +                     add  1 to cell #4\n<<<< -                  decrement counter (cell #0)\n]\n> ++ .                  print 'H'\n> + .                   print 'e'\n+++++ ++ .              print 'l'\n.                       print 'l'\n+++ .                   print 'o'\n> ++ .                  print ' '\n<< +++++ +++++ +++++ .  print 'W'\n> .                     print 'o'\n+++ .                   print 'r'\n----- - .               print 'l'\n----- --- .             print 'd'\n> + .                   print '!'"



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
                |> Lexer.tokenize
                |> Parser.parse
                |> Result.andThen (Evaluator.evaluate VirtualMachine.init)
    in
    case newVmResult of
        Ok newVm ->
            { model
                | vm = newVm
                , result = Ok newVm.output
            }

        Err error ->
            { model | result = Err error }


setCode : String -> Model -> Model
setCode code model =
    { model | programCode = code }



-- View


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.id "container"
        , Attr.class "flex min-h-screen bg-white w-3xl mx-auto shadow-lg border border-slate-200 font-mono"
        ]
        [ viewAside
        , viewBody model
        ]


viewAside : Html msg
viewAside =
    let
        paperTapeHoles =
            "text-sm bg-[url(./circle.svg)] bg-center bg-repeat-y bg-size-[1.25em]"

        paperTapeBorder =
            "w-1/11 border-[0.125em] border-dashed-75 border-l-0 border-t-0 border-b-0 border-black"
    in
    Html.aside
        [ Attr.id "paper-strip"
        , Attr.class <| paperTapeBorder ++ " " ++ paperTapeHoles
        ]
        []


viewBody : Model -> Html Msg
viewBody model =
    Html.div
        [ Attr.id "body"
        , Attr.class "flex flex-col w-10/11 bg-white mx-5 mt-4 tracking-wider"
        ]
        [ viewHeader
        , viewTitle
        , viewInterpreter model
        ]


viewHeader : Html msg
viewHeader =
    Html.div
        [ Attr.id "header-section"
        , Attr.class "w-full flex flex-row justify-between text-sm font-medium"
        ]
        [ Html.div [ Attr.class "text-left" ]
            [ Html.text "DRAGON & ROBOT"
            , Html.br [] []
            , Html.text "BUSINESS MACHINES INC."
            ]
        , Html.div [ Attr.class "text-right" ]
            [ Html.text "SECTION 4.2"
            , Html.br [] []
            , Html.text "SEP 1988"
            ]
        ]


viewTitle : Html msg
viewTitle =
    Html.div
        [ Attr.id "title-section"
        , Attr.class "mx-auto"
        ]
        [ Html.div
            [ Attr.id "title-box"
            , Attr.class "mt-4 px-6 py-2 text-sm text-center border border-[0.125em]"
            ]
            [ Html.text "DATA SHEET"
            , Html.br [] []
            , Html.text "MACHINE BF-4000"
            ]
        ]


viewInterpreter : Model -> Html Msg
viewInterpreter model =
    let
        viewSubHeader =
            Html.div [ Attr.id "subheader" ]
                [ Html.span [] [ Html.text "PROGRAM EXECUTION" ] ]

        viewPrompt =
            Html.div
                [ Attr.id "request"
                , Attr.class "mt-2 flex flex-row"
                ]
                [ Html.span [ Attr.class "w-1/6" ] [ Html.text "PROMPT" ]
                , Html.textarea
                    [ Attr.id "code-input"
                    , Attr.class "w-5/6 border-none outline-none resize-none"
                    , Attr.rows 22
                    , Attr.cols 60
                    , Events.onInput SetCode
                    ]
                    [ Html.text model.programCode ]
                ]

        viewRun =
            -- TODO: Replace with a key listener on CTRL + ENTER
            Html.div
                [ Attr.id "run"
                , Attr.class "mt-2"
                ]
                [ Html.button
                    [ Attr.type_ "submit"
                    , Attr.class "p-2 border cursor-pointer"
                    , Events.onClick Evaluate
                    ]
                    [ Html.i [ Attr.class "fa-solid fa-play mr-2" ] []
                    , Html.text "RUN"
                    ]
                ]

        viewResult =
            let
                ( status, response ) =
                    case model.result of
                        Err error ->
                            ( "ERROR", error )

                        Ok result ->
                            ( "SUCCESS", result )
            in
            Html.div []
                [ Html.div
                    [ Attr.id "response-type"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "RESPONSE" ]
                    , Html.span [ Attr.class "w-5/6" ] [ Html.text "TEXT" ]
                    ]
                , Html.div
                    [ Attr.id "response-status"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "STATUS" ]
                    , Html.span [ Attr.class "w-5/6" ] [ Html.text status ]
                    ]
                , Html.div
                    [ Attr.id "response-body"
                    , Attr.class "flex flex-row"
                    ]
                    [ Html.span [ Attr.class "w-1/6" ] [ Html.text "DATA" ]
                    , Html.pre
                        [ Attr.id "code-output"
                        , Attr.class "w-5/6 border-none outline-none resize-none"
                        ]
                        [ Html.text response ]
                    ]
                ]

        viewDescription =
            Html.div [ Attr.id "function-description" ]
                [ Html.text "Once the PROMPT text has been entered, hit CTRL plus ENTER to send the program from the terminal to the BF-4000 mainframe for execution. Once completed, the STATUS field will display the message SUCCESS along with the output of the program in the DATA field."
                , Html.br [] []
                , Html.br [] []
                , Html.text "If the STATUS field displays ERROR then the DATA field includes a description of the error encountered during execution."
                , Html.br [] []
                , Html.br [] []
                , Html.text "A reference to the BF-4000 machine language can be found on pages 138-149."
                ]

        viewFooter =
            Html.div
                [ Attr.id "footer"
                , Attr.class "mt-4 mb-4 flex flex-row justify-between"
                ]
                [ Html.span [] [ Html.text "DENMARK" ]
                , Html.span [ Attr.class "font-semibold" ] [ Html.text "Computation as a Service™" ]
                , Html.span [] [ Html.text "Page 127" ]
                ]
    in
    Html.div
        [ Attr.id "interpreter-section"
        , Attr.class "mt-4 text-sm"
        ]
        [ viewSubHeader
        , Html.hr [ Attr.class "mt-1.5" ] []
        , Html.hr [ Attr.class "mt-0.25" ] []
        , viewPrompt
        , viewRun
        , Html.hr [ Attr.class "my-2" ] []
        , viewResult
        , Html.hr [ Attr.class "my-2" ] []
        , viewDescription
        , Html.hr [ Attr.class "my-2" ] []
        , viewFooter
        ]
